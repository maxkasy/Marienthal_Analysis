# Generating aggregate files separately for Marienthal and control towns
# The following loops over the entire aggregation process for both groups

for (year in c(2021, 2022)){
for (sample_group in c("Participants", "Control-towns")) {
    if (year == 2022){
        source_file = paste0(veracrypt_path, "jobguarantee/2022-02-survey-data-raw/linked-by-AMS/MAGMA-", sample_group, "-Feb2022-Survey.csv")
        destination_file = paste0(veracrypt_path, "jobguarantee/2022-02-survey-data-processed/MAGMA-", sample_group, "-Aggregated-Survey.csv")
        destination_file_detailed = paste0(veracrypt_path, "jobguarantee/2022-02-survey-data-processed/MAGMA-", sample_group, "-Aggregated_detailed.csv")
    } else {
        source_file = paste0(veracrypt_path, "jobguarantee/2021-02-survey-data-raw/MAGMA-", sample_group, "-Feb2021-Numeric.csv")
        destination_file = paste0(veracrypt_path, "jobguarantee/2021-02-survey-data-processed/MAGMA-", sample_group, "-Aggregated-Survey.csv")
        destination_file_detailed = paste0(veracrypt_path, "jobguarantee/2021-02-survey-data-processed/MAGMA-", sample_group, "-Aggregated_detailed.csv")
    }
    
# Survey_responses_processed/
    
    ## Reading in survey data  -----
    
    # reading survey data
    responses = read_csv(source_file)[-1, ] %>% 
        drop_na('PSTNR') |>  # dropping surveys that were not from sample (could not be inked to PSTNR)
        filter(as.numeric(`Duration..in.seconds.`) > 100) %>%  # dropping surveys that were no actually responded to
        mutate(PSTNR = as.numeric(PSTNR) ) 
    
    
        # reading variable descriptions
    variable_descriptions = read_csv("variable_description.csv")[-1, ] %>%
        mutate(variable_exists = variable_name %in% colnames(responses)) %>%
        filter(variable_type %in% c("ordinal", "continuous"),
               variable_exists) %>% # retaining only those variables that exist in the response data
        mutate(
            valuation_signing = as.integer(valuation_signing),
            question_number = as.integer(question_number)
        )
    
    nvars = nrow(variable_descriptions)
    groups = unique(variable_descriptions$grouping_variable_aggregate2)
    
    
    ## Processing survey data ------
    
    # Replace -99 by NA throughout
    # Flip signs so that bigger is better for all variables
    # normalize scale from 0 to 1
    responses_normalized = responses
    for (i in 1:nvars) {
 
      variable_name = variable_descriptions[[i, "variable_name"]]
      variable_sign = variable_descriptions[[i, "valuation_signing"]]
      variable_scale = as.numeric(variable_descriptions[[i, "scale_value"]])
      variable_type = variable_descriptions[[i, "variable_type"]]
      
      if (variable_type == "ordinal") {
        shift = ifelse(variable_sign == 1, -1, -variable_scale)# shift minimal value to 0
        multiplier = variable_sign / (variable_scale - 1) # flip sign if needed and scale maximum to 1
      } else {
        shift = ifelse(variable_sign == 1, 0, -variable_scale)# shift minimal value to 0
        multiplier = variable_sign / variable_scale # flip sign if needed and scale maximum to 1
      }
      
      responses_normalized[, variable_name] =
        multiplier * (shift +
                        as.numeric(pull(responses, variable_name)) %>% na_if(-99)) # replacing -99 by na
 
    }
    
    # Create aggregate indices
    if (sample_group == "Participants"){
        responses_aggregated = responses_normalized["PSTNR"]
    } else {
        responses_aggregated = responses_normalized["PSTNR"] %>% 
            mutate(survey_mode = ifelse(responses$RecipientEmail == "klaudia.marschalek@wu.ac.at", "postal", "online"))
    }
    
# create responses_aggregated_detailed
    responses_aggregated_detailed = responses_aggregated

## main outcome variables        
    for (variable_group in groups) {
        # find indices corresponding to variable group
        indices =
            variable_descriptions %>%
            filter(grouping_variable_aggregate2 == variable_group) %>%
            pull(variable_name)
        
        # take average across all variables corresponding to these indices
        # Important note: if any of the constituent variables is NA, it will be ignored in the mean (matters for health)
        responses_aggregated[[variable_group]] =
            rowMeans(responses_normalized[, indices], na.rm = T)
        
        # Output NA statistics for non-response tracking
        cat(paste0(
            "NAs encountered for ", variable_group, " based on ", length(indices), " variables: ",
            sum(is.na(responses_normalized[, indices])), "\n",
            " Corresponding aggregated NAs: ", sum(is.na(responses_aggregated[[variable_group]])), "\n"
        ))
    }


    # special treatment of selected variables ----
    
    # mean values in income brackets for imputation where income is missing
    income_bracket = c(300, 800, 1200, 1600, 2000, 2400, 2800)
    # create income variable, normalized by 2000
    responses_normalized = responses_normalized %>%
        mutate(Q23= as.numeric(Q23),
               Q23 = ifelse(Q23 > 0, Q23, NA), # Taking care of -99 values for income bracket
                income = ifelse(!is.na(Q22), as.numeric(Q22),
                               income_bracket[as.numeric(Q23)] / 2000))
    
    responses_aggregated = responses_aggregated %>%
        mutate(income = responses_normalized$income) 
    
    # Constructing number of contacts listed, divided by 5
    contact_names = read_csv("variable_description.csv")[-1, ] %>%
        filter(grouping_variable == "social_network", variable_type == "string") %>% 
        pull(variable_name)
    responses_aggregated$number_of_contacts =
            rowSums((responses[, contact_names] %>% na_if(".") )!= "-99", na.rm = F)
    responses_aggregated = responses_aggregated %>% 
        mutate(number_of_contacts = 0.2 *number_of_contacts)
    
    responses_aggregated %>%
        write_csv(destination_file)


   
   
#### Construct responses_aggregated_detailed ####
    groups_detailed = unique(variable_descriptions$grouping_variable_detailed)[-1]
       
    ## detailed outcome variables
    for (variable_group in groups_detailed) {
      # find indices corresponding to variable group
      indices_detailed =
        variable_descriptions %>%
        filter(grouping_variable_detailed == variable_group) %>%
        pull(variable_name)
      
      # take average across all variables corresponding to these indices
      # Important note: if any of the constituent variables is NA, it will be ignored in the mean (matters for health)
      responses_aggregated_detailed[[variable_group]] =
        rowMeans(responses_normalized[, indices_detailed], na.rm = T)
      
      # Output NA statistics for non-response tracking
      cat(paste0(
        "NAs encountered for ", variable_group, " based on ", length(indices_detailed), " variables: ",
        sum(is.na(responses_normalized[, indices_detailed])), "\n",
        " Corresponding aggregated NAs: ", sum(is.na(responses_aggregated_detailed[[variable_group]])), "\n"
      ))
    }
    
  # keep only lamb and social_capital disaggregated
   responses_aggregated_detailed <- responses_aggregated_detailed  %>% 
        select(PSTNR, contains("lamb"), contains("social_capital") )

   responses_aggregated_detailed %>%
        write_csv(destination_file_detailed)
}}
   