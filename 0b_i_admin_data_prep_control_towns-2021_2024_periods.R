# Define paths
  startdate <- "2020-09-30"  
  data_path <- paste0(veracrypt_path, "jobguarantee/2024-02-admin-data-raw/")
  # survey_path = paste0(veracrypt_path, "jobguarantee/2024-02-survey-data-processed/") # no survey data for 2024
  admin_out <- paste0(veracrypt_path, "jobguarantee/2024-02-admin-data-processed/")
  enddate <- "2024-02-28"      
    
    filenames = c("Erwerbsbiographie_Kontrollgruppe.csv",
                 "Erwerbsbiographie_ProgrammteilnehmerInnen.csv",
                 "Leistungsbezug_Kontrollgruppe.csv",
                 "Leistungsbezug_ProgrammteilnehmerInnen.csv",
                 "Unistatus_ProgrammteilnehmerInnen.csv") 
    
    file_paths = paste0(data_path, filenames)

    data_list_raw = map(file_paths, function(path) read_delim(path, delim=";",
                                                             locale = locale(encoding = "latin1", decimal_mark = ",")))
  
    
    ### 2.b work history - via Erwerbshistorie instad of Unistatus (days EMPLOYED since program start) ---------------------------------
    
    work_history_ams_raw <- 
      read_delim(
        file_paths[[1]], 
        delim = ";",
        locale = locale(encoding = "latin1", decimal_mark = ",")
      ) %>% 
      mutate(
        STICHTAG = as.Date(STICHTAG, format = "%d.%m.%Y")  # Adjust format as needed
      )
    
    outcomes_work_history = c("TAGE_BESCHAEFTIGUNG_GESAMT", 
                              "TAGE_BESCHAEFTIGUNG_GEF",
                              "TAGE_BESCHAEFTIGUNG_UNGEF", 
                              "TAGE_BESCHAEFTIGUNG_SELBST",
                              "TAGE_AMS_VORMERKUNG", 
                              "TAGE_OUT_OF_LABOUR_FORCE")
    
    # adjust start date since only 29 Sept is available in 2024 delivery of this data file
    startdate <- "2020-09-29"
    
    outcomes_work_history_start = paste0(outcomes_work_history, "_", startdate)
    
    outcomes_work_history_end = paste0(outcomes_work_history, "_", enddate)
    
 ## OLD CODE FOR TOTAL VALUES   
#     # compute work history since MAGMA start date (2020-09-30)
#     work_history_ams <- work_history_ams_raw  %>% 
#       filter(STICHTAG == ymd(startdate) | STICHTAG == ymd(enddate)) 
#     
#     work_history_ams <- work_history_ams %>% 
#       pivot_wider(id_cols = PSTNR, 
#                   names_from = STICHTAG, 
#                   values_from = all_of(outcomes_work_history) )
#     
#     # Calculate days since MAGMA start
#       work_history_ams <- work_history_ams %>% 
#         mutate( 
#           TAGE_BESCHAEFTIGUNG_GESAMT = `TAGE_BESCHAEFTIGUNG_GESAMT_2024-02-28` - `TAGE_BESCHAEFTIGUNG_GESAMT_2020-09-29` ,
#           TAGE_BESCHAEFTIGUNG_GEF = `TAGE_BESCHAEFTIGUNG_GEF_2024-02-28` - `TAGE_BESCHAEFTIGUNG_GEF_2020-09-29` ,
#           TAGE_BESCHAEFTIGUNG_UNGEF = `TAGE_BESCHAEFTIGUNG_UNGEF_2024-02-28` - `TAGE_BESCHAEFTIGUNG_UNGEF_2020-09-29` , 
#           TAGE_BESCHAEFTIGUNG_SELBST = `TAGE_BESCHAEFTIGUNG_SELBST_2024-02-28` - `TAGE_BESCHAEFTIGUNG_SELBST_2020-09-29` ,
#           TAGE_AMS_VORMERKUNG = `TAGE_AMS_VORMERKUNG_2024-02-28` - `TAGE_AMS_VORMERKUNG_2020-09-29` , 
#           TAGE_OUT_OF_LABOUR_FORCE = `TAGE_OUT_OF_LABOUR_FORCE_2024-02-28` - `TAGE_OUT_OF_LABOUR_FORCE_2020-09-29`
#         )
#     
#       work_history_ams <- work_history_ams %>% 
# #      select(all_of(PSTNR, outcomes_work_history)) %>%
#      select(PSTNR, outcomes_work_history) %>% # old command, now depriciated
#       # sum employment and unemployment
#       mutate( 
#         employment_days_since_2020_ams = TAGE_BESCHAEFTIGUNG_GESAMT,
#         non_employment_days_since_2020_ams = TAGE_AMS_VORMERKUNG + TAGE_OUT_OF_LABOUR_FORCE,
#         unemployment_registered_days_since_2020_ams = TAGE_AMS_VORMERKUNG
#       )
#     
  

      
      library(lubridate)
      library(tidyverse)
      
      # 1. Convert all dates to year-month and filter the relevant period
      work_history_monthly <- work_history_ams_raw %>%
        mutate(month = floor_date(STICHTAG, "month")) %>%
        filter(month >= ymd("2020-09-01") & month <= ymd("2024-02-01")) %>%
        
        # 2. For each month, keep only the latest observation
        group_by(PSTNR, month) %>%
        arrange(desc(STICHTAG)) %>%
        slice(1) %>%
        ungroup() %>%
        
        # 3. Arrange by date for proper lag calculation
        arrange(PSTNR, month) %>%
        
        # 4. Calculate month-to-month differences
        group_by(PSTNR) %>%
        mutate(across(
          all_of(outcomes_work_history),
          list(
            diff = ~ . - lag(.),  # Difference from previous month
            cumsum = ~ cumsum(ifelse(is.na(lag(.)), 0, . - lag(.)))  # Cumulative difference
          ),
          .names = "{.col}_{.fn}"
        )) %>%
        ungroup() %>%
        
        # 5. Keep only months with differences (skip first month)
        filter(month > ymd("2020-09-01")) %>%
        mutate(month_label = format(month, "%Y_%m")) %>%
        
        # 6. Pivot to wide format
        pivot_wider(
          id_cols = PSTNR,
          names_from = month_label,
          values_from = ends_with(c("_diff", "_cumsum")),
          names_glue = "{.value}_month_{month_label}"
        )
      
      # 7. Create summary metrics
      work_history_monthly <- work_history_monthly %>%
        mutate(
          across(
            contains("TAGE_BESCHAEFTIGUNG_GESAMT_diff"),
            list(employment_monthly = ~ .),
            .names = "{str_replace(.col, 'TAGE_BESCHAEFTIGUNG_GESAMT_diff', 'employment_days_change')}"
          ),
          across(
            contains("TAGE_BESCHAEFTIGUNG_GESAMT_cumsum"),
            list(employment_cumulative = ~ .),
            .names = "{str_replace(.col, 'TAGE_BESCHAEFTIGUNG_GESAMT_cumsum', 'employment_days_total')}"
          )
        )

## now create a new df with the average value for each column
      # Create summary dataframe with column averages
      monthly_averages <- work_history_monthly %>%
        select(-PSTNR) %>%
        summarize(across(everything(), ~ mean(., na.rm = TRUE)))      
      
      # Now reshape to long format while keeping outcomes as columns
      monthly_averages_long <- monthly_averages %>%
        # Pivot all columns except PSTNR
        pivot_longer(
          cols = everything(),
          names_to = c("outcome", "calculation", "month"),
          names_pattern = "(.*)_(diff|cumsum)_month_(.*)",
          values_to = "mean_value"
        ) %>%
        # Filter out rows where the pattern didn't match
        filter(!is.na(calculation)) %>%
        # Pivot wider to get outcomes as columns
        pivot_wider(
          names_from = outcome,
          values_from = mean_value
        ) %>%
        # Clean up and organize
        select(month, calculation, everything()) %>%
        arrange(month, calculation)
      
      # For better readability, we can split into two dataframes:
      monthly_diff <- monthly_averages_long %>%
        filter(calculation == "diff") %>%
        select(-calculation)
      
      monthly_cumsum <- monthly_averages_long %>%
        filter(calculation == "cumsum") %>%
        select(-calculation)
   
         
      ## Write data
      monthly_diff %>% 
        write_csv(paste0(admin_out, "control_individuals_monthly_empl_diff.csv"))
      
      ## Write data
      monthly_cumsum %>% 
        write_csv(paste0(admin_out, "control_individuals_monthly_empl_cumsum.csv"))      
      
            
      
  #### STOP HERE    
        
    ### 4. Merge all covariates for participants ---------------------------------
    
    participants_merged <- work_history_ams # %>% # merge with work history from Erwerbsbiographie_ProgrammteilnehmerInnen.csv
 #     left_join(participants_benefits, by = "PSTNR") %>% # merge with benefit history
#      left_join(participants_work_history, by = "PSTNR") # merge with work history from Unistatatus
    
    ### 5. Normalise admin outcomes ---------------------------------
    
    ## Normalise admin outcomes to be comparable in size on one single chart with survey outcomes
    # reading variable descriptions
    variable_descriptions = read_csv("variable_description.csv")[-1, ] %>%
      mutate(variable_exists = variable_name %in% colnames(participants_merged))%>%
      filter(variable_type %in% c("ordinal", "continuous"),
             variable_exists) %>% # retaining only those variables that exist in the response data
      mutate(
        valuation_signing = as.integer(valuation_signing),
        question_number = as.integer(question_number)
      )
    
    nvars = nrow(variable_descriptions)
    groups = unique(variable_descriptions$grouping_variable_aggregate2)
    
    
    # Replace -99 by NA throughout
    # Flip signs so that bigger is better for all variables
    # normalize scale from 0 to 1
    participants_normalized = participants_merged
    for (i in 1:nvars) {
      
      variable_name = variable_descriptions[[i, "variable_name"]]
      variable_sign = variable_descriptions[[i, "valuation_signing"]]
      variable_scale = as.numeric(variable_descriptions[[i, ifelse(year == 2021, "scale_value", "scale_value_2022")]])
      variable_type = variable_descriptions[[i, "variable_type"]]
      
      if (variable_type == "ordinal") {
        shift = ifelse(variable_sign == 1, -1, -variable_scale)# shift minimal value to 0
        multiplier = variable_sign / (variable_scale - 1) # flip sign if needed and scale maximum to 1
      } else {
        shift = ifelse(variable_sign == 1, 0, -variable_scale)# shift minimal value to 0
        multiplier = variable_sign / variable_scale # flip sign if needed and scale maximum to 1
      }
      
      participants_normalized[, variable_name] =
        multiplier * (shift +
                        as.numeric(pull(participants_merged, variable_name)) %>% na_if(-99)) # replacing -99 by na
      
    }
    
    
    participants_normalized_selected = participants_normalized[c("PSTNR", "STICHTAG")]
    for (variable_group in groups) {
      # find indices corresponding to variable group
      indices =
        variable_descriptions %>%
        filter(grouping_variable_aggregate2 == variable_group) %>%
        pull(variable_name)
      
      # take average across all variables corresponding to these indices
      # Important note: if any of the constituent variables is NA, it will be ignored in the mean (matters for health)
      participants_normalized_selected[[variable_group]] =
        rowMeans(participants_normalized[, indices], na.rm = T)
    
      # Output NA statistics for non-response tracking
      cat(paste0(
        "NAs encountered for ", variable_group, " based on ", length(indices), " variables: ",
        sum(is.na(participants_normalized[, indices])), "\n",
        " Corresponding aggregated NAs: ", sum(is.na(participants_normalized_selected[[variable_group]])), "\n"
      ))
      
    }
    
    ## Write data
    participants_normalized_selected %>% 
      write_csv(paste0(admin_out, "participants_merged.csv"))
    
    ## Write data
    participants_merged %>% 
      write_csv(paste0(admin_out, "participants_merged_non_normalized.csv"))    
    
    
## Commented out since 2024 does not have any survey data    
    # ## Merge Survey and Admin data ####
    # # reading in aggregated survey data file
    # 
    # survey_file_path = paste0(survey_path, "MAGMA-Participants-Aggregated-Survey.csv")
    # 
    # participants_survey_aggregated =
    #   read_delim(survey_file_path, delim=",",
    #              locale = locale(encoding = "latin1", decimal_mark = "."))
    # 
    # participants_merged_admin_survey <- participants_normalized_selected %>% 
    #   left_join(participants_survey_aggregated, by = "PSTNR") %>% 
    #   filter(PSTNR != 1920209,
    #          PSTNR != 76914426)  # drop 2 individuals that were excluded from study before study started
    # 
    # 
    # participants_merged_admin_survey %>% 
    #   write_csv(paste0(survey_path, "MAGMA-Participants-Aggregated.csv"))
    
# } # use in case the script runs as a loop for all years
    
