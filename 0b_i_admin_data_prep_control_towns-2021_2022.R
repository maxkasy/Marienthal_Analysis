for (year in c(2021, 2022)){
    if (year == 2021){
        data_path = paste0(veracrypt_path, "jobguarantee/2021-09-admin-data-raw/")
        survey_path = paste0(veracrypt_path, "jobguarantee/2021-02-survey-data-processed/")
        admin_out = paste0(veracrypt_path, "jobguarantee/2021-09-admin-data-processed/")
        startdate = "2020-09-30"
        enddate = "2021-02-28"
    } else {
        data_path = paste0(veracrypt_path, "jobguarantee/2022-02-admin-data-raw/")
        survey_path = paste0(veracrypt_path, "jobguarantee/2022-02-survey-data-processed/")
        admin_out = paste0(veracrypt_path, "jobguarantee/2022-02-admin-data-processed/")
        startdate = "2020-09-30"
        enddate = "2022-02-28"
    }
}
    

    filenames = c("Erwerbsbiographie_Kontrollgruppe.csv",
                 "Erwerbsbiographie_ProgrammteilnehmerInnen.csv",
                 "Leistungsbezug_Kontrollgruppe.csv",
                 "Leistungsbezug_ProgrammteilnehmerInnen.csv",
                 "Unistatus_ProgrammteilnehmerInnen.csv"
                 )
    
    file_paths = paste0(data_path, filenames)
    
    
    data_list_raw = map(file_paths, function(path) read_delim(path, delim=";",
                                                             locale = locale(encoding = "latin1", decimal_mark = ",")))
    
    ### 1. benefit receipt (TAGSATZLEIST) ---------------------------------
    
    # reading in benefits file from source data
    
    benefits =
      read_delim(file_paths[[3]], delim=";",
                 locale = locale(encoding = "latin1", decimal_mark = ","))
    
    # for each participant (indexed by PST_KEY), only keep most recent (as indicated by STICHTAG) row of raw data
    # and select variables
    participants_benefits =
      benefits %>%
      mutate(PSTNR = as.integer(PST_KEY),
             STICHTAG = ymd(STICHTAG),
             TAGSATZLEIST = as.integer(TAGSATZLEIST)) %>%
      group_by(PSTNR) %>%
      arrange(STICHTAG) %>%
      slice(n()) %>%
      ungroup() %>%
      select(PSTNR, TAGSATZLEIST) %>%
      rename(benefit_level_post_intervention = TAGSATZLEIST)
    
    ### 2. work history (days EMPLOYED since program start) ---------------------------------
    ### 2.b work history - via Erwerbshistorie instad of Unistatus (days EMPLOYED since program start) ---------------------------------
    
    work_history_ams_raw =
      read_delim(file_paths[[1]], delim=";",
                 locale = locale(encoding = "latin1", decimal_mark = ",")) %>% 
      mutate( STICHTAG = ymd(STICHTAG))
    
    outcomes_work_history = c("TAGE_BESCHAEFTIGUNG_GESAMT", 
                 "TAGE_BESCHAEFTIGUNG_GEF",
                 "TAGE_BESCHAEFTIGUNG_UNGEF", 
                 "TAGE_BESCHAEFTIGUNG_SELBST",
                 "TAGE_AMS_VORMERKUNG", 
                 "TAGE_OUT_OF_LABOUR_FORCE")
    
    outcomes_work_history_start = paste0(outcomes_work_history, "_", startdate)
      
    outcomes_work_history_end = paste0(outcomes_work_history, "_", enddate)
    
    
    # compute work history since MAGMA start date (2022-09-30)
    work_history_ams <- work_history_ams_raw  %>% 
      filter(STICHTAG == ymd(startdate) | STICHTAG == ymd(enddate)) 
    
    work_history_ams <- work_history_ams %>% 
      pivot_wider(id_cols = PSTNR, 
                  names_from = STICHTAG, 
                  values_from = all_of(outcomes_work_history) )
    
    if (year == 2021){
        work_history_ams <- work_history_ams %>% 
            mutate( 
                TAGE_BESCHAEFTIGUNG_GESAMT = `TAGE_BESCHAEFTIGUNG_GESAMT_2021-02-28` - `TAGE_BESCHAEFTIGUNG_GESAMT_2020-09-30` ,
                TAGE_BESCHAEFTIGUNG_GEF = `TAGE_BESCHAEFTIGUNG_GEF_2021-02-28` - `TAGE_BESCHAEFTIGUNG_GEF_2020-09-30` ,
                TAGE_BESCHAEFTIGUNG_UNGEF = `TAGE_BESCHAEFTIGUNG_UNGEF_2021-02-28` - `TAGE_BESCHAEFTIGUNG_UNGEF_2020-09-30` , 
                TAGE_BESCHAEFTIGUNG_SELBST = `TAGE_BESCHAEFTIGUNG_SELBST_2021-02-28` - `TAGE_BESCHAEFTIGUNG_SELBST_2020-09-30` ,
                TAGE_AMS_VORMERKUNG = `TAGE_AMS_VORMERKUNG_2021-02-28` - `TAGE_AMS_VORMERKUNG_2020-09-30` , 
                TAGE_OUT_OF_LABOUR_FORCE = `TAGE_OUT_OF_LABOUR_FORCE_2021-02-28` - `TAGE_OUT_OF_LABOUR_FORCE_2020-09-30`
            ) 
    } else {
        work_history_ams <- work_history_ams %>% 
            mutate( 
                TAGE_BESCHAEFTIGUNG_GESAMT = `TAGE_BESCHAEFTIGUNG_GESAMT_2022-02-28` - `TAGE_BESCHAEFTIGUNG_GESAMT_2020-09-30` ,
                TAGE_BESCHAEFTIGUNG_GEF = `TAGE_BESCHAEFTIGUNG_GEF_2022-02-28` - `TAGE_BESCHAEFTIGUNG_GEF_2020-09-30` ,
                TAGE_BESCHAEFTIGUNG_UNGEF = `TAGE_BESCHAEFTIGUNG_UNGEF_2022-02-28` - `TAGE_BESCHAEFTIGUNG_UNGEF_2020-09-30` , 
                TAGE_BESCHAEFTIGUNG_SELBST = `TAGE_BESCHAEFTIGUNG_SELBST_2022-02-28` - `TAGE_BESCHAEFTIGUNG_SELBST_2020-09-30` ,
                TAGE_AMS_VORMERKUNG = `TAGE_AMS_VORMERKUNG_2022-02-28` - `TAGE_AMS_VORMERKUNG_2020-09-30` , 
                TAGE_OUT_OF_LABOUR_FORCE = `TAGE_OUT_OF_LABOUR_FORCE_2022-02-28` - `TAGE_OUT_OF_LABOUR_FORCE_2020-09-30`
            )
    }
    work_history_ams <- work_history_ams %>% 
        select(PSTNR, outcomes_work_history) %>% 
      mutate( 
            employment_days_since_2020_ams = TAGE_BESCHAEFTIGUNG_GESAMT,
            non_employment_days_since_2020_ams = TAGE_AMS_VORMERKUNG + TAGE_OUT_OF_LABOUR_FORCE,
            unemployment_registered_days_since_2020_ams = TAGE_AMS_VORMERKUNG
        )
    
    
    # To do: 
    # 2022-02-28 - 1990-01-31
    
    ### 3. Main covariate file -------------------------------
    # reading in participant file from source data 2022-02
     file_path = paste0(data_path, "Arbeitslose_Kontrollgruppe.csv")
    
    # reading in participant file from initial source data (2020-09) since the newer files do not
    # contain all participants but only those that are unemploymed at the respective times
    file_path = paste0(veracrypt_path, "jobguarantee/2020-12-control-admin-data-raw/Individualdaten_AL_inkl_Tagsatz_Kontrollgemeinden.dsv")
    
    participants_raw =
      read_delim(file_path, delim=";",
                 locale = locale(encoding = "latin1", decimal_mark = ","))
    
    
    # for each participant (indexed by PSTNR), only keep most recent (as indicated by STICHTAG) row of raw data
    participants =
      participants_raw %>%
      mutate(PSTNR = as.integer(PSTNR),
             STICHTAG = ymd(STICHTAG)) %>%
      group_by(PSTNR) %>%
      arrange(STICHTAG) %>%
      slice(n()) %>% # cuts rows with the same person observed twice
      ungroup() %>%
      mutate(
        MIG = as.integer((MIGRATIONSHINTERGRUND != "-")),
        MALE = as.integer(GESCHLECHT == "M"),
        BILDUNG = as.integer((AUSBILDUNG != "PS") & (AUSBILDUNG != "PO")), # more than Pflichtschule
        EINSCHRAENKUNG = as.integer(VERMITTLUNGSEINSCHRAENKUNG != "-") # employment relevant health condition
      )
    
    ### keep only GKZ from participants baseline file to distinguish individuals by control town
    participants_gkz =
      participants %>%
      select(PSTNR, GKZ)
    
    ### 4. Merge all covariates for participants ---------------------------------
    
    # Merge all covariates for participants
    control_individuals_merged <- work_history_ams  %>% 
      left_join(participants_gkz, by = "PSTNR") %>%  # merge with additional covariates for matching
      left_join(participants_benefits, by = "PSTNR")
    
    
    
    ### 5. Normalise admin outcomes ---------------------------------
    
    ## Normalise admin outcomes to be comparable in size on one single chart with survey outcomes
    # reading variable descriptions
    variable_descriptions = read_csv("variable_description.csv")[-1, ] %>%
      mutate(variable_exists = variable_name %in% colnames(control_individuals_merged))%>%
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
    participants_normalized = control_individuals_merged
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
                        as.numeric(pull(control_individuals_merged, variable_name)) %>% na_if(-99)) # replacing -99 by na
      
    }
    
    
    participants_normalized_selected = participants_normalized[c("PSTNR", "GKZ"
    )]
    
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
      write_csv(paste0(admin_out, "control_individuals_merged.csv"))
    
    
    ## Merge Survey and Admin data ####
    
    # reading in aggregated survey data file
        
    survey_file_path = paste0(survey_path, "MAGMA-Control-towns-Aggregated-Survey.csv")
    
    participants_survey_aggregated =
      read_delim(survey_file_path, delim=",",
                 locale = locale(encoding = "latin1", decimal_mark = "."))
    
    participants_merged_admin_survey <- participants_normalized_selected %>%
      left_join(participants_survey_aggregated, by = "PSTNR")
    
    
    participants_merged_admin_survey %>%
      write_csv(paste0(survey_path, "MAGMA-Control-towns-Aggregated.csv"))

}
