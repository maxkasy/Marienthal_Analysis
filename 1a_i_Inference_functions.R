# Summary statistics ----
summary_descriptives = function(responses_and_controls, responses_aggregated){
    surveyed = !is.na(responses_and_controls$ResponseId)
    
    summary_stats = tribble(
        ~Stat, ~Value,
        "Baseline sample size", nrow(responses_and_controls),
        "Survey sample size", sum(surveyed),
        "Missing", sum(!surveyed),
        "Treated sample size", sum((responses_and_controls$treatment_wave == 1), na.rm = T),
        "Complete pairs",  sum(responses_and_controls$match_observed, na.rm=T) / 2
    )
    
    non_missing_items = responses_aggregated %>% 
        summarize(across(outcome_variables, ~ sum(!is.na(.x)))) %>% 
        pivot_longer(cols = everything())
    
    summary_stats %>%
        kable(
            col.names = c("", "Number"),
            row.names = F,
            digits = 0,
            format = "latex",
            booktabs = TRUE,
            escape = F,
            linesep = ""
        ) %>%
        write("Figures/summary_stats.tex")
    
    non_missing_items %>%
        kable(
            col.names = c("", "Observations"),
            row.names = F,
            digits = 0,
            format = "latex",
            booktabs = TRUE,
            escape = F,
            linesep = ""
        ) %>%
        write("Figures/non_missing_items.tex")
}


# Point estimates -----
# calculating outcome means by treatment group

treatment_effects_lm_fun = function(data, outcome_variables, control_variables = "") {
    if (control_variables[1] != "") {
        # if control variable is a factor, expand into dummies
        if (is.factor(data %>%  pull(control_variables[[1]]))) {
            data = data %>% 
                fastDummies::dummy_cols(select_columns = control_variables[[1]],
                                        remove_selected_columns = T)
            control_variables = names(data)[startsWith(names(data),control_variables[[1]])]
        }
            
        # Demean controls, so that regression main effects can be interpreted as sample averages
        data = data %>% 
            mutate(across(all_of(control_variables), ~.x - mean(.x, na.rm = T)))
    }
    
    # Create string with list of regressors
    regressors = 
        ifelse(control_variables[1] == "",
               "treatment_wave",
               paste0(c("treatment_wave", control_variables), collapse = " + "))
    
    # Run regressions for all outcomes, and store coefficients in rows of data frame
    te_lm = 
    map(outcome_variables,
        function(outcome) 
            lm(formula = as.formula(paste0(
                outcome, "~", regressors)), data)$coefficients) %>% 
        bind_rows()
    
    te_lm = te_lm[,1:2]
    names(te_lm) = c("Group_2", "ATE")

    te_lm %>% 
         mutate(Outcome = outcome_variables,
                Group_1 = Group_2 + ATE) %>% 
        select(Outcome, Group_1, Group_2, ATE)
}


treatment_effects_counterfactual_fun = function(data, 
                                                outcome_variables,
                                                control_variables = "",
                                                seed) {
    treatment_RI =
        assign.grp(pairwise_matches_rand_inf, seed = seed) %>%
        select(Group1.ID, treatment.grp) %>%
        mutate(treatment.grp = factor(treatment.grp) %>% as.numeric()) %>%
        rename(PSTNR = Group1.ID,
               treatment_wave = treatment.grp)
    
    data %>%
        select(-treatment_wave) %>%
        left_join(treatment_RI, by = "PSTNR") %>%
        treatment_effects_lm_fun(outcome_variables)
}




randomization_inference_fun = function(data, 
                                       outcome_variables,
                                       control_variables = "",
                                   number_of_draws = 1000,
                                   # choose seeds for each of the counterfactual assignments
                                   counterfactual_seeds = 1929 + (1:number_of_draws)) {
    # calculate actual estimates
    treatment_effects = treatment_effects_lm_fun(data, outcome_variables, control_variables)

    # calculate estimates for each of the counterfactual assignments
    counterfactual_estimates = future_map(counterfactual_seeds,
                                          ~ treatment_effects_counterfactual_fun(data, outcome_variables, control_variables, .x))
    # calculate p-values based on the counterfactual estimates
    treatment_effects$p_value =
        do.call(cbind,
                map(counterfactual_estimates,
                    ~ (treatment_effects[["ATE"]] - .x[["ATE"]] < 0))) %>%
        rowMeans()
    
    # number of observations for each outcome
    nobs = map_int(outcome_variables,
                   ~ sum(!is.na(data[[.x]])))
    nobs_treated = map_int(outcome_variables,
                           ~ sum(!is.na(data[[.x]]) & (data$treatment_wave == 1)))
    nobs_control = nobs - nobs_treated
    
    treatment_effects |> 
        mutate(n = nobs, n1 = nobs_treated, n2= nobs_control)
}




# calculate pairwise differences, to get standard errors

treatment_effects_with_SEs_fun = function(data) {
    wave_matrices_1 = data %>%
        filter(treatment_wave == 1) %>%
        arrange(PSTNR) %>%
        select(-c( "treatment_wave", "PSTNR", "ResponseId", "PSTNR_match", "match_observed")) %>%
        as.matrix()
    
    wave_matrices_2 = data %>%
        filter(treatment_wave == 2) %>%
        arrange(PSTNR_match) %>%
        select(-c( "treatment_wave", "PSTNR", "ResponseId", "PSTNR_match", "match_observed")) %>%
        as.matrix()
    
    
    # the following returns NAs wherever 1 observation was NA among the matches
    treatment_effects = (wave_matrices_1 - wave_matrices_2) %>%
        as_tibble() %>%
        summarize(across(.fns = list(st_mean = mean, st_sd = sd))) %>% 
        pivot_longer(everything(),
                     names_to = c("Outcome", "Stat"),
                     names_sep = "_st_") %>% 
        pivot_wider(names_from = "Stat",
                    values_from = "value") %>% 
        mutate(se = sd / sqrt(nrow(data)/2))
    
    treatment_effects
}

# treatment effects, but with robust standard errors
treatment_effects_lm_fun_SE = function(data, outcome_variables, control_variables = "") {
    if (control_variables[1] != "") {
        # if control variable is a factor, expand into dummies
        if (is.factor(data %>%  pull(control_variables[[1]]))) {
            data = data %>% 
                fastDummies::dummy_cols(select_columns = control_variables[[1]],
                                        remove_selected_columns = T)
            control_variables = names(data)[startsWith(names(data),control_variables[[1]])]
        }
        
        # Demean controls, so that regression main effects can be interpreted as sample averages
        data = data %>% 
            mutate(across(all_of(control_variables), ~.x - mean(.x, na.rm = T)))
    }
    
    # Create string with list of regressors
    regressors = 
        ifelse(control_variables[1] == "",
               "treatment_wave",
               paste0(c("treatment_wave", control_variables), collapse = " + "))
    
    # Run regressions for all outcomes, and store coefficients in rows of data frame
    te_lm = 
        map(outcome_variables,
            function(outcome) {
                        lm_robust(formula = as.formula(paste0(
                            outcome, "~", regressors)), data) |> 
                                tidy() |> 
                                slice(2) |>  # keep row for coefficient on treatment wave
                                mutate(Outcome = outcome)
                    }) %>% 
        bind_rows()
    
    te_lm
}
