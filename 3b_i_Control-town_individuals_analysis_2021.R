# Marienthal ----
# Loading outcomes
Marienthal_responses_aggregated =
    paste0(veracrypt_path, "jobguarantee/2021-02-survey-data-processed/MAGMA-Participants-Aggregated.csv") %>% 
    read_csv() %>%
    mutate(town = "Gramatneusiedl")

# Loading baseline covariates
Marienthal_covariates =
    paste0(veracrypt_path, "jobguarantee/2020-09-admin-data-processed/participant_assignment_full.csv") %>% 
    read_csv() %>% 
    mutate(treatment_wave = factor(treatment_wave, levels = c(2,1)))


control_variables = names(Marienthal_covariates)[-(1:4)]

# merging outcomes and covariates
Marienthal_responses_aggregated = Marienthal_responses_aggregated %>% 
    right_join(Marienthal_covariates, by = "PSTNR") %>% 
    mutate(pair_dummy = factor(pmin(PSTNR, match))) 


# Control towns -----
# Loading outcomes
Control_responses_aggregated =
  paste0(veracrypt_path, "jobguarantee/2021-02-survey-data-processed/MAGMA-Control-towns-Aggregated.csv") %>%  
  read_csv() |> 
  mutate(town = "Control") 

# Loading baseline covariates
Control_covariates =
    paste0(veracrypt_path, "jobguarantee/2020-12-control-admin-data-processed/control_participants_for_survey.csv") %>% 
    read_csv()

# merging outcomes and covariates
Control_responses_aggregated = Control_responses_aggregated %>% 
    right_join(Control_covariates, by = "PSTNR") 



# Combining Marienthal and control ----
# Loading outcome names to label plots
group_names = read_csv("variable_description.csv")[-1, ] %>% 
    filter(include == 1) |> 
    select(grouping_variable_aggregate2, grouping_variable_name_aggregate2) %>% 
    group_by(grouping_variable_aggregate2) %>% 
    slice(1) %>% 
    rename(Outcome = grouping_variable_aggregate2,
           Name = grouping_variable_name_aggregate2) %>% 
    rbind(tibble(Outcome = c("income", "number_of_contacts"),
                 Name = c("Income", "Number of contacts")))

outcome_variables = group_names$Outcome

Combined_responses =
    bind_rows(Marienthal_responses_aggregated, Control_responses_aggregated) |> 
    mutate(comparison_group = ifelse(town == "Gramatneusiedl", as.character(treatment_wave), "Control") |> 
               factor(),
           town = factor(town)) 
# Calculate mean of control variables for Marienthal
GN_means = Combined_responses  |> 
    filter(town == "Gramatneusiedl") |> 
    select(control_variables) |> 
    summarise_all(mean)
# Normalize control variables to mean 0 for Marienthal
Combined_responses = Combined_responses  |> 
    mutate(across(all_of(control_variables), ~.x - GN_means[[cur_column()]]))
    
  


# Descriptives -----
bind_rows(Marienthal_responses_aggregated, 
          Control_responses_aggregated |> filter(!is.na(survey_mode))) %>%
    mutate(control_town = (town != "Gramatneusiedl")) |> 
    select(control_town, all_of(control_variables)) |> 
    pivot_longer(all_of(control_variables),
                 names_to = "Covariate") %>%
    mutate(Covariate = factor(Covariate,
                              levels = c("MALE","PEALTER","MIG","BILDUNG","EINSCHRAENKUNG","BENEFIT_LEVEL","UE_DAYS"),
                              labels = c("Male","Age","Migration Background","Education","Medical condition","Benefit level","Days unemployed"))) %>% 
    group_by(Covariate) %>%
    rstatix::t_test(value ~ control_town, detailed = T) %>%
    ungroup() %>%
    select(Covariate, estimate1, estimate2, estimate, statistic, p)  %>%
    mutate(estimate = -estimate) |> # Flipping sign of difference column
    kableExtra::kable(col.names = c("Covariate","Gramatneusiedl","Control towns","Difference","T-statistic","P-value"),
          row.names = F,
          digits = 3,
          format = "latex",
          booktabs = TRUE,
          escape = F, 
          linesep = "") %>%
    write("Figures/controltown_comparison_table.tex")




# Estimation, linear model -----
regressors_1 = paste0(c("-1 + comparison_group", control_variables), collapse = " + ")

te_lm1 = 
    map(outcome_variables,
        function(outcome) 
            lm(formula = as.formula(paste0(
                outcome, "~", regressors_1)), Combined_responses)$coefficients) %>% 
    bind_rows()

te_lm1 = te_lm1[,1:3]
names(te_lm1) = c("Group_1", "Group_2", "Control_town")

# Number of observations
nobs_treated = map_int(outcome_variables,
                       ~ sum(!is.na(Combined_responses[[.x]]) & 
                                 (Combined_responses$town == "Gramatneusiedl") & 
                                 (Combined_responses$treatment_wave == 1)))
nobs_control = map_int(outcome_variables,
                       ~ sum(!is.na(Combined_responses[[.x]]) & 
                                 (Combined_responses$town == "Gramatneusiedl")  & 
                                 (Combined_responses$treatment_wave == 2)))
nobs_controltown = map_int(outcome_variables,
                       ~ sum(!is.na(Combined_responses[[.x]]) & 
                                 (Combined_responses$town == "Control")))

te_lm1 = te_lm1 %>% 
    mutate(Outcome = outcome_variables,
           n1 = nobs_treated, n2= nobs_control, nct = nobs_controltown) %>% 
    select(Outcome, Group_2, Control_town, Group_1, n1, n2, nct)



# Estimation of treatment effects of Group 2 vs. control with standard errors

Combined_responses_group2control =
    Combined_responses |> 
    filter(comparison_group != "1") |> 
    mutate(comparison_group = (comparison_group == "2"))

regressors_2 = paste0(c("comparison_group", control_variables), collapse = " + ")

te_lm_se = 
    map(outcome_variables,
        function(outcome) 
            lm_robust(formula = as.formula(paste0(
                outcome, "~", regressors_2)), 
                Combined_responses_group2control) |> 
            tidy() |> 
            filter(term == "comparison_groupTRUE")) |> 
    bind_rows() |> 
    select(outcome, estimate, std.error) |> 
    rename(Outcome = outcome)



# Plots -----
back_col = "white"
treat_col = "firebrick"
control_col = "grey65"
line_col = "grey50"
towns_col = "royalblue4"



plot_treatment_effects = function(treatment_effects, filename_modifier = "") {
    # scaling of plot output
    plot_height = .8 + nrow(treatment_effects) / 6 

    label_outcome = sort(treatment_effects$Group_1, index.return =T)$ix[[2]]
    
    labs = tibble(
        lab = c("Group 1", "Group 2", "Control towns"),
        y = c(treatment_effects[[label_outcome, "Group_1"]] + .03, treatment_effects[[label_outcome, "Group_2"]] - .03,
              treatment_effects[[label_outcome, "Control_town"]] + .03),
        hjust = c("left", "right", "left")
    )
    
    
    p1 = treatment_effects %>%
      left_join(group_names, by = "Outcome") %>%
      mutate(Name = fct_reorder(Name, Group_1)) %>% 
      ggplot(aes(x = Name, y = Group_1)) +
      geom_segment(aes(xend = Name, yend = Group_2), color = line_col) +
      geom_point(aes(y = Control_town), color = towns_col, shape = 21, size = 2) +
      geom_point(color = treat_col, size = 2) +
      geom_point(aes(y = Group_2), color = control_col, shape = 17, size = 2) +
      # geom_text(data = labs, aes(label = lab, y = y, hjust = hjust), x = 2, size = 3) +
      scale_y_continuous(limits=c(0,1), expand = c(0, 0)) +
      coord_flip() +
      theme_minimal() +
      theme(plot.margin = margin(.1,.2,.1,.1, unit = "in"),
            plot.subtitle = element_markdown()) +
      labs(
        x = "",
        y = "",
        title = "Outcomes for 2021",
        subtitle = "<span style='color:firebrick;'>Group 1</span> (treated),  <span style='color:grey60;'>Group 2</span> (control), and <span style='color:royalblue4;'>Control towns</span>."
      )
    
    ggsave(paste0("Figures/Survey_averages_with_controltowns_plot_2021_linear", filename_modifier, ".png"),
           p1,
           width = 7,
           height = plot_height)
}


plot_confidence_intervals = function(treatment_effects_with_SEs,
                                     filename_modifier = "",
                                     title = element_blank()) {
    # scaling of plot output
    plot_height = .8 + nrow(treatment_effects_with_SEs) / 6 
    
    p1 = treatment_effects_with_SEs %>%
        left_join(group_names, by = "Outcome") %>% 
        mutate(Name = fct_reorder(Name, estimate)) %>% 
        ggplot(aes(x = Name, y = estimate)) +
        geom_hline(yintercept = 0, color = "grey80") +
        geom_segment(aes(
            xend = Name,
            y = estimate - 1.96 * std.error,
            yend = estimate + 1.96 * std.error
        ), color = line_col) +
        geom_segment(aes(
            xend = Name,
            y = estimate - 1.65 * std.error,
            yend = estimate + 1.65 * std.error
        ), color = line_col, size =1) +
        geom_point(color = treat_col, size = 2) +
        ylim(c(-.6, .8)) +
        coord_flip() +
        theme_minimal() +
        theme(plot.margin = margin(.1,.2,.1,.1, unit = "in")) +
        labs(
            title = title,
            x = "",
            y = ""
        )
    
    ggsave(paste0("Figures/Confidence_intervals_Group2_vs_controltown_2021_linear", filename_modifier, ".png"),
           p1,
           width = 7,
           height=plot_height)
    
    p1
}

table_combined = function(treatment_effects,
                          filename_modifier = "") {
    filename = paste0("Figures/Survey_table_2021_linear", filename_modifier, ".tex")
    
    treatment_effects |> 
        select(Name, Group_1, Group_2, Control_town, estimate, std.error, n1, n2, nct) |> 
        arrange(-Group_1) |>
        knitr::kable(
            col.names = c(
                "Outcome", "Treated", "Control", "Control towns",
                "Ct vs. Ct towns",
                "SE",
                "$n_1$", "$n_2$", "$n_{ct}$"
            ),
            row.names = F,
            digits = 3,
            format = "latex",
            booktabs = TRUE,
            escape = F
        ) %>%
        write(filename)
}


# plot average outcomes
plot_treatment_effects(te_lm1)

# Plot separately for economic and non-economic variables ---- 

economic_variables = c("employment_ams", "income_security","unemployment_ams", "unemployment_spells", "income")



plot_treatment_effects(te_lm1 |> 
                            filter(Outcome %in% economic_variables),
                        "_econ")
plot_treatment_effects(te_lm1 |> 
                            filter(!(Outcome %in% economic_variables)),
                        "_nonecon")


# Plot confidence intervals ----


plot_confidence_intervals(te_lm_se)


p_ccf1 = plot_confidence_intervals(te_lm_se |> 
                           filter(Outcome %in% economic_variables),
                       "_econ", "Economic outcomes")
p_ccf2 = plot_confidence_intervals(te_lm_se |> 
                           filter(!(Outcome %in% economic_variables)),
                       "_nonecon", "Other outcomes")

ggsave(paste0("Figures/Confidence_intervals_Group2_vs_controltown_2021_combined_linear.png"),
       p_ccf1 / p_ccf2 + plot_layout(heights = c(1, 3)),
       width = 7, height = 7)

# Print table ----


te = te_lm1 %>%
    left_join(te_lm_se, by = "Outcome") |> 
    left_join(group_names, by = "Outcome") 
    
te |> 
    filter(Outcome %in% economic_variables) |> 
    table_combined("_econ")

te |> 
    filter(!(Outcome %in% economic_variables)) |> 
    table_combined("_nonecon")



# Robustness check: Lee bounds for control vs control towns ----
# Load helper function that trims sample to get Lee bounds
source("3c_leebounds.R")

# Get upper and lower bounds
te_lm_upper = 
    map(outcome_variables,
        function(outcome) 
            lm(formula = as.formula(paste0(
                outcome, "~", regressors_2)), 
                trimmed_responses(outcome, control_variables, 
                  data = Combined_responses_group2control, upper = T))$coefficients
        ) %>% bind_rows()

te_lm_lower = 
    map(outcome_variables,
        function(outcome) 
            lm(formula = as.formula(paste0(
                outcome, "~", regressors_2)), 
                trimmed_responses(outcome, control_variables, 
                  data = Combined_responses_group2control, upper = F))$coefficients
        ) %>% bind_rows()

bound_estimates = te_lm_se |> 
    select(-std.error) |> 
    mutate(lower_bound = te_lm_lower[["comparison_groupTRUE"]],
                upper_bound = te_lm_upper[["comparison_groupTRUE"]])
    
table_bounds = function(estimates, filename){
    estimates |> 
        left_join(group_names, by = "Outcome") |> 
        select(Name, estimate, lower_bound, upper_bound) |> 
        knitr::kable(
            col.names = c(
                "Outcome", "Ct vs. Ct towns",
                "Lower bound", "Upper bound"
            ),
            row.names = F,
            digits = 3,
            format = "latex",
            booktabs = TRUE,
            escape = F
        ) %>%
        write(filename)
}

bound_estimates |> 
    filter(Outcome %in% economic_variables) |> 
    table_bounds("Figures/Survey_leebounds_2021_econ.tex")

bound_estimates |> 
    filter(!(Outcome %in% economic_variables)) |> 
    table_bounds("Figures/Survey_leebounds_2021_nonecon.tex")
