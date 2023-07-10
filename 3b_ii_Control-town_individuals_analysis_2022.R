# Marienthal ----
# Loading outcomes
Marienthal_responses_aggregated =
    paste0(veracrypt_path, "jobguarantee/2022-02-survey-data-processed/MAGMA-Participants-Aggregated.csv") %>% 
    read_csv() %>%
    mutate(town = "Marienthal") 

# Loading baseline covariates
Marienthal_covariates =
    paste0(veracrypt_path, "jobguarantee/2020-09-admin-data-processed/participant_assignment_full.csv") %>% 
    read_csv()

control_variables = names(Marienthal_covariates)[-(1:4)]

# merging outcomes and covariates
Marienthal_responses_aggregated = Marienthal_responses_aggregated %>% 
    right_join(Marienthal_covariates, by = "PSTNR")


# Control towns -----
# Loading outcomes
Control_responses_aggregated =
  paste0(veracrypt_path, "jobguarantee/2022-02-survey-data-processed/MAGMA-Control-towns-Aggregated.csv") %>%  
  read_csv() |> 
  mutate(town = "Control") 

# Loading baseline covariates
Control_covariates =
    paste0(veracrypt_path, "jobguarantee/2020-12-control-admin-data-processed/control_participants_for_survey.csv") %>% 
    read_csv()

# merging outcomes and covariates
Control_responses_aggregated = Control_responses_aggregated %>% 
    right_join(Control_covariates, by = "PSTNR") |> 
    filter(UE_DAYS < 4000, # to balance unemployment duration across the two samples))
           !is.na(survey_mode)) # dropping non-respondents




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

outcome_variables = outcome_variables[-c(14)]

Combined_responses =
    bind_rows(Marienthal_responses_aggregated, Control_responses_aggregated) |> 
    mutate(town = factor(town))
# Calculate mean of control variables for Marienthal
GN_means = Combined_responses  |> 
    filter(town == "Marienthal") |> 
    select(control_variables) |> 
    summarise_all(mean)
# Normalize control variables to mean 0 for Marienthal
Combined_responses = Combined_responses  |> 
    mutate(across(all_of(control_variables), ~.x - GN_means[[cur_column()]]))



# Estimation, linear model -----
regressors_2 = paste0(c("-1 + town", control_variables), collapse = " + ")

te_lm2 = 
    map(outcome_variables,
        function(outcome) 
            lm(formula = as.formula(paste0(
                outcome, "~", regressors_2)), Combined_responses)$coefficients) %>% 
    bind_rows()

te_lm2 = te_lm2[,1:2]
names(te_lm2) = c("Control_towns", "Marienthal")


# Number of observations
nobs_marienthal= map_int(outcome_variables,
                       ~ sum(!is.na(Combined_responses[[.x]]) & 
                                 (Combined_responses$town == "Marienthal")))
nobs_controltown = map_int(outcome_variables,
                           ~ sum(!is.na(Combined_responses[[.x]]) & 
                                     (Combined_responses$town == "Control")))

te_lm2 = te_lm2 %>% 
    mutate(Outcome = outcome_variables,
           nmt= nobs_marienthal, nct = nobs_controltown) %>% 
    select(Outcome, Marienthal, Control_towns, nmt, nct)


# Estimation of treatment effects of Group 2 vs. control with standard errors

Combined_responses =
    Combined_responses |> 
    mutate(comparison_group = (town == "Marienthal"))

regressors_2 = paste0(c("comparison_group", control_variables), collapse = " + ")

te_lm_se =
    map(outcome_variables,
        function(outcome) 
            lm_robust(formula = as.formula(paste0(
                outcome, "~", regressors_2)), 
                Combined_responses) |> 
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
    
    p1 = treatment_effects %>%
          left_join(group_names, by = "Outcome") %>%
          mutate(Name = fct_reorder(Name, Marienthal)) %>% 
          ggplot(aes(x = Name, y = Marienthal)) +
          geom_segment(aes(xend = Name, yend = Control_towns), color = line_col) +
          geom_point(aes(y = Control_towns), color = towns_col, shape = 21, size = 2) +
          geom_point(color = treat_col, size = 2) +
          scale_y_continuous(limits=c(0,1), expand = c(0, 0)) +
          coord_flip() +
          theme_minimal() +
          theme(plot.margin = margin(.1,.2,.1,.1, unit = "in"),
                plot.subtitle = element_markdown()) +
          # theme(plot.background = element_rect(fill = back_col)) +
          labs(
            x = "",
            y = "",
            title = "Outcomes for 2022",
            subtitle = "<span style='color:firebrick;'>Marienthal</span> (all treated), and <span style='color:royalblue4;'>Control towns</span>."
          )

    
    ggsave(paste0("Figures/Survey_averages_with_controltowns_plot_2022_linear", filename_modifier, ".png"),
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
    
    ggsave(paste0("Figures/Confidence_intervals_GN_vs_controltown_2022_linear", filename_modifier, ".png"),
           p1,
           width = 7,
           height=plot_height)
    
    p1
}
table_combined = function(treatment_effects,
                          filename_modifier = "") {
    filename = paste0("Figures/Survey_table_2022_linear", filename_modifier, ".tex")
    
    treatment_effects |> 
        select(Name, Marienthal, Control_towns, estimate, std.error, nmt, nct) |> 
        arrange(-Marienthal) |>
        knitr::kable(
            col.names = c(
                "Outcome", "Marienthal", "Control towns",
                "Mt vs. Ct towns",
                "SE",
                "$n_{mt}$",
                "$n_{ct}$"
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
plot_treatment_effects(te_lm2)

# Plot separately for economic and non-economic variables ---- 

economic_variables = c("employment_ams", "income_security","unemployment_ams", "unemployment_spells", "income")



plot_treatment_effects(te_lm2 |> 
                           filter(Outcome %in% economic_variables),
                       "_econ")
plot_treatment_effects(te_lm2 |> 
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

ggsave(paste0("Figures/Confidence_intervals_GN_vs_controltown_2022_combined_linear.png"),
       p_ccf1 / p_ccf2 + plot_layout(heights = c(1, 3)),
       width = 7, height = 7)

# Print table ----
te = te_lm2 %>%
    left_join(te_lm_se, by = "Outcome") |>
    left_join(group_names, by = "Outcome")

te |>
    filter(Outcome %in% economic_variables) |>
    table_combined("_econ")

te |>
    filter(!(Outcome %in% economic_variables)) |>
    table_combined("_nonecon")
