# Use admin earnings to estimate person level outcomes ----
# for contrast 1), contrast to 3)
# 2025-10 for AEJ:Policy revisions
# set wd and veracrypt path first in master.R first before runnig this file
# data ends with 2024-02-01

# Load packages ----
library(tidyverse)
library(data.table)
library(dplyr)
library(lubridate)
library(tidyr)

library(readr)
library(purrr)
library(broom)
library(estimatr)
library(ggtext)



# Settings ----

# Define paths
#data_path <- paste0(veracrypt_path, "jobguarantee/2025-10-admin-earnings-raw/")
data_path <- paste0(veracrypt_path, "jobguarantee/2025-10-admin-earnings-processed/")

# Loading data ----

# Loading outcomes
Marienthal_responses_aggregated =
  paste0(veracrypt_path, "jobguarantee/2025-10-admin-earnings-processed/magma_admin_income_earnings_normalized_magma.csv") %>% 
  read_csv() %>%
  mutate(town = "Gramatneusiedl") 

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
  paste0(veracrypt_path, "jobguarantee/2025-10-admin-earnings-processed/magma_admin_income_earnings_normalized_control.csv") %>%  
  read_csv() |> 
  mutate(town = "Control") 

# Loading baseline covariates
Control_covariates =
  paste0(veracrypt_path, "jobguarantee/2020-12-control-admin-data-processed/control_participants_for_survey.csv") %>% 
  read_csv()

# merging outcomes and covariates
Control_responses_aggregated = Control_responses_aggregated %>% 
  right_join(Control_covariates, by = "PSTNR") |> 
  filter(UE_DAYS < 4000) # to balance unemployment duration across the two samples))


# Combining Marienthal and control ----
# Loading outcome names to label plots
group_names = read_csv("variable_description_admin_incomes.csv")[-1, ] %>% 
  filter(include == 1) |> 
  select(grouping_variable_aggregate2, grouping_variable_name_aggregate2) %>% 
  group_by(grouping_variable_aggregate2) %>% 
  slice(1) %>% 
  rename(Outcome = grouping_variable_aggregate2,
         Name = grouping_variable_name_aggregate2) # %>% 
  # rbind(tibble(Outcome = c("income", "number_of_contacts"),
  #              Name = c("Income", "Number of contacts")))

outcome_variables = group_names$Outcome

#outcome_variables = outcome_variables[-c(14)]

Combined_responses =
  bind_rows(Marienthal_responses_aggregated, Control_responses_aggregated) |> 
  mutate(town = factor(town))
# Calculate mean of control variables for Marienthal
GN_means = Combined_responses  |> 
  filter(town == "Gramatneusiedl") |> 
  select(all_of(control_variables)) |> 
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
names(te_lm2) = c("Control_towns", "Gramatneusiedl")


# Number of observations
nobs_marienthal= map_int(outcome_variables,
                         ~ sum(!is.na(Combined_responses[[.x]]) & 
                                 (Combined_responses$town == "Gramatneusiedl")))
nobs_controltown = map_int(outcome_variables,
                           ~ sum(!is.na(Combined_responses[[.x]]) & 
                                   (Combined_responses$town == "Control")))

te_lm2 = te_lm2 %>% 
  mutate(Outcome = outcome_variables,
         nmt= nobs_marienthal, nct = nobs_controltown) %>% 
  select(Outcome, Gramatneusiedl, Control_towns, nmt, nct)


# Estimation of treatment effects with standard errors

Combined_responses =
  Combined_responses |> 
  mutate(comparison_group = (town == "Gramatneusiedl"))

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
#    mutate(Name = fct_reorder(Name, Gramatneusiedl)) %>% # ordering by mean for GN
    ggplot(aes(x = Name, y = Gramatneusiedl)) +
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
      title = "Admin earnings and income",
      subtitle = "<span style='color:firebrick;'>Gramatneusiedl</span> (all treated), and <span style='color:royalblue4;'>Control towns</span>."
    )
  
  
  ggsave(paste0("Figures/Admin_earnings_incmoe_controltowns_plot_linear", filename_modifier, ".png"),
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
#    mutate(Name = fct_reorder(Name, estimate)) %>% # ordering by treatment effect size
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
  
  ggsave(paste0("Figures/Admin_earnings_incmoe_GN_vs_controltown_linear", filename_modifier, ".png"),
         p1,
         width = 7,
         height=plot_height)
  
  p1
}
table_combined = function(treatment_effects,
                          filename_modifier = "") {
  filename = paste0("Figures/Admin_earnings_incmoe_table_linear", filename_modifier, ".tex")
  
  treatment_effects |> 
    select(Name, Gramatneusiedl, Control_towns, estimate, std.error, nmt, nct) |> 
    arrange(-Gramatneusiedl) |>
    knitr::kable(
      col.names = c(
        "Outcome", "Gramatneusiedl", "Control towns",
        "Gn vs. Ct towns",
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



# plot_treatment_effects(te_lm2 |> 
#                          filter(Outcome %in% economic_variables),
#                        "_econ")
# plot_treatment_effects(te_lm2 |> 
#                          filter(!(Outcome %in% economic_variables)),
#                        "_nonecon")



# Plot confidence intervals ----


plot_confidence_intervals(te_lm_se)

# p_ccf1 = plot_confidence_intervals(te_lm_se |> 
#                                      filter(Outcome %in% economic_variables),
#                                    "_econ", "Economic outcomes")
# p_ccf2 = plot_confidence_intervals(te_lm_se |> 
#                                      filter(!(Outcome %in% economic_variables)),
#                                    "_nonecon", "Other outcomes")

ggsave(paste0("Figures/Admin_earnings_incmoe_confidence_intervals_GN_vs_controltown_linear.png"),
       p_ccf1 / p_ccf2 + plot_layout(heights = c(1, 3)),
       width = 7, height = 7)

# Print table ----
te = te_lm2 %>%
  left_join(te_lm_se, by = "Outcome") |>
  left_join(group_names, by = "Outcome")

# te |>
#   filter(Outcome %in% economic_variables) |>
#   table_combined("_econ")

te |>
  filter(!(Outcome %in% economic_variables)) |>
  table_combined("_admin_earnings_income")



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
                            data = Combined_responses, upper = T))$coefficients
  ) %>% bind_rows()

te_lm_lower = 
  map(outcome_variables,
      function(outcome) 
        lm(formula = as.formula(paste0(
          outcome, "~", regressors_2)), 
          trimmed_responses(outcome, control_variables, 
                            data = Combined_responses, upper = F))$coefficients
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
        "Outcome", "Gn vs. Ct towns",
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

# bound_estimates |> 
#   filter(Outcome %in% economic_variables) |> 
#   table_bounds("Figures/admin_earnings_income_econ.tex")

bound_estimates |> 
  filter(!(Outcome %in% economic_variables)) |> 
  table_bounds("Figures/admin_earnings_income_leebounds.tex")

