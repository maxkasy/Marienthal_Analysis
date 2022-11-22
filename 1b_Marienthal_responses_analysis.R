source("1b_i_Inference_functions.R")
source("1b_ii_plot_functions.R")

# set up parallel computing
future::plan(multisession)
# Number of draws for randomization inference
number_of_draws = 1000


if (aggregated) {
    inputfile = "MAGMA-Participants-Aggregated.csv"
    suff = "" # file suffix for output
    outcome = "grouping_variable_aggregate2"
    name = "grouping_variable_name_aggregate2"
} else { # disaggregated estimates
    inputfile = "MAGMA-Participants-Aggregated_detailed.csv"
    suff = "_detailed"
    outcome = "grouping_variable_detailed"
    name = "grouping_variable_name_detailed"
}



# Loading data ----

# Loading actual outcomes
responses_aggregated =
    paste0(veracrypt_path,
           "jobguarantee/2021-02-survey-data-processed/",
           inputfile) %>%
    read_csv()

# Loading outcome names to label plots
variable_description = read_csv("variable_description.csv")[-1, ] %>% 
  filter(include == 1) 

group_names = tibble(Outcome = variable_description[[outcome]],
                     Name =  variable_description[[name]]) %>% 
    group_by(Outcome) %>% 
    slice(1)  %>% 
    rbind(tibble(Outcome = c("income", "number_of_contacts"),
                 Name = c("Income", "Number of contacts")))

outcome_variables = intersect(
    group_names$Outcome,
    colnames(responses_aggregated) # eliminating variables that don't exist in the data
)
# Subset of economic variables:
economic_variables = c("employment_ams", "income_security","unemployment_ams", "unemployment_spells", "income")

# Loading baseline covariates
baseline_covariates =
  paste0(veracrypt_path, "jobguarantee/2020-09-admin-data-processed/participant_assignment_full.csv") %>% 
  read_csv() %>% 
  mutate(treatment_wave = factor(treatment_wave, levels = c(2,1))) # specifying factor levels for linear regression

control_variables = names(baseline_covariates)[-(1:4)]

# merging outcomes and covariates 
responses_and_controls = responses_aggregated %>%
    right_join(baseline_covariates, by = "PSTNR") %>% 
    mutate(pair_dummy = factor(pmin(PSTNR, match))) 

# Loading pairwise matches, for randomization inference using the nbpMatching package
pairwise_matches_rand_inf =
    paste0(veracrypt_path, "/jobguarantee/2020-09-admin-data-processed/Pairwise_matches_for_randomization_inference.csv") %>%
    read_csv() %>%
    as.data.frame()





# Sample descriptives -----
if (aggregated){
    summary_descriptives(responses_and_controls, responses_aggregated)
}


# Treatment effects and p-values ----

# Looping over different specifications for the controls to get treatment effects and p-values
control_tbl = tibble(
    controls = list("", control_variables, "pair_dummy"),
    suffix = c("no_controls", "linear_controls", "pair_controls")
)

for (c in 1:3){
    suffix = paste0(control_tbl[[c,"suffix"]], suff) # file suffix for output
    
    # Treatment effects and Randomization inference
    pvalues = responses_and_controls |> 
        randomization_inference_fun(outcome_variables, 
                                    unlist(control_tbl[[c,"controls"]]),
                                    number_of_draws = number_of_draws) 
    p_te = pvalues |> 
        plot_treatment_effects(suffix)
    p_pv = pvalues |> 
        plot_p_values(suffix)
    # Combined figure
    ggsave(paste0("Figures/Survey_combined_", suffix, ".png"),
           plot_combined(p_te, p_pv),
           width = 10, height= 3)
    # Confidence intervals  
    treatment_effects_CI_SE = responses_and_controls  |>  
        treatment_effects_lm_fun_SE(outcome_variables, unlist(control_tbl[[c,"controls"]])) 
    treatment_effects_CI_SE |> 
        plot_confidence_intervals(suffix)
    # Table
    pvalues |> 
        left_join(treatment_effects_CI_SE |> select(Outcome, std.error), 
                  by = "Outcome") |> 
        table_p_values(suffix)
    
    # Split plots between economic and other variables, for aggregate indices
    if (aggregated){
        # plots for economic variables
        econ_suffix = paste0(control_tbl[[c,"suffix"]], "_econ", suff) # file suffix for output
        pvalues_ec = pvalues |> 
            filter(Outcome %in% economic_variables)
        p_te = pvalues_ec |> 
            plot_treatment_effects(econ_suffix, 
                                   TE_title = "Economic outcomes")
        p_pv = pvalues_ec |> 
             plot_p_values(econ_suffix)
        p_comb1 = plot_combined(p_te, p_pv)
        p_ccf1 = treatment_effects_CI_SE |> 
            filter(Outcome %in% economic_variables) |> 
            plot_confidence_intervals(econ_suffix, "Economic outcomes")
        pvalues_ec |> 
            left_join(treatment_effects_CI_SE |> select(Outcome, std.error), 
                      by = "Outcome") |> 
            table_p_values(econ_suffix)
        
        # plots for non-economic variables
        non_econ_suffix = paste0(control_tbl[[c,"suffix"]], "_nonecon", suff) # file suffix for output
        pvaluesnonec = pvalues |> 
            filter(!(Outcome %in% economic_variables)) 
        p_te = pvaluesnonec |> 
            plot_treatment_effects(non_econ_suffix, 
                                   TE_title = "Other outcomes")
        p_pv = pvaluesnonec |> 
             plot_p_values(non_econ_suffix)
        p_comb2 = plot_combined(p_te, p_pv)
        p_ccf2 = treatment_effects_CI_SE |> 
            filter(!(Outcome %in% economic_variables)) |> 
            plot_confidence_intervals(non_econ_suffix, title = "Other outcomes")
        pvaluesnonec |> 
            left_join(treatment_effects_CI_SE |> select(Outcome, std.error), 
                      by = "Outcome") |> 
            table_p_values(non_econ_suffix)
        
        # combined figures
        ggsave(paste0("Figures/Survey_combined_", suffix, ".png"),
               (p_comb1 / p_comb2) + plot_layout(heights = c(1, 3)),
               width = 10, height= 5.5)
        
        ggsave(paste0("Figures/Confidence_intervals_combined_", suffix, ".png"),
               p_ccf1 / p_ccf2 + plot_layout(heights = c(1, 3)),
               width = 7, height = 7)
    }
    
}








  