outcomes_subpath = "jobguarantee/2023-02-municipal-data-processed/"
data_path = paste(veracrypt_path, outcomes_subpath, sep = "")

Gramatneusiedl = 30731 # GKZ for treated town

outcomes = c("UE_rate_tot_sv_control", "UE_long_rate_tot_sv", "UE_short_rate_tot_sv",
             "EMP_rate_tot", "Inactive_rate_tot" )

outcome_labels = c("Unemployment", "Long-term unemployment", "Short-term unemployment",
                   "Employment", "Inactivity rate")

# previously used this file in 2022-02-municipal-data-processed/:
# paste0(data_path, "municipalities_merged_monthly_outcomes.csv") %>%
admin_data_towns =
  paste0(data_path, "municipalities_merged_monthly.csv") %>%
  read_csv( 
    col_types = paste(c("icc", rep("n", 81)), collapse = '')   ) %>% 
  mutate(month =  parse_date_time(month, "%Y-%m")) %>% 
  filter(month >  parse_date_time("2019-01", "%Y-%m"),
         month < parse_date_time("2022-10", "%Y-%m"))


synthetic_control_weights =
  "synthetic_control_weights.csv" %>%
  read_csv() %>%
  rename(GKZ = unit.numbers, weight = w.weights)

synthetic_permutation_weights =
  "synthetic_permutation_weights.csv" %>% 
  read_csv() %>%
  rename(GKZ = control)


synthetic_estimates = function(treated_town,
                               synthetic_control_weights) {
  admin_data_treated_town = 
    admin_data_towns  %>% 
    filter(GKZ == treated_town) %>% 
    select(c("GKZ", "month", outcomes))
  
  admin_data_control_towns =
    admin_data_towns  %>%
    filter(GKZ %in% synthetic_control_weights$GKZ) %>%
    left_join(synthetic_control_weights, by = "GKZ")
  
  # calculate synthetic control averages of outcomes
  synthetic_control_averages = admin_data_control_towns %>%
    group_by(month) %>%
    summarize(across(outcomes, ~ weighted.mean(.x, w = weight))) %>%
    mutate(GKZ = -1)
  
  town_treatment_effects =
    synthetic_control_averages %>% 
    mutate_at(outcomes, ~ -.x) %>% 
    bind_rows(admin_data_treated_town) %>% 
    group_by(month) %>% 
    select(-GKZ) %>% 
    summarise_all(sum) %>% 
    pivot_longer(cols = outcomes, names_to = "Outcome")
  
  town_treatment_effects
}

town_treatment_effects =
  synthetic_estimates(Gramatneusiedl, synthetic_control_weights)

permutation_treatment_effects=
  map(unique(synthetic_permutation_weights$GKZ),
    function(GKZ) synthetic_estimates(GKZ, 
                          synthetic_permutation_weights %>% 
                            filter(GKZ == GKZ)) %>% 
      mutate(GKZ = GKZ)) %>% 
  bind_rows()



