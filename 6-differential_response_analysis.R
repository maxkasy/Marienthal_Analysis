# 6-differential_response_analysis
# Balance table to assess differential survey response rates

## Settings ----

home <- getwd()

data_out <- file.path(home, "Figures")

data_path_2021 = paste0(veracrypt_path, "jobguarantee/2021-02-survey-data-processed")
data_path_2022 = paste0(veracrypt_path, "jobguarantee/2022-02-survey-data-processed")

# treatment
data_path_treat_2021 <- file.path(data_path_2021, "MAGMA-Participants-Aggregated.csv")
data_path_treat_2022 <- file.path(data_path_2022, "MAGMA-Participants-Aggregated.csv")

# control towns
data_path_control_2021 <- file.path(data_path_2021, "MAGMA-Control-towns-Aggregated.csv")
data_path_control_2022 <- file.path(data_path_2022, "MAGMA-Control-towns-Aggregated.csv")

# Start code ####
### load data #####
# treat
data_treat_2021 <- fread(data_path_treat_2021)
data_treat_2022 <- fread(data_path_treat_2022)

# control
data_control_2021 <- fread(data_path_control_2021)
data_control_2022 <- fread(data_path_control_2022)

# load baseline characteristics
# treat
data_treat_baseline =
  paste0(veracrypt_path, "jobguarantee/2020-09-admin-data-processed/participant_assignment_full.csv") %>% 
  read_csv() 

# control
data_control_baseline =
  paste0(veracrypt_path, "jobguarantee/2020-12-control-admin-data-processed/control_participants_for_survey.csv") %>% 
  read_csv() 

### create dummy for survey response ####
# treat 2021
data_treat_response_2021 <- data_treat_2021 %>% 
  mutate(response2021 = if_else(!is.na(wellbeing_direct), 1, 0)) %>% 
  select(PSTNR, response2021) # drop outcomes

# treat 2022
data_treat_response_2022 <- data_treat_2022 %>% 
  mutate(response2022 = if_else(!is.na(wellbeing_direct), 1, 0)) %>% 
  select(PSTNR, response2022) # drop outcomes

# control 2021
data_control_response_2021 <- data_control_2021 %>% 
  mutate(response2021 = if_else(!is.na(wellbeing_direct), 1, 0)) %>% 
  select(PSTNR, response2021) # drop outcomes

# control 2022
data_control_response_2022 <- data_control_2022 %>% 
  mutate(response2022 = if_else(!is.na(wellbeing_direct), 1, 0)) %>% 
  select(PSTNR, response2022) # drop outcomes

### merge ####
## outcomes and baseline
# treat 2021
merged_treat <- data_treat_baseline %>%
  full_join(data_treat_response_2021, by = "PSTNR") %>% 
  full_join(data_treat_response_2022, by = "PSTNR")

merged_treat %>%
  group_by(response2021, treatment_wave) %>%
  summarise(across(!any_of(c("PSTNR", "match", "match_row")), \(x) mean(x, na.rm = TRUE)))

merged_treat %>%
  group_by(response2022, treatment_wave) %>%
  summarise(across(!any_of(c("PSTNR", "match", "match_row")), \(x) mean(x, na.rm = TRUE)))

# control
merged_control <- data_control_baseline %>%
  full_join(data_control_response_2021, by = "PSTNR") %>% 
  full_join(data_control_response_2022, by = "PSTNR")

merged_control %>%
  group_by(response2021) %>%
  summarise(across(-PSTNR, \(x) mean(x, na.rm = TRUE)))

merged_control %>%
  group_by(response2022) %>%
  summarise(across(-PSTNR, \(x) mean(x, na.rm = TRUE)))

## append treat and control
merged_treat <- merged_treat %>% 
  rename(group = treatment_wave)

merged_control <- merged_control %>% 
  mutate(group = 3)

merged <- bind_rows(merged_treat, merged_control)

#### Balance table ####
## Export LaTeX table ####
# balance table for respondents only
merged_respondents_2021  <- merged %>% 
  filter(response2021 == 1)

## 2021 Group wise balance tables----

### Experimental ----
## Comparison of experimental treatment and control group (wave 1 and wave 2)
merged_respondents_2021_experimental <- merged_respondents_2021 %>%
  filter(group != 3)

# Perform t-tests and calculate counts of non-missing observations
merged_respondents_2021_experimental %>%
  select(-c(PSTNR, match, match_row, response2021, response2022)) %>%
  pivot_longer(c(MALE, PEALTER, MIG, BILDUNG, EINSCHRAENKUNG, BENEFIT_LEVEL, UE_DAYS),
               names_to = "Covariate") %>%
  mutate(Covariate = factor(Covariate,
                            levels = c("MALE", "PEALTER", "MIG", "BILDUNG", "EINSCHRAENKUNG", "BENEFIT_LEVEL", "UE_DAYS"),
                            labels = c("Male", "Age", "Migration background", "Education", "Medical condition", "Benefit level", "Days unemployed"))) %>%
  group_by(Covariate) %>%
  # Calculate t-tests
  rstatix::t_test(value ~ group, detailed = TRUE) %>%
  ungroup() %>%
  # Calculate counts for each group (non-missing values)
  left_join(
    merged_respondents_2021_experimental %>%
      select(-c(PSTNR, match, match_row, response2021, response2022)) %>%
      pivot_longer(c(MALE, PEALTER, MIG, BILDUNG, EINSCHRAENKUNG, BENEFIT_LEVEL, UE_DAYS),
                   names_to = "Covariate") %>%
      group_by(Covariate, group) %>%
      summarise(n = sum(!is.na(value)), .groups = 'drop') %>%
      pivot_wider(names_from = group, values_from = n, names_prefix = "n")
  ) %>%
  # Reorder columns so n1 and n2 are on the right-hand side
  select(Covariate, estimate1, estimate2, estimate, statistic, p, n1, n2) %>%
  kable(col.names = c("Covariate", "Wave 1", "Wave 2", "Difference", "t-statistic", "p-value", "$n_1$", "$n_2$"),
        row.names = FALSE,
        digits = 3,
        format = "latex",
        booktabs = TRUE,
        escape = FALSE,
        linesep = "") %>%
  write(file.path(data_out, "balance_respondents_2021_experimental_ttests.tex"))


### Control towns ----
## Comparison of non-experimental treatment and control groups (Gramatneusiedl and Control towns)
merged_respondents_2021_control_towns <- merged_respondents_2021 %>%
  mutate(group = if_else(group == 2, 1, group))


# Perform t-tests and calculate counts of non-missing observations for control towns
merged_respondents_2021_control_towns %>%
  select(-c(PSTNR, match, match_row, response2021, response2022)) %>%
  pivot_longer(c(MALE, PEALTER, MIG, BILDUNG, EINSCHRAENKUNG, BENEFIT_LEVEL, UE_DAYS),
               names_to = "Covariate") %>%
  mutate(Covariate = factor(Covariate,
                            levels = c("MALE", "PEALTER", "MIG", "BILDUNG", "EINSCHRAENKUNG", "BENEFIT_LEVEL", "UE_DAYS"),
                            labels = c("Male", "Age", "Migration background", "Education", "Medical condition", "Benefit level", "Days unemployed"))) %>%
  group_by(Covariate) %>%
  # Calculate t-tests
  rstatix::t_test(value ~ group, detailed = TRUE) %>%
  ungroup() %>%
  # Calculate counts for each group (non-missing values)
  left_join(
    merged_respondents_2021_control_towns %>%
      select(-c(PSTNR, match, match_row, response2021, response2022)) %>%
      pivot_longer(c(MALE, PEALTER, MIG, BILDUNG, EINSCHRAENKUNG, BENEFIT_LEVEL, UE_DAYS),
                   names_to = "Covariate") %>%
      group_by(Covariate, group) %>%
      summarise(n = sum(!is.na(value)), .groups = 'drop') %>%
      pivot_wider(names_from = group, values_from = n, names_prefix = "n")
  ) %>%
  # Reorder columns so n1 and n2 are on the right-hand side
  select(Covariate, estimate1, estimate2, estimate, statistic, p, n1, n2) %>%
  kable(col.names = c("Covariate", "Gramatneusiedl", "Control towns", "Difference", "t-statistic", "p-value", "$n_1$", "$n_2$"),
        row.names = FALSE,
        digits = 3,
        format = "latex",
        booktabs = TRUE,
        escape = FALSE,
        linesep = "") %>%
  write(file.path(data_out, "balance_respondents_2021_control_towns_ttests.tex"))



### 2022 -----
# 2022 only for Gramatneusiedl vs. control towns
# balance table for respondents only
merged_respondents_2022  <- merged %>% 
  filter(response2022 == 1) %>% 
  mutate(treat_2022 = if_else(group == 3, 0, 1),
         control_town = if_else(group == 3, 1, 0)) 

# t-tests for equality of covariate means
merged_respondents_2022 %>%
  select(-c(PSTNR, match, match_row, response2021, response2022, group)) %>%
  pivot_longer(c(MALE,PEALTER,MIG,BILDUNG,EINSCHRAENKUNG,BENEFIT_LEVEL,UE_DAYS),
               names_to = "Covariate") %>%
  mutate(Covariate = factor(Covariate,
                            levels = c("MALE","PEALTER","MIG","BILDUNG","EINSCHRAENKUNG","BENEFIT_LEVEL","UE_DAYS"),
                            labels = c("Male","Age","Migration background","Education","Medical condition","Benefit level","Days unemployed"))) %>% 
  group_by(Covariate) %>%
  rstatix::t_test(value ~ control_town, detailed = T) %>%
  ungroup() %>%
  # Calculate counts for each group (non-missing values)
  left_join(
    merged_respondents_2022 %>%
      select(-c(PSTNR, match, match_row, response2021, response2022, group)) %>%
      pivot_longer(c(MALE, PEALTER, MIG, BILDUNG, EINSCHRAENKUNG, BENEFIT_LEVEL, UE_DAYS),
                   names_to = "Covariate") %>%
      group_by(Covariate, control_town) %>%
      summarise(n = sum(!is.na(value)), .groups = 'drop') %>%
      pivot_wider(names_from = control_town, values_from = n, names_prefix = "n") %>%
      select(Covariate, n2 = `n1`, n1 = `n0`) # Switch n1 and n2 to correspond to 2021 comparison
  ) %>%
  # Reorder columns so n1 and n2 are on the right-hand side
  select(Covariate, estimate1, estimate2, estimate, statistic, p, n1, n2) %>%
  kable(col.names = c("Covariate","Gramatneusiedl","Control towns","Difference","t-statistic","p-value", "$n_1$", "$n_2$"),
        row.names = F,
        digits = 3,
        format = "latex",
        booktabs = TRUE,
        escape = F, 
        linesep = "") %>%
  write(file.path(data_out, "balance_respondents_2022_ttests.tex"))

