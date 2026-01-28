# run after 0b_i_admin_data_prep_2021_2024_periods.R

# Define paths
startdate <- "2020-09-30"  
data_path <- paste0(veracrypt_path, "jobguarantee/2024-02-admin-data-raw/")
# survey_path = paste0(veracrypt_path, "jobguarantee/2024-02-survey-data-processed/") # no survey data for 2024
admin_out <- paste0(veracrypt_path, "jobguarantee/2024-02-admin-data-processed/")
enddate <- "2024-02-28"    
output_path = "Figures/Employment_periods/"

filenames = c("Erwerbsbiographie_Kontrollgruppe.csv",
              "Erwerbsbiographie_ProgrammteilnehmerInnen.csv",
              "Leistungsbezug_Kontrollgruppe.csv",
              "Leistungsbezug_ProgrammteilnehmerInnen.csv",
              "Unistatus_ProgrammteilnehmerInnen.csv") 

file_paths = paste0(data_path, filenames)

data_list_raw = map(file_paths, function(path) read_delim(path, delim=";",
                                                          locale = locale(encoding = "latin1", decimal_mark = ",")))

### Settings  ---------------------------------
# 2.b work history - via Erwerbshistorie instad of Unistatus (days EMPLOYED since program start)
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


#### Gramatneusiedl ----

work_history_ams_raw <- 
  read_delim(
    file_paths[[2]], 
    delim = ";",
    locale = locale(encoding = "latin1", decimal_mark = ",")
  ) %>% 
  mutate(
    STICHTAG = as.Date(STICHTAG, format = "%d.%m.%Y")  # Adjust format as needed
  )


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
  write_csv(paste0(admin_out, "participants_monthly_empl_diff.csv"))

## Write data
monthly_cumsum %>% 
  write_csv(paste0(admin_out, "participants_monthly_empl_cumsum.csv"))      


#### Control towns ----

work_history_ams_raw <- 
  read_delim(
    file_paths[[1]], 
    delim = ";",
    locale = locale(encoding = "latin1", decimal_mark = ",")
  ) %>% 
  mutate(
    STICHTAG = as.Date(STICHTAG, format = "%d.%m.%Y")  # Adjust format as needed
  )

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

