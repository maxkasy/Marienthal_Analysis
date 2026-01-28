# 5-cost-comparison
library(tidyverse)
library(readr)
library(data.table)
library(ggtext)
library(janitor)

## Settings ----

home <- dirname(getwd())
# home <- getwd() 

Windows = F
# Switch path for data-files T or F
if (Windows) {veracrypt_path = "A:/"} else {veracrypt_path = "/Volumes/NO NAME/"}

AMS = F
# Switch path for data-files between AMS and Lukas/Max computing environment
if (AMS) {
  
  setwd("V:/AES/03_Forschung/0_Projekte/2020/1_MAGMA/!Uni Oxford/5_Daten/202401 Kosten/Cost_Calculation")
  
  data_path = file.path(home, "cost_data")
  
  pstnr_data <- file.path(home, "pstnr_magma_control_towns.csv")
  
} else {
  
  data_path = file.path(dirname(home), "Cost_data", "cost_data")
  
  pstnr_data <- paste0(veracrypt_path, "jobguarantee/pstnr_magma_control_towns.csv")
}

## Set paths ----
data_path = file.path(home, "cost_data")

alv_data <- file.path(data_path, "alv.csv")

foer_individual_treat_data <- file.path(data_path, "individualfoerderung_treat.csv")
foer_individual_control_data <- file.path(data_path, "individualfoerderung_control.csv")
foer_traeger_treat_data <- file.path(data_path, "traegerfoerderung_treat.csv")
foer_traeger_control_data <- file.path(data_path, "traegerfoerderung_control.csv")

## Load data ----
# load costs data
data_alv <- fread(alv_data)

data_foer_individual_treat <- fread(foer_individual_treat_data)
data_foer_individual_control <- fread(foer_individual_control_data)
data_foer_traeger_treat <- fread(foer_traeger_treat_data)
data_foer_traeger_control <- fread(foer_traeger_control_data)

magma_pstnr <- fread("pstnr_magma.csv")
control_pstnr <- fread("pstnr_control_towns.csv")

# # Daniel extra code for AMS computer
# magma_pstnr <- magma_pstnr$magma_pstnr
# control_pstnr <- control_pstnr$control_pstnr

## Prepare data ----
# Create a treat variable based on whether PSTNR is in magma_pstnr
data_alv <- data_alv %>%
  mutate(treat = ifelse(PST_KEY %in% magma_pstnr, 1, 0),
         control = ifelse(PST_KEY %in% control_pstnr, 1, 0))

# Filter out rows that are neither treat == 1 nor control == 1
data_alv <- data_alv %>%
  filter(treat == 1 | control == 1)


# add treatment dummy: foer
data_foer_individual_treat <- data_foer_individual_treat %>% 
  janitor::clean_names() %>%  # Clean column names to ensure uniqueness
  mutate(treat = 1)

data_foer_individual_control <- data_foer_individual_control %>% 
  janitor::clean_names() %>%  # Clean column names to ensure uniqueness
  mutate(treat = 0)

data_foer_traeger_treat <- data_foer_traeger_treat %>% 
  janitor::clean_names() %>%  # Clean column names to ensure uniqueness
  mutate(treat = 1)

data_foer_traeger_control <- data_foer_traeger_control %>% 
  janitor::clean_names() %>%  # Clean column names to ensure uniqueness
  mutate(treat = 0)


## append datasets
# Append the two datasets
data_foer_individual <- bind_rows(data_foer_individual_treat, data_foer_individual_control)

data_foer_traeger <- bind_rows(data_foer_traeger_treat, data_foer_traeger_control)

### FOER calculation ----
# separately for Individualfoerderung and Traegerfoerderung

## filter for treat and control individuals ----
# Filter out rows that are neither treat == 1 nor control == 1
data_foer_individual <- data_foer_individual %>%
  filter(pst_key %in% c(magma_pstnr, control_pstnr))

data_foer_traeger <- data_foer_traeger %>%
  filter(pst_key %in% c(magma_pstnr, control_pstnr))


## Exclude / shorten spells that started before 01.01.2020 ----
# Convert BEGINN and ENDE columns to Date format
data_foer_individual <- data_foer_individual %>%
  mutate(
    foerderung_von = as.Date(foerderung_von, format = "%d.%m.%Y"),
    foerderung_bis = as.Date(foerderung_bis, format = "%d.%m.%Y")
  )

data_foer_traeger <- data_foer_traeger %>%
  mutate(
    foerderung_von = as.Date(foerderung_von, format = "%d.%m.%Y"),
    foerderung_bis = as.Date(foerderung_bis, format = "%d.%m.%Y")
  )


# Exclude rows where ENDE is before 01.10.2020
data_foer_individual <- data_foer_individual %>%
  filter(foerderung_bis >= as.Date("2020-10-01"))

data_foer_traeger <- data_foer_traeger %>%
  filter(foerderung_bis >= as.Date("2020-10-01"))


# Set BEGINN before 01.01.2020 to 01.01.2020
data_foer_individual <- data_foer_individual %>%
  mutate(START = pmax(foerderung_von, as.Date("2020-10-01")))

data_foer_traeger <- data_foer_traeger %>%
  mutate(START = pmax(foerderung_von, as.Date("2020-10-01")))


# Exclude rows where BEGINN is after 31.03.2024 (ADDED)
data_foer_individual <- data_foer_individual %>%
  filter(foerderung_von <= as.Date("2024-03-31"))
 
data_foer_traeger <- data_foer_traeger %>%
  filter(foerderung_von <= as.Date("2024-03-31"))

# Set ENDE after 31.03.2024 to 31.03.2024 (ADDED)
data_foer_individual <- data_foer_individual %>%
  mutate(ENDE = pmin(foerderung_bis, as.Date("2024-03-31")))

data_foer_traeger <- data_foer_traeger %>%
  mutate(ENDE = pmin(foerderung_bis, as.Date("2024-03-31")))


# Calculate days since program start (2020-10-01) (ADDED or MODIFIED see START)
data_foer_individual <- data_foer_individual %>%
  mutate(DAYS = as.numeric(difftime(ENDE, START, units = "days")),
         DAYS = DAYS + 1, # to include the end day as well
         # calcualate days undadjusted:
         DAYS_unadjusted = as.numeric(difftime(foerderung_bis, foerderung_von, units = "days")),
         DAYS_unadjusted = DAYS_unadjusted + 1) # to include the end day as well) 

data_foer_traeger <- data_foer_traeger %>%
  mutate(DAYS = as.numeric(difftime(ENDE, START, units = "days")),
         DAYS = DAYS + 1, # to include the end day as well
         # calcualate days undadjusted:
         DAYS_unadjusted = as.numeric(difftime(foerderung_bis, foerderung_von, units = "days")),
         DAYS_unadjusted = DAYS_unadjusted + 1) # to include the end day as well) 


## Drop rows that appear twice ----
# with same gesamtbewilligung, foerderung_von, foerderung_bis
data_foer_individual <- data_foer_individual %>%
  group_by(gesamtbewilligung, foerderung_von, foerderung_bis) %>%
  # If beihilfe_2 differs within the group, keep the first row; otherwise keep all
  filter(n() == 1 | n_distinct(beihilfe_2) > 1 & row_number() == 1) %>%
  ungroup()

# with same foerderung_von, foerderung_bis
data_foer_traeger <- data_foer_traeger %>%
  group_by(foerderung_von, foerderung_bis) %>%
  # If beihilfe_2 differs within the group, keep the first row; otherwise keep all
  filter(n() == 1 | n_distinct(beihilfe_2) > 1 & row_number() == 1) %>%
  ungroup()


## Calculate daily cost for foer ----

# Convert 'gesamtbewilligung' to numeric
data_foer_individual <- data_foer_individual %>%
  mutate(gesamtbewilligung = str_replace_all(gesamtbewilligung, ",", ""), # Remove commas
         gesamtbewilligung = as.numeric(gesamtbewilligung))                # Convert to numeric


# Convert 'kosten_pro_tn_tag' to numeric
data_foer_traeger <- data_foer_traeger %>%
  mutate(
#    kosten_pro_tn_tag = str_replace_all(kosten_pro_tn_tag, ",", ""), # Remove commas
    kosten_pro_tn_tag = as.numeric(kosten_pro_tn_tag), # Convert to numeric
    kosten_pro_tn_tag_inkl_mitfinanzierungen = as.numeric(kosten_pro_tn_tag_inkl_mitfinanzierungen))                # Convert to numeric


# create daily cost variable
data_foer_traeger <- data_foer_traeger %>%
  mutate(daily_cost = ifelse(!is.na(kosten_pro_tn_tag_inkl_mitfinanzierungen), 
                             kosten_pro_tn_tag_inkl_mitfinanzierungen, 
                             kosten_pro_tn_tag))


# calculate foer cost by actual days since 01.10.2020
data_foer_individual <- data_foer_individual %>%
  mutate(foer_cost_daily = as.numeric(gesamtbewilligung) / DAYS_unadjusted,
         foer_cost = foer_cost_daily * DAYS)

data_foer_traeger <- data_foer_traeger %>%
  mutate(foer_cost = daily_cost * DAYS)


## calculate total sum
# Compute the sum of 'foer_cost' grouped by 'treat'
total_cost_foer_individual <- data_foer_individual %>%
  group_by(treat) %>%
  summarize(total_foer_cost = sum(foer_cost, na.rm = TRUE))

total_cost_foer_traeger <- data_foer_traeger %>%
  group_by(treat) %>%
  summarize(total_foer_cost = sum(foer_cost, na.rm = TRUE))


## Divide total cost by number of people in treatment and control group ----
## count nr of persons: number of unique PST_KEY values
n_foer_individual <- data_foer_individual %>%
  group_by(treat) %>%
  summarize(n_people = n_distinct(pst_key))

n_foer_traeger <- data_foer_traeger %>%
  group_by(treat) %>%
  summarize(n_people = n_distinct(pst_key))

## calculate average cost per person
# merge
total_cost_foer_individual <- total_cost_foer_individual %>%
  left_join(n_foer_individual, by = "treat")

total_cost_foer_traeger <- total_cost_foer_traeger %>%
  left_join(n_foer_individual, by = "treat")


total_cost_foer_individual <- total_cost_foer_individual %>%
  group_by(treat) %>%
  mutate(n_people_start = ifelse(treat == 1, 62, 211))

total_cost_foer_traeger <- total_cost_foer_traeger %>%
  group_by(treat) %>%
  mutate(n_people_start = ifelse(treat == 1, 62, 211))

# divide total cost by nr of individuals by treatment and control group
total_cost_foer_individual <- total_cost_foer_individual %>%
  mutate(person_foer_cost = total_foer_cost / n_people_start)

total_cost_foer_traeger <- total_cost_foer_traeger %>%
  mutate(person_foer_cost = total_foer_cost / n_people_start)


## sum foer individual and traeger costs ----
# merge
total_cost_foer <- bind_rows(total_cost_foer_individual, total_cost_foer_traeger)
  
# sum
total_cost_foer <- total_cost_foer %>% 
group_by(treat) %>%
  summarize(total_foer_cost = sum(total_foer_cost, na.rm = TRUE),
            person_foer_cost = sum(person_foer_cost, na.rm = TRUE), )


### ALV calculation ----

## Exclude / shorten spells that started before 01.01.2020 ----
# Convert BEGINN and ENDE columns to Date format
# data_alv <- data_alv %>%
#   mutate(
#     BEGINN = as.Date(BEGINN, format = "%m/%d/%Y"),
#     ENDE = as.Date(ENDE, format = "%m/%d/%Y")
#   )

#CODE REQUIRED FOR DANIEL since data seems to be read in differently on his computer
data_alv <- data_alv %>%
  mutate(
    BEGINN = as.Date(BEGINN, format = "%m.%d.%Y"),
    ENDE = as.Date(ENDE, format = "%m.%d.%Y")
  )

# Exclude rows where ENDE is before 01.10.2020
data_alv <- data_alv %>%
  filter(ENDE >= as.Date("2020-10-01"))

# Set BEGINN before 01.01.2020 to 01.01.2020
data_alv <- data_alv %>%
  mutate(START = pmax(BEGINN, as.Date("2020-10-01")))

# Exclude rows where BEGINN is after 31.03.2024 (ADDED)
data_alv <- data_alv %>%
  filter(BEGINN <= as.Date("2024-03-31"))

# Set ENDE after 31.03.2024 to 31.03.2024 (ADDED)
data_alv <- data_alv %>%
  mutate(ENDE = pmin(ENDE, as.Date("2024-03-31")))


## Exclude rows that appear erroneously twice ----
# Adjust the Enddatum for overlapping periods
data_alv <- data_alv %>%
  group_by(PST_KEY) %>%
  arrange(START) %>%  # Sort by person and BEGINN date
  mutate(
    next_start = lead(START),  # Get the BEGINN of the next period for the same person
    ENDE = case_when(
      !is.na(next_start) & ENDE >= next_start ~ next_start - as.difftime(1, unit = "days"),  # Shorten ENDE if overlapping
      TRUE ~ ENDE  # Otherwise keep the original ENDE
    )
  ) %>%
  ungroup() %>%
  distinct(PST_KEY, START, ENDE, .keep_all = TRUE)  # Remove exact duplicate periods

# Calculate days since program start (2020-10-01)
data_alv <- data_alv %>%
  mutate(DAYS = as.numeric(difftime(ENDE, START, units = "days")),
         DAYS = DAYS + 1) # to include the end day as well

## Adjust alv for indirect costs (social insurance: health & pension) ----
# add amount to tagsatz varied by level of bemessungsgrundlage (in EUR per month)
# bemessungsgrundlage in steps of EUR 500
# e.g.:
# 3500 -> 19.16
# 4000 -> 21.90
# 4500 -> + 24.64
## add SV costs only for Arbeitslosengeld and Notstandshilfe Bezieher
# do not add SV for those in subsidized employment c("BV" "CL" "FS" "GB" "GC" "GT" "MN" "TO" "XT" "XU")
data_alv <- data_alv %>%
  mutate(
    kv_cost = 0,  # Initialize the column with 0
    kv_cost = case_when( ## calculate kv_cost
      LEISTUNGSART %in% c("AD", "AG", "AL", "AQ", "AS", "ND", "NH") ~ case_when(
        BEMESSUNGSGRUNDLAGE >= 0 & BEMESSUNGSGRUNDLAGE <= 250 ~ -0.27,
        BEMESSUNGSGRUNDLAGE >= 251 & BEMESSUNGSGRUNDLAGE <= 750 ~ 0.15,
        BEMESSUNGSGRUNDLAGE >= 751 & BEMESSUNGSGRUNDLAGE <= 1250 ~ 0.71,
        BEMESSUNGSGRUNDLAGE >= 1251 & BEMESSUNGSGRUNDLAGE <= 1750 ~ 1.28,
        BEMESSUNGSGRUNDLAGE >= 1751 & BEMESSUNGSGRUNDLAGE <= 2250 ~ 1.84,
        BEMESSUNGSGRUNDLAGE >= 2251 & BEMESSUNGSGRUNDLAGE <= 2750 ~ 2.40,
        BEMESSUNGSGRUNDLAGE >= 2751 & BEMESSUNGSGRUNDLAGE <= 3250 ~ 2.96,
        BEMESSUNGSGRUNDLAGE >= 3251 & BEMESSUNGSGRUNDLAGE <= 3750 ~ 3.53,
        BEMESSUNGSGRUNDLAGE >= 3751 & BEMESSUNGSGRUNDLAGE <= 4250 ~ 4.09,
        BEMESSUNGSGRUNDLAGE >= 4251 & BEMESSUNGSGRUNDLAGE <= 4750 ~ 4.65,
        BEMESSUNGSGRUNDLAGE >= 4751 & BEMESSUNGSGRUNDLAGE <= 5249 ~ 5.21,
        BEMESSUNGSGRUNDLAGE >= 5250 & BEMESSUNGSGRUNDLAGE <= 5749 ~ 5.78,
        BEMESSUNGSGRUNDLAGE >= 5750 & BEMESSUNGSGRUNDLAGE <= 6249 ~ 6.34,
        TRUE ~ kv_cost  # Keep the original value if no conditions are met
      ),
      TRUE ~ kv_cost  # Keep the original value
    ),
    pv_cost = 0, ## calculate pv_cost
    pv_cost = case_when(
      LEISTUNGSART %in% c("AD", "AG", "AL", "AQ", "AS", "ND", "NH") ~ case_when(
        BEMESSUNGSGRUNDLAGE >= 0 & BEMESSUNGSGRUNDLAGE <= 250 ~ 0.67,
        BEMESSUNGSGRUNDLAGE >= 251 & BEMESSUNGSGRUNDLAGE <= 750 ~ 2.66,
        BEMESSUNGSGRUNDLAGE >= 751 & BEMESSUNGSGRUNDLAGE <= 1250 ~ 5.32,
        BEMESSUNGSGRUNDLAGE >= 1251 & BEMESSUNGSGRUNDLAGE <= 1750 ~ 7.98,
        BEMESSUNGSGRUNDLAGE >= 1751 & BEMESSUNGSGRUNDLAGE <= 2250 ~ 10.64,
        BEMESSUNGSGRUNDLAGE >= 2251 & BEMESSUNGSGRUNDLAGE <= 2750 ~ 13.30,
        BEMESSUNGSGRUNDLAGE >= 2751 & BEMESSUNGSGRUNDLAGE <= 3250 ~ 15.96,
        BEMESSUNGSGRUNDLAGE >= 3251 & BEMESSUNGSGRUNDLAGE <= 3750 ~ 18.62,
        BEMESSUNGSGRUNDLAGE >= 3751 & BEMESSUNGSGRUNDLAGE <= 4250 ~ 21.28,
        BEMESSUNGSGRUNDLAGE >= 4251 & BEMESSUNGSGRUNDLAGE <= 4750 ~ 23.94,
        BEMESSUNGSGRUNDLAGE >= 4751 & BEMESSUNGSGRUNDLAGE <= 5249 ~ 26.60,
        BEMESSUNGSGRUNDLAGE >= 5250 & BEMESSUNGSGRUNDLAGE <= 5749 ~ 29.26,
        BEMESSUNGSGRUNDLAGE >= 5750 & BEMESSUNGSGRUNDLAGE <= 6249 ~ 31.92,
        TRUE ~ pv_cost  # Keep the original value if no conditions are met
      ),
      TRUE ~ pv_cost  # Keep the original value
    )
  )

data_alv <- data_alv %>%
  mutate(
    pv_cost = case_when(
      LEISTUNGSART %in% c("NH", "ND") ~ pv_cost * 0.92,
      TRUE ~ pv_cost 
    ) 
  )

# exclude costs for subsidized employment etc. that are already in FOER data included
data_alv <- data_alv %>%
  filter(
    !LEISTUNGSART %in% c("CL", "FS", "GB", "GC", "GT", "TO", "XT", "XU")
  )

data_alv <- data_alv %>%
  mutate(benefit_cost_day = TAGSATZLEIST + kv_cost + pv_cost)


## Calculate total cost for alv + components ----
data_alv <- data_alv %>%
  mutate(benefit_cost = benefit_cost_day * DAYS,
         benefits_net = TAGSATZLEIST * DAYS,
         benefits_health_si = kv_cost * DAYS,
         benefits_pension_si = pv_cost * DAYS)

# Sum the values of benefit_cost for treat == 1 and control == 1
sum_treat <- data_alv %>%
  filter(treat == 1) %>%
  summarise(total_benefit_cost = sum(benefit_cost, na.rm = TRUE),
            total_benefits_net = sum(benefits_net, na.rm = TRUE),
            total_benefits_health_si = sum(benefits_health_si, na.rm = TRUE),
            total_benefits_pension_si = sum(benefits_pension_si, na.rm = TRUE)
            )

sum_control <- data_alv %>%
  filter(control == 1) %>%
  summarise(total_benefit_cost = sum(benefit_cost, na.rm = TRUE),
            total_benefits_net = sum(benefits_net, na.rm = TRUE),
            total_benefits_health_si = sum(benefits_health_si, na.rm = TRUE),
            total_benefits_pension_si = sum(benefits_pension_si, na.rm = TRUE)
            )

# Combine the results into a single data frame
total_cost_alv <- bind_rows(
  sum_treat %>% mutate(group = "treat"),
  sum_control %>% mutate(group = "control")
)


## Divide total cost by number of people in treatment and control group ----
## count nr of persons: number of unique PST_KEY values
n_alv <- data_alv %>%
  group_by(treat) %>%
  summarize(n_people = n_distinct(PST_KEY))

# rename treatment avariable in total_cost_alv
total_cost_alv <- total_cost_alv %>%
  # Rename 'group' to 'treat'
  rename(treat = group) %>%
  # Recode values in 'treat'
  mutate(treat = recode(treat, "treat" = 1, "control" = 0))

## calculate average cost per person
# merge
total_cost_alv <- total_cost_alv %>%
  left_join(n_alv, by = "treat")

total_cost_alv <- total_cost_alv %>%
  group_by(treat) %>%
  mutate(n_people_start = ifelse(treat == 1, 62, 211))

# divide total cost by nr of individuals by treatment and control group
total_cost_alv <- total_cost_alv %>%
  mutate(person_benefit_cost = total_benefit_cost / n_people_start,
         person_benefits_net = total_benefits_net / n_people_start,
         person_benefits_health_si = total_benefits_health_si / n_people_start,
         person_benefits_pension_si = total_benefits_pension_si / n_people_start)


## sum foer and alv costs ----
# rename variables
total_cost_foer <- total_cost_foer %>%
  rename(total_cost = total_foer_cost,
         person_cost = person_foer_cost)

# create cost per month and per year
total_cost_foer <- total_cost_foer %>%
  group_by(treat) %>%
  mutate(person_cost_month = person_cost / 42,
         person_cost_year = person_cost_month * 12)

total_cost_alv <- total_cost_alv %>%
  rename(total_cost = total_benefit_cost,
         person_cost = person_benefit_cost)

# create cost per month and per year
total_cost_alv <- total_cost_alv %>%
  group_by(treat) %>%
  mutate(person_cost_month = person_cost / 42,
         person_cost_year = person_cost_month * 12,
         
         person_benefits_net_month = person_benefits_net / 42,
         person_benefits_net_year = person_benefits_net_month * 12,
         
         person_benefits_health_si_month = person_benefits_health_si / 42,
         person_benefits_health_si_year = person_benefits_health_si_month * 12,
         
         person_benefits_pension_si_month = person_benefits_pension_si / 42,
         person_benefits_pension_si_year = person_benefits_pension_si_month * 12
         )

# merge
total_cost <- bind_rows(total_cost_foer, total_cost_alv)

# sum
total_cost <- total_cost %>% 
  group_by(treat) %>%
  summarize(total_cost = sum(total_cost, na.rm = TRUE),
            person_cost = sum(person_cost, na.rm = TRUE), 
            person_cost_month = sum(person_cost_month, na.rm = TRUE), 
            person_cost_year = sum(person_cost_year, na.rm = TRUE) )

total_cost

# Export to CSV
write.csv(total_cost, "magma_costs_total.csv", row.names = FALSE)

write.csv(total_cost_alv, "magma_costs_alv.csv", row.names = FALSE)

write.csv(total_cost_foer, "magma_costs_foer.csv", row.names = FALSE)

## addition: costs per month for different periods ----
library(lubridate)

## foer
## traeger
# Expand the dataset into 6-month periods
data_foer_traeger_expanded <- data_foer_traeger %>%
  rowwise() %>%
  mutate(period_sequence = list(seq(floor_date(START, "6 months"),
                                    floor_date(ENDE, "6 months"), 
                                    by = "6 months"))) %>%
  unnest(period_sequence) %>%
  ungroup()

## NEW (2025-03-26): Define period specific start and end dates to calculate days within period
data_foer_traeger_expanded <- data_foer_traeger_expanded %>%
  mutate(
    START_period = pmax(START, period_sequence), # Set BEGINN 
    ENDE_period = pmin(ENDE, period_sequence %m+% months(6) - days(1)), # Set ENDE
    days_period = as.numeric(ENDE_period - START_period) + 1, # Calculate nr of days between START_period and ENDE_period
    foer_cost_period = days_period * daily_cost # Calculate cost per period
  )

# Now summarize total cost per period
total_cost_foer_traeger_periods <- data_foer_traeger_expanded %>%
  group_by(treat, period_sequence) %>%
  summarize(total_foer_cost = sum(foer_cost_period, na.rm = TRUE), .groups = 'drop')

# Now count distinct persons (pst_key) per 6-month period
n_foer_traeger_periods <- data_foer_traeger_expanded %>%
  group_by(treat, period_sequence) %>%
  summarize(n_people = n_distinct(pst_key), .groups = 'drop')

## individual
data_foer_individual_expanded <- data_foer_individual %>%
  rowwise() %>%
  mutate(period_sequence = list(seq(floor_date(START, "6 months"),
                                    floor_date(ENDE, "6 months"), 
                                    by = "6 months"))) %>%
  unnest(period_sequence) %>%
  ungroup()

## NEW (2025-03-26): Define period specific start and end dates to calculate days within period
data_foer_individual_expanded <- data_foer_individual_expanded %>%
  mutate(
    START_period = pmax(START, period_sequence), # Set BEGINN 
    ENDE_period = pmin(ENDE, period_sequence %m+% months(6) - days(1)), # Set ENDE
    days_period = as.numeric(ENDE_period - START_period) + 1, # Calculate nr of days between START_period and ENDE_period
    foer_cost_period = days_period * foer_cost_daily # Calculate cost per period
  )

# Now summarize total cost per period
total_cost_foer_individual_periods <- data_foer_individual_expanded %>%
  group_by(treat, period_sequence) %>%
  summarize(total_foer_cost = sum(foer_cost_period, na.rm = TRUE), .groups = 'drop')

# Now count distinct persons (pst_key) per 6-month period
n_foer_individual_periods <- data_foer_individual_expanded %>%
  group_by(treat, period_sequence) %>%
  summarize(n_people = n_distinct(pst_key), .groups = 'drop')


## calculate average cost per person
# merge
total_cost_foer_individual_periods <- total_cost_foer_individual_periods %>%
  left_join(n_foer_individual_periods, by = c("treat", "period_sequence"))

total_cost_foer_traeger_periods <- total_cost_foer_traeger_periods %>%
  left_join(n_foer_traeger_periods, by = c("treat", "period_sequence"))

## set original number of people (ADDED)
total_cost_foer_individual_periods <- total_cost_foer_individual_periods %>%
  group_by(treat) %>%
  mutate(n_people_start = ifelse(treat == 1, 62, 211))

## set original number of people (ADDED)
total_cost_foer_traeger_periods <- total_cost_foer_traeger_periods %>%
  group_by(treat) %>%
  mutate(n_people_start = ifelse(treat == 1, 62, 211))

# divide total cost by nr of individuals by treatment and control group
total_cost_foer_individual_periods <- total_cost_foer_individual_periods %>%
  mutate(person_foer_cost = total_foer_cost / n_people_start)

total_cost_foer_traeger_periods <- total_cost_foer_traeger_periods %>%
  mutate(person_foer_cost = total_foer_cost / n_people_start)


## sum foer individual and traeger costs ----
# merge
total_cost_foer_periods <- bind_rows(total_cost_foer_individual_periods, total_cost_foer_traeger_periods)

# sum
total_cost_foer_periods <- total_cost_foer_periods %>% 
  group_by(treat, period_sequence) %>%
  summarize(total_foer_cost = sum(total_foer_cost, na.rm = TRUE),
            person_foer_cost = sum(person_foer_cost, na.rm = TRUE), )


## alv
# Step 1: Filter out cases where ENDE is earlier than START
data_alv_expanded <- data_alv %>%
  filter(ENDE >= START)

# Step 2: Expand the data to cover each 6-month period between START and ENDE
data_alv_expanded <- data_alv_expanded %>%
  rowwise() %>%
  mutate(period_sequence = list(seq(floor_date(START, "6 months"),
                                    floor_date(ENDE, "6 months"),
                                    by = "6 months"))) %>%
  unnest(period_sequence) %>%
  ungroup()

# Step 3: Calculate the start and end of each period
data_alv_expanded <- data_alv_expanded %>%
  mutate(period_start = START,
         period_end = if_else(period_sequence == floor_date(ENDE, "6 months"), ENDE, period_start + months(6) - days(1)),
         period_end = pmin(period_end, ENDE))  # Ensure the period_end doesn't exceed ENDE

# Step 4: Calculate the days within each 6-month period
data_alv_expanded <- data_alv_expanded %>%
  mutate(DAYS_in_period = as.numeric(difftime(period_end, period_start, units = "days")) + 1)  # Add 1 to include the last day

# Step 5: Summarize the days for each 6-month period by treatment or any other grouping variable
days_alv_by_period <- data_alv_expanded %>%
  group_by(treat, period_sequence) %>%
  summarize(total_days = sum(DAYS_in_period), .groups = 'drop')

# calculate alv cost by days per period
# Step 5: Calculate benefit cost per 6-month period
data_alv_period_cost <- data_alv_expanded %>%
  mutate(benefit_cost_period = benefit_cost_day * DAYS_in_period)  # Multiply by days in each period

# Step 6: Summarize the total cost for each 6-month period by treatment or any other grouping variable
total_cost_alv_periods <- data_alv_period_cost %>%
  group_by(treat, period_sequence) %>%
  summarize(total_cost = sum(benefit_cost_period, na.rm = TRUE), .groups = 'drop')

## Incorporate nr of people per period

# Step 7: Count distinct people (PST_KEY) per 6-month period and treatment group
n_people_alv_by_period <- data_alv_expanded %>%
  group_by(treat, period_sequence) %>%
  summarize(n_people = n_distinct(PST_KEY), .groups = 'drop')

# Step 8: Merge the total cost and number of people by period and treatment group
total_cost_alv_periods <- total_cost_alv_periods %>%
  left_join(n_people_alv_by_period, by = c("treat", "period_sequence"))

## set original number of people (ADDED)
total_cost_alv_periods <- total_cost_alv_periods %>%
  group_by(treat) %>%
  mutate(n_people_start = ifelse(treat == 1, 62, 211))

# Step 9: Calculate the average cost per person for each 6-month period
total_cost_alv_periods <- total_cost_alv_periods %>%
  mutate(person_cost = total_cost / n_people_start)


## create cost per month

# foer
total_cost_foer_periods <- total_cost_foer_periods %>%
  group_by(treat, period_sequence) %>%
  mutate(
    person_cost_month = ifelse(
      period_sequence == as.Date("2020-07-01") | period_sequence == as.Date("2024-01-01"),
      person_foer_cost / 3,
      person_foer_cost / 6
    )
  )

total_cost_foer_periods <- total_cost_foer_periods %>%
  arrange(period_sequence)

# alv
total_cost_alv_periods <- total_cost_alv_periods %>%
  group_by(treat, period_sequence) %>%
  mutate(
    person_cost_month = ifelse(
      period_sequence == as.Date("2020-07-01") | period_sequence == as.Date("2024-01-01"),
      person_cost / 3,
      person_cost / 6
    )
  )

total_cost_foer_periods <- total_cost_foer_periods %>%
  arrange(period_sequence)

## merge
# harmonize dataframes
# rename variables
total_cost_foer_periods <- total_cost_foer_periods %>%
  rename(total_cost = total_foer_cost,
         person_cost = person_foer_cost)

total_cost_alv_periods <- total_cost_alv_periods %>%
  select(-n_people)

total_cost_periods <- bind_rows(total_cost_foer_periods, total_cost_alv_periods)

# sum
total_cost_periods <- total_cost_periods %>% 
  group_by(treat, period_sequence) %>%
  summarize(total_cost = sum(total_cost, na.rm = TRUE),
            person_cost = sum(person_cost, na.rm = TRUE),
            person_cost_month = sum(person_cost_month, na.rm = TRUE))

total_cost_periods <- total_cost_periods %>%
  arrange(period_sequence)

# Export to CSV
write.csv(total_cost_periods, "magma_costs_total_periods.csv", row.names = FALSE)

write.csv(total_cost_alv_periods, "magma_costs_alv_periods.csv", row.names = FALSE)

write.csv(total_cost_foer_periods, "magma_costs_foer_periods.csv", row.names = FALSE)

## Figure ----
## NEW ADDED BY LUKAS 2025-05-02
## Create a stacked bar chart with results

data_raw_figure <- read.csv("magma_costs_table_for_figure.csv", sep = ",")

## format data_raw_figure to create the gglopt below
# Read and adjust data
data_long <- data_raw_figure %>%
  mutate(
    period_sequence = as.Date(period_sequence, format = "%d/%m/%Y"),
    # Change first date while keeping others
    period_sequence = if_else(
      period_sequence == as.Date("2020-07-01"),
      as.Date("2020-10-01"),
      period_sequence
    )
  ) %>%
  # Create ordered factor for consistent spacing
  mutate(
    period_factor = as.numeric(factor(period_sequence, levels = unique(sort(period_sequence))))
  ) %>%
  pivot_longer(
    cols = c(foer_person_cost_month, alv_person_cost_month),
    names_to = "cost_type",
    values_to = "cost"
  ) %>%
  mutate(
    treat = factor(treat, levels = c(1, 0), labels = c("Treatment", "Control")),
    cost_type = factor(cost_type,
                       levels = c("foer_person_cost_month", "alv_person_cost_month"),
                       labels = c("Programs", "Social Benefits"))
  )

# Bar dimensions
bar_width <- 0.4  # Relative width (0-1)
spacing <- 0.8      # Base spacing unit

# Calculate positions using period_factor instead of dates
plot_data <- data_long %>%
  mutate(
    group_pos = ifelse(
      treat == "Treatment",
      period_factor - spacing/4,  # Treatment left
      period_factor + spacing/4   # Control right
    )
  )

# Define alpha values
treat_alpha <- 1      # Opaque for Treatment
control_alpha <- 0.6  # Transparent for Control

# Plot without patterns
ggplot(plot_data, aes(
  x = group_pos,
  y = cost,
  fill = cost_type,
  alpha = treat,
  group = interaction(treat, period_sequence)
)) +
  geom_col(position = "stack", width = bar_width) +
  # Add text labels for first period
  geom_text(
    data = subset(plot_data, period_sequence == min(period_sequence)),
    aes(
      x = group_pos,
      y = max(cost) * 0.6,  # Position slightly above highest bar
      label = ifelse(treat == "Treatment", "Gramatneusiedl", "Control towns"),
      angle = 90,  # Vertical text
      hjust = 0    # Align to bottom of text
    ),
    vjust = 0,  # Align text at bottom of position
    size = 3.5,
    color = "black",
    alpha = 1,  # Override alpha for text
    inherit.aes = FALSE
  ) +
  scale_x_continuous(
    breaks = unique(plot_data$period_factor),
    labels = function(x) format(unique(sort(data_long$period_sequence))[x], "%b %Y")
  ) +
  scale_fill_manual(values = c(
    "Programs" = "#005E87",  # other colour options: steelblue, #005E87, 1F77B4
    "Social Benefits" = "#E35205" # darkorange, #E35205, #FF7F0E
  )) + 
  scale_alpha_manual(
    values = c("Treatment" = treat_alpha, "Control" = control_alpha),
    guide = "none"  # Hide alpha legend
  ) +
  labs(
    title = "",
    x = "",
    y = "Monthly cost per person in EUR",
    fill = ""
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.title.y = element_text(size = 11, margin = margin(r = 10)),
    legend.position = "top",
    legend.text = element_text(size = 10),
    plot.title = element_text(face = "bold", size = 12)
  )
# Note:
# Programs refer to Active labor market policy
# Social Benefits to Passive labor market policy

# Save the plot
ggsave(
  filename = "cost_comparison_stacked_bar.png",
  width = 10,
  height = 6,
  dpi = 300
)



# Plot with firebrick style for manuscript
# Create a custom legend

# Main plot without legend
#main_plot <- 
ggplot(plot_data, aes(
  x = group_pos,
  y = cost,
  fill = treat,
  alpha = cost_type,
  group = interaction(treat, period_sequence)
)) +
  geom_col(position = "stack", width = bar_width) +
  # Add text labels for first period
  geom_text(
    data = subset(plot_data, period_sequence == min(period_sequence)),
    aes(
      x = group_pos,
      y = max(cost) * 0.6,
      label = ifelse(treat == "Treatment", "Gramatneusiedl", "Control towns"),
      angle = 90,
      hjust = 0
    ),
    vjust = 0,
    size = 3.5,
    color = "black",
    alpha = 1,
    inherit.aes = FALSE
  ) +
  scale_x_continuous(
    breaks = unique(plot_data$period_factor),
    labels = function(x) {
      dates <- unique(sort(data_long$period_sequence))[x]
      # Format with month and year on separate lines
      paste0(format(dates, "%b"), "\n", format(dates, "%Y"))
    }
  ) +
  scale_fill_manual(
    values = c(
      "Treatment" = "firebrick",
      "Control" = "grey50"
    ),
    guide = "none"  # Hide fill legend
  ) + 
  scale_alpha_manual(
    values = c(
      "Programs" = 0.9,
      "Social Benefits" = 0.6
    ),
    guide = "none"  # Hide alpha legend
  ) +
  labs(
    title = "",
    x = "",
    y = "Monthly cost per person in EUR"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 10),  # Center the labels
    axis.text.y = element_text(angle = 0, hjust = 0.5, size = 10),
    axis.title.y = element_text(size = 11, margin = margin(r = 10)),
    plot.title = element_text(face = "bold", size = 12)
  )

# Save the plot
ggsave(
  filename = "cost_comparison_stacked_bar_firebrick.png",
  width = 7.5,
  height = 4.2,
  dpi = 300
)






# Clearer annotation
# Create modified data for alpha reversal
# Reverse period order and create modified alpha values
plot_data_rev <- plot_data %>% 
  mutate(
    cost_type_alpha = case_when(
      treat == "Treatment" ~ cost_type,
      treat == "Control" & cost_type == "Programs" ~ "Social Benefits",
      treat == "Control" & cost_type == "Social Benefits" ~ "Programs"
    ),
    # Reverse the order of periods by reordering factor levels
    period_factor = factor(
      period_factor,
      levels = rev(unique(period_factor[order(period_sequence)])),  # REVERSE HERE
      labels = rev(format(unique(period_sequence[order(period_sequence)]), "%b %Y"))
    )
  )


p <- ggplot(plot_data_rev, aes(
  x = period_factor,
  y = cost,
  fill = treat,
  alpha = cost_type_alpha,
  group = interaction(treat, period_sequence)
)) +
  geom_col(position = "stack", width = 0.7) +
  facet_wrap(~treat, ncol = 1, 
             labeller = labeller(treat = c("Treatment" = "Gramatneusiedl", 
                                           "Control" = "Control towns"))) +
  
  # MANUALLY ADJUSTED ANNOTATIONS
  geom_text(
    data = data.frame(
      label = c("Total Cost", "Program Cost", "Benefits"),
      x = c("Oct 2020", "Oct 2020", "Oct 2020"),  # Same period for all (adjust as needed)
      y = c(1500, 800, 200),  # Exact EUR values for positioning
      treat = "Treatment"  # Ensure annotations appear only in the Treatment facet
    ),
    aes(x = x, y = y, label = label),
    inherit.aes = FALSE,
    size = 3.5,
    color = "black",
    hjust = 0  # Left-align text (adjust if needed)
  ) +
  
  # Rest of the plot code...
  scale_fill_manual(values = c("Treatment" = "firebrick", "Control" = "grey50"), guide = "none") + 
  scale_alpha_manual(values = c("Programs" = 0.9, "Social Benefits" = 0.3), guide = "none") +
  labs(x = "", y = "Monthly cost per person in EUR") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text.y = element_text(size = 10),
    axis.title.y = element_text(size = 11, margin = margin(r = 10)),
    strip.text = element_text(size = 11, face = "bold"),
    panel.spacing = unit(1, "lines")
  ) +
  coord_flip()

print(p)

# Save the plot
ggsave(
  filename = "cost_comparison_stacked_bar_firebrick_facet.png",
  width = 8,
  height = 4.5,
  dpi = 300
)



# Added 2025-06-12
## addition: beihilfe types ----------------
# Compute total sum by beihilfe type
total_cost_foer_individual_by_type <- data_foer_individual %>%
  mutate(beihilfe_type = case_when(
    beihilfe %in% c("0KK", "BEM", "DLU", "FKS", "KBH", "KNK") ~ "Training",
    beihilfe %in% c("0EB", "0GB", "KOM") ~ "Employment subsidies",
    TRUE ~ "Other"
  )) %>%
  group_by(beihilfe_type, treat) %>%
  summarize(
    total_foer_cost = sum(foer_cost, na.rm = TRUE),
    n_people = n(),
    .groups = "drop"
  )

total_cost_foer_traeger_by_type <- data_foer_traeger %>%
  mutate(beihilfe_type = case_when(
    beihilfe %in% c("BBE") ~ "Coaching",
    beihilfe %in% c("BFA", "BMN") ~ "Training",
    beihilfe %in% c("KUA", "UGP") ~ "Employment subsidies",
    beihilfe %in% c("GBP", "SÖB") ~ "Direct job creation",
    TRUE ~ "Other"
  )) %>%
  group_by(beihilfe_type, treat) %>%
  summarize(total_foer_cost = sum(foer_cost, na.rm = TRUE), 
            n_people = n(),
            .groups = "drop")



total_cost_foer_individual_by_type <- total_cost_foer_individual_by_type %>%
  group_by(treat) %>%
  mutate(n_people_start = ifelse(treat == 1, 62, 211))

total_cost_foer_traeger_by_type <- total_cost_foer_traeger_by_type %>%
  group_by(treat) %>%
  mutate(n_people_start = ifelse(treat == 1, 62, 211))

# divide total cost by nr of individuals by treatment and control group
total_cost_foer_individual_by_type <- total_cost_foer_individual_by_type %>%
  mutate(person_foer_cost = total_foer_cost / n_people_start)

total_cost_foer_traeger_by_type <- total_cost_foer_traeger_by_type %>%
  mutate(person_foer_cost = total_foer_cost / n_people_start)


## sum foer individual and traeger costs ----
# merge
total_cost_foer_by_type <- bind_rows(total_cost_foer_individual_by_type, total_cost_foer_traeger_by_type)

# sum
total_cost_foer_by_type <- total_cost_foer_by_type %>% 
  group_by(treat, beihilfe_type) %>%
  summarize(total_foer_cost = sum(total_foer_cost, na.rm = TRUE),
            person_foer_cost = sum(person_foer_cost, na.rm = TRUE), )

# reshape to have beihilfe_type as columns
# drop total cost first

total_cost_foer_by_type <- total_cost_foer_by_type %>%
  select(-total_foer_cost) 

# create cost per month and per year
total_cost_foer_by_type <- total_cost_foer_by_type %>%
  group_by(treat) %>%
  mutate(person_cost_month = person_foer_cost / 42) %>%
  select(-person_foer_cost)

total_cost_foer_by_type_wide <- total_cost_foer_by_type %>%
  pivot_wider(
    names_from = beihilfe_type,
    values_from = c(person_cost_month),
    names_glue = "{beihilfe_type}" 
#    names_glue = "{.value}_{beihilfe_type}" 
  )


# Export to CSV
write.csv(total_cost_foer_by_type_wide, "magma_costs_foer_by_type_and_person.csv", row.names = FALSE)


# Added 2025-06-18
## addition: beihilfe types per period ----------------

# Compute costs per category and time period for individual benefits
cost_foer_individual_type_period <- data_foer_individual_expanded %>%
  mutate(beihilfe_type = case_when(
    beihilfe %in% c("0KK", "BEM", "DLU", "FKS", "KBH", "KNK") ~ "Training",
    beihilfe %in% c("0EB", "0GB", "KOM") ~ "Employment subsidies",
    TRUE ~ "Other"
  )) %>%
  group_by(treat, period_sequence, beihilfe_type) %>%
  summarize(
    total_foer_cost = sum(foer_cost_period, na.rm = TRUE),
    n_people = n_distinct(pst_key),
    .groups = "drop"
  ) %>%
  group_by(treat) %>%
  mutate(n_people_start = ifelse(treat == 1, 62, 211)) %>%
  mutate(person_foer_cost = total_foer_cost / n_people_start)

# Compute costs per category and time period for traeger benefits
cost_foer_traeger_type_period <- data_foer_traeger_expanded %>%
  mutate(beihilfe_type = case_when(
    beihilfe %in% c("BBE") ~ "Coaching",
    beihilfe %in% c("BFA", "BMN") ~ "Training",
    beihilfe %in% c("KUA", "UGP") ~ "Employment subsidies",
    beihilfe %in% c("GBP", "SÖB") ~ "Direct job creation",
    TRUE ~ "Other"
  )) %>%
  group_by(treat, period_sequence, beihilfe_type) %>%
  summarize(
    total_foer_cost = sum(foer_cost_period, na.rm = TRUE),
    n_people = n_distinct(pst_key),
    .groups = "drop"
  ) %>%
  group_by(treat) %>%
  mutate(n_people_start = ifelse(treat == 1, 62, 211)) %>%
  mutate(person_foer_cost = total_foer_cost / n_people_start)

# Combine individual and traeger benefits
total_cost_foer_type_period <- bind_rows(cost_foer_individual_type_period, 
                                         cost_foer_traeger_type_period) %>%
  group_by(treat, period_sequence, beihilfe_type) %>%
  summarize(
    total_foer_cost = sum(total_foer_cost, na.rm = TRUE),
    person_foer_cost = sum(person_foer_cost, na.rm = TRUE),
    .groups = "drop"
  )

# Calculate monthly costs (adjusting for partial periods)
total_cost_foer_type_period <- total_cost_foer_type_period %>%
  mutate(
    person_cost_month = ifelse(
      period_sequence == as.Date("2020-07-01") | period_sequence == as.Date("2024-01-01"),
      person_foer_cost / 3,  # 3-month period
      person_foer_cost / 6   # 6-month period
    )
  )

# Create wide format for easier analysis
total_cost_foer_type_period_wide <- total_cost_foer_type_period %>%
  select(-total_foer_cost, -person_foer_cost) %>%
  pivot_wider(
    names_from = beihilfe_type,
    values_from = person_cost_month,
    values_fill = 0
  )

# Add total_programs column that sums the main 4 program categories
total_cost_foer_type_period_wide <- total_cost_foer_type_period_wide %>%
  mutate(
    total_programs = select(., 
                            "Coaching", 
                            "Training", 
                            "Employment subsidies", 
                            "Direct job creation") %>% 
      rowSums(na.rm = TRUE)
  )

# Export results
write.csv(total_cost_foer_type_period_wide, "magma_costs_foer_by_type_and_period_wide.csv", row.names = FALSE)








