# 5-cost-comparison

# This script computes the costs of the program for the treatment and control group, based on the ALV data and the FOER data. It computes total costs as well as costs per person and month, which are used in the cost comparison plot (Figure 12) and in Table 7, Table A.8 and Table A.9.

# keep packages here to run script on AMS computer
library(tidyverse)
library(readr)
library(data.table)
library(ggtext)
library(janitor)
library(lubridate)
library(knitr)
library(stringr)

## Functinos ----
parse_number_robust <- function(x) {
  x <- as.character(x)
  x <- gsub("\\s", "", x)
  x <- gsub("[^0-9,.-]", "", x)
  has_comma <- grepl(",", x, fixed = TRUE)
  has_dot <- grepl("\\.", x)
  x <- ifelse(has_comma & has_dot, gsub("\\.", "", x), x)
  x <- ifelse(has_comma, gsub(",", ".", x), x)
  as.numeric(x)
}

## Settings ----
rm(list = ls())
Windows = T
# Switch path for data-files T or F
if (Windows) {veracrypt_path = "A:/"} else {veracrypt_path = "/Volumes/NO NAME/"}

AMS = T
# Switch path for data-files between AMS and Lukas/Max computing environment
if (AMS) {
  
  setwd("V:/AES/03_Forschung/0_Projekte/2020/1_MAGMA/!Uni Oxford/5_Daten/202401 - Kosten/Cost_Calculation")
  home <- getwd()
  
  data_path = file.path(home, "cost_data")
  
  pstnr_data <- file.path(home, "pstnr_magma_control_towns.csv")
  
} else {
  
  data_path = file.path(dirname(home), "Cost_data", "cost_data")
  home <- getwd()
  
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

magma_pstnr <- fread(file.path(home, "pstnr_magma.csv"))[[1]]
control_pstnr <- fread(file.path(home, "pstnr_control_towns.csv"))[[1]]

# USE FOR REAL DATA (AMS)
# magma_pstnr <- magma_pstnr$magma_pstnr
# control_pstnr <- control_pstnr$control_pstnr

## Prepare data ----
# Create a treat variable based on whether PSTNR is in magma_pstnr
data_alv <- data_alv %>%
  mutate(treat = ifelse(PST_KEY %in% magma_pstnr, 1, 0),
         control = ifelse(PST_KEY %in% control_pstnr, 1, 0))

# USE FOR REAL DATA (AMS)
# Filter out rows that are neither treat == 1 nor control == 1
# data_alv <- data_alv %>%
#   filter(treat == 1 | control == 1)


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

# USE FOR REAL DATA (AMS)
# ## filter for treat and control individuals 
# # Filter out rows that are neither treat == 1 nor control == 1
# data_foer_individual <- data_foer_individual %>%
#   filter(pst_key %in% c(magma_pstnr, control_pstnr))

# data_foer_traeger <- data_foer_traeger %>%
#   filter(pst_key %in% c(magma_pstnr, control_pstnr))


## Exclude / shorten spells that started before 01.01.2020 
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


# Exclude rows where BEGINN is after 31.03.2024
data_foer_individual <- data_foer_individual %>%
  filter(foerderung_von <= as.Date("2024-03-31"))
 
data_foer_traeger <- data_foer_traeger %>%
  filter(foerderung_von <= as.Date("2024-03-31"))

# Set ENDE after 31.03.2024 to 31.03.2024
data_foer_individual <- data_foer_individual %>%
  mutate(ENDE = pmin(foerderung_bis, as.Date("2024-03-31")))

data_foer_traeger <- data_foer_traeger %>%
  mutate(ENDE = pmin(foerderung_bis, as.Date("2024-03-31")))


# Calculate days since program start (2020-10-01)
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


## Drop rows that appear twice 
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


## Calculate daily cost for foer 

# Convert 'gesamtbewilligung' to numeric
data_foer_individual <- data_foer_individual %>%
  mutate(gesamtbewilligung = str_replace_all(gesamtbewilligung, ",", ""), # Remove commas
         gesamtbewilligung = as.numeric(gesamtbewilligung))                # Convert to numeric


# Convert 'kosten_pro_tn_tag' to numeric
data_foer_traeger <- data_foer_traeger %>%
  mutate(
    kosten_pro_tn_tag = parse_number_robust(kosten_pro_tn_tag),
    kosten_pro_tn_tag_inkl_mitfinanzierungen = parse_number_robust(kosten_pro_tn_tag_inkl_mitfinanzierungen)
  )


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


## Divide total cost by number of people in treatment and control group 
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
  left_join(n_foer_traeger, by = "treat")


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


## sum foer individual and traeger costs 
# merge
total_cost_foer <- bind_rows(total_cost_foer_individual, total_cost_foer_traeger)
  
# sum
total_cost_foer <- total_cost_foer %>% 
group_by(treat) %>%
  summarize(total_foer_cost = sum(total_foer_cost, na.rm = TRUE),
            person_foer_cost = sum(person_foer_cost, na.rm = TRUE), )


### ALV calculation ----

# ALV data is in MM-DD-YYYY (contrary to foer data)
## Exclude / shorten spells that started before 01.01.2020 
# Convert BEGINN and ENDE columns to Date format
data_alv <- data_alv %>%
  mutate(
    BEGINN = as.Date(BEGINN, format = "%m/%d/%Y"),
    ENDE = as.Date(ENDE, format = "%m/%d/%Y")
  )

# USE FOR REAL DATA (AMS)
# CODE REQUIRED FOR DANIEL since data is loaded differently on AMS computer
# data_alv <- data_alv %>%
#   mutate(
#     BEGINN = as.Date(BEGINN, format = "%m.%d.%Y"),
#     ENDE = as.Date(ENDE, format = "%m.%d.%Y")
#   )

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


## Exclude rows that appear erroneously twice 
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

## Adjust alv for indirect costs (social insurance: health & pension) 
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


## Calculate total cost for alv + components
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


## Divide total cost by number of people in treatment and control group 
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


## Table 7: sum foer and alv costs ----
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


## Figure 12: 6-month periods comparison ----

make_6m_seq <- function(start, end) {
  start <- as.Date(start[1])
  end <- as.Date(end[1])
  if (is.na(start) || is.na(end) || end < start) {
    return(as.Date(character(0)))
  }
  seq(floor_date(start, "6 months"), floor_date(end, "6 months"), by = "6 months")
}

## foer
## traeger
# Expand the dataset into 6-month periods
data_foer_traeger_expanded <- data_foer_traeger %>%
  mutate(period_sequence = purrr::map2(START, ENDE, make_6m_seq)) %>%
  unnest(period_sequence) %>%
  mutate(
    START = as.Date(START),
    ENDE = as.Date(ENDE),
    period_sequence = as.Date(period_sequence)
  )

## Define period specific start and end dates to calculate days within period
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
  mutate(period_sequence = purrr::map2(START, ENDE, make_6m_seq)) %>%
  unnest(period_sequence) %>%
  mutate(
    START = as.Date(START),
    ENDE = as.Date(ENDE),
    period_sequence = as.Date(period_sequence)
  )

## Define period specific start and end dates to calculate days within period
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

## set original number of people
total_cost_foer_individual_periods <- total_cost_foer_individual_periods %>%
  group_by(treat) %>%
  mutate(n_people_start = ifelse(treat == 1, 62, 211))

## set original number of people
total_cost_foer_traeger_periods <- total_cost_foer_traeger_periods %>%
  group_by(treat) %>%
  mutate(n_people_start = ifelse(treat == 1, 62, 211))

# divide total cost by nr of individuals by treatment and control group
total_cost_foer_individual_periods <- total_cost_foer_individual_periods %>%
  mutate(person_foer_cost = total_foer_cost / n_people_start)

total_cost_foer_traeger_periods <- total_cost_foer_traeger_periods %>%
  mutate(person_foer_cost = total_foer_cost / n_people_start)


## sum foer individual and traeger costs
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
  mutate(period_sequence = purrr::map2(START, ENDE, make_6m_seq)) %>%
  unnest(period_sequence) %>%
  mutate(
    START = as.Date(START),
    ENDE = as.Date(ENDE),
    period_sequence = as.Date(period_sequence)
  )

# Step 3: period-specific start/end inside each 6-month bin
data_alv_expanded <- data_alv_expanded %>%
  mutate(
    period_start = pmax(START, period_sequence),
    period_end   = pmin(
      ENDE,
      period_sequence %m+% months(6) - days(1)
    ),
    DAYS_in_period = as.numeric(difftime(period_end, period_start, units = "days")) + 1
  )

# Step 4: Calculate the days within each 6-month period
data_alv_expanded <- data_alv_expanded %>%
  mutate(DAYS_in_period = as.numeric(difftime(period_end, period_start, units = "days")) + 1)  # Add 1 to include the last day

# Step 5: Calculate ALV costs per 6-month period (total + components)
data_alv_period_cost <- data_alv_expanded %>%
  mutate(
    benefit_total_period       = benefit_cost_day * DAYS_in_period,
    benefits_net_period        = TAGSATZLEIST   * DAYS_in_period,
    benefits_health_si_period  = kv_cost        * DAYS_in_period,
    benefits_pension_si_period = pv_cost        * DAYS_in_period
  )

# Step 6: Summarize the total cost per 6-month period by treatment
total_cost_alv_periods <- data_alv_period_cost %>%
  group_by(treat, period_sequence) %>%
  summarize(
    total_cost                = sum(benefit_total_period,       na.rm = TRUE),
    total_benefits_net        = sum(benefits_net_period,        na.rm = TRUE),
    total_benefits_health_si  = sum(benefits_health_si_period,  na.rm = TRUE),
    total_benefits_pension_si = sum(benefits_pension_si_period, na.rm = TRUE),
    .groups = "drop"
  )

# Step 7: Count distinct people per 6-month period and treatment group
n_people_alv_by_period <- data_alv_expanded %>%
  group_by(treat, period_sequence) %>%
  summarize(n_people = n_distinct(PST_KEY), .groups = "drop")

# Step 8: Merge the total cost and number of people by period and treatment group
total_cost_alv_periods <- total_cost_alv_periods %>%
  left_join(n_people_alv_by_period, by = c("treat", "period_sequence"))

# Step 9: Use original (fixed) denominators and compute per-person costs
total_cost_alv_periods <- total_cost_alv_periods %>%
  group_by(treat) %>%
  mutate(
    n_people_start = ifelse(treat == 1, 62, 211),
    
    person_cost                 = total_cost / n_people_start,
    person_benefits_net         = total_benefits_net / n_people_start,
    person_benefits_health_si   = total_benefits_health_si / n_people_start,
    person_benefits_pension_si  = total_benefits_pension_si / n_people_start
  ) %>%
  ungroup()

# Step 10: Convert period totals to monthly (3-month bins at start/end, otherwise 6 months)
total_cost_alv_periods <- total_cost_alv_periods %>%
  mutate(
    months_in_bin = ifelse(
      period_sequence == as.Date("2020-07-01") | period_sequence == as.Date("2024-01-01"),
      3L, 6L
    ),
    
    person_cost_month                 = person_cost / months_in_bin,
    person_benefits_net_month         = person_benefits_net / months_in_bin,
    person_benefits_health_si_month   = person_benefits_health_si / months_in_bin,
    person_benefits_pension_si_month  = person_benefits_pension_si / months_in_bin
  ) %>%
  select(-months_in_bin)


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

# # alv
# total_cost_alv_periods <- total_cost_alv_periods %>%
#   group_by(treat, period_sequence) %>%
#   mutate(
#     person_cost_month = ifelse(
#       period_sequence == as.Date("2020-07-01") | period_sequence == as.Date("2024-01-01"),
#       person_cost / 3,
#       person_cost / 6
#     )
#   )

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


## --- Create magma_costs_table_for_figure.csv 

magma_costs_table_for_figure <- total_cost_foer_periods %>%
  select(treat, period_sequence, person_cost_month) %>%
  rename(foer_person_cost_month = person_cost_month) %>%
  full_join(
    total_cost_alv_periods %>%
      select(treat, period_sequence, person_cost_month) %>%
      rename(alv_person_cost_month = person_cost_month),
    by = c("treat", "period_sequence")
  ) %>%
  mutate(
    foer_person_cost_month = tidyr::replace_na(foer_person_cost_month, 0),
    alv_person_cost_month  = tidyr::replace_na(alv_person_cost_month, 0),
    # Match the plotting code expectation: DD/MM/YYYY strings (e.g. "01/07/2020")
    period_sequence = format(as.Date(period_sequence), "%d/%m/%Y")
  ) %>%
  arrange(treat, as.Date(period_sequence, format = "%d/%m/%Y"))

write.csv(
  magma_costs_table_for_figure,
  "magma_costs_table_for_figure.csv",
  row.names = FALSE
)


## Appendix table: short-term vs long-term (AMS) costs vs benefits ----
# Table A.8 and Table A.9

# --- Robust bin anchors (first bin starts at program launch: 2020-10-01)
period_sequence_vec <- as.Date(c(
  "2020-10-01", # Oct–Dec 2020 (3 months)
  "2021-01-01", # Jan–Jun 2021 (6 months)
  "2021-07-01", # Jul–Dec 2021
  "2022-01-01", # Jan–Jun 2022
  "2022-07-01", # Jul–Dec 2022
  "2023-01-01", # Jan–Jun 2023
  "2023-07-01", # Jul–Dec 2023
  "2024-01-01"  # Jan–Mar 2024 (3 months, truncated)
))

# compute bin end dates and months/days in bin from anchors
bins_df <- tibble(period_sequence = period_sequence_vec) %>%
  mutate(
    bin_start = period_sequence,
    # bin_end is the day before the next anchor, except for last anchor use Mar 31 2024
    bin_end = lead(period_sequence, default = as.Date("2024-04-01")) - days(1),
    months_in_bin = as.integer(interval(bin_start, bin_end + days(1)) %/% months(1)),
    days_in_bin   = as.integer(bin_end - bin_start) + 1L
  )

# lookup months in bin for any date within a bin (not just the anchor)
months_in_bin <- function(d) {
  d <- as.Date(d)
  vapply(
    d,
    function(di) {
      idx <- which(bins_df$bin_start <= di & di <= bins_df$bin_end)
      if (length(idx) == 0) {
        NA_integer_
      } else {
        bins_df$months_in_bin[[idx[1]]]
      }
    },
    integer(1)
  )
}

# horizon mapping by interval membership (robust to exact anchor values)
horizon_map_from_date <- function(d) {
  case_when(
    d >= as.Date("2020-10-01") & d <= as.Date("2022-06-30") ~ "Short-term (Oct 2020–Jun 2022)",
    d >= as.Date("2022-07-01") & d <= as.Date("2024-03-31") ~ "Long-term (Jul 2022–Mar 2024)",
    TRUE ~ NA_character_
  )
}

# --- Add ALV benefit subcomponents to short/long comparison

alv_components <- c(
  "person_cost_month",                 # total benefit payments (existing)
  "person_benefits_net_month",
  "person_benefits_health_si_month",
  "person_benefits_pension_si_month"
)

# Stack program costs (FOER) and benefit payments (ALV) at bin level
ams_bins <- bind_rows(
  # FOER (program costs)
  total_cost_foer_periods %>%
    transmute(
      treat,
      period_sequence = as.Date(period_sequence),
      component = "Program costs",
      person_cost_month
    ),
  
  # ALV (benefits): total + subcomponents
  total_cost_alv_periods %>%
    transmute(
      treat,
      period_sequence = as.Date(period_sequence),
      # keep a clear "total" column name before pivoting
      benefit_total_month = person_cost_month,
      person_benefits_net_month,
      person_benefits_health_si_month,
      person_benefits_pension_si_month
    ) %>%
    pivot_longer(
      cols      = c(benefit_total_month,
                    person_benefits_net_month,
                    person_benefits_health_si_month,
                    person_benefits_pension_si_month),
      names_to  = "component_raw",
      values_to = "person_cost_month"
    ) %>%
    mutate(
      component = recode(
        component_raw,
        benefit_total_month              = "Benefit payments (total)",
        person_benefits_net_month        = "Benefit payments (net)",
        person_benefits_health_si_month  = "Benefit payments (health SI)",
        person_benefits_pension_si_month = "Benefit payments (pension SI)"
      )
    ) %>%
    select(treat, period_sequence, component, person_cost_month)
) %>%
  mutate(
    horizon        = horizon_map_from_date(period_sequence),
    months_in_bin  = months_in_bin(period_sequence),
    treat_label    = if_else(treat == 1, "Gramatneusiedl", "Control towns")
  ) %>%
  filter(!is.na(horizon))

# Weighted (by months) mean monthly cost per person within horizon
ams_short_long_components <- ams_bins %>%
  group_by(horizon, component, treat_label) %>%
  summarize(
    value = weighted.mean(person_cost_month, w = months_in_bin, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = treat_label, values_from = value) %>%
  mutate(Difference = Gramatneusiedl - `Control towns`)

# Add totals (Program + TOTAL benefits only; do NOT sum benefit subcomponents)
ams_short_long_total <- ams_short_long_components %>%
  filter(component %in% c("Program costs", "Benefit payments (total)")) %>%
  group_by(horizon) %>%
  summarize(
    component      = "Total costs for AMS",
    Gramatneusiedl  = sum(Gramatneusiedl, na.rm = TRUE),
    `Control towns` = sum(`Control towns`, na.rm = TRUE),
    Difference     = Gramatneusiedl - `Control towns`,
    .groups = "drop"
  )

ams_short_long_table <- bind_rows(ams_short_long_components, ams_short_long_total) %>%
  mutate(
    component = factor(
      component,
      levels = c(
        "Program costs",
        "Benefit payments (total)",
        "Benefit payments (net)",
        "Benefit payments (health SI)",
        "Benefit payments (pension SI)",
        "Total costs for AMS"
      )
    )
  ) %>%
  arrange(horizon, component) %>%
  mutate(across(c(Gramatneusiedl, `Control towns`, Difference)))


# --- Export CSV for appendix material
write_csv(ams_short_long_table, "magma_costs_ams_short_long.csv")

# --- Export LaTeX (two panels) and also save panel kables separately
tab_short <- ams_short_long_table %>%
  filter(horizon == "Short-term (Oct 2020–Jun 2022)") %>%
  select(component, Gramatneusiedl, `Control towns`, Difference) %>%
  kable(format = "latex", booktabs = TRUE, align = "lrrr",
        col.names = c("", "Gramatneusiedl", "Control towns", "Difference"))

tab_long <- ams_short_long_table %>%
  filter(horizon == "Long-term (Jul 2022–Mar 2024)") %>%
  select(component, Gramatneusiedl, `Control towns`, Difference) %>%
  kable(format = "latex", booktabs = TRUE, align = "lrrr",
        col.names = c("", "Gramatneusiedl", "Control towns", "Difference"))

# write full two-panel table
readr::write_lines(
  c(
    "% Appendix version of Table 12: AMS costs vs benefits by horizon",
    "\\begin{tabular}{lrrr}",
    "\\toprule",
    "\\multicolumn{4}{c}{\\textit{Short-term (Oct 2020--Jun 2022)}}\\\\",
    "\\midrule",
    tab_short %>% str_split("\n") %>% unlist() %>% .[!str_detect(., "^\\\\begin\\{|^\\\\end\\{|^\\\\toprule|^\\\\bottomrule|^\\\\midrule$")],
    "\\midrule",
    "\\multicolumn{4}{c}{\\textit{Long-term (Jul 2022--Mar 2024)}}\\\\",
    "\\midrule",
    tab_long %>% str_split("\n") %>% unlist() %>% .[!str_detect(., "^\\\\begin\\{|^\\\\end\\{|^\\\\toprule|^\\\\bottomrule|^\\\\midrule$")],
    "\\bottomrule",
    "\\end{tabular}"
  ),
  "cost_comparison_ams_short_long.tex"
)

# save panel-level .tex files for transparency/audit
write_lines(tab_short, "table_ams_short_term.tex")
write_lines(tab_long,  "table_ams_long_term.tex")
