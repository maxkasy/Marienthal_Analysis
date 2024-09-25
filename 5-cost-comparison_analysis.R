# 5-cost-comparison_analysis
# calculate transition rates from unemployment to employment by month of unemployment spell duration
library(tidyverse)
library(readr)
library(data.table)
library(ggtext)
library(janitor)

## Settings ----

AMS = F
# Switch path for data-files between AMS and Lukas/Max computing environment
if (AMS) {
  home <- getwd()
  
  data_path = file.path(home, "cost_data")
  
  pstnr_data <- file.path(home, "pstnr_magma_control_towns.csv")
  
  } else {

    data_path = file.path(dirname(home), "Cost_data", "cost_data")
    
    pstnr_data <- paste0(veracrypt_path, "jobguarantee/pstnr_magma_control_towns.csv")
    }

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

# filter for individuals in sample
# Read the PSTNR of treat and control individuals
data_pstnr <- fread(pstnr_data)

magma_pstnr <- data_pstnr$magma_pstnr
control_pstnr <- data_pstnr$control_pstnr

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


# Calculate days since program start (2020-10-01)
data_foer_individual <- data_foer_individual %>%
  mutate(DAYS = as.numeric(difftime(foerderung_bis, START, units = "days")),
         DAYS = DAYS + 1, # to include the end day as well
         # calcualate days undadjusted:
         DAYS_unadjusted = as.numeric(difftime(foerderung_bis, foerderung_von, units = "days")),
         DAYS_unadjusted = DAYS_unadjusted + 1) # to include the end day as well) 

data_foer_traeger <- data_foer_traeger %>%
  mutate(DAYS = as.numeric(difftime(foerderung_bis, START, units = "days")),
         DAYS = DAYS + 1, # to include the end day as well
         # calcualate days undadjusted:
         DAYS_unadjusted = as.numeric(difftime(foerderung_bis, foerderung_von, units = "days")),
         DAYS_unadjusted = DAYS_unadjusted + 1) # to include the end day as well) 

data_foer_individual2 <- data_foer_individual
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

# divide total cost by nr of individuals by treatment and control group
total_cost_foer_individual <- total_cost_foer_individual %>%
  mutate(person_foer_cost = total_foer_cost / n_people)

total_cost_foer_traeger <- total_cost_foer_traeger %>%
  mutate(person_foer_cost = total_foer_cost / n_people)


## sum foer individual and traeger costs ----
# merge
total_cost_foer <- bind_rows(total_cost_foer_individual, total_cost_foer_traeger)
  
# sum
total_cost_foer <- total_cost_foer %>% 
group_by(treat) %>%
  summarize(total_foer_cost = sum(total_foer_cost, na.rm = TRUE),
            person_foer_cost = sum(person_foer_cost, na.rm = TRUE), )

total_cost_foer



### ALV calculation ----

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


## Calculate total cost for alv ----
data_alv <- data_alv %>%
  mutate(benefit_cost = benefit_cost_day * DAYS)

# Sum the values of benefit_cost for treat == 1 and control == 1
sum_treat <- data_alv %>%
  filter(treat == 1) %>%
  summarise(total_benefit_cost = sum(benefit_cost, na.rm = TRUE))

sum_control <- data_alv %>%
  filter(control == 1) %>%
  summarise(total_benefit_cost = sum(benefit_cost, na.rm = TRUE))

# Combine the results into a single data frame
total_cost_alv <- bind_rows(
  sum_treat %>% mutate(group = "treat"),
  sum_control %>% mutate(group = "control")
)

total_cost_alv


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

# divide total cost by nr of individuals by treatment and control group
total_cost_alv <- total_cost_alv %>%
  mutate(person_benefit_cost = total_benefit_cost / n_people)




## sum foer and alv costs ----
# rename variables
total_cost_foer <- total_cost_foer %>%
  rename(total_cost = total_foer_cost,
         person_cost = person_foer_cost)

# create cost per month and per year
total_cost_foer <- total_cost_foer %>%
  group_by(treat) %>%
  mutate(person_cost_month = person_cost / 27,
         person_cost_year = person_cost_month * 12)

total_cost_alv <- total_cost_alv %>%
  rename(total_cost = total_benefit_cost,
         person_cost = person_benefit_cost)

# create cost per month and per year
total_cost_alv <- total_cost_alv %>%
  group_by(treat) %>%
  mutate(person_cost_month = person_cost / 27,
         person_cost_year = person_cost_month * 12)

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

