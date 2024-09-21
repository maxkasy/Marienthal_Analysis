# 4a-hazard_rates_prep_data
# merge AMDB spell data with the AMS data on all PENR in short-term unemployment (<12 months) for treatment and control towns by GKZ
# Code runs only on WU Virtual Machine with access to original AMDB data
library(tidyverse)
library(readr)
library(data.table)

## Settings ####
# home <- getwd()
home <- "D:/magma"
amdb_path = "D:/amdb/"
output_path = "D:/magma/output/"

ams_data <- file.path(home, "short_term_unemployed_ams.csv")

amdb_data <- file.path(amdb_path, "mon_uni_status_intnew.csv")

output_csv <- file.path(home, "short_term_unemployed.csv")


## Load data ####
# load AMS data for short term unemployed from control municipalities
data_ams <- fread(ams_data)

# collapse by PENR
data_ams_collapsed <- data_ams %>%
  group_by(penr) %>%
  summarise(GKZ = first(GKZ)) %>%  # Use first GKZ value for each penr
  ungroup()


# load AMDB
data_amdb <- fread(amdb_data)

## Merge ####
# Merge the two data tables based on "penr" and keep only those observations that could be merged
merged_data <- inner_join(data_amdb, data_ams_collapsed, by = "penr")


# Drop data earlier than 2020
merged_data_filtered <- merged_data %>%
  filter(beginn >= 20200101)

# Create a treatment dummy variable
merged_data_filtered <- merged_data_filtered %>%
  mutate(treat = ifelse(GKZ == 30731, 1, 0))


## Save ####
# Save the transition_rates_filtered data frame as CSV to home directory
merged_data_filtered %>% 
  write.csv(file = output_csv, row.names = FALSE)

