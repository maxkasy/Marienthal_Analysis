# Loading packages ----
# for data prep
library(tidyverse)
library(readr)
library(lubridate)
library(readxl) # for excel input files
# for analysis
library(nbpMatching) # For re-running the treatment assignment, for randomizaton inference
library(furrr) # For parallel computation in randomization inference
library(estimatr) # for robust standard errors
# for figures and tables
library(kableExtra) # for table export
library(patchwork) # for combining figures
library(ggtext) # add annotations to figures
library(data.table) # process csv files faster
library(broom)
library(ggtext)


home <- getwd()
Lukas = T
# Switch path for data-files
if (Lukas) {veracrypt_path = "A:/"} else {veracrypt_path = "/Volumes/NO NAME/"}


# 0 Data preparation ----
# prepare survey data
# from Marienthal and control towns, 2021 and 2022 survey wave
source("0a_survey_responses_aggregation_2021_2022.R")

# prepare admin data
# from Marienthal, 2021 and 2022
source("0b_i_admin_data_prep_2021_2022.R")
# from control towns, 2021 and 2022
source("0b_i_admin_data_prep_control_towns-2021_2022.R")

#prepare synthetic control data
source("0c_i_synth_data_prep_outcome_data.R")
source("0c_ii_synth_data_prep_control_data.R")
source("0c_iii_synth_data_prep_merge_control_outcome_data.R")
source("0c_iv_synth_data_prep_merge_new_lzbl.R")


# 1 Experimental analysis for Marienthal ----
# Switching analysis between different levels of aggregation
aggregated = T # Analysis for aggregate indices
source("1b_Marienthal_responses_analysis.R")
aggregated = F # Disaggregated analysis for LAMB, Preferences
source("1b_Marienthal_responses_analysis.R")

# 2 Synthetic control analysis ----
source("2a_synthetic_control_analysis.R")
source("2b_synthetic_control_plots.R")

# 3 Analysis comparing to control town individuals ----
source("3b_i_Control-town_individuals_analysis_2021.R")
source("3b_ii_Control-town_individuals_analysis_2022.R")

# 4 Mechanisms: hazard rates ----
# need to run 4a on WU virutal machine to merge AMDB data with AMS provided IDs for short-term unemployed
source("4a-hazard_rates_prep_data.R")
source("4b-hazard_rates_analysis.R")

# 5 Cost comparison ----
# need to run 5 on AMS computer where cost data is stored
source("5-cost-comparison_analysis.R")

# 5 Differential survey response analysis ----
source("6-differential_response_analysis.R")

