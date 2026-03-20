# Loading packages ----
# for data prep
library(tidyverse)
library(readr)
library(lubridate)
library(readxl) # for excel input files
library(data.table)
library(tidyr)
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
library(zoo)
library(xtable)
library(slider) # to calculate moving averages

home <- getwd()

Windows = F
# Switch path for data-files T (Windows) or F (Mac)
if (Windows) {veracrypt_path = "A:/"} else {veracrypt_path = "/Volumes/NO NAME/"}

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
# Produces Figure 3: Experimental estimates with linear controls
# Table 4: Experimental estimates with linear controls
# Figure 4: Experimental estimates with linear controls, disaggregated outcomes
# Figure A.7: Experimental estimates with pair controls
# Figure A.8: Experimental estimates with no controls
# Switching analysis between different levels of aggregation
aggregated = T # Analysis for aggregate indices
source("1a_Marienthal_responses_analysis.R")
aggregated = F # Disaggregated analysis for LAMB, Preferences
source("1a_Marienthal_responses_analysis.R")

# 2 Synthetic control analysis ----
# Produces Figure 5: Synthetic control estimates of the program effect on unemployment
# Figure 6: Synthetic control estimates of the program effect on employment and inactivity
source("2a_synthetic_control_analysis.R")
source("2b_synthetic_control_plots.R")

# 3 Analysis comparing to control town individuals ----
# Produces Figure 10: Control town comparisons with linear controls, economic outcomes
# Figure 11: Control town comparisons with linear controls, other outcomes
# Table 5: Control town comparisons with linear controls, economic outcomes
# Table 6: Control town comparisons with linear controls, other outcomes
# Figure A.4: Confidence intervals for contrast of Group 2 and Group 1 in February 2021
# Figure A.5: Confidence intervals for contrast of Group 2 and control town individuals, February 2021
# Figure A.6: Confidence intervals for contrast of participants in both groups and control town individuals, February 2022
source("3a_i_Control-town_individuals_analysis_2021.R")
source("3a_ii_Control-town_individuals_analysis_2022.R")
# Table A.6: Lee bounds, economic outcomes
# Table A.7: Lee bounds, other outcomes
source("3b_leebounds.R")

# 4 Hazard rate analysis ----
# produces Figure 9: Hazard rates out of unemployment
# need to run 4a on WU virutal machine to merge AMDB data with AMS provided IDs for short-term unemployed
# source("4a-hazard_rates_prep_data.R")
source("4b-hazard_rates_analysis.R")

# 5 Cost comparison ----
# produces Figure 12: Expenditures per person and month
# computes costs used in Table 7: Expenditures and revenues per person and month, October 2020 to March 2024
# computes costs used in Table A.8: Expenditures and revenues per person and month, Oct 2020 to June 2022
# computes costs used in Table A.9: Expenditures and revenues per person and month, July 2022 to March 2024
# Script "5-cost-comparison_analysis.R" needs to be executed on the AMS computer where the cost data is stored.
# source("5a-cost-comparison_analysis.R")
source("5b-cost-comparison_plot.R")

# 6 Differential survey response analysis ----
# Produces Table A.3: Covariate balance for survey respondents in Gramatneusiedl, 2021
# Table A.4: Covariate balance for survey respondents in our control town sample, 2021
# Table A.5: Covariate balance for survey respondents in our control town sample, 2022
source("6-differential_response_analysis.R")

# 7 Employment status analysis ----
# produces Figure 7: Cumulative days per person
source("7a-cumulative_employment_prep.R")
source("7b-cumulative_employment_analysis.R")

# 8 Earnings analysis ----
# produces Figure 8: Average gross monthly earnings for those eligible at baseline
# computes earnings used in Table 7: Expenditures and revenues per person and month, October 2020 to March 2024
# computes earnings used in Table A.8: Expenditures and revenues per person and month, Oct 2020 to June 2022
# computes earnings used in Table A.9: Expenditures and revenues per person and month, July 2022 to March 2024
# "VM PART" needs to be executed on the WU virtual machine where the earnings data is stored
source("8-admin-earnings_prep_costs.R")

