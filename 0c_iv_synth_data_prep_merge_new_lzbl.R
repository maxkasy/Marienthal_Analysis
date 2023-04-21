###### Data preparation for new AMS Data: - 2022-02
# New SV data are now splitted into long and short term unemployed
# Source: AMS Erwerbskarrieremonitoring

home <- getwd()

#### Prep new SV data #### 
read_data_NOE = function(path, filename) {
  paste(path, filename, sep = "") %>%
    read_delim(delim = ";",
               locale = locale(encoding = "latin1", decimal_mark = ","))
}

subpath = "jobguarantee/2022-02-municipal-data-raw/AL_NOE/"
data_path_ams = paste(veracrypt_path, subpath, sep = "")

data_ams = read_data_NOE(data_path_ams, 
                              "AL_NOE_LZBL_pro_Stichtag_ab_201101.dsv")

data_ams = data_ams %>%
  rename(n = "SUM(XT.REEMPLOYEES)" )

data_ams <- data_ams %>%
  mutate(STICHTAG = ymd(STICHTAG)) 

# reshape dataset to wide
data_ams_wide = data_ams %>%
  spread(LBZL, n) %>%
  mutate(J = replace_na(J, 0), # replace NAs with 0 to allow summarise
         N = replace_na(N, 0)
         ) %>%
  rename(ue_long_ams = J,
         ue_short_ams = N)

# sum rows of same category
data_ams_wide <- data_ams_wide %>%
  group_by(GKZ, GEMEINDE, STICHTAG) %>%
  summarize(ue_long_ams = sum(ue_long_ams),
            ue_short_ams = sum(ue_short_ams) )


# compute ue rates
data_ams_wide = data_ams_wide %>%
  mutate(
    ue_tot_ams = ue_long_ams + ue_short_ams,
    ue_short_by_ue_tot_ams = ue_short_ams / ue_tot_ams,
    # short term UE as a share of total UE
    ue_long_by_ue_tot_ams = ue_long_ams / ue_tot_ams     # long term UE as a share of total UE
  ) 

# create monthly average for merging
data_ams_wide$month <- data_ams_wide$STICHTAG %>%
  format("%Y-%m") 

# export
data_out = paste0(veracrypt_path, "jobguarantee/2022-02-municipal-data-processed/") 
data_ams_wide %>%
  write_csv(paste(data_out,
                  "municipalities_ams_ue.csv",
                  sep = ""))


#### Merge to municipalities_merged_monthly ####
outcomes_subpath = "jobguarantee/2021-09-municipal-data-processed/"
data_path = paste(veracrypt_path, outcomes_subpath, sep = "")

admin_data_towns =
  paste0(data_path, "municipalities_merged_monthly.csv") %>%
  read_csv( 
    col_types = paste(c("icc", rep("n", 81)), collapse = '')   ) 


# change variable type for merging
data_ams_wide <- data_ams_wide %>%
  mutate(GKZ = as.double(GKZ))


municipalities =
  admin_data_towns %>%  # long & short term UE longitudinal LM status longitudinal
  group_by(GKZ, month) %>%
  left_join(data_ams_wide, by = c("GKZ", "month"))

# compute long-term ue rates with SV long term ue data
municipalities = municipalities %>%
mutate(
  UE_rate_tot_sv_control = ue_tot_ams / POP_workingage,
  UE_short_rate_tot_sv = ue_short_ams / POP_workingage,
  UE_long_rate_tot_sv = ue_long_ams / POP_workingage
# EMP_rate_tot_sv = BE / POP_workingage_revised,
#  Inactive_rate_tot_sv = SO / POP_workingage_revised 
) 

# export
municipalities %>%
  write_csv(paste(data_out,
                  "municipalities_merged_monthly_outcomes.csv",
                  sep = ""))



###### Data preparation for new AMS Data: - 2023-02
# New SV data are now splitted into long and short term unemployed
# Source: AMS Erwerbskarrieremonitoring

home <- getwd()

#### Prep new SV data #### 
read_data_NOE = function(path, filename) {
  paste(path, filename, sep = "") %>%
    read_delim(delim = ";",
               locale = locale(encoding = "latin1", decimal_mark = ","))
}

subpath = "jobguarantee/2023-02-municipal-data-raw/AL_NOE/"
data_path_ams = paste(veracrypt_path, subpath, sep = "")

data_ams_2023 = read_data_NOE(data_path_ams, 
                         "AL_NOE_LZBL_pro_Stichtag_ab_201101.csv")

data_ams_2023 = data_ams_2023 %>%
  rename(n = "SUM(XT.REEMPLOYEES)" )

# data_ams <- data_ams %>%
#  mutate(STICHTAG = ymd(STICHTAG)) 

data_ams_2023 <- data_ams_2023 %>%
  mutate(dateTime=as.Date(STICHTAG, format = "%Y-%m-%dT%H:%M:%S+0000"))

# reshape dataset to wide
data_ams_wide_2023 = data_ams_2023 %>%
  spread(LBZL, n) %>%
  mutate(J = replace_na(J, 0), # replace NAs with 0 to allow summarise
         N = replace_na(N, 0)
  ) %>%
  rename(ue_long_ams = J,
         ue_short_ams = N)

# sum rows of same category
data_ams_wide_2023 <- data_ams_wide_2023 %>%
  group_by(GKZ, dateTime) %>%
  summarize(ue_long_ams = sum(ue_long_ams),
            ue_short_ams = sum(ue_short_ams) )


# compute ue rates
data_ams_wide_2023 = data_ams_wide_2023 %>%
  mutate(
    ue_tot_ams = ue_long_ams + ue_short_ams,
    ue_short_by_ue_tot_ams = ue_short_ams / ue_tot_ams,
    # short term UE as a share of total UE
    ue_long_by_ue_tot_ams = ue_long_ams / ue_tot_ams     # long term UE as a share of total UE
  ) 

# create monthly average for merging
data_ams_wide_2023$month <- data_ams_wide_2023$dateTime %>%
  format("%Y-%m") 

data_ams_wide_2023 = data_ams_wide_2023 %>%
  rename(STICHTAG = dateTime )

# export
data_out = paste0(veracrypt_path, "jobguarantee/2023-02-municipal-data-processed/") 
data_ams_wide_2023 %>%
  write_csv(paste(data_out,
                  "municipalities_ams_ue.csv",
                  sep = ""))


#### Merge to municipalities_merged_monthly ####
outcomes_subpath = "jobguarantee/2023-02-municipal-data-processed/"
data_path = paste(veracrypt_path, outcomes_subpath, sep = "")

admin_data_towns =
  paste0(data_path, "municipalities_merged_monthly.csv") %>%
  read_csv(
    col_types = paste(c("icc", rep("n", 81)), collapse = '')   )


# change variable type for merging
data_ams_wide_2023 <- data_ams_wide_2023 %>%
  mutate(GKZ = as.double(GKZ))

## drop 2021-06 to 2021-12 data from old outcome files
# and use data from new 2023 outcome files for this period
# to avoid having double entries and outcomes for this period
# outcome variables differ slightly between old and new files
# we use the new files to incorporate revisions
data_ams_wide <- data_ams_wide %>% 
  filter(month < "2021-06")


### Append data update: data_ams_wide_2023 to data_ams_wide before merging ###
data_ams_wide_merged <- rbind(data_ams_wide, data_ams_wide_2023) %>% 
  group_by(GKZ, month) %>% 
  select(-GEMEINDE)



## drop monthly observations that exist in both datasets before merge
# drop 2021-06, 2021-07 and 2021-08 from admin_data_towns
# admin_data_towns <- admin_data_towns %>%
#   filter(month < "2021-06")


# municipalities =
#   admin_data_towns %>%  # long & short term UE longitudinal LM status longitudinal
#   group_by(GKZ, month) %>%
#   full_join(data_ams_wide, by = c("GKZ", "month"))


 municipalities =
  admin_data_towns %>%  # long & short term UE longitudinal LM status longitudinal
  group_by(GKZ, month) %>%
  full_join(data_ams_wide_merged, by = c("GKZ", "month"))


municipalities <- municipalities %>% 
  arrange(GKZ, month)

# drop observations before 2018
municipalities <- municipalities %>%
  filter(month > "2018-01")

# compute long-term ue rates with SV long term ue data
municipalities = municipalities %>%
  mutate(
    UE_rate_tot_sv_control = ue_tot_ams / POP_workingage,
    UE_short_rate_tot_sv = ue_short_ams / POP_workingage,
    UE_long_rate_tot_sv = ue_long_ams / POP_workingage
    # EMP_rate_tot_sv = BE / POP_workingage_revised,
    #  Inactive_rate_tot_sv = SO / POP_workingage_revised 
  ) 


# export
municipalities %>%
  write_csv(paste(data_out,
                  "municipalities_merged_monthly.csv",
                  sep = ""))

# #### Merge to municipalities_merged_monthly_outcomes ####
# 
# admin_data_towns_outcomes =
#   paste0(data_path, "municipalities_merged_monthly_outcomes.csv") %>%
#   read_csv(
#     col_types = paste(c("innc", rep("n", 81)), collapse = '')   )
# 
# municipalities_outcomes =
#   admin_data_towns_outcomes %>%  # long & short term UE longitudinal LM status longitudinal
#   group_by(GKZ, month) %>%
#   full_join(data_ams_wide_merged, by = c("GKZ", "month"))
# 
# municipalities_outcomes <- municipalities_outcomes %>% 
#   arrange(GKZ, month)
# 
# # drop observations before 2018
# municipalities_outcomes <- municipalities_outcomes %>%
#   filter(month > "2018-01")
# 
# # export
# municipalities_outcomes %>%
#   write_csv(paste(data_out,
#                   "municipalities_merged_monthly_outcomes.csv",
#                   sep = ""))

