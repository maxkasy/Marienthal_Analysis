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


