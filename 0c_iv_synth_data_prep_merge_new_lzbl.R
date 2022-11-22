###### Data preparation for corrected AMS Data: - 2022-02
# New SV data are now splitted into long and short term unemployed
# Source: AMS Erwerbskarrieremonitoring
# Diese Daten basieren auf der SV Datenbank und damit der geographischen PLZ Zuordnung.
# Die PLZ Zuordnung überschneidet sich mit anderen Gemeinden und 
# entspricht nicht ausschließlich Gramatneusiedl.
# Tatsächlich verwenden wir diese SV Daten aber weiterhin, auch für den split zwischen
# langzeit und kurzzeit Arbeitslosigkeit, um eine einheitliche Evaluierung konsistent
# mit den anderen Quellen zu gewährleisten.
# Würden wir nur die - geographisch korrekten - AMS Daten verwenden, und die SV Daten 
# nicht verwenden, hätten wir keine Daten zu Beschäftigung und könnten somit keine AL Raten 
# oder Wages etc berechnen und auch nicht für unsere Outcomes evaluieren.


home <- getwd()

#### Prep new SV data #### 
# from AMS Erwerbskarrieremonitoring - split between long and short term unemployed
# weird that it is in the AL_NOE subfolder instead of the BEV_NOE subfolder
# but the data corresponds to the previous SV data
# long- and short term ue sum up to total ue based on SV from previous deliveries

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
data_out = "A:/jobguarantee/2022-02-municipal-data-processed/" # Lukas
# data_out = "/Volumes/MARIENTHAL/jobguarantee/2021-09-municipal-data-processed/" # Max

data_ams_wide %>%
  write_csv(paste(data_out,
                  "municipalities_ams_ue.csv",
                  sep = ""))


#### Merge to municipalities_merged_monthly ####

 veracrypt_path = "A:/" # Lukas
#veracrypt_path = "/Volumes/MARIENTHAL/" # Max

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


# sum up to total population working-age using new unemployment variable
# that excludes AMS course participants
# municipalities <- municipalities %>%
#  mutate(POP_workingage_revised = ue_tot_ams + BE + SO)

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


