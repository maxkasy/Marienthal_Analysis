###### Data preparation for synthetic control
data_path = paste0(veracrypt_path, "jobguarantee/2021-09-municipal-data-raw/")


#### 1) Unemployed - cross-section ####
## Unemployed
# cross-section data
sub_paths_2020_12 = "2020-12_AL_NOE/2020-12_"
sub_paths_2021_07 = "2021-07_AL_NOE/2021-07_"

path_AL = "2020-09_bis_2021-08_AL_NOE/"

files_used = c("AL_NOE_ALTER.dsv",
               "AL_NOE_AUSBILDUNG.dsv",
               "AL_NOE_DEUTSCH.dsv",
               "AL_NOE_GESCHLECHT.dsv",
               "AL_NOE_MIGRATIONSHINTERGRUND.dsv")


#sub_paths = c("2021-07_AL_NOE/2021-07_")
#file_paths = paste(data_path, sub_paths, files_used, sep="")
#data_list_al = map(file_paths, function(path) read_delim(path, delim=";",
#                                                         locale = locale(encoding = "latin1", decimal_mark = ",")))



# loop over AL cross-section variables
length(files_used)
year_vec <- 2020:2021

path_list <- list(sub_paths_2020_12 , sub_paths_2021_07)
names_list <- list( "2020" , "2021" )

months_list <- list("2020-12", "2021-07")

mean_age_AL_list <- list()
edu_AL_list <- list()
german_AL_list <- list()
sex_AL_list <- list()
mig_AL_list <- list()

file_path <- list()

for( y in 1:length(path_list)) {
  
### age ####
  file_path[[y]] = paste(data_path,  path_list[[y]], files_used[1], sep = "")
  names(file_path)[y] <- names_list[[y]]
  
  data_age_AL_tmp = read_delim(
    file_path[[y]],
    delim = ";",
    locale = locale(encoding = "latin1", decimal_mark = ",")
  )
  
  mean_age_AL_tmp <- data_age_AL_tmp %>%
    group_by(GKZ) %>%
    summarize(age_mean_AL = weighted.mean(ALTER, n)) %>% # weighted average
    mutate(month = months_list[[y]] %>%  # add year or month for merging
             as.character()) # change variable type for merging
  
  
  # change GKZ variable type to double to allow merging
  mean_age_AL_tmp = mean_age_AL_tmp %>%
    mutate(GKZ = as.double(GKZ))
  
  mean_age_AL_list[[y]] <-
    mean_age_AL_tmp
  names(mean_age_AL_list)[y] <- names_list[[y]]


### education ####
  file_path[[y]] = paste(data_path,  path_list[[y]], files_used[2], sep = "")
  names(file_path)[y] <- names_list[[y]]

data_edu_AL_tmp = read_delim(
  file_path[[y]], 
  delim=";",
  locale = locale(encoding = "latin1", decimal_mark = ",")
)

# aggregate edu status
# ISCED 5-6 = edu_high_AL
# ISCED 3-4 = edu_middle_AL
# ISCED 1-2 = edu_low_AL
# -- = edu_NA_AL
data_edu_AL_tmp <- data_edu_AL_tmp %>%
  mutate(
    AUSBILDUNG = replace(AUSBILDUNG, AUSBILDUNG == "AK", "edu_high_AL"),
    # AK Akademie (ISCED 5+6) = high
    AUSBILDUNG = replace(AUSBILDUNG, AUSBILDUNG == "FB", "edu_high_AL"),
    # FB Fachhochschule Bakkalaureat (ISCED 5+6) = high
    AUSBILDUNG = replace(AUSBILDUNG, AUSBILDUNG == "FH", "edu_high_AL"),
    # FH Fachhochschule (ISCED 5+6) = high
    AUSBILDUNG = replace(AUSBILDUNG, AUSBILDUNG == "UB", "edu_high_AL"),
    # UB Bakkalaureatstudium (ISCED 5+6) = high
    AUSBILDUNG = replace(AUSBILDUNG, AUSBILDUNG == "UV", "edu_high_AL"),
    # UV Universitaet (ISCED 5+6) = high
    AUSBILDUNG = replace(AUSBILDUNG, AUSBILDUNG == "HB", "edu_middle_AL"),
    # HB Hoehere berufsbildende Schule (ISCED 4) = edu_middle_AL
    AUSBILDUNG = replace(AUSBILDUNG, AUSBILDUNG == "HK", "edu_middle_AL"),
    # HK Hoehere kaufmaennische Schule (ISCED 4) = edu_middle_AL
    AUSBILDUNG = replace(AUSBILDUNG, AUSBILDUNG == "HT", "edu_middle_AL"),
    # HT Hoehere technische Schule (ISCED 4) = edu_middle_AL
    AUSBILDUNG = replace(AUSBILDUNG, AUSBILDUNG == "LM", "edu_middle_AL"),
    # LM Lehre und Meisterpruefung  (ISCED 4) = edu_middle_AL
    AUSBILDUNG = replace(AUSBILDUNG, AUSBILDUNG == "H*", "edu_middle_AL"),
    # H* Hoehere Schule (ISCED 3) = edu_middle_AL
    AUSBILDUNG = replace(AUSBILDUNG, AUSBILDUNG == "HA", "edu_middle_AL"),
    # HA Allgemeinbildende hoehere Schule (ISCED 3) = edu_middle_AL
    AUSBILDUNG = replace(AUSBILDUNG, AUSBILDUNG == "HS", "edu_middle_AL"),
    # HS Hoehere sonstige Schule (ISCED 3) = edu_middle_AL
    AUSBILDUNG = replace(AUSBILDUNG, AUSBILDUNG == "L*", "edu_middle_AL"),
    # L* Allgemeine Lehrausbildung (ISCED 3) = edu_middle_AL
    AUSBILDUNG = replace(AUSBILDUNG, AUSBILDUNG == "LE", "edu_middle_AL"),
    # LE Lehre (ISCED 3) = edu_middle_AL
    AUSBILDUNG = replace(AUSBILDUNG, AUSBILDUNG == "LT", "edu_middle_AL"),
    # LT Teilintegrierte Lehre (ISCED 3) = edu_middle_AL
    AUSBILDUNG = replace(AUSBILDUNG, AUSBILDUNG == "M*", "edu_middle_AL"),
    # M* Mittlere Schule (ISCED 3) = edu_middle_AL
    AUSBILDUNG = replace(AUSBILDUNG, AUSBILDUNG == "MB", "edu_middle_AL"),
    # MB Mittlere berufsbildende Schule (ISCED 3) = edu_middle_AL
    AUSBILDUNG = replace(AUSBILDUNG, AUSBILDUNG == "MK", "edu_middle_AL"),
    # MK Mittlere kaufmaennische Schule (ISCED 3) = edu_middle_AL
    AUSBILDUNG = replace(AUSBILDUNG, AUSBILDUNG == "MS", "edu_middle_AL"),
    # MS Sonstige mittlere Schule (ISCED 3) = edu_middle_AL
    AUSBILDUNG = replace(AUSBILDUNG, AUSBILDUNG == "MT", "edu_middle_AL"),
    # MT Mittlere technische Schule (ISCED 3) = edu_middle_AL
    AUSBILDUNG = replace(AUSBILDUNG, AUSBILDUNG == "PO", "edu_low_AL"),
    # PO Keine abgeschlossene Pflichtschule (ISCED 2) = edu_low_AL
    AUSBILDUNG = replace(AUSBILDUNG, AUSBILDUNG == "PS", "edu_low_AL"),
    # PS Pflichtschule (ISCED 2) = edu_low_AL
    AUSBILDUNG = replace(AUSBILDUNG, AUSBILDUNG == "XX", "edu_NA_AL"),
    # XX Ungeklaert = edu_NA_AL
    AUSBILDUNG = replace(AUSBILDUNG, AUSBILDUNG == "--", "edu_NA_AL")
    # -- Ungeklaert = edu_NA_AL
  )

# sum rows of same category
data_edu_AL_tmp <- data_edu_AL_tmp %>%
  group_by(GKZ, AUSBILDUNG) %>%
  summarize(n = sum(n))

# reshape dataset to wide
edu_AL_wide_tmp = data_edu_AL_tmp %>%
  spread(AUSBILDUNG, n) %>%
  mutate(edu_NA_AL = replace_na(edu_NA_AL, 0), # replace NAs with 0 to allow summarise
        edu_low_AL = replace_na(edu_low_AL, 0),
        edu_middle_AL = replace_na(edu_middle_AL, 0),
        edu_high_AL = replace_na(edu_high_AL, 0) 
        )

# compute rates
edu_AL_wide_tmp = edu_AL_wide_tmp %>%
  mutate(
    edu_high_AL_by_tot_AL = edu_high_AL / (edu_high_AL + edu_middle_AL + edu_low_AL + edu_NA_AL),
    edu_middle_AL_by_tot_AL = edu_middle_AL / (edu_high_AL + edu_middle_AL + edu_low_AL + edu_NA_AL),
    edu_low_AL_by_tot_AL = edu_low_AL / (edu_high_AL + edu_middle_AL + edu_low_AL + edu_NA_AL)
  ) %>%
  mutate(month = months_list[[y]] %>% # add year or month for merging
        as.character()) # change variable type for merging

# change GKZ variable type to double to allow merging
edu_AL_wide_tmp = edu_AL_wide_tmp %>%
  mutate(GKZ = as.double(GKZ))

edu_AL_list[[y]] <-
  edu_AL_wide_tmp
names(edu_AL_list)[y] <- names_list[[y]]



### German ####
file_path[[y]] = paste(data_path,  path_list[[y]], files_used[3], sep = "")
names(file_path)[y] <- names_list[[y]]

data_german_AL_tmp = read_delim(
  file_path[[y]], 
  delim=";",
  locale = locale(encoding = "latin1", decimal_mark = ",")
)


# aggregate German by >A or no limitation (NA)
data_german_AL_tmp <- data_german_AL_tmp %>%
  group_by(GKZ) %>%
  mutate(DEUTSCH = as.integer((DEUTSCH != "K") & (DEUTSCH != "A")
         & (DEUTSCH != "A1")) & (DEUTSCH != "A2"), # more than A or no info
  )        %>%
  group_by(GKZ, DEUTSCH) %>%
  summarize(sum_n = sum(n)) %>%
  mutate(month = months_list[[y]] %>% # add year or month for merging
         as.character()) # change variable type for merging

# reshape dataset to wide
german_AL_wide_tmp = data_german_AL_tmp %>%
  spread(DEUTSCH, sum_n) %>%
  rename(ok_german = "TRUE",
         poor_german = "FALSE" ) %>%
  mutate(ok_german = replace_na(ok_german, 0), # replace NAs with 0 to allow summarise
         poor_german = replace_na(poor_german, 0) 
  )

# compute rates
german_AL_wide_tmp = german_AL_wide_tmp %>%
  mutate(
    poor_german_AL_by_tot_AL = poor_german / (ok_german + poor_german)
    ) 

# change GKZ variable type to double to allow merging
german_AL_wide_tmp = german_AL_wide_tmp %>%
  mutate(GKZ = as.double(GKZ))

german_AL_list[[y]] <-
  german_AL_wide_tmp
names(german_AL_list)[y] <- names_list[[y]]

### sex ####
file_path[[y]] = paste(data_path,  path_list[[y]], files_used[4], sep = "")
names(file_path)[y] <- names_list[[y]]

data_sex_AL_tmp = read_delim(
  file_path[[y]], 
  delim=";",
  locale = locale(encoding = "latin1", decimal_mark = ",")
) 

# prep data
sex_AL_wide_tmp = data_sex_AL_tmp %>%
  spread(GESCHLECHT, n) %>% # reshape dataset to wide
  mutate(M = replace_na(M, 0), # replace NAs with 0 to allow summarise
         W = replace_na(W, 0)) %>%
  group_by(GKZ) %>%
  summarize(M = sum(M), # sum rows of same GKZ to bring observations in same line
            W = sum(W)
  ) %>%
  mutate(
    men_AL_by_tot_AL = M / (M + W) # compute share of male unemployed
  ) %>%
  mutate(month = months_list[[y]] %>% # add year or month for merging
         as.character()) %>% # change variable type for merging
    rename(men_AL = M,
           women_AL = W)

# change GKZ variable type to double to allow merging
sex_AL_wide_tmp = sex_AL_wide_tmp %>%
  mutate(GKZ = as.double(GKZ))

sex_AL_list[[y]] <-
  sex_AL_wide_tmp
names(sex_AL_list)[y] <- names_list[[y]]



### migration ####
file_path[[y]] = paste(data_path,  path_list[[y]], files_used[5], sep = "")
names(file_path)[y] <- names_list[[y]]

data_mig_AL_tmp = read_delim(
  file_path[[y]], 
  delim=";",
  locale = locale(encoding = "latin1", decimal_mark = ",")
)

# aggregate migration first and second gen
data_mig_AL_tmp = data_mig_AL_tmp %>%
  mutate(
    MIGRATIONSHINTERGRUND = replace(MIGRATIONSHINTERGRUND, MIGRATIONSHINTERGRUND == "-", "no_mig_AL"),
    # "-" no migration background = no_mig
    MIGRATIONSHINTERGRUND = replace(MIGRATIONSHINTERGRUND, MIGRATIONSHINTERGRUND == "MIG1", "mig_AL"),
    # MIG1 migration background first gen = mig
    MIGRATIONSHINTERGRUND = replace(MIGRATIONSHINTERGRUND, MIGRATIONSHINTERGRUND == "MIG2", "mig_AL")
    # MIG2 migration background second gen = mig
  )

# prep data
mig_AL_wide_tmp = data_mig_AL_tmp %>%
  spread(MIGRATIONSHINTERGRUND, n) %>% # reshape dataset to wide
  mutate(mig_AL = replace_na(mig_AL, 0),
         no_mig_AL = replace_na(no_mig_AL, 0)) %>% # replace NAs with 0 to allow summarise
  group_by(GKZ) %>%
  summarize(mig_AL = sum(mig_AL), # sum rows of same GKZ to bring observations in same line
            no_mig_AL = sum(no_mig_AL)
  ) %>%
  mutate(
    mig_AL_by_tot_AL = mig_AL / (mig_AL + no_mig_AL) # compute share of male unemployed
  ) %>%
  mutate(month = months_list[[y]] %>% # add year or month for merging
         as.character()) # change variable type for merging

# change GKZ variable type to double to allow merging
mig_AL_wide_tmp = mig_AL_wide_tmp %>%
  mutate(GKZ = as.double(GKZ))


mig_AL_list[[y]] <-
  mig_AL_wide_tmp
names(mig_AL_list)[y] <- names_list[[y]]


}


## Bind 2020 and 2021 data into 1 file

mig_AL =
  do.call(rbind, mig_AL_list)

sex_AL =
  do.call(rbind, sex_AL_list)

german_AL =
  do.call(rbind, german_AL_list)

edu_AL =
  do.call(rbind, edu_AL_list)

mean_age_AL =
  do.call(rbind, mean_age_AL_list)


### unemployment benefits ####
# average 2020-2021 "Tagsatz"
file_path = paste(data_path,
                  path_AL,
                  "2020-09_bis_2021-08_AL_NOE_TAGSATZ.dsv",
                  sep = "")

Ubenefit_AL = read_delim(
  file_path,
  delim = ";",
  locale = locale(encoding = "latin1", decimal_mark = ",")
) %>%
  select( -1) %>% # drop first column X1
  rename(Ubenefit_daily_mean2020_2021 = TAGSATZ_DURCHSCHNITT) %>%
  mutate(month = "2021-07") %>% # add month or year for merging
  mutate(month = as.character(month), # change variable type for merging
         GKZ = as.numeric(GKZ) # change variable type for merging
         ) 


#### 2) Unemployed - longitudinal ####
### long term unemployed (LZBL) ####
file_path = paste(data_path,
                  path_AL,
                  "2020-09_bis_2021-08_AL_NOE_LZBL.dsv",
                  sep = "")

data_LZBL_AL = read_delim(
  file_path,
  delim = ";",
  locale = locale(encoding = "latin1", decimal_mark = ",")
)

# drop X1 (var for number of row)
myvars <- c("GKZ", "STICHTAG", "LZBL", "n")
data_LZBL_AL <- data_LZBL_AL[myvars]

# reshape dataset to wide for LZBL categories (short vs long term ue)
data_LZBL_AL = data_LZBL_AL %>%
  spread(LZBL, n)

# replace NA with 0 to create a balanced panel
data_LZBL_AL = data_LZBL_AL %>%
  mutate(ue_long = replace_na(J, 0),
         ue_short = replace_na(N, 0))

# create annual averages
# data_LZBL_AL$year <- data_LZBL_AL$STICHTAG %>%
#   format("%Y")
# 
# mean_LZBL_AL <- data_LZBL_AL %>%
#   group_by(GKZ, month) %>%
#   summarize(ue_long = mean(ue_long),
#             ue_short = mean(ue_short))

# create monthly averages
 data_LZBL_AL$month <- data_LZBL_AL$STICHTAG %>%
   format("%Y-%m") 
 
 mean_LZBL_AL <- data_LZBL_AL %>%
   select(-"J",-"N",-"STICHTAG")

 

# Link municipalities that changed GKZ over time (2017 reform)
oldGKZ <-
  c(
    32401,
    32402,
    32405,
    32406,
    32406,
    32407,
    32409,
    32410,
    32411,
    32413,
    32417,
    32418,
    32419,
    32424,
    32404,
    32403,
    32412,
    32415,
    32416,
    32421,
    32423,
    32408
  )

newGKZ <-
  c(
    30729,
    30730,
    30731,
    30732,
    30732,
    30733,
    30734,
    30735,
    30736,
    30737,
    30738,
    30739,
    30740,
    30741,
    31235,
    31949,
    31950,
    31951,
    31952,
    31953,
    31954,
    32144
  )

x <- c(1:length(oldGKZ)) 

for (i in x) {
  mean_LZBL_AL$GKZ[which(mean_LZBL_AL$GKZ == oldGKZ[i])] <- newGKZ[i]
}


# compute ue rates
mean_LZBL_AL = mean_LZBL_AL %>%
  mutate(
    ue_tot = ue_long + ue_short,
    ue_short_by_ue_tot = ue_short / ue_tot,
    # short term UE as a share of total UE
    ue_long_by_ue_tot = ue_long / ue_tot     # long term UE as a share of total UE
  )


# test for balanced panel - remaining GKZ have less than 10 observations
unbalanced = mean_LZBL_AL %>%
  group_by(GKZ) %>%
  summarize(n = n()) %>%
  filter(n < 10)

### health condition ####
file_path = paste(data_path,
                  path_AL,
                  "2020-09_bis_2021-08_AL_NOE_VERMITTLUNGSEINSCHRAENKUNG.dsv",
                  sep = "")

data_health_AL = read_delim(
  file_path,
  delim = ";",
  locale = locale(encoding = "latin1", decimal_mark = ",")
)

# aggregate health status
data_health_AL = data_health_AL %>%
  mutate(
    VERMITTLUNGSEINSCHRAENKUNG = as.integer(VERMITTLUNGSEINSCHRAENKUNG != "-"), # job relevant health issues
    VERMITTLUNGSEINSCHRAENKUNG = replace(VERMITTLUNGSEINSCHRAENKUNG, VERMITTLUNGSEINSCHRAENKUNG == "0", "health_condition_AL"),
    VERMITTLUNGSEINSCHRAENKUNG = replace(VERMITTLUNGSEINSCHRAENKUNG, VERMITTLUNGSEINSCHRAENKUNG == "1", "healthy_AL")
    )

# reshape dataset to wide for LZBL categories (short vs long term ue)
health_AL_wide = data_health_AL %>%
  spread(VERMITTLUNGSEINSCHRAENKUNG, n)

# sum rows of same category
health_AL_wide <- health_AL_wide %>%
  mutate(health_condition_AL = replace_na(health_condition_AL, 0),
         healthy_AL = replace_na(healthy_AL, 0)) %>%
  group_by(GKZ, STICHTAG) %>%
  summarize(health_condition_AL = sum(health_condition_AL),
            healthy_AL = sum(healthy_AL)
            )

# create annual averages
# health_AL_wide$year <- health_AL_wide$STICHTAG %>%
#   format("%Y")
# 
# mean_health_AL_wide <- health_AL_wide %>%
#   group_by(GKZ, year) %>%
#   summarize(health_condition_AL = mean(health_condition_AL),
#             healthy_AL = mean(healthy_AL))

# create monthly averages
health_AL_wide$month <- health_AL_wide$STICHTAG %>%
  format("%Y-%m") 

mean_health_AL_wide <- health_AL_wide %>%
  select(-"STICHTAG")


# Link municipalities that changed GKZ over time (2017 reform)

oldGKZ <-
  c(
    32401,
    32402,
    32405,
    32406,
    32406,
    32407,
    32409,
    32410,
    32411,
    32413,
    32417,
    32418,
    32419,
    32424,
    32404,
    32403,
    32412,
    32415,
    32416,
    32421,
    32423,
    32408
  )

newGKZ <-
  c(
    30729,
    30730,
    30731,
    30732,
    30732,
    30733,
    30734,
    30735,
    30736,
    30737,
    30738,
    30739,
    30740,
    30741,
    31235,
    31949,
    31950,
    31951,
    31952,
    31953,
    31954,
    32144
  )

x <- c(1:length(oldGKZ))

for (i in x) {
  mean_health_AL_wide$GKZ[which(mean_health_AL_wide$GKZ == oldGKZ[i])] <- newGKZ[i]
}

# compute share of ue with a health condition
mean_health_AL_wide = mean_health_AL_wide %>%
  mutate(
    ue_health_condition_by_ue_tot = health_condition_AL / (health_condition_AL + healthy_AL)
    # share of unemployed with a health condition
  )

# test for balanced panel - remaining GKZ have less than 10 observations
unbalanced = mean_health_AL_wide %>%
  group_by(GKZ) %>%
  summarize(n = n()) %>%
  filter(n < 10)


#### 3) Population - cross section ####
sub_paths_pop = c(
  "2020-12_BEVOELKERUNG_NOE/2020-12_",
  "2021-07_BEVOELKERUNG_NOE/2021-07_"
)

files_used_pop = c(
  "BEVOELKERUNG_NOE_ALTER.dsv",
  "BEVOELKERUNG_NOE_DIENSTGEBERGROESSE_UNTERNEHMEN.dsv",
  "BEVOELKERUNG_NOE_GESCHLECHT.dsv",
  "BEVOELKERUNG_NOE_INL_AUSL.dsv",
  "BEVOELKERUNG_NOE_MIGRATIONSHINTERGRUND.dsv",
  "BEVOELKERUNG_NOE_VERSORGUNGSPFLICHT.dsv"
)

# path for cross-section files not included in loop (education)
file_paths_pop = paste(data_path, sub_paths_pop, files_used_pop, sep = "")

# loop over POP cross-section variables
length(files_used)
year_vec <- 2020:2021

path_list <- list(sub_paths_pop[1] , sub_paths_pop[2])
names_list <- list( "2020" , "2021" )

months_list <- list("2020-12", "2021-07")

mean_age_POP_list <- list()
firmsize_POP_list <- list()
sex_POP_list <- list()
nationality_POP_list <- list()
mig_POP_list <- list()
care_POP_list <- list()

file_path <- list()

for( y in 1:length(path_list)) {

### age ####
file_path[[y]] = paste(data_path,  path_list[[y]], files_used_pop[1], sep = "")
  names(file_path)[y] <- names_list[[y]]

data_age_POP_tmp = read_delim(
  file_path[[y]],
  delim = ";",
  locale = locale(encoding = "latin1", decimal_mark = ",")
)

mean_age_POP_tmp <- data_age_POP_tmp %>%
  group_by(GKZ) %>%
  summarize(age_mean_POP = weighted.mean(ALTER, n)) %>% # weighted average
  mutate(month = months_list[[y]] %>% # add year or month for merging
         as.character()) # change variable type for merging

# change GKZ variable type to double to allow merging
mean_age_POP_tmp = mean_age_POP_tmp %>%
  mutate(GKZ = as.double(GKZ))

mean_age_POP_list[[y]] <-
  mean_age_POP_tmp
names(mean_age_POP_tmp)[y] <- names_list[[y]]



### firmsize ####
file_path[[y]] = paste(data_path,  path_list[[y]], files_used_pop[2], sep = "")
names(file_path)[y] <- names_list[[y]]

data_firmsize_POP_tmp = read_delim(
  file_path[[y]],
  delim = ";",
  locale = locale(encoding = "latin1", decimal_mark = ",")
)

# aggregate firm sizes
# > 249 employees = firmsize_large
# 10 - 249 employees = firmsize_middle
# < 10 employees = firmsize_small
# keine Angabe = firmsize_NA
data_firmsize_POP_tmp <- data_firmsize_POP_tmp %>%
rename(firmsize = DIENSTGEBERGROESSE_UNTERNEHMEN) %>%
  mutate(
    firmsize = replace(firmsize, firmsize == "> 1000 DN", "firmsize_large"),
    firmsize = replace(firmsize, firmsize == "250 bis 999 DN", "firmsize_large"),
    firmsize = replace(firmsize, firmsize == "50 bis 249 DN", "firmsize_middle"),
    firmsize = replace(firmsize, firmsize == "10 bis 49 DN", "firmsize_middle"),
    firmsize = replace(firmsize, firmsize == "bis 9 DN", "firmsize_small"),
    firmsize = replace(firmsize, firmsize == "keine Angabe", "firmsize_NA")
)

# sum rows of same category
data_firmsize_POP_tmp <- data_firmsize_POP_tmp %>%
  group_by(GKZ, firmsize) %>%
  summarize(n = sum(n))

# reshape dataset to wide
firmsize_POP_wide_tmp = data_firmsize_POP_tmp %>%
  spread(firmsize, n)

# compute rates
firmsize_POP_wide_tmp = firmsize_POP_wide_tmp %>%
  mutate(
    firmsize_large_POP_by_tot_POP = firmsize_large / (firmsize_large + firmsize_middle + firmsize_small ),
    firmsize_middle_POP_by_tot_POP = firmsize_middle / (firmsize_large + firmsize_middle + firmsize_small ),
    firmsize_small_POP_by_tot_POP = firmsize_small / (firmsize_large + firmsize_middle + firmsize_small )
  ) %>%
  mutate(month = months_list[[y]] %>% # add year or month for merging
         as.character()) %>% # change variable type for merging
  select(-firmsize_NA) # drop firmsize_NA as those are the inactive and unemployed

# change GKZ variable type to double to allow merging
firmsize_POP_wide_tmp = firmsize_POP_wide_tmp %>%
  mutate(GKZ = as.double(GKZ))

firmsize_POP_list[[y]] <-
  firmsize_POP_wide_tmp
names(firmsize_POP_list)[y] <- names_list[[y]]


### sex ####
file_path[[y]] = paste(data_path,  path_list[[y]], files_used_pop[3], sep = "")
names(file_path)[y] <- names_list[[y]]

data_sex_POP_tmp = read_delim(
  file_path[[y]],
  delim = ";",
  locale = locale(encoding = "latin1", decimal_mark = ",")
)

# prep data
sex_POP_wide_tmp = data_sex_POP_tmp %>%
  mutate(GESCHLECHT = replace(GESCHLECHT, GESCHLECHT == "F", "W")) %>% # rename F to W to avoid logical operator
  spread(GESCHLECHT, n) %>% # reshape dataset to wide
  mutate(M = replace_na(M, 0), # replace NAs with 0 to allow summarise
         W = replace_na(W, 0)) %>%
  group_by(GKZ) %>%
  summarize(M = sum(M), # sum rows of same GKZ to bring observations in same line
            W = sum(W)
  ) %>%
  mutate(
    men_POP_by_tot_POP = M / (M + W) # compute share of male population
  ) %>%
  mutate(month = months_list[[y]] %>% # add year or month for merging
         as.character()) %>% # change variable type for merging
    rename(men_POP = M,
         women_POP = W)
 
# change GKZ variable type to double to allow merging
sex_POP_wide_tmp = sex_POP_wide_tmp %>%
  mutate(GKZ = as.double(GKZ))

sex_POP_list[[y]] <-
  sex_POP_wide_tmp
names(sex_POP_list)[y] <- names_list[[y]]


### nationality ####
file_path[[y]] = paste(data_path,  path_list[[y]], files_used_pop[4], sep = "")
names(file_path)[y] <- names_list[[y]]

data_nationality_POP_tmp = read_delim(
  file_path[[y]],
  delim = ";",
  locale = locale(encoding = "latin1", decimal_mark = ",")
)

# prep data
nationality_POP_wide_tmp = data_nationality_POP_tmp %>%
  spread(INL_AUSL, n) %>% # reshape dataset to wide
  mutate(foreign_nationalitiy = replace_na(Ausländer, 0), # replace NAs with 0 to allow summarise
         AUT_nationality = replace_na(Inländer, 0)) %>%
  group_by(GKZ) %>%
  summarize(foreign_nationalitiy = sum(foreign_nationalitiy), # sum rows of same GKZ to bring observations in same line
            AUT_nationality = sum(AUT_nationality)
  ) %>%
  mutate(
    foreign_nationality_POP_by_tot_POP = foreign_nationalitiy / (foreign_nationalitiy + AUT_nationality) # compute share of foreign nationality
  ) %>%
  mutate(month = months_list[[y]] %>% # add year or month for merging
        as.character()) # change variable type for merging

# change GKZ variable type to double to allow merging
nationality_POP_wide_tmp = nationality_POP_wide_tmp %>%
  mutate(GKZ = as.double(GKZ))

nationality_POP_list[[y]] <-
  nationality_POP_wide_tmp
names(nationality_POP_list)[y] <- names_list[[y]]


### migration ####
file_path[[y]] = paste(data_path,  path_list[[y]], files_used_pop[5], sep = "")
names(file_path)[y] <- names_list[[y]]

data_mig_POP_tmp = read_delim(
  file_path[[y]],
  delim = ";",
  locale = locale(encoding = "latin1", decimal_mark = ",")
)

# aggregate migration first and second gen
data_mig_POP_tmp = data_mig_POP_tmp %>%
  mutate(
    MIGRATIONSHINTERGRUND = replace(MIGRATIONSHINTERGRUND, MIGRATIONSHINTERGRUND == "-", "no_mig_POP"),
    # "-" no migration background = no_mig
    MIGRATIONSHINTERGRUND = replace(MIGRATIONSHINTERGRUND, MIGRATIONSHINTERGRUND == "MIG1", "mig_POP"),
    # MIG1 migration background first gen = mig
    MIGRATIONSHINTERGRUND = replace(MIGRATIONSHINTERGRUND, MIGRATIONSHINTERGRUND == "MIG2", "mig_POP")
    # MIG2 migration background second gen = mig
  )

# prep data
mig_POP_wide_tmp = data_mig_POP_tmp %>%
  spread(MIGRATIONSHINTERGRUND, n) %>% # reshape dataset to wide
  mutate(mig_POP = replace_na(mig_POP, 0),
         no_mig_POP = replace_na(no_mig_POP, 0)) %>% # replace NAs with 0 to allow summarise
  group_by(GKZ) %>%
  summarize(mig_POP = sum(mig_POP), # sum rows of same GKZ to bring observations in same line
            no_mig_POP = sum(no_mig_POP)
  ) %>%
  mutate(
    mig_POP_by_tot_POP = mig_POP / (mig_POP + no_mig_POP) # compute share of male unemployed
  ) %>%
  mutate(month = months_list[[y]] %>% # add year or month for merging
         as.character()) # change variable type for merging

# change GKZ variable type to double to allow merging
mig_POP_wide_tmp = mig_POP_wide_tmp %>%
  mutate(GKZ = as.double(GKZ))

mig_POP_list[[y]] <-
  mig_POP_wide_tmp
names(mig_POP_list)[y] <- names_list[[y]]

# ### sector tbd####
# file_path = paste(file_paths_pop[10], sep = "")
# 
# data_sector_POP = read_delim(
#   file_path,
#   delim = ";",
#   locale = locale(encoding = "latin1", decimal_mark = ",")
# ) %>%
# select( -1) # drop first column X1

### care responsibility ####
# indicates whether the person has a care responsibility for a child <15 "Versorgungspflicht"
file_path[[y]] = paste(data_path,  path_list[[y]], files_used_pop[6], sep = "")
names(file_path)[y] <- names_list[[y]]

data_care_POP_tmp = read_delim(
  file_path[[y]],
  delim = ";",
  locale = locale(encoding = "latin1", decimal_mark = ",")
)

data_care_POP_tmp = data_care_POP_tmp %>%
  mutate(VERSORGUNGSPFLICHT = as.integer(VERSORGUNGSPFLICHT != "-")) # care responsibility for children <15

# prep data
care_POP_wide_tmp = data_care_POP_tmp %>%
  spread(VERSORGUNGSPFLICHT, n) %>% # reshape dataset to wide
  rename(care_POP = "1",
         no_care_POP = "<NA>") %>% # rename columns
  mutate(care_POP = replace_na(care_POP, 0),
         no_care_POP = replace_na(no_care_POP, 0)) %>% # replace NAs with 0 to allow summarise
  group_by(GKZ) %>%
  summarize(care_POP = sum(care_POP), # sum rows of same GKZ to bring observations in same line
            no_care_POP = sum(no_care_POP)
  ) %>%
  mutate(
    care_POP_by_tot_POP = care_POP / (care_POP + no_care_POP) # compute share of male unemployed
  ) %>%
  mutate(month = months_list[[y]] %>% # add year or month for merging
         as.character()) # change variable type for merging

# change GKZ variable type to double to allow merging
care_POP_wide_tmp = care_POP_wide_tmp %>%
  mutate(GKZ = as.double(GKZ))

care_POP_list[[y]] <-
  care_POP_wide_tmp
names(care_POP_list)[y] <- names_list[[y]]


}

## Bind 2019 and 2020 data into 1 file

care_POP =
  do.call(rbind, care_POP_list)

mig_POP = 
  do.call(rbind, mig_POP_list)

nationality_POP = 
  do.call(rbind, nationality_POP_list)

sex_POP =
  do.call(rbind, sex_POP_list)

firmsize_POP =
  do.call(rbind, firmsize_POP_list)

mean_age_POP =
  do.call(rbind, mean_age_POP_list)

### education #### - exclude because file contains only "keine Angabe" and no other data.
# excluded of loop because 2020 file contains only "keine Angabe" for education
#file_path = paste(file_paths_pop[5], sep = "")

#data_edu_POP = read_delim(
#  file_path,
#  delim = ";",
#  locale = locale(encoding = "latin1", decimal_mark = ",")
#)

# aggregate edu status
# ISCED 5-6 = edu_high_POP
# ISCED 3-4 = edu_middle_POP
# ISCED 1-2 = edu_low_POP
# -- = edu_NA_POP
#data_edu_POP <- data_edu_POP %>%
#  rename(education = GESCHAETZTE_AUSBILDUNG) %>%
#  mutate(
#    education = replace(education, education == "Uni/FH", "edu_high_POP"),
#    education = replace(education, education == "AHS", "edu_middle_POP"),
#    education = replace(education, education == "BHS", "edu_middle_POP"),
#    education = replace(education, education == "BMS", "edu_middle_POP"),
#    education = replace(education, education == "Lehre", "edu_middle_POP"),
#    education = replace(education, education == "Pflichtschule", "edu_low_POP"),
#    education = replace(education, education == "keine Angabe", "edu_NA_POP")
#  )

# sum rows of same category
#data_edu_POP <- data_edu_POP %>%
#  group_by(GKZ, education) %>%
#  summarize(n = sum(n))

# reshape dataset to wide
#edu_POP_wide = data_edu_POP %>%
#  spread(education, n)

# compute rates
#edu_POP_wide = edu_POP_wide %>%
#  mutate(
#    edu_high_POP_by_tot_POP = edu_high_POP / (edu_high_POP + edu_middle_POP + edu_low_POP + edu_NA_POP),
#    edu_middle_POP_by_tot_POP = edu_middle_POP / (edu_high_POP + edu_middle_POP + edu_low_POP + edu_NA_POP),
#    edu_low_POP_by_tot_POP = edu_low_POP / (edu_high_POP + edu_middle_POP + edu_low_POP + edu_NA_POP)
#  ) %>%
#  mutate(year = 2019 %>% # add year for merging
#           as.character()) # change variable type for merging

#### 4) Population - longitudinal ####

#path_pop = "Bevoelkerung/"

read_data_NOE = function(path, filename) {
  paste(data_path, filename, sep = "") %>%
    read_delim(delim = ";",
               locale = locale(encoding = "latin1", decimal_mark = ","))
}

### labor market status ####

# Pop every status - ONLY AL AVAILABLE IN DATA FOR NOW - CHECK WITH AMS

data_pop_2021 = read_data_NOE(data_path, 
                              "Bev_Uni_Status_GKZ_Stichtag.dsv")


data_pop = data_pop_2021 %>%
  mutate(STICHTAG = ymd(STICHTAG)) %>%
  rename(status = PK_E_L3 ,
         n = "SUM(XT.REEMPLOYEES)")



#Disable commands below to keep monthly rates.

# create annual averages
# # create year variable for annual rates
# data_pop$year <- data_pop$STICHTAG %>%
#   format("%Y")
# 
# # collapse by GKZ, (=Gemeinde), year, status (computing annual averages)
# pop_status_annual <- data_pop %>%
#   group_by(GKZ, year, status) %>%
#   summarize(status_n = mean(n))

# create monthly averages
data_pop$month <- data_pop$STICHTAG %>%
  format("%Y-%m") 

data_pop <- data_pop %>%
  select("GKZ", "month", "status", "n")



# aggregate status

pop_status_summed <- data_pop %>%
  mutate(
    status_fine = status,
    status = replace(status, status == "AQ", "AM"),
    # AMS Qualifikation = AMS Vormerkung
    status = replace(status, status == "AO", "AM"),
    # Arbeitslos laut HV = AMS Vormerkung
    status = replace(status, status == "AS", "AM"),
    # sonstige AMS Vormerkung = AMS Vormerkung
    status = replace(status, status == "AL", "AM"),
    # Arbeitslos = AMS Vormerkung
    status = replace(status, status == "NU", "BE"),
    # Nicht gefÃ¶rderte UnselbststÃ¤ndige = BeschÃ¤ftigte
    status = replace(status, status == "GU", "BE"),
    # GefÃ¶rderte UnselbststÃ¤ndige = BeschÃ¤ftigte
    status = replace(status, status == "SB", "BE"),
    # SelbststÃ¤nidge = BeschÃ¤ftigte
    status = replace(status, status == "GE", "SO"),
    # Gesichert Erwerbsfern = Sonstige
    status = replace(status, status == "GB", "BE"),
    # GeringfÃ¼gige BeschÃ¤ftigung = BeschÃ¤ftigung
    status = replace(status, status == "SE", "SO"),
    # Sonstig erwerbsfern = Sonstige
    status = replace(status, status == "UN", "SO") 
    # Tod bzw. keine Daten = Sonstige
  )

# sum rows of same category
pop_status_summed <- pop_status_summed %>%
  group_by(GKZ, month, status) %>%
  summarize(n = sum(n))

# reshape dataset to wide
pop_status_broad_wide = pop_status_summed %>%
  spread(status, n)


# Added later after Talk to AMS Jan 2022
# add AL as a narrower definition of unemployment to dataset 
# (in addition to the broader definition AM)
pop_status_fine_summed <- data_pop %>%
  group_by(GKZ, month, status) %>%
  summarize(n = sum(n))

# reshape dataset to wide
pop_status_fine_wide = pop_status_fine_summed %>%
  spread(status, n)

# merge dataset with narrower status definition with dataset with broader status definition
pop_status_wide =
  pop_status_broad_wide %>%  # long & short term UE longitudinal LM status longitudinal
  group_by(GKZ, month) %>%
  left_join(pop_status_fine_wide, by = c("GKZ", "month")) 



# no municipalities have NAs for AM & BE
# replace NA with 0 to create a balanced panel
pop_status_wide = pop_status_wide %>%
  mutate(AM = replace_na(AM, 0),
         BE = replace_na(BE, 0),
         AL = replace_na(AL, 0))

# sum up to total population workingage
pop_status_wide <- pop_status_wide %>%
  mutate(POP_workingage = AM + BE + SO)

# compute rates
pop_status_wide = pop_status_wide %>%
  mutate(
    UE_rate_U3 = AM / (BE + AM),
    UE_rate_tot = AM / (BE + AM + SO),
    EMP_rate_tot = BE / (BE + AM + SO),
    Inactive_rate_tot = SO / (BE + AM + SO),
    UE_rate_tot_AMS_definition = AL / (BE + AM + SO)
  )

# replace NaN with 0 in few municipalities with 0 AM & 0 BE for balanced panel
pop_status_wide = pop_status_wide %>%
  mutate(UE_rate_U3 = replace_na(UE_rate_U3, 0),)

# test for balanced panel - remaining GKZ have less than 5 observations
unbalanced = pop_status_wide %>%
  group_by(GKZ) %>%
  summarize(n = n()) %>%
  filter(n < 10)


#### 5) Wages - cross section  ####
read_data_NOE = function(path, filename) {
  paste(path, filename, sep = "") %>%
    read_delim(delim = ";",
               locale = locale(encoding = "latin1", decimal_mark = ","))
}

sub_path_wage = "UB_AVG_BMG/"

file_paths_wage = paste(data_path, sub_path_wage, sep="")

data_wage_POP = read_data_NOE(file_paths_wage, 
                              "UB_AVG_BMG_GKZ.dsv")

data_wage_POP = data_wage_POP %>%
  rename(wage_mean = DS_BEITRAGSGRUNDLAGE_INKL_SZ,
         n = BESTAND)


data_wage_POP <- data_wage_POP %>%
  mutate(STICHTAG = ymd(STICHTAG))

# # create annual averages
# data_wage_POP$year <- data_wage_POP$STICHTAG %>%
#   format("%Y")
# 
# # create 2 summary variables: 1: mean wage for 2020; 2: mean wage for first half of 2021
# mean_wage_POP <- data_wage_POP %>%
#   group_by(GKZ, year) %>%
#   summarize(mean_wage = weighted.mean(wage_mean, n)) %>% # weighted average
#   mutate(GKZ = as.numeric(GKZ))

# create monthly averages
data_wage_POP$month <- data_wage_POP$STICHTAG %>%
  format("%Y-%m") 

mean_wage_POP <- data_wage_POP %>%
  select("GKZ", "month", "wage_mean")


### 6) Communal tax ####
#file_path = paste(data_path, "20-597_STATcube_Kommunalsteuer_NOE-Gem_j00-19.csv", sep = "")

#data_communal_tax = read_csv(file_path)

# reshape dataset to long
#data_tax_long = data_communal_tax %>%
#  gather(key = year, value = communal_tax, -GEMEINDE)

# seperate GEMEINDE & GKZ
#data_tax_long_sep = data_tax_long %>%
#  separate(GEMEINDE, sep="<", into=c("GEMEINDE", "GKZ"))

# remove >
#data_tax_long_sep = data_tax_long_sep %>%
#  separate(GKZ, sep=">", into=c("GKZ", "GKZ2")) %>%
#     select(-"GKZ2", - "GEMEINDE") %>%
 #       mutate(GKZ = as.numeric(GKZ),
#               communal_tax = as.numeric(communal_tax) # change variable type for merging
#               ) 

#### 7) Merge ####
#align GKZ variable type from character to double

mean_LZBL_AL <- mean_LZBL_AL %>%
  mutate(GKZ = as.double(GKZ))

Ubenefit_AL <- Ubenefit_AL %>%
  mutate(GKZ = as.double(GKZ))

mean_health_AL_wide <- mean_health_AL_wide %>%
  mutate(GKZ = as.double(GKZ))

mean_wage_POP <- mean_wage_POP %>%
  mutate(GKZ = as.double(GKZ))

pop_status_wide <- pop_status_wide %>%
  mutate(GKZ = as.double(GKZ))


municipalities =
  mean_LZBL_AL %>%  # long & short term UE longitudinal LM status longitudinal
  group_by(GKZ, month) %>%
  left_join(pop_status_wide, by = c("GKZ", "month")) %>% # LM status longitudinal
  left_join(mean_age_AL, by = c("GKZ", "month")) %>% # age AL cross-section
  left_join(edu_AL, by = c("GKZ", "month")) %>% # edu AL cross-section
  left_join(german_AL, by = c("GKZ", "month")) %>% # German AL cross-section
  left_join(sex_AL, by = c("GKZ", "month")) %>% # sex AL cross-section
  left_join(mig_AL, by = c("GKZ", "month")) %>% # migration background AL cross-section
  left_join(Ubenefit_AL, by = c("GKZ", "month")) %>% # unemployment benefit AL cross-section
  left_join(mean_health_AL_wide, by = c("GKZ", "month")) %>%  # health AL longitudinal
  left_join(mean_age_POP, by = c("GKZ", "month")) %>%  # age POP cross-section  
  left_join(firmsize_POP, by = c("GKZ", "month")) %>%  # firmsize POP cross-section
#  left_join(edu_POP_wide, by = c("GKZ", "year")) %>%  # edu POP cross-section - excluded because 2020 files contain only NAs
  left_join(sex_POP, by = c("GKZ", "month")) %>%  # sex POP cross-section    
  left_join(nationality_POP, by = c("GKZ", "month")) %>%  # nationality POP cross-section   
  left_join(mig_POP, by = c("GKZ", "month")) %>%  # migration background POP cross-section   
  left_join(care_POP, by = c("GKZ", "month")) %>%  # care responsibility POP cross-section  
  left_join(mean_wage_POP, by = c("GKZ", "month")) # mean wage cross-section for 2019 & 2020
#  left_join(data_tax_long_sep, by = c("GKZ", "year")) # communal tax

  
  
# drop Pop without GKZ (GKZ = 0)
municipalities <- municipalities %>%
  filter(GKZ != 0)

# compute rates for long & short UE
#municipalities = municipalities %>%
#  mutate(ue_long_by_pop = ue_long / (BE + AM + SO),
#         ue_short_by_pop = ue_short / (BE + AM + SO),
#         communal_tax_by_pop = communal_tax / POP_workingage)


# export
 data_out = "A:/jobguarantee/2021-09-municipal-data-processed/" # Lukas
# data_out = "/Volumes/MARIENTHAL/jobguarantee/2021-09-municipal-data-processed/" # Max

municipalities %>%
  write_csv(paste(data_out,
                  "municipalities_merged_monthly_outcomes.csv",
                  sep = ""))
