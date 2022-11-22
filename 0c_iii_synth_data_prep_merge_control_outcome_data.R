###### Data preparation for synthetic control
design_subpath = "jobguarantee/2020-09-municipal-data-processed/"
design_path = paste(veracrypt_path, design_subpath, sep = "")

outcomes_subpath = "jobguarantee/2021-09-municipal-data-processed/"
outcomes_path = paste(veracrypt_path, outcomes_subpath, sep = "")


municipalities_design =
  paste0(design_path, "municipalities_merged_monthly_design.csv") %>%
  read_csv(    
    col_types = paste(c("icc", rep("n", 77)), collapse = '') )

# , encoding = "UTF-8"
# ,locale = locale(encoding = "latin1", decimal_mark = ",")

municipalities_outcomes =
  paste0(outcomes_path, "municipalities_merged_monthly_outcomes.csv") %>%
  read_csv(
    col_types = paste(c("innc", rep("n", 52)), collapse = '') )

# Merge
# municipalities_2 =
#   municipalities_design %>%
#   group_by(GKZ, month) %>%
#   left_join(municipalities_outcomes, by = c("GKZ", "month"))

municipalities = bind_rows(municipalities_design, municipalities_outcomes)  %>%
     group_by(GKZ, month) %>%
      arrange(GKZ, month)


# complete missing municipality names
municipalities <- municipalities %>%
  group_by(GKZ) %>%
  fill(GEMEINDE, .direction = "downup") %>%
  ungroup()

# filter since 2018
municipalities <- municipalities %>% 
  filter(month>=2018)

# change month variable from character to date
# municipalities <- municipalities %>%
#   mutate(month = ym(month))

municipalities <- municipalities %>%
  filter(GEMEINDE != 0)

# Export
municipalities %>%
  write_csv(paste(outcomes_path,
                  "municipalities_merged_monthly.csv",
                  sep = ""))


