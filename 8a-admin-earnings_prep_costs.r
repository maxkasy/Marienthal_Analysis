# Extract earnings data from AMDB and prepare for analysis ----
# for contrast 1), contrast to 3), and 4) cost comparison for Marienthal Job Guarantee Paper
# 2025-10 for AEJ:Policy revisions
# set wd and veracrypt path first in master.R first before runnig this file
# data ends with 2024-02-01

# Load packages ----
library(data.table)
library(dplyr)
library(lubridate)
library(tidyr)
library(slider) # to calculate moving averages

# Settings ----

# Define paths
data_path <- paste0(veracrypt_path, "jobguarantee/2025-10-admin-earnings-raw/")
data_out <- paste0(veracrypt_path, "jobguarantee/2025-10-admin-earnings-processed/")


startdate <- "2020-09-30"  
enddate <- "2024-03-31"    

# Define start date of the program for calculation of monthly averages
program_start <- ymd("2020-10-01")

## VM PART ----
# Run this part on the VM, otherwise the GB RAM is too small to open hv_mbgm_bas.csv, 
# which as around 48 GB and takes around 70 GB when loaded in R Studio

# Load data ----
# data_hv_mbgm_bas_raw <- fread("hv_mbgm_bas.csv")
data_hv_mbgm_bas_raw <- fread("~/Documents/hv_mbgm_bas.csv")

# dataframe with identifiers (small)
ids_magma <- fread("penr_magma.csv")   # or data.frame with one column: "id"

# Extract data from AMDB ----
# ensure column names match (assuming "id" is the key variable)
setkey(data_hv_mbgm_bas_raw, PENR)
setkey(ids_magma, PENR)

# efficient join to extract matching rows
data_earnings_raw <- data_hv_mbgm_bas_raw[ids_magma, nomatch = 0]

# optional: write out to disk
fwrite(data_earnings_raw, "data_earnings_raw.csv")

## LOCAL PART ----

# Load data ----
# includes all BMG incl. from pensions and benefits

# take from local:
# data_income_raw <- fread("data_earnings_raw.csv")   # or data.frame with one column: "id"
# take from Veracrypt:
 data_income_raw <- fread(paste0(data_path, "data_earnings_raw.csv"))

# new column names
new_names <- c(
  "JAHR",          # Jahr der Beitragsgrundlage
  "MONAT",         # Monat in welchem die Beitragsgrundlage gilt
  "BTAG",          # Beitragstage (zuordenbar)
  "PENR",          # Personennummer
  "BENR",          # HV Dienstgeberkontonummer
  "BMG",           # Beitragsgrundlage (in Cent)
  "WAE",           # Währung (Cent)
  "TETG",          # Teilentgelttage (Anzahl Tage im Monat)
  "EF",            # Echtes Konto (E), oder fingiertes Konto (F)
  "BG_STF",        # Art der Beitragsgrundlage; Steuerfeld
  "BG_ART",        # Kennzeichen der Art der Beitragsgrundlage
  "BG_ZEITENDECKUNG", # Zeitendeckung mit Versicherungszeiten (J/N)
  "BG_EXOFFO",     # Amtlich festgelegt
  "BG_UEBER",      # HBG zulässig / nicht zulässig
  "BG_REST"        # optional: last column if needed
)

data_income_raw <- data_income_raw %>%
  rename_with(~ new_names, .cols = 1:15)

# Filter periods earnings ----
data_income <- data_income_raw %>%
  mutate(
    date = make_date(year = JAHR, month = MONAT, day = 1)
  ) %>%
  filter(date >= startdate, date <= enddate)

## Balanced panel
# define full month sequence
all_months <- seq(
  floor_date(min(data_income$date), "month"),
  floor_date(max(data_income$date), "month"),
  by = "month"
)

# create balanced panel: each PENR has all months
data_income_balanced <- data_income %>%
  mutate(month = floor_date(date, "month")) %>%
  distinct(PENR, treat, GKZ, PSTNR) %>%
  crossing(month = all_months) %>%
  left_join(
    data_income %>%
      mutate(month = floor_date(date, "month")),
    by = c("PENR", "treat", "GKZ", "PSTNR", "month")
  ) %>%
  mutate(
    BMG = coalesce(BMG, 0),
    BTAG = coalesce(BTAG, 0)
  )


## INCOME ----
# Prepare earnings variables ----

# Compute cumulative income at annual dates ----
income_cumulative <- data_income_balanced %>%
  mutate(
    BMG_eur = BMG / 100,  # convert cents to euros
    days_in_m = days_in_month(date),
    BMG_adj = BMG_eur * (BTAG / days_in_m)  # adjust for true month length
  ) %>%
  group_by(PENR) %>%
  summarise(
    income_cum_feb2021 = sum(BMG_adj[date <= ymd("2021-02-28")], na.rm = TRUE),
    income_cum_feb2022 = sum(BMG_adj[date <= ymd("2022-02-28")], na.rm = TRUE),
    income_cum_feb2023 = sum(BMG_adj[date <= ymd("2023-02-28")], na.rm = TRUE),
    income_cum_feb2024 = sum(BMG_adj[date <= ymd("2024-02-29")], na.rm = TRUE),
    income_cum_march2024 = sum(BMG_adj[date <= ymd("2024-03-31")], na.rm = TRUE),
    #   earnings_cum_march2024 = sum(BMG_adj[date <= ymd("2024-03-31")], na.rm = TRUE),
    # average monthly earnings (divide by months since program start)
    income_avg_feb2021 = income_cum_feb2021 / interval(program_start, ymd("2021-02-28")) %/% months(1),
    income_avg_feb2022 = income_cum_feb2022 / interval(program_start, ymd("2022-02-28")) %/% months(1),
    income_avg_feb2023 = income_cum_feb2023 / interval(program_start, ymd("2023-02-28")) %/% months(1),
    income_avg_feb2024 = income_cum_feb2024 / interval(program_start, ymd("2024-03-31")) %/% months(1),
    #   earnings_avg_march2024 = earnings_cum_march2024 / interval(program_start, ymd("2024-01-31")) %/% months(1),
    treat = first(treat),
    GKZ = first(GKZ),
    PSTNR = first(PSTNR)
  ) %>%
  ungroup()

# check nr of unique persons with earnings:
income_cumulative %>%
  count(treat)
# 60 in treatment group (out of 62)
# 210 in control group

# Compute earnings at specific dates ----

income_feb_wide <- data_income_balanced %>%
  mutate(
    BMG_eur = BMG / 100,  # convert cents to euros
    # adjust BMG proportionally for BTAG (Beitragstage)
    days_in_m = days_in_month(date),
    BMG_adj = BMG_eur * (BTAG / days_in_m)  # adjust for true month length
  ) %>%
  filter(month(date) == 2 & year(date) %in% 2021:2024) %>%
  group_by(PENR, year = year(date), treat, GKZ, PSTNR) %>%
  summarise(
    BMG_feb = sum(BMG_adj, na.rm = TRUE),   # sum across multiple spells in Feb
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = year,
    values_from = BMG_feb,
    names_prefix = "income_feb_"
  )

## Merge cumulative and february earnings

income_merged <- full_join(
  income_cumulative,
  income_feb_wide,
  by = c("PENR", "treat", "GKZ", "PSTNR")
)

# order columns
income_merged <- income_merged %>%
  select(PENR, treat, GKZ, PSTNR, everything()) %>%
  arrange(desc(treat == 1), treat)

# export
# fwrite(income_merged, "magma_admin_income.csv")

fwrite(
  income_merged,
  file = file.path(data_out, "magma_admin_income.csv")
)

# compare group means ----

income_summary <- income_merged %>%
  group_by(treat) %>%
  summarise(
    across(
      starts_with("income_"),      # all cumulative earnings columns
      mean,
      na.rm = TRUE
    )
  ) %>%
  ungroup()

# export
# fwrite(income_summary, "magma_admin_income_summary.csv")

fwrite(
  income_summary,
  file = file.path(data_out, "magma_admin_income_summary.csv")
)

# income includes unemployment benefits and pensions
# keep only earnings in next step
## EARNINGS ----
# Prepare earnings variables ----

## explore Beitragsarten
# get all unique categories
data_income_balanced %>%
  distinct(BG_STF)

## keep only earnings and drop benefits and pension
data_earnings <- data_income_balanced %>%
  mutate(
    is_earnings = BG_STF %in% c("AB", # Allgemeine Beitragsgrundlage
                       "02", # GSVG-/FSVG-Pflichtversicherung 
                       "1M",
                       "41",
                       "04",
                       "40",
                       "03",
                       "1K",
                       "TE",
                       "KG",
                       "90",
                       "91",
                       "WG",
                       "42",
                       "35",
                       "GB", # Geringfügige Beschäftigung
                       "GC",
                       "38",
                       "WI",
                       "SZ", # Sonderzahlung
                       "BZ", # Sonderzahlung public sector
                       "GY", # Sonderzahlung geringfügige Beschäftigung
                       "GZ", # Sonderzahlung geringfügige Beschäftigung
                       "KI", # Krankenversicherung bei Erwerbstätigkeit
                       "55" # Pflichtversicherung Werkvertrag
                       )) 

# Compute cumulative earnings at annual dates ----
earnings_cumulative <- data_earnings %>%
  mutate(
    BMG_eur = BMG / 100,  # convert cents to euros
    days_in_m = days_in_month(date),
    BMG_adj = if_else(is_earnings, BMG_eur * (BTAG / days_in_m), 0) # calculate BMG per month for earnings, otherwise 0 (not to lose observations)
  ) %>%
  group_by(PENR) %>%
  summarise(
    earnings_cum_feb2021 = sum(BMG_adj[date <= ymd("2021-02-28")], na.rm = TRUE),
    earnings_cum_feb2022 = sum(BMG_adj[date <= ymd("2022-02-28")], na.rm = TRUE),
    earnings_cum_feb2023 = sum(BMG_adj[date <= ymd("2023-02-28")], na.rm = TRUE),
    earnings_cum_feb2024 = sum(BMG_adj[date <= ymd("2024-03-31")], na.rm = TRUE),
 #   earnings_cum_march2024 = sum(BMG_adj[date <= ymd("2024-03-31")], na.rm = TRUE),
    # average monthly earnings (divide by months since program start)
    earnings_avg_feb2021 = earnings_cum_feb2021 / interval(program_start, ymd("2021-02-28")) %/% months(1),
    earnings_avg_feb2022 = earnings_cum_feb2022 / interval(program_start, ymd("2022-02-28")) %/% months(1),
    earnings_avg_feb2023 = earnings_cum_feb2023 / interval(program_start, ymd("2023-02-28")) %/% months(1),
    earnings_avg_feb2024 = earnings_cum_feb2024 / interval(program_start, ymd("2024-03-31")) %/% months(1),
 #   earnings_avg_march2024 = earnings_cum_march2024 / interval(program_start, ymd("2024-01-31")) %/% months(1),
    # other identifiers
    treat = first(treat),
    GKZ = first(GKZ),
    PSTNR = first(PSTNR)
  ) %>%
  ungroup()

# check nr of unique persons with earnings:
earnings_cumulative %>%
  count(treat)
# 60 in treatment group (out of 62)
# 210 in control group

# Compute earnings at specific dates ----

earnings_feb_wide <- data_earnings %>%
  mutate(
    BMG_eur = BMG / 100,  # convert cents to euros
    # adjust BMG proportionally for BTAG (Beitragstage)
    days_in_m = days_in_month(date),
    BMG_adj = if_else(is_earnings, BMG_eur * (BTAG / days_in_m), 0) # calculate BMG per month for earnings, otherwise 0 (not to lose observations)
  ) %>%
  filter(month(date) == 2 & year(date) %in% 2021:2024) %>%
  group_by(PENR, year = year(date), treat, GKZ, PSTNR) %>%
  summarise(
    BMG_feb = sum(BMG_adj, na.rm = TRUE),   # sum across multiple spells in Feb
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = year,
    values_from = BMG_feb,
    names_prefix = "earnings_feb_"
  )

## Merge cumulative and february earnings

earnings_merged <- full_join(
  earnings_cumulative,
  earnings_feb_wide,
  by = c("PENR", "treat", "GKZ", "PSTNR")
)

# order columns
earnings_merged <- earnings_merged %>%
  select(PENR, treat, GKZ, PSTNR, everything()) %>%
  arrange(desc(treat == 1), treat)

# export
# fwrite(earnings_merged, "magma_admin_earnings.csv")

fwrite(
  earnings_merged,
  file = file.path(data_out, "magma_admin_earnings.csv")
)

# compare group means ----

earnings_summary <- earnings_merged %>%
  group_by(treat) %>%
  summarise(
    across(
      starts_with("earnings_"),      # all cumulative earnings columns
      mean,
      na.rm = TRUE
    )
  ) %>%
  ungroup()

# export
# fwrite(earnings_summary, "magma_admin_earnings_summary.csv")

fwrite(
  earnings_summary,
  file = file.path(data_out, "magma_admin_earnings_summary.csv")
)


## NORMALISE ----
# Merge income_merged with earnings_merged

data_merged <- full_join(
  income_merged,
  earnings_merged,
  by = c("PENR", "treat", "GKZ", "PSTNR")
)

### Normalise admin outcomes ---------------------------------

## Normalise continuous admin outcomes to 0–1 scale (excluding identifiers)
# data_normalized <- data_merged %>%
#   mutate(across(
#     .cols = -c(PENR, treat, GKZ, PSTNR),
#     .fns = ~ (. - min(., na.rm = TRUE)) / (max(., na.rm = TRUE) - min(., na.rm = TRUE))
#   ))

# divide all variables by 2000 (to be consistent with survey income variable scaled 0-1)
# and by 100 to bring amounts from cents to euros
data_normalized <- data_merged %>%
  mutate(across(
    .cols = -c(PENR, treat, GKZ, PSTNR),
    .fns = ~ ./2000
  ))


# write in 2025-10-admin-earnings-processed as MAGMA-Admin-Income-Normalized.csv
fwrite(
  data_normalized,
  file = file.path(data_out, "magma_admin_income_earnings_normalized.csv")
)

## write MAGMA and Control towns separately
data_normalized_magma <- data_normalized %>%
  filter(treat == 1)

data_normalized_control <- data_normalized %>%
  filter(treat == 0)

fwrite(
  data_normalized_magma,
  file = file.path(data_out, "magma_admin_income_earnings_normalized_magma.csv")
)

fwrite(
  data_normalized_control,
  file = file.path(data_out, "magma_admin_income_earnings_normalized_control.csv")
)




## MONTHLY SERIES ----

data_income_monthly <- data_income_raw %>%
  mutate(
    date = make_date(year = JAHR, month = MONAT, day = 1)
  )

## Balanced panel
# define full month sequence
all_months <- seq(
  floor_date(min(data_income_monthly$date), "month"),
  floor_date(max(data_income_monthly$date), "month"),
  by = "month"
)

# create balanced panel: each PENR has all months
data_income_monthly_balanced <- data_income_monthly %>%
  mutate(month = floor_date(date, "month")) %>%
  distinct(PENR, treat, GKZ, PSTNR) %>%
  crossing(month = all_months) %>%
  left_join(
    data_income_monthly %>%
      mutate(month = floor_date(date, "month")),
    by = c("PENR", "treat", "GKZ", "PSTNR", "month")
  ) %>%
  mutate(
    BMG = coalesce(BMG, 0),
    BTAG = coalesce(BTAG, 0)
  )

# Income monthly ----
# compute average monthly earnings (for each month separately)
income_monthly <- data_income_monthly_balanced %>%
  mutate(
    BMG_eur = BMG / 100,  # convert cents to euros
    days_in_m = days_in_month(date),
    BMG_adj = BMG_eur * (BTAG / days_in_m)
  ) %>%
  group_by(PENR, month = floor_date(date, "month")) %>%
  summarise(
    income_month_person = sum(BMG_adj, na.rm = TRUE),  # sum across jobs/spells
    treat = first(treat),
    GKZ = first(GKZ),
    PSTNR = first(PSTNR),
    .groups = "drop"
  ) %>%
  group_by(treat, month) %>%
  summarise(
    income_avg_month = mean(income_month_person, na.rm = TRUE),  # average across persons
    .groups = "drop"
  )


# plot line figure
p <- ggplot(income_monthly, aes(x = month)) +
  # Control Towns (bottom layer)
  geom_line(
    data = filter(income_monthly, treat == 0),
    aes(y = income_avg_month, color = "Control Towns", linetype = "Control Towns"),
    linewidth = 1.2
  ) +
  # Gramatneusiedl (top layer)
  geom_line(
    data = filter(income_monthly, treat == 1),
    aes(y = income_avg_month, color = "Gramatneusiedl", linetype = "Gramatneusiedl"),
    linewidth = 1.2
  ) +
  # vertical lines
  geom_vline(
    xintercept = as.Date(c("2020-10-01", "2021-02-01", "2024-04-01")),
    linetype = "dashed",
    color = "grey70",
    linewidth = 0.7
  ) +
  # annotations
  annotate("text",
           x = as.Date("2020-10-01"), y = Inf, label = "Wave 1 start",
           color = "grey40", size = 3, angle = 90, vjust = -0.5, hjust = 1
  ) +
  annotate("text",
           x = as.Date("2021-02-01"), y = Inf, label = "Wave 2 start",
           color = "grey40", size = 3, angle = 90, vjust = -0.5, hjust = 1
  ) +
  annotate("text",
           x = as.Date("2024-04-01"), y = Inf, label = "Program end",
           color = "grey40", size = 3, angle = 90, vjust = -0.5, hjust = 1
  ) +
#  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
  scale_x_date(date_breaks = "12 months", date_labels = "%Y") +
  scale_y_continuous(labels = scales::comma) +
  scale_color_manual(
    values = c("Gramatneusiedl" = "firebrick", "Control Towns" = "grey65"),
    breaks = c("Gramatneusiedl", "Control Towns")
  ) +
  scale_linetype_manual(
    values = c("Gramatneusiedl" = "solid", "Control Towns" = "dashed"),
    breaks = c("Gramatneusiedl", "Control Towns")
  ) +
  labs(
    title = "Average monthly income",
    x = "", y = "Euros",
    color = "", linetype = ""
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 1, size = 6.5),
    axis.title.y = element_text(size = 11, margin = margin(r = 10)),
    legend.position = "top",
    legend.text = element_text(size = 10),
    plot.title = element_text(face = "bold", size = 12)
  )

p

ggsave(
  filename = "Figures/admin_bmg_monthly_line_plot.pdf",
  plot = p,
  width = 10,
  height = 6,
  dpi = 300,
  device = cairo_pdf
)

# Earnings monthly, mean ----
## keep only earnings and drop benefits and pension
data_earnings_monthly <- data_income_monthly_balanced %>%
  mutate(
    is_earnings = BG_STF %in% c("AB", # Allgemeine Beitragsgrundlage
                        "02", # GSVG-/FSVG-Pflichtversicherung 
                       # "1M",
                       # "41",
                       # "04",
                       # "40",
                       # "03",
                       # "1K",
                       # "TE",
                       # "KG",
                       # "90",
                       # "91",
                       # "WG",
                       # "42",
                       # "35",
                        "GB", # Geringfügige Beschäftigung
                       # "GC",
                       # "38",
                       # "WI",
                       # "SZ", # Sonderzahlung
                       # "BZ", # Sonderzahlung public sector
                       # "GY", # Sonderzahlung geringfügige Beschäftigung
                       # "GZ", # Sonderzahlung geringfügige Beschäftigung
                       # "KI", # Krankenversicherung bei Erwerbstätigkeit
                       "55" # Pflichtversicherung Werkvertrag
  )) 


# # how many unique PENR are in data_income_monthly by treat?
# data_income_monthly %>%
#   distinct(PENR, treat) %>%
#   count(treat)

# compute average monthly earnings (for each month separately)
earnings_monthly <- data_earnings_monthly %>%
  mutate(
    BMG_eur = BMG / 100,  # convert cents to euros
    days_in_m = days_in_month(date),
    BMG_adj = if_else(is_earnings, BMG_eur * (BTAG / days_in_m), 0) # calculate BMG per month for earnings, otherwise 0 (not to lose observations)
  ) %>%
  group_by(PENR, month = floor_date(date, "month")) %>%
  summarise(
    earnings_month_person = sum(BMG_adj, na.rm = TRUE),  # sum across jobs/spells
    treat = first(treat),
    GKZ = first(GKZ),
    PSTNR = first(PSTNR),
    .groups = "drop"
  ) %>%
  group_by(treat, month) %>%
  summarise(
    earnings_avg_month = mean(earnings_month_person, na.rm = TRUE),  # average across persons
    earnings_median_month = median(earnings_month_person, na.rm = TRUE),  # median across persons
    .groups = "drop"
  ) %>%
  arrange(treat, month) %>%
  group_by(treat) %>%
  mutate(
    earnings_avg_month_ma3 = slide_dbl(
      earnings_avg_month,
      mean,
      .before = 1, .after = 1,
      .complete = TRUE
    )
  ) %>%
  ungroup()

# Mean earnings ----
# plot line figure
p <- ggplot(earnings_monthly, aes(x = month)) +
  # Control Towns (bottom layer)
  geom_line(
    data = filter(earnings_monthly, treat == 0),
    aes(y = earnings_avg_month, color = "Control Towns", linetype = "Control Towns"),
    linewidth = 1.2
  ) +
  # Gramatneusiedl (top layer)
  geom_line(
    data = filter(earnings_monthly, treat == 1),
    aes(y = earnings_avg_month, color = "Gramatneusiedl", linetype = "Gramatneusiedl"),
    linewidth = 1.2
  ) +
  # vertical lines
  geom_vline(
    xintercept = as.Date(c("2020-10-01", "2021-02-01", "2024-03-31")),
    linetype = "dashed",
    color = "grey70",
    linewidth = 0.7
  ) +
  # annotations
  annotate("text",
           x = as.Date("2020-10-01"), y = Inf, label = "Wave 1 start",
           color = "grey40", size = 3, angle = 90, vjust = -0.5, hjust = 1
  ) +
  annotate("text",
           x = as.Date("2021-02-01"), y = Inf, label = "Wave 2 start",
           color = "grey40", size = 3, angle = 90, vjust = -0.5, hjust = 1
  ) +
  annotate("text",
           x = as.Date("2024-03-31"), y = Inf, label = "Program end",
           color = "grey40", size = 3, angle = 90, vjust = -0.5, hjust = 1
  ) +
  #  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
  scale_x_date(date_breaks = "12 months", date_labels = "%Y") +
  scale_y_continuous(labels = scales::comma) +
  scale_color_manual(
    values = c("Gramatneusiedl" = "firebrick", "Control Towns" = "grey65"),
    breaks = c("Gramatneusiedl", "Control Towns")
  ) +
  scale_linetype_manual(
    values = c("Gramatneusiedl" = "solid", "Control Towns" = "dashed"),
    breaks = c("Gramatneusiedl", "Control Towns")
  ) +
  labs(
    title = "Average gross monthly earnings",
    x = "", y = "Euros",
    color = "", linetype = ""
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 1, size = 6.5),
    axis.title.y = element_text(size = 11, margin = margin(r = 10)),
    legend.position = "top",
    legend.text = element_text(size = 10),
    plot.title = element_text(face = "bold", size = 12)
  )

p

ggsave(
  filename = "Figures/admin_earnings_monthly_line_plot_only_regular_earnings.pdf",
  plot = p,
  width = 10,
  height = 6,
  dpi = 300,
  device = cairo_pdf
)


# Moving average, mean earnings ----
# plot line figure
p <- ggplot(earnings_monthly, aes(x = month)) +
  # Control Towns (bottom layer)
  geom_line(
    data = filter(earnings_monthly, treat == 0),
    aes(y = earnings_avg_month_ma3, color = "Control Towns", linetype = "Control Towns"),
    linewidth = 1.2
  ) +
  # Gramatneusiedl (top layer)
  geom_line(
    data = filter(earnings_monthly, treat == 1),
    aes(y = earnings_avg_month_ma3, color = "Gramatneusiedl", linetype = "Gramatneusiedl"),
    linewidth = 1.2
  ) +
  # vertical lines
  geom_vline(
    xintercept = as.Date(c("2020-10-01", "2021-02-01", "2024-04-01")),
    linetype = "dashed",
    color = "grey70",
    linewidth = 0.7
  ) +
  # annotations
  annotate("text",
           x = as.Date("2020-10-01"), y = Inf, label = "Wave 1 start",
           color = "grey40", size = 3, angle = 90, vjust = -0.5, hjust = 1
  ) +
  annotate("text",
           x = as.Date("2021-02-01"), y = Inf, label = "Wave 2 start",
           color = "grey40", size = 3, angle = 90, vjust = -0.5, hjust = 1
  ) +
  annotate("text",
           x = as.Date("2024-04-01"), y = Inf, label = "Program end",
           color = "grey40", size = 3, angle = 90, vjust = -0.5, hjust = 1
  ) +
  #  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
  scale_x_date(date_breaks = "12 months", date_labels = "%Y") +
  scale_y_continuous(labels = scales::comma) +
  scale_color_manual(
    values = c("Gramatneusiedl" = "firebrick", "Control Towns" = "grey65"),
    breaks = c("Gramatneusiedl", "Control Towns")
  ) +
  scale_linetype_manual(
    values = c("Gramatneusiedl" = "solid", "Control Towns" = "dashed"),
    breaks = c("Gramatneusiedl", "Control Towns")
  ) +
  labs(
    title = "Average gross monthly earnings, 3 month moving average",
    x = "", y = "Euros",
    color = "", linetype = ""
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 1, size = 6.5),
    axis.title.y = element_text(size = 11, margin = margin(r = 10)),
    legend.position = "top",
    legend.text = element_text(size = 10),
    plot.title = element_text(face = "bold", size = 12)
  )

p

ggsave(
  filename = "Figures/admin_earnings_monthly_3ma_line_plot.pdf",
  plot = p,
  width = 10,
  height = 6,
  dpi = 300,
  device = cairo_pdf
)


# Median earnings ----
# plot line figure
p <- ggplot(earnings_monthly, aes(x = month)) +
  # Control Towns (bottom layer)
  geom_line(
    data = filter(earnings_monthly, treat == 0),
    aes(y = earnings_median_month, color = "Control Towns", linetype = "Control Towns"),
    linewidth = 1.2
  ) +
  # Gramatneusiedl (top layer)
  geom_line(
    data = filter(earnings_monthly, treat == 1),
    aes(y = earnings_median_month, color = "Gramatneusiedl", linetype = "Gramatneusiedl"),
    linewidth = 1.2
  ) +
  # vertical lines
  geom_vline(
    xintercept = as.Date(c("2020-10-01", "2021-02-01", "2024-04-01")),
    linetype = "dashed",
    color = "grey70",
    linewidth = 0.7
  ) +
  # annotations
  annotate("text",
           x = as.Date("2020-10-01"), y = Inf, label = "Wave 1 start",
           color = "grey40", size = 3, angle = 90, vjust = -0.5, hjust = 1
  ) +
  annotate("text",
           x = as.Date("2021-02-01"), y = Inf, label = "Wave 2 start",
           color = "grey40", size = 3, angle = 90, vjust = -0.5, hjust = 1
  ) +
  annotate("text",
           x = as.Date("2024-04-01"), y = Inf, label = "Program end",
           color = "grey40", size = 3, angle = 90, vjust = -0.5, hjust = 1
  ) +
  #  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
  scale_x_date(date_breaks = "12 months", date_labels = "%Y") +
  scale_y_continuous(labels = scales::comma) +
  scale_color_manual(
    values = c("Gramatneusiedl" = "firebrick", "Control Towns" = "grey65"),
    breaks = c("Gramatneusiedl", "Control Towns")
  ) +
  scale_linetype_manual(
    values = c("Gramatneusiedl" = "solid", "Control Towns" = "dashed"),
    breaks = c("Gramatneusiedl", "Control Towns")
  ) +
  labs(
    title = "Median gross monthly earnings",
    x = "", y = "Euros",
    color = "", linetype = ""
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 1, size = 6.5),
    axis.title.y = element_text(size = 11, margin = margin(r = 10)),
    legend.position = "top",
    legend.text = element_text(size = 10),
    plot.title = element_text(face = "bold", size = 12)
  )

p

ggsave(
  filename = "Figures/admin_earnings_median_monthly_line_plot.pdf",
  plot = p,
  width = 10,
  height = 6,
  dpi = 300,
  device = cairo_pdf
)

# FINAL: PAPER ----
# Stop March 2024 ----
# limit data up to March 2024
earnings_monthly_trimmed <- earnings_monthly %>%
  filter(month <= as.Date("2024-03-31"))

## does not work
# # topcoxed for Dec 2021
# earnings_monthly_topcoded <- earnings_monthly_trimmed %>%
#   mutate(
#     earnings_avg_month = if_else(
#       month == as.Date("2021-12-01") & earnings_avg_month > 6475,
#       6475,
#       earnings_avg_month
#     )
#   )

# library(tidyr)
library(zoo)

earnings_monthly_interpolated <- earnings_monthly_trimmed %>%
  # drop the spike month
  filter(month != as.Date("2021-12-01")) %>%
  group_by(treat) %>%
  # ensure all months are present so interpolation works
  complete(month = seq(min(month), max(month), by = "month")) %>%
  arrange(treat, month) %>%
  # interpolate linearly for missing values (including the dropped one)
  mutate(
    earnings_avg_month = na.approx(earnings_avg_month, x = month, na.rm = FALSE),
    earnings_median_month = na.approx(earnings_median_month, x = month, na.rm = FALSE)
  ) %>%
  ungroup()



series_annotations = function(label_y){
  list(
    annotate("rect", xmin = parse_date_time("2020-10", "%Y-%m"), 
             xmax = parse_date_time("2021-02", "%Y-%m"),
             ymin = -Inf, ymax = Inf, fill = "gray90", alpha = .5),
    annotate("rect", xmin = parse_date_time("2021-02", "%Y-%m"), 
             xmax = parse_date_time("2024-03", "%Y-%m"),
             ymin = -Inf, ymax = Inf, fill = "gray80", alpha = .5),
    annotate("text", x = parse_date_time("2020-12", "%Y-%m"), 
             y=label_y, label = "Wave 1", size = 2.1) ,
    annotate("text", x = parse_date_time("2021-08", "%Y-%m"), 
             y=label_y, label = "Both waves", size = 2.1)
    # geom_vline(xintercept = parse_date_time("2020-03", "%Y-%m"), alpha =.3),
    # annotate("text", x = parse_date_time("2020-06", "%Y-%m"), 
    #          y=label_y, label = "Covid starts", size = 2),
#    geom_hline(yintercept = 0, alpha = .2),
    # scale_x_continuous(breaks = parse_date_time(c("2019-12", "2020-06", "2020-12", "2021-06"), "%Y-%m")),
#    theme_minimal() 
  )
}

# plot line figure (mean earnings up to Jan 2024)
p <- ggplot(earnings_monthly_interpolated, aes(x = month)) +
  series_annotations(-.035) +
  geom_line(
    data = filter(earnings_monthly_interpolated, treat == 0),
    aes(y = earnings_avg_month, color = "Control Towns", linetype = "Control Towns"),
    linewidth = 0.9
  ) +
  geom_line(
    data = filter(earnings_monthly_interpolated, treat == 1),
    aes(y = earnings_avg_month, color = "Gramatneusiedl", linetype = "Gramatneusiedl"),
    linewidth = 0.9
  ) +
  # geom_vline(
  #   xintercept = as.Date(c("2020-10-01", 
  #                          "2021-02-01" #, 
  #                          # "2024-03-31"
  #                          )),
  #   linetype = "dashed", color = "grey70", linewidth = 0.7
  # ) +
  # annotate("text",
  #          x = as.Date("2020-10-01"), y = Inf, label = "Wave 1 start",
  #          color = "grey60", size = 3, angle = 90, vjust = -0.5, hjust = 1
  # ) +
  # annotate("text",
  #          x = as.Date("2021-02-01"), y = Inf, label = "Wave 2 start",
  #          color = "grey60", size = 3, angle = 90, vjust = -0.5, hjust = 1
  # ) +
  # annotate("text",
  #          x = as.Date("2024-04-01"), y = Inf, label = "Program end",
  #          color = "grey40", size = 3, angle = 90, vjust = -0.5, hjust = 1
  # ) +
#   scale_x_date(
# #   limits = c(as.Date("2019-01-01"), as.Date("2024-03-31")),
#     date_breaks = "12 months", date_labels = "%Y"
#   ) +
  scale_x_date(
    date_breaks = "12 months",
    #   limits = c(as.Date("2019-01-01"), as.Date("2024-03-31")),
    date_labels = "%Y",
    expand = expansion(mult = c(0, 0.02)),
    breaks = c(
      scales::pretty_breaks()(range(earnings_monthly_interpolated$month)),
      max(earnings_monthly_interpolated$month)
    ),
    labels = c(
      format(
        scales::pretty_breaks()(range(earnings_monthly_interpolated$month)), "%Y"
      ),
      "2024"
    )
  ) +
  scale_y_continuous(labels = scales::comma) +
  scale_color_manual(
    values = c("Gramatneusiedl" = "firebrick", "Control Towns" = "grey65"),
    breaks = c("Gramatneusiedl", "Control Towns")
  ) +
  scale_linetype_manual(
    values = c("Gramatneusiedl" = "solid", "Control Towns" = "dashed"),
    breaks = c("Gramatneusiedl", "Control Towns")
  ) +
  labs(
    title = "", # Average gross monthly earnings
    subtitle = "<span style='color:firebrick;'>Gramatneusiedl</span>, and <span style='color:grey65;'>control towns</span>.",
    x = "", y = "Euros", color = "", linetype = "",
    caption = "" # Note: Data for December 2021 were interpolated to address irregularities in reporting. Bonus payments (13th and 14th salaries) were excluded to smooth seasonal volatility.
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 10),
    axis.text.y = element_text(size = 10),
    axis.title.y = element_text(size = 11, margin = margin(r = 10)),
    legend.position = "", # top
#    legend.text = element_text(size = 10),
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_markdown()
  )

p

p <- p + theme(plot.margin = margin(0, 5, -13, 5))

p

ggsave(
  filename = "Figures/admin_earnings_monthly_line_plot_only_regular_earnings.pdf",
  plot = p,
  width = 4.5, height = 3, dpi = 300, device = cairo_pdf
)


### SPIKE Investigate ----

spike_month <- as.Date("2021-12-01")

spike_data <- data_earnings_monthly %>%
  mutate(month = floor_date(date, "month")) %>%
  filter(month == spike_month)

spike_summary <- spike_data %>%
#  filter(BG_STF == "AB") %>% 
  mutate(BMG_eur = BMG / 100) %>%
  group_by(PENR, treat) %>%
  summarise(
    total_BMG = sum(BMG_eur, na.rm = TRUE),
    n_spells = n(),
    top_BG_STF = paste(unique(BG_STF), collapse = ", "),
    .groups = "drop"
  ) %>%
  arrange(desc(total_BMG))


head(spike_summary, 20)

spike_summary %>% 
ggplot(aes(x = total_BMG)) +
  geom_histogram(bins = 100) +
  scale_x_continuous(labels = scales::comma) +
  labs(title = "Distribution of total BMG in Dec 2021", x = "Total monthly BMG (EUR)")

spike_data %>%
  count(BG_STF, sort = TRUE)

# compare nearby months
data_earnings_monthly %>%
#  filter(BG_STF == "AB") %>% 
  mutate(month = floor_date(date, "month")) %>%
  filter(month %in% seq(as.Date("2021-11-01"), as.Date("2022-01-01"), by = "month")) %>%
  group_by(month, BG_STF) %>%
  summarise(total_BMG = sum(BMG, na.rm = TRUE) / 100, .groups = "drop") %>%
  ggplot(aes(x = month, y = total_BMG, fill = BG_STF)) +
  geom_col() +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "BMG composition by type around Dec 2021", y = "Total BMG (EUR)", x = "")


