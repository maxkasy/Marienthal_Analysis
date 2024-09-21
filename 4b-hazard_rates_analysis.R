# 4b-hazard_rates_analysis
# calculate transition rates from unemployment to employment by month of unemployment spell duration

## Settings ####

## Paths for WU Virtual Machine
 home <- getwd()
# home <- "D:/magma"
# data_path = "D:/amdb/"
# output_path = "D:/magma/output/"

output_path = "Figures/Hazard_rates"
output_path2 = file.path(home, output_path)


data_path = paste0(veracrypt_path, "jobguarantee/2024-04-hazard-rates-processed/")

ams_amdb_data <- file.path(data_path, "short_term_unemployed.csv")

## Start code ####
# load merged data from AMS and AMDB
data_ams <- fread(ams_amdb_data)

#### aggregate employment status ####

# values to aggregate into "AM" (status: unemployment)
am_values <- c("AL", "D2", "SC", "LS", "SF", "SR", "66", "AG", "AS", "AM", "AF", "LF", "TA", "VM")

# values to aggregate into "BE" (status: employment)
be_values <- c("LFINL", "FBINP", "FBINW", "FBINF", "FU", "LFJAS", "LFIBA", "LFUBA", 
               "LFLST", "LFP30", "LFVOL", "FBEB", "FBBEB", "FBBS2", "FBBS3", "FBEB1", 
               "FBEB2", "FBGEB", "FBSOL", "FBKUA", "FBKOM", "FBSOB", "FBBS1", "FBGBP", 
               "FBEPU", "LFTEL", "LFVRL", "LFNRM", "FBES", "FBES1", "FBASZ", "FBEZU", 
               "FBBP", "BE", "LE", "AA", "FD", "SO", "S1S2", "SBSVA", "LW", "BA")

# values to aggregate into "SO" (status: other (incl. out of labor force))
so_values <- c("W1", "W2", "ED", "EO", "KG", "KO", "PZ", "RE", "SG", "AO", 
               "G1", "AU", "MK", "MP", "MS", "SV", "TO", "LL", "KD")

# Aggregate
merged_data_filtered_aggregated <- data_ams %>%
  mutate(status = ifelse(status %in% am_values, "AM", status),
         status = ifelse(status %in% be_values, "BE", status),
         status = ifelse(status %in% so_values, "SO", status) )

#### Create transition variable ####

# sort by penr and beginn
merged_data_filtered_aggregated <- merged_data_filtered_aggregated %>%
  arrange(penr, beginn)

# Create transit variable for AM
merged_data_filtered_aggregated <- merged_data_filtered_aggregated %>%
  group_by(penr) %>%
  mutate(transit = ifelse(status == "AM" & lead(status) == "BE", 1, 0)) %>%
  ungroup()

# Replace NA in transit with 0 (for the first occurrence where there's no lag value)
merged_data_filtered_aggregated <- merged_data_filtered_aggregated %>%
  mutate(transit = replace_na(transit, 0))

#### Calculate Unemployment duration ####

# Transform beginn and ende to date variables
merged_data_filtered_aggregated <- merged_data_filtered_aggregated %>%
  mutate(
    beginn = ymd(beginn),
    ende = ymd(ende)
  )

#### Aggregate ####

# Calculate duration_months with decimals for each observation accurately
spell_duration_data <- merged_data_filtered_aggregated %>%
  mutate(
    period_between = interval(beginn, ende),
    months_diff = as.numeric(period_between / months(1))
  )

# Add a group identifier for consecutive same status within each penr
spell_duration_data <- spell_duration_data %>%
  arrange(penr, beginn) %>%
  group_by(penr) %>%
  mutate(
    status_group = cumsum(lag(status, default = first(status)) != status)
  ) %>%
  group_by(penr, status_group) %>%
  mutate(
    months_diff_cumsum = cumsum(months_diff) # add period length of consecutive identical periods
  ) %>%
  ungroup()

# Create a new variable that rounds months_diff_cumsum to integers
spell_duration_data <- spell_duration_data %>%
  mutate(spell_duration = ceiling(months_diff_cumsum))

# Filter to keep only the final observation of any consecutive status observations
spell_duration_data_filtered <- spell_duration_data %>%
  group_by(penr, status_group) %>%
  filter(row_number() == n()) %>%
  ungroup()

# keep only AM Spells
spell_duration_data_filtered <- spell_duration_data_filtered %>%
  filter(status == "AM")

# Generate a sequence of months for each observation to include all months covered by at least one day in period_between
spell_duration_long <- spell_duration_data_filtered %>%
  rowwise() %>%
  mutate(month_sequence = list(seq(floor_date(beginn, "month"), ceiling_date(ende, "month") - days(1), by = "month"))) %>%
  unnest(month_sequence)

# Generate spell_duration_running that counts every month within the same penr, spell_duration
spell_duration_long <- spell_duration_long %>%
  group_by(penr, status_group) %>%
  mutate(spell_duration_running = row_number() -1 ) %>%
  ungroup()

# Count the number of observations in each calendar month
monthly_stock <- spell_duration_long %>%
  group_by(spell_duration_running, treat) %>%
  summarise(stock = n()) %>%
  arrange(spell_duration_running) %>% 
  rename(spell_duration = spell_duration_running) # rename for merging datasets

# Count the number of transitions by duration_months and by treat
transitions_summary <- spell_duration_data_filtered %>%
  filter(transit == 1) %>%  # Filter rows where transit is 1
  group_by(spell_duration, treat) %>%  # Group by duration_months and treat
  summarise(
    transitions = n()  # Count the number of transitions (E)
    #    total_unemployed = sum(U_beginning$U_beginning)  # Calculate total number of unemployed individuals at the beginning of the month (U_beginning)
  ) 

# merge data frames on transitions and on stock of unemployed
transition_rates_data <- monthly_stock %>%
  left_join(transitions_summary, by = c("spell_duration", "treat"))

# Replace NA values in the merged data with 0
transition_rates_data <- transition_rates_data %>%
  mutate_all(~replace_na(., 0))

# calculate the transition rate
transition_rates_data <- transition_rates_data %>%
  mutate(transition_rate = transitions / stock) %>%  # Calculate transition rate) %>%  # Count the number of transitions
  ungroup()

# Filter to include only the first 18 months
transitions_summary_filtered <- transition_rates_data %>%
  filter(spell_duration <= 18,
         spell_duration > 0)

#### Plot transition rates ####
p <- ggplot(transitions_summary_filtered, aes(x = spell_duration, y = transition_rate, color = as.factor(treat))) +
  annotate("rect", xmin = 9, xmax = 18, ymin = -Inf, ymax = Inf, fill = "gray90", alpha = 0.5) +  # Add shaded rectangle
  geom_line() +
  scale_color_manual(values = c("1" = "firebrick", "0" = "grey65")) +
  labs(
    # title = "Transition Rates by Duration (in Months)",
    subtitle = "<span style='color:firebrick;'>Gramatneusiedl</span>, and <span style='color:grey50;'>synthetic control</span>.",
    x = "Duration (months)",
    y = "Transition rate",
    color = "Treatment"
  ) +
  theme_minimal() +
  theme(plot.subtitle = element_markdown(),
        legend.position="none") +
  scale_x_continuous(breaks = 0:20)  +  # Ensure x-axis has breaks for each month up to 18
  scale_y_continuous(limits = c(0, NA), expand = c(0, 0))  # Start y-axis at 0 and expand up to the max value
  #  annotate("text", x = 11, y = 0.05, label = "Job guarantee eligibility starts", size = 3, angle = 0, vjust = -0.5) +  # Add text annotation at x = 9

p

output_filename <- file.path(output_path, "transition_rates_ue.png", sep = "")
ggsave(output_filename, plot = p, width = 5, height = 3)

# # export data frame to csv
 output_csv <- file.path(output_path, "transition_rates_data_ue.csv")

transition_rates_data %>% 
  write.csv(file = output_csv, row.names = FALSE)

