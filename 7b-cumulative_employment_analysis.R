# 7b-cumulative_employment_analysis.R

# This script produces the figure and table for Figure 7: Cumulative days per person
# The figures includes cumulative days per person in different employment statuses, comparing Gramatneusiedl to the control towns. The table includes the cumulative days over the program period for both groups.

# run directly after 7a-cumulative_employment_prep.R

#### Settings ----

admin_out <- paste0(veracrypt_path, "jobguarantee/2024-02-admin-data-processed/")
output_path = "Figures/Employment_periods/"

# 1. Load both datasets
treated <- read_csv(paste0(admin_out, "participants_monthly_empl_cumsum.csv")) %>%
  mutate(group = "Treated")

control <- read_csv(paste0(admin_out, "control_individuals_monthly_empl_cumsum.csv")) %>%
  mutate(group = "Control")

outcomes_work_history <- c( 
  "TAGE_BESCHAEFTIGUNG_GESAMT", 
                           "TAGE_BESCHAEFTIGUNG_GEF",
  "TAGE_BESCHAEFTIGUNG_UNGEF", 
                           "TAGE_BESCHAEFTIGUNG_SELBST",
                           "TAGE_AMS_VORMERKUNG", 
                           "TAGE_OUT_OF_LABOUR_FORCE"
)

# Prepare human-readable names for titles
outcome_names <- c(
    "TAGE_BESCHAEFTIGUNG_GESAMT" = "in total employment",
    "TAGE_BESCHAEFTIGUNG_GEF" = "in subsidized employment",
  "TAGE_BESCHAEFTIGUNG_UNGEF" = "in unsubsidized employment",
    "TAGE_BESCHAEFTIGUNG_SELBST" = "in self-employment",
    "TAGE_AMS_VORMERKUNG" = "in registered unemployment",
    "TAGE_OUT_OF_LABOUR_FORCE" = "out of the labor force"
)

#### Analysis cumulative ----
# Figure 7b: Cumulative days per person

# 2. Merge and prepare for comparison
combined <- bind_rows(treated, control) %>%
  mutate(
    month_date = ym(month),  # Convert year-month to date
    employment_days = TAGE_BESCHAEFTIGUNG_GESAMT
  )

combined <- combined %>%
  mutate(
    group = case_when(
      group == "Treated" ~ "Gramatneusiedl",
      group == "Control" ~ "Control Towns",
      TRUE ~ group
    ),
    group = factor(group, levels = c("Control Towns", "Gramatneusiedl")) # Bottom to top
  )

for (outcome in outcomes_work_history) {
  plot_data <- combined %>%
    rename(outcome_value = !!sym(outcome)) %>%
    mutate(month_date = ym(month)) %>%
    filter(!is.na(outcome_value))
  
  if (nrow(plot_data) == 0) next
  
  p <- ggplot(plot_data, aes(x = month_date)) +
    # Control Towns (bottom layer)
    geom_line(
      data = filter(plot_data, group == "Control Towns"),
      aes(y = outcome_value, color = group, linetype = group),
      linewidth = 0.9
    ) +
    # Gramatneusiedl (top layer)
    geom_line(
      data = filter(plot_data, group == "Gramatneusiedl"),
      aes(y = outcome_value, color = group, linetype = group),
      linewidth = 0.9
    ) +
    scale_x_date(date_breaks = "6 months", date_labels = "%b\n%Y") +
    scale_y_continuous(labels = scales::comma) +
    scale_color_manual(
      values = c("Gramatneusiedl" = "firebrick", "Control Towns" = "grey65"),
      breaks = c("Gramatneusiedl", "Control Towns") # Legend order
    ) +
    scale_linetype_manual(
      values = c("Gramatneusiedl" = "solid", "Control Towns" = "dashed"),
      breaks = c("Gramatneusiedl", "Control Towns")
    ) +
    # scale_shape_manual(
    #   values = c("Gramatneusiedl" = 16, "Control Towns" = 17),
    #   breaks = c("Gramatneusiedl", "Control Towns")
    # ) +
    labs(
      subtitle = "<span style='color:firebrick;'>Gramatneusiedl</span>, and <span style='color:grey65;'>control towns</span>.",
      x = "", y = "Cumulative days per person",
      color = "", linetype = ""
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5, size = 10),
      axis.title.y = element_text(size = 11, margin = margin(r = 10)),
      legend.position = "none", # top
      legend.text = element_text(size = 10),
      plot.title = element_text(face = "bold", size = 12),
      plot.subtitle = element_markdown(size = 12),
      plot.margin = margin(5, 5, -10, 5)
    )
  
  ggsave(
    filename = paste0(output_path, "cumsum_", outcome, ".pdf"),
    plot = p,
    width = 4.5, height = 3, dpi = 300, device = cairo_pdf
  )
}

### Table for program period ----
# Table for Figure 7a: Cumulative days per person

combined_table <- combined %>%
  filter(month == "2024_02")

combined_table <- combined_table %>%
  select(-month, -month_date, -employment_days) %>%
  select(group, everything())

# rename to english for table
combined_table <- combined_table %>%
  rename_with(~ str_replace(., "TAGE_BESCHAEFTIGUNG_GESAMT", "Total Employment")) %>%
  rename_with(~ str_replace(., "TAGE_BESCHAEFTIGUNG_GEF", "Subsidized Employment")) %>%
  rename_with(~ str_replace(., "TAGE_BESCHAEFTIGUNG_UNGEF", "Unsubsidized Employment")) %>%
  rename_with(~ str_replace(., "TAGE_BESCHAEFTIGUNG_SELBST", "Self-Employment")) %>%
  rename_with(~ str_replace(., "TAGE_AMS_VORMERKUNG", "Registered Unemployment")) %>%
  rename_with(~ str_replace(., "TAGE_OUT_OF_LABOUR_FORCE", "Out of Labor Force"))

## Transpose rows and columns
combined_table_transposed <- combined_table %>%
  pivot_longer(-group, names_to = "employment_status", values_to = "days") %>%
  pivot_wider(names_from = group, values_from = days)

## Write data
combined_table_transposed %>% 
  write_csv(paste0(output_path, "days_empl_cumsum_transposed.csv"))  

## output as latex table

latex_table <- xtable(combined_table_transposed, digits = 0)

# export only with begin tabular
# First capture the output as text
latex_output <- print(latex_table, 
                      include.rownames = FALSE,
                      booktabs = TRUE,
                      sanitize.text.function = identity,
                      print.results = FALSE)


# Extract just the tabular part using regex
tabular_content <- sub(".*(\\\\begin\\{tabular\\}.*\\\\end\\{tabular\\}).*", "\\1", latex_output)

# Write to file
writeLines(tabular_content, paste0(output_path, "cumsum_days_empl.tex"))



## German figure cumulative for AMS report ----

combined <- combined %>%
  mutate(
    group = case_when(
      group == "Control Towns" ~ "Kontrollgemeinden",
      group == "Gramatneusiedl" ~ "Gramatneusiedl",
      TRUE ~ group
    ),
    group = factor(group, levels = c("Kontrollgemeinden", "Gramatneusiedl"))
  )

for (outcome in outcomes_work_history) {
  plot_data <- combined %>%
    rename(outcome_value = !!sym(outcome)) %>%
    mutate(month_date = ym(month)) %>%
    filter(!is.na(outcome_value))
  
  if (nrow(plot_data) == 0) next
  
  p <- ggplot(plot_data, aes(x = month_date)) +
    # Control Towns (bottom layer)
    geom_line(
      data = filter(plot_data, group == "Kontrollgemeinden"),
      aes(y = outcome_value, color = group, linetype = group),
      linewidth = 0.9
    ) +
    geom_line(
      data = filter(plot_data, group == "Gramatneusiedl"),
      aes(y = outcome_value, color = group, linetype = group),
      linewidth = 0.9
    ) +
    scale_x_date(date_breaks = "6 months", date_labels = "%b\n%Y") +
    #    scale_x_date(date_breaks = "12 months", date_labels = "%Y") +
    scale_y_continuous(labels = scales::comma) +
    scale_color_manual(
      values = c("Gramatneusiedl" = "firebrick", "Kontrollgemeinden" = "grey65"),
      breaks = c("Gramatneusiedl", "Kontrollgemeinden") # Legend order
    ) +
    scale_linetype_manual(
      values = c("Gramatneusiedl" = "solid", "Kontrollgemeinden" = "dashed"),
      breaks = c("Gramatneusiedl", "Kontrollgemeinden")
    ) +
    # scale_shape_manual(
    #   values = c("Gramatneusiedl" = 16, "Kontrollgemeinden" = 17),
    #   breaks = c("Gramatneusiedl", "Kontrollgemeinden")
    # ) +
    labs(
      x = "", y = "Tage pro Person, kumulativ",
      color = "", linetype = ""
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5, size = 10),
      axis.title.y = element_text(size = 11, margin = margin(r = 10)),
      legend.position = "none", # top
      legend.text = element_text(size = 10),
      plot.title = element_text(face = "bold", size = 12),
      plot.subtitle = element_markdown(size = 12),
      plot.margin = margin(5, 5, -10, 5)
    )
  
  ggsave(
    filename = paste0(output_path, "cumsum_", outcome, "_german.png"),
    plot = p,
    width = 4.5, height = 3, dpi = 300
  )
  
}

