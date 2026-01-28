# run directly after 7a-cumulative_employment_prep.R

#### Analysis cumulative ----
library(ggplot2)

admin_out <- paste0(veracrypt_path, "jobguarantee/2024-02-admin-data-processed/")
output_path = "Figures/Employment_periods/"

# 1. Load both datasets
treated <- read_csv(paste0(admin_out, "participants_monthly_empl_cumsum.csv")) %>%
  mutate(group = "Treated")

control <- read_csv(paste0(admin_out, "control_individuals_monthly_empl_cumsum.csv")) %>%
  mutate(group = "Control")

### LOOP Export

# 2. Merge and prepare for comparison
combined <- bind_rows(treated, control) %>%
  mutate(
    month_date = ym(month),  # Convert year-month to date
    employment_days = TAGE_BESCHAEFTIGUNG_GESAMT
  )

# # Custom theme based on your adjustments
# my_theme <- function() {
#   theme_minimal() +
#     theme(
#       plot.title = element_text(size = 14, face = "bold"),
#       plot.subtitle = element_text(size = 12),
#       axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
#       axis.text.y = element_text(size = 10),
#       axis.title = element_text(size = 12),
#       legend.position = "bottom",
#       legend.title = element_text(size = 11),
#       legend.text = element_text(size = 10),
#       panel.grid.minor = element_blank()
#     )
# }

outcomes_work_history <- c("TAGE_BESCHAEFTIGUNG_GESAMT", 
                           "TAGE_BESCHAEFTIGUNG_GEF",
                           "TAGE_BESCHAEFTIGUNG_UNGEF", 
                           "TAGE_BESCHAEFTIGUNG_SELBST",
                           "TAGE_AMS_VORMERKUNG", 
                           "TAGE_OUT_OF_LABOUR_FORCE")

# Prepare human-readable names for titles
outcome_names <- c(
  "TAGE_BESCHAEFTIGUNG_GESAMT" = "in total employment",
  "TAGE_BESCHAEFTIGUNG_GEF" = "in subsidized employment",
  "TAGE_BESCHAEFTIGUNG_UNGEF" = "in unsubsidized employment",
  "TAGE_BESCHAEFTIGUNG_SELBST" = "in self-employment",
  "TAGE_AMS_VORMERKUNG" = "in registered unemployment",
  "TAGE_OUT_OF_LABOUR_FORCE" = "out of the labor force"
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
    # geom_point(
    #   data = filter(plot_data, group == "Control Towns"),
    #   aes(y = outcome_value, color = group, shape = group),
    #   size = 1.8
    # ) +
    # Gramatneusiedl (top layer)
    geom_line(
      data = filter(plot_data, group == "Gramatneusiedl"),
      aes(y = outcome_value, color = group, linetype = group),
      linewidth = 0.9
    ) +
    # geom_point(
    #   data = filter(plot_data, group == "Gramatneusiedl"),
    #   aes(y = outcome_value, color = group, shape = group),
    #   size = 1.8
    # ) +
    scale_x_date(date_breaks = "6 months", date_labels = "%b\n%Y") +
#    scale_x_date(date_breaks = "12 months", date_labels = "%Y") +
    scale_y_continuous(labels = scales::comma) +
    scale_color_manual(
      values = c("Gramatneusiedl" = "firebrick", "Control Towns" = "grey65"),
      breaks = c("Gramatneusiedl", "Control Towns") # Legend order
    ) +
    scale_linetype_manual(
      values = c("Gramatneusiedl" = "solid", "Control Towns" = "dashed"),
      breaks = c("Gramatneusiedl", "Control Towns")
    ) +
    scale_shape_manual(
      values = c("Gramatneusiedl" = 16, "Control Towns" = 17),
      breaks = c("Gramatneusiedl", "Control Towns")
    ) +
    labs(
      #      title = paste("Cumulative days", outcome_names[outcome]),
      #    subtitle = "Gramatneusiedl and Control Towns",
      subtitle = "<span style='color:firebrick;'>Gramatneusiedl</span>, and <span style='color:grey65;'>control towns</span>.",
      x = "", y = "Cumulative days per person",
      color = "", linetype = "", shape = ""
    ) +
    theme_minimal() +
    theme(
#      panel.grid = element_blank(),
      axis.text.x = element_text(angle = 0, hjust = 0.5, size = 10),
      axis.title.y = element_text(size = 11, margin = margin(r = 10)),
      legend.position = "", # top
      legend.text = element_text(size = 10),
      plot.title = element_text(face = "bold", size = 12),
      plot.subtitle = element_markdown(size = 12),
      plot.margin = margin(5, 5, -10, 5)
    )
  # my_theme() # +
  # geom_vline(xintercept = as.Date("2020-10-01"), 
  #            linetype = "dashed", color = "gray80", linewidth = 0.8) +
  # annotate("text", x = as.Date("2020-10-01") + days(10),
  #          y = max(plot_data$outcome_value, na.rm = TRUE) * 0.95,
  #          label = "Program Start", color = "gray80", hjust = 0, size = 3.5)
  
  # ggsave(paste0(output_path, "cumsum_", outcome, ".pdf"), 
  #        p, width = 4.5, height = 3.0, dpi = 300)
  
  ggsave(
    filename = paste0(output_path, "cumsum_", outcome, ".pdf"),
    plot = p,
    width = 4.5, height = 3, dpi = 300, device = cairo_pdf
  )
}


#### Analysis monthly ----

# 1. Load both datasets
treated <- read_csv(paste0(admin_out, "participants_monthly_empl_diff.csv")) %>%
  mutate(group = "Treated")

control <- read_csv(paste0(admin_out, "control_individuals_monthly_empl_diff.csv")) %>%
  mutate(group = "Control")

# 2. Merge and prepare for comparison
combined_monthly <- bind_rows(treated, control) %>%
  mutate(
    month_date = ym(month)  # Convert year-month to date
  )

combined_monthly <- combined_monthly %>%
  mutate(
    group = case_when(
      group == "Treated" ~ "Gramatneusiedl",
      group == "Control" ~ "Control Towns",
      TRUE ~ group
    ),
    group = factor(group, levels = c("Control Towns", "Gramatneusiedl")) # Bottom to top
  )

for (outcome in outcomes_work_history) {
  plot_data <- combined_monthly %>%
    rename(outcome_value = !!sym(outcome)) %>%
    mutate(month_date = ym(month)) %>%
    filter(!is.na(outcome_value))
  
  if (nrow(plot_data) == 0) next
  
  p <- ggplot(plot_data, aes(x = month_date)) +
    # Control Towns (bottom layer)
    geom_line(
      data = filter(plot_data, group == "Control Towns"),
      aes(y = outcome_value, color = group, linetype = group),
      linewidth = 1.2
    ) +
    # geom_point(
    #   data = filter(plot_data, group == "Control Towns"),
    #   aes(y = outcome_value, color = group, shape = group),
    #   size = 1.8
    # ) +
    # Gramatneusiedl (top layer)
    geom_line(
      data = filter(plot_data, group == "Gramatneusiedl"),
      aes(y = outcome_value, color = group, linetype = group),
      linewidth = 1.2
    ) +
    # geom_point(
    #   data = filter(plot_data, group == "Gramatneusiedl"),
    #   aes(y = outcome_value, color = group, shape = group),
    #   size = 1.8
    # ) +
    scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
    scale_y_continuous(labels = scales::comma) +
    scale_color_manual(
      values = c("Gramatneusiedl" = "firebrick", "Control Towns" = "grey65"),
      breaks = c("Gramatneusiedl", "Control Towns") # Legend order
    ) +
    scale_linetype_manual(
      values = c("Gramatneusiedl" = "solid", "Control Towns" = "dashed"),
      breaks = c("Gramatneusiedl", "Control Towns")
    ) +
    scale_shape_manual(
      values = c("Gramatneusiedl" = 16, "Control Towns" = 17),
      breaks = c("Gramatneusiedl", "Control Towns")
    ) +
    labs(
      title = paste("Monthly days", outcome_names[outcome]),
      #    subtitle = "Gramatneusiedl and Control Towns",
      x = "", y = "Monthly days",
      color = "", linetype = "", shape = ""
    ) +
    theme_minimal() +
    theme(
      # panel.grid = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
      axis.title.y = element_text(size = 11, margin = margin(r = 10)),
      legend.position = "top",
      legend.text = element_text(size = 10),
      plot.title = element_text(face = "bold", size = 12)
    )
  # my_theme() # +
  # geom_vline(xintercept = as.Date("2020-10-01"), 
  #            linetype = "dashed", color = "gray80", linewidth = 0.8) +
  # annotate("text", x = as.Date("2020-10-01") + days(10),
  #          y = max(plot_data$outcome_value, na.rm = TRUE) * 0.95,
  #          label = "Program Start", color = "gray80", hjust = 0, size = 3.5)
  
  
  ggsave(paste0(output_path, "monthly_", outcome, ".png"), 
         p, width = 10, height = 6, dpi = 300)
}

### Table for final period

combined_table <- combined %>%
  filter(month == "2024_02")

combined_table <- combined_table %>%
  select(-month, -month_date, -employment_days) %>%
  select(group, everything())

# rename to english for econ paper table
combined_table <- combined_table %>%
  rename_with(~ str_replace(., "TAGE_BESCHAEFTIGUNG_GESAMT", "Total Employment")) %>%
  rename_with(~ str_replace(., "TAGE_BESCHAEFTIGUNG_GEF", "Subsidized Employment")) %>%
  rename_with(~ str_replace(., "TAGE_BESCHAEFTIGUNG_UNGEF", "Unsubsidized Employment")) %>%
  rename_with(~ str_replace(., "TAGE_BESCHAEFTIGUNG_SELBST", "Self-Employment")) %>%
  rename_with(~ str_replace(., "TAGE_AMS_VORMERKUNG", "Registered Unemployment")) %>%
  rename_with(~ str_replace(., "TAGE_OUT_OF_LABOUR_FORCE", "Out of Labor Force"))

# ## Write data
# combined_table %>% 
#   write_csv(paste0(output_path, "days_empl_cumsum.csv"))    

## Transpose rows and columns
combined_table_transposed <- combined_table %>%
  pivot_longer(-group, names_to = "employment_status", values_to = "days") %>%
  pivot_wider(names_from = group, values_from = days)

## Write data
combined_table %>% 
  write_csv(paste0(output_path, "days_empl_cumsum_transposed.csv"))  

## output as latex table
library(xtable)
latex_table <- xtable(combined_table_transposed, digits = 0)

# export with begin table
# print(latex_table, 
#       file = paste0(output_path, "days_empl_cumsum.tex"), 
#       include.rownames = FALSE, 
#       booktabs = TRUE,
#       sanitize.text.function = identity,
#       floating = TRUE,
#       floating.environment = "table", 
#       table.placement = "hbt!", 
#       caption.placement = "top",
#       caption = "Cumulative days by employment status")

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

outcomes_work_history <- c(
  # "TAGE_BESCHAEFTIGUNG_GESAMT", 
  #                          "TAGE_BESCHAEFTIGUNG_GEF",
                            "TAGE_BESCHAEFTIGUNG_UNGEF" 
  #                          "TAGE_BESCHAEFTIGUNG_SELBST",
  #                          "TAGE_AMS_VORMERKUNG", 
  #                          "TAGE_OUT_OF_LABOUR_FORCE"
                           )

# Prepare human-readable names for titles
outcome_names <- c(
  # "TAGE_BESCHAEFTIGUNG_GESAMT" = "in total employment",
  # "TAGE_BESCHAEFTIGUNG_GEF" = "in subsidized employment",
   "TAGE_BESCHAEFTIGUNG_UNGEF" = "in unsubsidized employment"
  # "TAGE_BESCHAEFTIGUNG_SELBST" = "in self-employment",
  # "TAGE_AMS_VORMERKUNG" = "in registered unemployment",
  # "TAGE_OUT_OF_LABOUR_FORCE" = "out of the labor force"
)

combined <- combined %>%
  mutate(
    group = case_when(
      group == "Treated" ~ "Gramatneusiedl",
      group == "Control" ~ "Kontrollgemeinden",
      TRUE ~ group
    ),
    group = factor(group, levels = c("Kontrollgemeinden", "Gramatneusiedl")) # Bottom to top
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
    # geom_point(
    #   data = filter(plot_data, group == "Control Towns"),
    #   aes(y = outcome_value, color = group, shape = group),
    #   size = 1.8
    # ) +
    # Gramatneusiedl (top layer)
    geom_line(
      data = filter(plot_data, group == "Gramatneusiedl"),
      aes(y = outcome_value, color = group, linetype = group),
      linewidth = 0.9
    ) +
    # geom_point(
    #   data = filter(plot_data, group == "Gramatneusiedl"),
    #   aes(y = outcome_value, color = group, shape = group),
    #   size = 1.8
    # ) +
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
    scale_shape_manual(
      values = c("Gramatneusiedl" = 16, "Kontrollgemeinden" = 17),
      breaks = c("Gramatneusiedl", "Kontrollgemeinden")
    ) +
    labs(
      #      title = paste("Cumulative days", outcome_names[outcome]),
      #    subtitle = "Gramatneusiedl and Control Towns",
      # subtitle = "<span style='color:firebrick;'>Gramatneusiedl</span>, and <span style='color:grey65;'>control towns</span>.",
      x = "", y = "Tage pro Person, kumulativ",
      color = "", linetype = "", shape = ""
    ) +
    theme_minimal() +
    theme(
      #      panel.grid = element_blank(),
      axis.text.x = element_text(angle = 0, hjust = 0.5, size = 10),
      axis.title.y = element_text(size = 11, margin = margin(r = 10)),
      legend.position = "", # top
      legend.text = element_text(size = 10),
      plot.title = element_text(face = "bold", size = 12),
      plot.subtitle = element_markdown(size = 12),
      plot.margin = margin(5, 5, -10, 5)
    )
  # my_theme() # +
  # geom_vline(xintercept = as.Date("2020-10-01"), 
  #            linetype = "dashed", color = "gray80", linewidth = 0.8) +
  # annotate("text", x = as.Date("2020-10-01") + days(10),
  #          y = max(plot_data$outcome_value, na.rm = TRUE) * 0.95,
  #          label = "Program Start", color = "gray80", hjust = 0, size = 3.5)
  
  # ggsave(paste0(output_path, "cumsum_", outcome, ".pdf"), 
  #        p, width = 4.5, height = 3.0, dpi = 300)
  
  ggsave(
    filename = paste0(output_path, "cumsum_", outcome, "_german.png"),
    plot = p,
    width = 4.5, height = 3, dpi = 300
  )
  
}

