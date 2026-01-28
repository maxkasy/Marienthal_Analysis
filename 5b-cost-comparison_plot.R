# 5-cost-comparison
library(tidyverse)
library(readr)
library(ggtext)
library(janitor)


data_raw_figure <- read.csv("magma_costs_table_for_figure.csv", sep = ",")

## format data_raw_figure to create the gglopt below
# Read and adjust data
data_long <- data_raw_figure %>%
  mutate(
    period_sequence = as.Date(period_sequence, format = "%d/%m/%Y"),
    # Change first date while keeping others
    period_sequence = if_else(
      period_sequence == as.Date("2020-07-01"),
      as.Date("2020-10-01"),
      period_sequence
    )
  ) %>%
  # Create ordered factor for consistent spacing
  mutate(
    period_factor = (factor(period_sequence))
  ) %>%
  pivot_longer(
    cols = c(foer_person_cost_month, alv_person_cost_month),
    names_to = "cost_type",
    values_to = "cost"
  ) %>%
  mutate(
    treat = factor(treat, levels = c(1, 0), labels = c("Gramatneusiedl", "Control towns")),
    cost_type = factor(cost_type,
                       levels = c("foer_person_cost_month", "alv_person_cost_month"),
                       labels = c("Net program costs", "Benefit payments"))
  )

data_totals = 
    data_long |> 
    group_by(treat, period_factor) |> 
    summarise(total = sum(cost))


ggplot(data_long, aes(
        x = period_factor,
        y = cost
    )) +
    geom_col(position = "stack", aes(fill = cost_type), width = 0.5) +
    geom_text(data = data_totals, 
              aes(y= total + 120, 
                  label = round(total)), 
              size = 3) +
    facet_grid(cols = vars(treat))+
    scale_x_discrete(
      breaks = unique(data_long$period_factor),
        labels =  paste0(format(unique(data_long$period_sequence), "%b"), "\n", format(unique(data_long$period_sequence), "%Y"))
    ) +
    scale_fill_manual(
        values = c(
            "Net program costs" = "darkblue",
            "Benefit payments" = "grey70"
        )) +
    theme_minimal() +
  theme(
    legend.position = "bottom"
  ) +
    labs(
        title = "",
        x = "",
        fill = "",
        y = "Monthly cost per person in EUR"
    )


# Save the plot
ggsave(
  filename = "Figures/cost_comparison_stacked_bar_faceted_RR.png",
  width = 8,
  height = 3.5,
  dpi = 300
)

# Save as PDF
ggsave(
  filename = "Figures/cost_comparison_stacked_bar_faceted_RR.pdf",
  width = 6,
  height = 3.5
)



## German for AMS report ----

data_long_german <- data_long %>%
  mutate(
    treat = recode(treat,
                   "Gramatneusiedl" = "Gramatneusiedl",
                   "Control towns" = "Kontrollgemeinden"),
    cost_type = recode(cost_type,
                       "Net program costs" = "Förderleistungen",
                       "Benefit payments" = "Transferleistungen")
  )

data_totals_german <- data_totals %>%
  mutate(
    treat = recode(treat,
                   "Gramatneusiedl" = "Gramatneusiedl",
                   "Control towns" = "Kontrollgemeinden")
  )

# change system settings to german to show x axis abbrevations in German
old_locale <- Sys.getlocale("LC_TIME")
Sys.setlocale("LC_TIME", "de_DE.UTF-8")

ggplot(data_long_german, aes(
  x = period_factor,
  y = cost
)) +
  geom_col(position = "stack", aes(fill = cost_type), width = 0.5) +
  geom_text(data = data_totals_german, 
            aes(y= total + 120, 
                label = round(total)), 
            size = 3) +
  facet_grid(cols = vars(treat))+
  scale_x_discrete(
    breaks = unique(data_long$period_factor),
    labels =  paste0(format(unique(data_long$period_sequence), "%b"), "\n", format(unique(data_long$period_sequence), "%Y"))
  ) +
  scale_fill_manual(
    values = c(
      "Förderleistungen" = "darkblue",
      "Transferleistungen" = "grey70"
    )) +
  theme_minimal() +
  theme(
    legend.position = "bottom"
  ) +
  labs(
    title = "",
    x = "",
    fill = "",
    y = "Kosten pro Person und Monat in Euro"
  )

# change system settings back to English
Sys.setlocale("LC_TIME", old_locale)

# Save the plot
ggsave(
  filename = "Figures/cost_comparison_stacked_bar_faceted_RR_German.png",
  width = 8,
  height = 3.5,
  dpi = 300
)


