series_annotations = function(label_y){
  list(
    annotate("rect", xmin = parse_date_time("2020-10", "%Y-%m"), 
             xmax = parse_date_time("2021-02", "%Y-%m"),
             ymin = -Inf, ymax = Inf, fill = "gray90", alpha = .5),
    annotate("rect", xmin = parse_date_time("2021-02", "%Y-%m"), 
             xmax = parse_date_time("2021-08", "%Y-%m"),
             ymin = -Inf, ymax = Inf, fill = "gray80", alpha = .5),
    annotate("text", x = parse_date_time("2020-12", "%Y-%m"), 
             y=label_y, label = "Wave 1", size = 2) ,
    annotate("text", x = parse_date_time("2021-05", "%Y-%m"), 
             y=label_y, label = "Both waves", size = 2),
    geom_vline(xintercept = parse_date_time("2020-03", "%Y-%m"), alpha =.3),
    annotate("text", x = parse_date_time("2020-06", "%Y-%m"), 
             y=label_y, label = "Covid starts", size = 2),
    geom_hline(yintercept = 0, alpha = .2),
    # scale_x_continuous(breaks = parse_date_time(c("2019-12", "2020-06", "2020-12", "2021-06"), "%Y-%m")),
    theme_minimal() 
  )
}

combined_synthetic_plot = function(outcomes, outcome_labels){

  p1 = town_treatment_effects %>% 
    filter(Outcome %in% outcomes) |> 
    ggplot(aes(x = month, y = value)) +
    series_annotations(-.035) +
    scale_y_continuous(labels = 
                         scales::percent(seq(-.08,.08, by = .02), accuracy = 1),
                       breaks = seq(-.08,.08, by = .02)) +
    geom_line(data = permutation_treatment_effects%>% 
                filter(Outcome %in% outcomes),
              aes(group = GKZ), alpha = .15) +
    geom_line(color = "firebrick") +
    facet_grid(~ factor(Outcome, levels = outcomes, labels = outcome_labels)) +
    labs(y = "",
         x = "",
         title = "Treatment effects",
         subtitle = "<span style='color:firebrick;'>Gramatneusiedl minus control</span>, and <span style='color:grey60;'>permuted comparisons</span>.")+
    theme(plot.subtitle = element_markdown())
  
  # the same thing in levels
  admin_data_treated_town = 
    admin_data_towns  %>% 
    filter(GKZ == Gramatneusiedl) %>% 
    select(c("GKZ", "month", outcomes))
  
  admin_data_control_towns =
    admin_data_towns  %>%
    filter(GKZ %in% synthetic_control_weights$GKZ) %>%
    left_join(synthetic_control_weights, by = "GKZ")
  
  # calculate synthetic control averages of outcomes
  synthetic_control_averages = admin_data_control_towns %>%
    group_by(month) %>%
    summarize(across(outcomes, ~ weighted.mean(.x, w = weight))) %>%
    mutate(GKZ = -1)
  
  
  selected_synth_levels = 
    synthetic_control_averages %>% 
    bind_rows(admin_data_treated_town) %>% 
    pivot_longer(cols = outcomes, names_to = "Outcome") %>% 
    filter(Outcome %in% outcomes)
  
  
  if (outcomes[1] %in% c("EMP_rate_tot", "Inactive_rate_tot")) {step = .1}
    else {step = .02}
  p2 =
    selected_synth_levels |> 
    ggplot(aes(x = month, y = value)) +
    series_annotations(step/2 - 0.005) +
    scale_y_continuous(labels = 
                         scales::percent(seq(-.1, 1, by = step), accuracy = 1),
                       breaks =(seq(-.1, 1, by = step))) +
    geom_line(aes(color = factor(GKZ, labels = c("Synthetic control", "Gramatneusiedl")))) +
    scale_color_manual(values = c("grey65", "firebrick")) +
    facet_grid(~ factor(Outcome, levels = outcomes, labels = outcome_labels)) +
    labs(y = "", color = "", x = "",
         title = "Outcome levels", 
         subtitle = "<span style='color:firebrick;'>Gramatneusiedl</span>, and <span style='color:grey50;'>synthetic control</span>.")+
    theme(plot.subtitle = element_markdown(),
          legend.position="none")
  
  
  # Permutation inference ranks
  permutation_ranks = 
    permutation_treatment_effects %>% 
    left_join(town_treatment_effects, by = c("month", "Outcome")) %>% 
    mutate(rank = (value.x < value.y)) %>% 
    group_by(month, Outcome) %>% 
    summarize(rank = mean(rank))
  
  p3 = permutation_ranks %>% 
    filter(Outcome %in% outcomes) |> 
    ggplot(aes(x = month, y = rank)) +
    geom_hline(yintercept = .5, alpha = .2) +
    series_annotations(.08) +
    ylim(c(0,1)) +
    geom_line() +
    facet_grid(~ factor(Outcome, levels = outcomes, labels = outcome_labels)) +
    labs(title = "Rank among permutations", y ="", x = "Year")
  
  p2/p1/p3
  
}


ggsave("Figures/synthetic_combined.png", 
       combined_synthetic_plot(outcomes[1:3], outcome_labels[1:3]),
       width = 8, height = 8)

ggsave("Figures/synthetic_employment.png", 
       combined_synthetic_plot(outcomes[4], outcome_labels[4]),
       width = 3, height = 8)

ggsave("Figures/synthetic_inactive.png", 
       combined_synthetic_plot(outcomes[5], outcome_labels[5]),
       width = 3, height = 8)

ggsave("Figures/synthetic_employment_inactive.png", 
       combined_synthetic_plot(outcomes[4:5], outcome_labels[4:5]),
       width = 6, height = 8)


