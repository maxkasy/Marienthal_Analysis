# defining colors
back_col = "white"
treat_col = "firebrick"
control_col = "grey65"
line_col = "grey50"


plot_treatment_effects = function(treatment_effects, filename_modifier = "",
                                  TE_title = "") {
    # scaling of plot output
    plot_height = .8 + nrow(treatment_effects) / 6 

    # find the outcome corresponding to the second-smallest control-group average
    label_row = ifelse( nrow(treatment_effects) >=6, 6, 1)
    label_outcome = 
        sort(-treatment_effects$p_value, index.return =T)$ix[[label_row]]
    
    labs = tibble(
        lab = c("Group 1", "Group 2"),
        y = c(treatment_effects[[label_outcome, "Group_1"]] + .03, treatment_effects[[label_outcome, "Group_2"]] - .03),
        hjust = c("left", "right")
    )
    
    
    p1 = treatment_effects %>%
    left_join(group_names, by = "Outcome") %>% 
    mutate(Name = fct_reorder(Name, -p_value)) %>% 
        ggplot(aes(x = Name, y = Group_1)) +
        geom_segment(aes(xend = Name, yend = Group_2), color = line_col) +
        geom_point(color = treat_col, size = 2) +
        geom_point(aes(y = Group_2), color = control_col, shape = 17, size = 2) +
        geom_text(data = labs, aes(label = lab, y = y, hjust = hjust), x = label_row, size = 3) +
        scale_y_continuous(limits=c(0,1), expand = c(0, .01)) +
        coord_flip() +
        theme_minimal() +
        theme(plot.margin = margin(.1,.2,.1,.1, unit = "in"),
              plot.subtitle = element_markdown()) +
        labs(
            x = "",
            y = "",
            title = TE_title,
            subtitle = "Average outcomes for <span style='color:firebrick;'>Group 1</span> (treated), and <span style='color:grey60;'>Group 2</span> (control)."
        )
    
        ggsave(paste0("Figures/Survey_averages_plot_", filename_modifier, ".png"),
               p1,
               width = 7,
               height = plot_height)
    
    
    
    p2 = treatment_effects %>%
    left_join(group_names, by = "Outcome") %>% 
    mutate(Name = fct_reorder(Name, -p_value)) %>% 
        ggplot(aes(x = Name, y = ATE)) +
        geom_point(color = treat_col) +
        geom_hline(yintercept = 0, color = "grey80") +
        coord_flip() +
        theme_minimal() +
        theme(plot.margin = margin(.1,.2,.1,.1, unit = "in")) +
        labs(
            x = "",
            y = ""
        )
    
    ggsave(paste0("Figures/Survey_ATE_plot_", filename_modifier, ".png"),
           p2,
           width = 7,
           height = plot_height)
    
    p1
}




plot_p_values = function(treatment_effects_RI,
                         filename_modifier = "") {
    # scaling of plot output
    plot_height = .8 + nrow(treatment_effects_RI) / 6 
    
    p1 = treatment_effects_RI %>%
    left_join(group_names, by = "Outcome") %>% 
    mutate(Name = fct_reorder(Name, -p_value)) %>% 
        ggplot(aes(x = Name, y = p_value)) +
        geom_point(color = treat_col, size = 2) +
        scale_y_continuous(
            limits = c(0, 1),
            breaks = seq(0, 1, by = .1),
            expand = c(0, .01),
            minor_breaks = NULL
        ) +
        geom_hline(yintercept = .05, color = "grey80") +
        geom_hline(yintercept = .1, color = "grey80") +
        coord_flip() +
        theme_minimal() +
        theme(plot.margin = margin(.1,.2,.1,.1, unit = "in")) +
        labs(
            x = "",
            y = "",
            subtitle = "P-values"
        )
    
    ggsave(paste0("Figures/Survey_pvalue_plot_", filename_modifier, ".png"),
           p1,
           width = 7,
           height = plot_height)
    
    p1
}


table_p_values = function(treatment_effects_RI,
                         filename_modifier = "") {
    
    
    filename = paste0("Figures/Survey_pvalue_table_", filename_modifier, ".tex")
    
    treatment_effects_RI |> 
    left_join(group_names, by = "Outcome") |> 
    select(Name, Group_1, Group_2, ATE, p_value, std.error, n1, n2) |> 
    arrange(p_value) |> 
    knitr::kable(
        col.names = c(
            "Outcome", "Treated", "Control", "Difference",
            "p-value",
            "SE",
            "$n_1$",
            "$n_2$"
        ),
        row.names = F,
        digits = 3,
        format = "latex",
        booktabs = TRUE,
        escape = F
    ) %>%
        write(filename)
           
}


plot_confidence_intervals = function(treatment_effects_with_SEs,
                                     filename_modifier = "",
                                     title = element_blank()) {
    # scaling of plot output
    plot_height = .8 + nrow(treatment_effects_with_SEs) / 6 
    
    p1 = treatment_effects_with_SEs %>%
    left_join(group_names, by = "Outcome") %>% 
    mutate(Name = fct_reorder(Name, estimate)) %>% 
        ggplot(aes(x = Name, y = estimate)) +
        geom_hline(yintercept = 0, color = "grey80") +
        geom_segment(aes(
            xend = Name,
            y = estimate - 1.96 * std.error,
            yend = estimate + 1.96 * std.error
        ), color = line_col) +
        geom_segment(aes(
            xend = Name,
            y = estimate - 1.65 * std.error,
            yend = estimate + 1.65 * std.error
        ), color = line_col, size = 1) +
        geom_point(color = treat_col, size = 2) +
        ylim(c(-.6, .8)) +
        coord_flip() +
        theme_minimal() +
        theme(plot.margin = margin(.1,.2,.1,.1, unit = "in")) +
        labs(
            title = title,
            x = "",
            y = ""
        )
    
    ggsave(paste0("Figures/Confidence_intervals_", filename_modifier, ".png"),
           p1,
           width = 7,
           height=plot_height)
    
    p1
}

# Helper function to combine plots for mean outcomes and p-values
plot_combined= function(p_te, p_pv) {
    p_te + 
        p_pv + theme(axis.text.y=element_blank())+ 
        plot_layout(widths = c(3, 2))
}

