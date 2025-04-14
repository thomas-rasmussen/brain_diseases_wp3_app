
# Utility function to make a stacked barplot for cost analyses
make_cost_plot <- function(
    x, 
    title = NULL,
    include_lost_production_plot = FALSE,
    x_axis_label = NULL,
    x_axis_limits_left_plot = NULL,
    x_axis_limits_right_plot = NULL,
    include_legend = TRUE,
    bar_width = NULL) {
  
  # Determine y-axis order based on sum of costs
  y_order <- x %>%
    group_by(closest_relative_group_label) %>%
    summarize(total_cost = sum(cost_var, na.rm = TRUE), .groups = "keep") %>%
    arrange(total_cost)

  x <- x %>%
    mutate(
      closest_relative_group_order = factor(
        closest_relative_group_label,
        levels = y_order$closest_relative_group_label,
        labels = y_order$closest_relative_group_label
      )
    )

  
  # cvd-friendly qualitative color palette from "Fundamentals of Data
  # Visualization" figure 19.10
  cvd_color_palette <- c(
    "#E69F00", "#56B4E9", "#009E73", "#F0E442","#0072B2", "#D55E00"
  )

  right_plot <- x %>%
    filter(cost_component != "Lost productivity (income loss)") %>%
    ggplot(aes(x = cost_var, y = closest_relative_group_order, fill = cost_component)) +
    geom_bar(position = "stack", stat = "identity", width = bar_width) +
    theme_bw() +
    # cvd-friendly qualitative color palette from "Fundamentals of Data
    # Visualization" figure 19.10
    scale_fill_manual(values = cvd_color_palette[3:6]) +
    labs(
      x = x_axis_label
    ) +
    theme(
      legend.position = "bottom",
      legend.title = element_blank(),
      axis.title.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.y = element_text(hjust = 0.5, vjust = 0.5),
      panel.grid.major.y = element_blank(),
      panel.border = element_blank(),
      axis.line.x.bottom = element_line(colour = "black"),
      axis.text = element_text(size = 12, colour = "black")
    )
  
  if (!is.null(x_axis_limits_right_plot)) {
    right_plot <- right_plot +
      scale_x_continuous(
        expand = expansion(mult = 0.01, add = 0),
        limits = x_axis_limits_right_plot
      )
  } else {
    right_plot <- right_plot +
      scale_x_continuous(
        expand = expansion(mult = 0.01, add = 0)
      )

  }
    
  if (include_lost_production_plot) {
    left_plot <- x %>%
      mutate(cost_var = -cost_var) %>%
      filter(cost_component == "Lost productivity (income loss)") %>%
      ggplot(aes(x = cost_var, y = closest_relative_group_order, fill = cost_component)) +
      geom_bar(position = "stack", stat = "identity", width = bar_width) +
      theme_bw() +
      # cvd-friendly qualitative color palette from "Fundamentals of Data
      # Visualization" figure 19.10
      scale_fill_manual(values = cvd_color_palette) +
      labs(
        x = x_axis_label
      ) +
      theme(
        legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.border = element_blank(),
        axis.line.x.bottom = element_line(colour = "black"),
        axis.text = element_text(size = 12, colour = "black")
      )
  }
  
  if (include_lost_production_plot && !is.null(x_axis_limits_left_plot)) {
    left_plot <- left_plot +
      scale_x_continuous(
        expand = expansion(mult = 0.01, add = 0),
        limits = x_axis_limits_left_plot,
        labels = function(x) -x
      )
  } else if (include_lost_production_plot) {
    left_plot <- left_plot +
      scale_x_continuous(
        expand = expansion(mult = 0.01, add = 0),
        labels = function(x) -x
      )
  }

  
  if (isFALSE(include_legend)) {
    right_plot <- right_plot + theme(legend.position = "none")
  }
  if (isFALSE(include_legend) && include_lost_production_plot) {
    left_plot <- left_plot + theme(legend.position = "none")
  }
    
  if (include_lost_production_plot) {
    combined_plot <- left_plot + right_plot
  } else {
    combined_plot <- right_plot
  }
  
  combined_plot <- combined_plot +
    plot_annotation(title = title)
  
  combined_plot
}

# Utility function to make barplots for relative costs
make_cost_plot_relative <- function(x,
                                    include_legend,
                                    title,
                                    x_axis_limits,
                                    bar_width) {

    # Determine y-axis order based on sum of costs
  y_order <- x %>%
    arrange(att_cost_prop)
  
  x <- x %>%
    mutate(
      closest_relative_group_order = factor(
        closest_relative_group_label,
        levels = y_order$closest_relative_group_label,
        labels = y_order$closest_relative_group_label
      )
    )
  
  cost_plot <- x %>%
    ggplot(aes(x = att_cost_prop, y = closest_relative_group_order, fill = cost_component)) +
    geom_bar(stat = "identity", width = bar_width) +
    theme_bw() +
    scale_fill_manual(values = "#E69F00") +
    labs(
      title = title,
      x = "Relative cost"
    ) +
    theme(
      plot.title.position = "plot",
      legend.position = "bottom",
      legend.title = element_blank(),
      axis.title.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.y = element_text(hjust = 0, vjust = 0.5),
      panel.grid.major.y = element_blank(),
      panel.border = element_blank(),
      axis.line.x.bottom = element_line(colour = "black"),
      axis.text = element_text(size = 12, colour = "black")
    ) +
    scale_x_continuous(
      expand = expansion(mult = 0.01, add = 0),
      limits = x_axis_limits,
      labels = scales::percent
    )

    if (isFALSE(include_legend)) {
      cost_plot <- cost_plot + theme(legend.position = "none")
    }
  
  cost_plot
}