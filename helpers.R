# Utility function to make a stacked barplot in the cost analyses
make_costs_barplot <- function(x,
                               title = NULL,
                               subtitle = NULL,
                               labs_x = NULL,
                               y_axis_labels = TRUE,
                               x_axis_digits = 0,
                               include_legend = FALSE,
                               flip_plot = FALSE) {

  if (flip_plot) {
    x <- x %>%
      mutate(cost_var = -cost_var)
  }
  
  tmp <- x %>%
    ggplot(aes(x = cost_var, y = var_name_label, fill = cost_component_label)) +
    geom_bar(position = "stack", stat = "identity") +    
    theme_bw()
  
  if (flip_plot) {
    tmp <- tmp +
            scale_x_continuous(
        expand = expansion(mult = 0.01, add = 0),
        labels = function(x) formatC(-x, format = "f", big.mark = ",", digits = x_axis_digits)
      )
  } else {
    tmp <- tmp + 
      scale_x_continuous(
        expand = expansion(mult = 0.01, add = 0),
        labels = function(x) formatC(x, format = "f", big.mark = ",", digits = x_axis_digits)
      ) 
  }

  tmp <- tmp +
    scale_y_discrete(expand = c(0, 0)) +
    # cvd-friendly qualitative color palette from "Fundamentals of Data
    # Visualization" figure 19.10
    scale_fill_manual(values = c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00")) +
    labs(
      title = title,
      subtitle = subtitle,
      x = labs_x
    ) +
    theme(legend.position = "bottom",
          panel.grid.major.y = element_blank(),
          axis.title.y = element_blank(),
          axis.title.x = element_text(size = 14),
          axis.text = element_text(size = 12, colour = "black"),
          axis.text.y = element_text(hjust = 0.5, vjust = 0.5),
          axis.ticks.y = element_blank(),
          axis.line.x.bottom = element_line(colour = "black"),
          panel.border = element_blank(),
          legend.title = element_blank(),
          legend.direction = "vertical",
          plot.title = element_text(size = 20),
          plot.subtitle = element_text(size = 18)
    ) +
    guides(fill = guide_legend(nrow = 1, reverse = TRUE, byrow = TRUE))
  
  if (isFALSE(y_axis_labels)) {
    tmp <- tmp +
      theme(axis.text.y = element_blank())
  }
  
  if (isFALSE(include_legend)) {
    tmp <- tmp +
      theme(legend.position = "none")
  }
  
  tmp
}

