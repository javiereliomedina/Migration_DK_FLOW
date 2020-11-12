# Define theme for ggplot2

  theme_plot <- function() {
    theme_bw() +
      theme(axis.text = element_text(size = 5),
            axis.title = element_text(size = 7, face = "bold"),
            legend.title = element_text(size = 8),
            title = element_text(size = 8, face = "bold"))
  }
  