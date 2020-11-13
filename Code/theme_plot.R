# Define theme for ggplot2

  theme_plot <- function() {
    theme_bw() +
      theme(axis.text = element_text(size = 7),
            axis.title = element_text(size = 9),
            legend.title = element_text(size = 9, face = "bold"),
            plot.title = element_text(size = 11, face = "bold"),
            title = element_text(size = 9))
  }
  