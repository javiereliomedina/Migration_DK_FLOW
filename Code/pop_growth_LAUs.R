###**************************************
###     Population changes by LAUs ######
###**************************************

  library(danstat) 
  library(tidyverse)
  library(forcats)
  library(readr)
  library(dint)
  library(gganimate)
  library(patchwork)
  library(gridExtra)
  library(furrr)
  library(forcats)
  library(sf)
  library(giscoR)
  # install.packages("devtools")
  # devtools::install_github("yutannihilation/ggsflabel")
  library(ggsflabel)

# Define theme for ggplot2
    theme_plot <- function() {
      theme_bw() +
        theme(axis.text = element_text(size = 7),
              axis.title = element_text(size = 9),
              legend.title = element_text(size = 9, face = "bold"),
              plot.title = element_text(size = 11, face = "bold"),
              title = element_text(size = 9))
    }

# Load data ----
#' Data from Statistic Denmark: https://www.statbank.dk/10021 
#' Table: FOLK1C
#' Population at the first day of the quarter by region, sex, age (5 years age groups), ancestry and country of origin
#' Subjects of interest: population and elections (02)
#' Immigrants and their descendants (2402)

  id_table <- "FOLK1C"
  var_pop <- get_table_metadata(table_id = id_table, variables_only = TRUE)
  var_pop %>% select(id, text)
  var_pop$values

# loop by quarter for getting the data 
  steps <- function(quarter){
    var_values <- list(id_region, id_ancestry, quarter)
    var_input <- purrr::map2(.x = var_codes, .y = var_values, .f = ~list(code = .x, values = .y))
    get_data(id_table, variables = var_input)
  }

# Codes for var_input
  var_codes <- c("OMRÅDE", "HERKOMST", "Tid")
  
  # Values for var_input
  ## Region: Denmark
  id_region <- as.numeric(var_pop$values[[1]]$id)
  id_region <- id_region[id_region > 100]
  ## Ancestry
  id_ancestry <- NA
  ## Quarters
  id_quarter <- var_pop$values[[6]]$id   # Select all quarters
  # Parallel process with {future}
  plan(multiprocess)  
  pop_LAU <- id_quarter %>% future_map(steps)
  pop_LAU <- bind_rows(pop_LAU)
  plan("default")

# Prepare data ----
  
# Clean column names and format some data 
  pop_LAU %>% 
    rename(LAU_NAME = OMRÅDE,
           ancestry = HERKOMST,
           date = TID, 
           pop = INDHOLD) %>% 
    mutate(date = gsub("Q", "", date),
           date = as_date_yq(as.integer(date)),
           date = first_of_quarter(date)) %>% 
    mutate(LAU_NAME = gsub("Copenhagen", "København", LAU_NAME),
           ancestry = ifelse(ancestry == "Persons of Danish origin", "Danish", ancestry)) %>% 
    # Short LAUs by Total population in 2020-Q4
    pivot_wider(c(LAU_NAME, date), names_from = ancestry, values_from = pop) %>% 
    mutate(LAU_NAME = factor(LAU_NAME), 
           LAU_NAME = fct_reorder2(LAU_NAME, date, Total, .fun = last2)) %>% 
    pivot_longer(cols = c(Total, Danish, Immigrants, Descendant),
               names_to = "ancestry",
               values_to = "pop") %>% 
    # Shorst ancestry 
    mutate(ancestry = factor(ancestry), 
           ancestry = fct_relevel(ancestry, "Immigrants", after = 1)) %>% 
    # Standardize population growth to % change with 2008-Q1 as baseline
    group_by(LAU_NAME, ancestry) %>% 
    arrange(LAU_NAME, date) %>% 
    mutate(pop_pct_2008 = (pop/first(pop) - 1) * 100) %>% 
    ungroup() -> pop_LAU
  
# Plot population variation  by LAU ----

# Total population growth 
  pop_LAU %>% 
    # filter(ancestry == "Total") %>% 
    ggplot(aes(x = date,
               y = pop/1000,
               colour = ancestry)) +
    geom_line( ) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey") + 
    facet_wrap(~LAU_NAME, ncol = 8, scale = "free") +
    theme_bw() +
    theme(legend.position = "bottom",
          axis.text = element_text(size = 10),
          axis.title = element_text(size = 14),
          legend.text = element_text(size = 14),
          legend.title = element_text(size = 14, face = "bold"),
          plot.title = element_text(size = 14, face = "bold"),
          title = element_text(size = 14))  +
    labs(title = "Population change by LAU (2008 - 2020)",
         caption = "Data source: Statistics Denmark\nAuthor: J. Elio (@Elio_Javi), C. Keßler, H.S. Hansen. Aalborg University, Department of Planning") +
    scale_y_continuous(name = "[x1000]") +
    scale_x_date(name = "", date_breaks = "3 year", date_labels = "%y") +
    scale_colour_manual(name = "Ancestry",
                        values = c("#0072B2", "#009E73", "#D55E00", "#000000"))
  
  ggsave("Results/pop_growth_lau_2008_2020_tot.png", width = 40, height = 60, units = "cm")
  
# Population change by LAU and ancestry (Danish origin, Immigrants, Descendant) 
  pop_LAU %>% 
    ggplot(aes(x = date,
               y = pop_pct_2008,
               colour = ancestry)) +
    geom_line( ) +
    facet_wrap(~LAU_NAME, ncol = 8) +
    theme_bw() +
    theme(legend.position = "bottom",
          axis.text = element_text(size = 10),
          axis.title = element_text(size = 14),
          legend.text = element_text(size = 14),
          legend.title = element_text(size = 14, face = "bold"),
          plot.title = element_text(size = 14, face = "bold"),
          title = element_text(size = 14)) +
    labs(title = "Population change by LAU and ancestry (2008 - 2020)",
         caption = "Data source: Statistics Denmark\nAuthor: J. Elio (@Elio_Javi), C. Keßler, H.S. Hansen. Aalborg University, Department of Planning") +
    scale_y_continuous(name = "[%]", limits = c(-25, 120)) +
    scale_x_date(name = "", date_breaks = "3 year", date_labels = "%y") +
    scale_colour_manual(name = "Ancestry",
                        values = c("#0072B2", "#009E73", "#D55E00", "#000000"))

  ggsave("Results/pop_growth_lau_2008_2020_pct.png", width = 40, height = 60, units = "cm")
  