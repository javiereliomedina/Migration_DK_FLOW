###**************************************
###     Population changes by LAUs ######
###**************************************

  library(danstat) 
  library(tidyverse)
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

  
  source("Code/theme_plot.R")

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
    mutate(LAU_NAME = gsub("Copenhagen", "København", LAU_NAME)) %>%  
    # Format ancestry 
    mutate(ancestry = ifelse(ancestry == "Persons of Danish origin", "Danish", ancestry),
           ancestry = factor(ancestry), 
           ancestry = fct_relevel(ancestry, "Immigrants", after = 1)) -> pop_LAU 
  
# Standardize population growth to % change with 2008-Q1 as baseline
  pop_LAU %>% 
    group_by(LAU_NAME, ancestry) %>% 
    arrange(LAU_NAME, date) %>% 
    mutate(pop_pct_2008 = (pop/first(pop) - 1) * 100) %>% 
    ungroup() -> pop_LAU
  
# Plot population variation  by LAU ----

# Total population growth 
  pop_LAU %>% 
    filter(ancestry == "Total") %>% 
    ggplot(aes(x = date,
               y = pop_pct_2008)) +
    geom_line( colour = "#0072B2" ) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey") + 
    facet_wrap(~LAU_NAME, ncol = 8) +
    theme_bw() +
    theme(legend.position = "bottom",
          axis.text = element_text(size = 10),
          axis.title = element_text(size = 14),
          legend.text = element_text(size = 14),
          legend.title = element_text(size = 14, face = "bold"),
          plot.title = element_text(size = 14, face = "bold"),
          title = element_text(size = 14))  +
    labs(title = "Population change by LAU (2008 - 2020)",
         caption = "Source: Statistics Denmark") +
    scale_y_continuous(name = "[%]") +
    scale_x_date(name = "", date_breaks = "3 year", date_labels = "%y") 
  
  ggsave("Results/pop_growth_lau_tot_2008_2020.png", width = 40, height = 60, units = "cm")
  
# Population change by LAU and ancestry (Danish origin, Immigrants, Descendant) 
  pop_LAU %>% 
    filter(ancestry != "Total") %>% 
    ggplot(aes(x = date,
               y = pop_pct_2008,
               colour = ancestry)) +
    geom_line( ) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey") + 
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
         caption = "Source: Statistics Denmark") +
    scale_y_continuous(name = "[%]", limits = c(-25, 150)) +
    scale_x_date(name = "", date_breaks = "3 year", date_labels = "%y") +
    scale_colour_manual(name = "Ancestry", values = c("#0072B2", "#009E73", "#D55E00"))

  ggsave("Results/pop_growth_lau_2008_2020.png", width = 40, height = 60, units = "cm")
  
# Spatial analysis  ----

# Local Administrative Units (LAU)
  options(gisco_cache_dir = "C:/Users/FU53VP/OneDrive - Aalborg Universitet/Dokumenter/GISCO_spatial_data")
  dk_lau <- gisco_get_lau(year = "2019", country = "DNK") %>%
    mutate(LAU_NAME = str_conv(LAU_NAME, "UTF-8")) %>% 
    arrange(LAU_NAME)

# Link population with LAUs
  dk_lau %>% 
    left_join(pop_LAU, by = "LAU_NAME") %>% 
    st_as_sf() %>% 
    mutate(date = as.Date(date)) -> dk_lau_pop
  
# Big cities/urban areas
  big_cities <- c("København", "Aarhus", "Odense", "Aalborg")
  big_cities <- dk_lau %>% filter(LAU_NAME %in% big_cities)
  
# Plot population change (2008 - 2020)

## Total population by LAUs
  ggplot() +
    geom_sf(data = filter(dk_lau_pop, date == as.Date("2020-07-01"), ancestry == "Total"),
            aes(fill = pop_pct_2008),
            color = "grey",
            size = 0.05) +
    scale_fill_gradient2(name = "Change [%]",
                         low = "red",
                         mid = "white",
                         high = "blue",
                         midpoint = 0) +
    labs(title = "Danish population change by LAUs",
         subtitle = "Period: 2008 - 2020") +
    theme_plot() +
    ylim(54.50, 58.0) +
    geom_sf_label_repel(data = big_cities,
                        aes(label = LAU_NAME),
                        force = 10,
                        nudge_y = 3,
                        nudge_x = 0.5,
                        seed = 10
                        )

  ggsave("Results/pop_growth_lau_spatial_total_2008_2020.png",
         width = 15,
         height = 15,
         units = "cm")

## Differentiate ancestry
  
  # Define theme for ggplot2
  theme_maps <- function() {
    theme_bw() +
      theme(legend.position = "bottom",
            axis.title = element_blank(),
            axis.text  = element_blank(),
            axis.ticks = element_blank(),
            legend.title = element_text(size = 18),
            legend.text = element_text(size = 12),
            plot.title = element_text(size = 18),
            title = element_text(size = 18),
            legend.key.width = unit(3, "cm"))
  }
  
  # Persons of Danish origin
  ggplot() +
    geom_sf(data = filter(dk_lau_pop, date == as.Date("2020-07-01"), ancestry == "Danish"),
            aes(fill = pop_pct_2008),
            color = "grey",
            size = 0.05) +
    scale_fill_gradient2(name = "[%]",
                         low = "red",
                         mid = "white",
                         high = "blue",
                         midpoint = 0) +
    labs(title = "Persons of Danish origin") +
    theme_maps() -> p_danish
  
  # Immigrants 
  ggplot() +
    geom_sf(data = filter(dk_lau_pop, date == as.Date("2020-07-01"), ancestry == "Immigrants"),
            aes(fill = pop_pct_2008),
            color = "grey",
            size = 0.05) +
    scale_fill_gradient2(name = "[%]",
                         low = "red",
                         mid = "white",
                         high = "blue",
                         midpoint = 0) +
    labs(title = "Immigrants") +
    theme_maps() -> p_migr
  
  # Descendant
  ggplot() +
    geom_sf(data = filter(dk_lau_pop, date == as.Date("2020-07-01"), ancestry == "Descendant"),
            aes(fill = pop_pct_2008),
            color = "grey",
            size = 0.05) +
    scale_fill_gradient2(name = "[%]",
                         low = "red",
                         mid = "white",
                         high = "blue",
                         midpoint = 0) +
    labs(title = "Descendants") +
    theme_maps() -> p_desc

# Export plots
  (p_danish + p_migr + p_desc)
  ggsave("Results/pop_growth_lau_spatial_ancestry_2008_2020.png",
         width = 60,
         height = 20,
         units = "cm")
  
## Animation ----
  anim <- ggplot() +
    # Not sure why I got an error if all the data are selected
    # and I've removed the first date (baseline: 2009-Q1)
    geom_sf(data = filter(dk_lau_pop, date >= "2009-01-01", ancestry == "Total"),
            aes(fill = pop_pct_2008),
            color = "grey",
            size = 0.05) +
    scale_fill_gradient2(name = "Change [%]",
                         low = "red",
                         mid = "white",
                         high = "blue",
                         midpoint = 0) +
    labs(title = "Danish population by LAUs",
         subtitle = "{closest_state}") +
    transition_states(date, wrap = FALSE) +
    theme_bw() + 
    ylim(54.50, 58.0) +
    geom_sf_label_repel(data = big_cities,
                        aes(label = LAU_NAME),
                        force = 10,
                        nudge_y = 3,
                        nudge_x = 0.5,
                        seed = 10)
  
  # export as .gif
  anim_save("Results/pop_lau_2008_2020_anim.gif", anim)
  