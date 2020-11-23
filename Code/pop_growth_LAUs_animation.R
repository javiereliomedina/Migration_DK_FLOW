###*******************************************
###   Population changes by LAUs   (GIF)   ###
###*******************************************

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
  
# Load data ----

## Population data from Statistic Denmark ----
## https://www.statbank.dk/10021 
## Table: FOLK1C
## Population at the first day of the quarter by region, sex, age (5 years age groups), ancestry and country of origin
## Subjects of interest: population and elections (02)
## Immigrants and their descendants (2402)

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
    # Short LAUs by Total population in 20208-Q1
    pivot_wider(c(LAU_NAME, date), names_from = ancestry, values_from = pop) %>% 
    mutate(LAU_NAME = factor(LAU_NAME), 
           LAU_NAME = fct_reorder2(LAU_NAME, date, Total, .fun = first2)) %>% 
    pivot_longer(cols = c(Total, Danish, Immigrants, Descendant),
                 names_to = "ancestry",
                 values_to = "pop") %>% 
    # Short ancestry 
    mutate(ancestry = factor(ancestry), 
           ancestry = fct_relevel(ancestry, "Immigrants", after = 1)) %>%
    # Standardize population growth to % change with 2008-Q1 as baseline
    group_by(LAU_NAME, ancestry) %>% 
    arrange(LAU_NAME, date) %>% 
    mutate(pop_pct_2008 = (pop/first(pop) - 1) * 100) %>% 
    ungroup() -> pop_LAU

## Local Administrative Units (LAU) ----
  options(gisco_cache_dir = "C:/Users/FU53VP/OneDrive - Aalborg Universitet/Dokumenter/GISCO_spatial_data")
  dk_lau <- gisco_get_lau(year = "2019", country = "DNK") %>%
    mutate(LAU_NAME = str_conv(LAU_NAME, "UTF-8")) %>% 
    arrange(LAU_NAME)
  
  # Link population and LAUs
  dk_lau %>% 
    left_join(pop_LAU, by = "LAU_NAME") %>% 
    st_as_sf() %>% 
    mutate(date = as.Date(date)) -> dk_lau_pop

  # Big cities/urban areas
  big_cities <- c("København", "Aarhus", "Odense", "Aalborg")
  big_cities <- dk_lau %>% filter(LAU_NAME %in% big_cities)

# GIF: population variation (2008-2020) by LAU ----
  anim <- ggplot() +
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
         subtitle = "{closest_state}",
         x = "", 
         y = "") +
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
