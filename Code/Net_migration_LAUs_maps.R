
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
  library(viridis)
  library(ggsflabel)
  
# Load data ----

## Net migration rate by LAU (KMSTA001) ----
## Data from Statistic Denmark: https://www.statbank.dk/10021 
## BEV107: Summary vital statistics by municipality, new increases/stock and sex
  
  id_table <- "BEV107"
  dat_meta <- get_table_metadata(table_id = id_table, variables_only = TRUE)
  dat_meta %>% select(id, text)
  dat_meta$values
    
  # Codes for var_input
  var_codes <- c("OMRÅDE", "BEVÆGELSE", "Tid")
  
  # Values for var_input
  ## All regions
  id_region <- NA
  ## Net migration, population ultimo the previous year and ultimo the present year
  id_movement <- c("B10", "B01A", "B20A")
  ## Years 
  id_time <- seq(2008, 2019, 1)
  
  var_values <- list(id_region, id_movement, id_time)
  var_input <- purrr::map2(.x = var_codes, .y = var_values, .f = ~list(code = .x, values = .y))
  
  # get data
  dat_migr <- get_data(id_table, variables = var_input)
  
# Prepare data ----
  dat_migr <- dat_migr %>% 
    # Translate column names into English
    rename(region = OMRÅDE,
           movement = BEVÆGELSE,
           date = TID, 
           count = INDHOLD) %>% 
    # Change description 
    mutate(movement = case_when(
      movement == "Netmigration"                        ~ "net_migr",
      movement == "Population ultimo the previous year" ~ "pop_start",
      movement == "Population ultimo the present year"  ~ "pop_end")) %>% 
    # Tidy table
    pivot_wider(names_from = movement, values_from = count) %>% 
    # Mid-year population
    mutate(pop_mid = 0.5 * (pop_start + pop_end)) %>% 
    # Net migration rate (per 1000 inhabitants)
    mutate(net_migr_rate = 1000 * net_migr / pop_mid) %>% 
    # Cumulative net migration
    group_by(region) %>% 
    arrange(region, date) %>% 
    mutate(cum_net_migr = cumsum(net_migr)) %>% 
    ungroup()
  
# Add spatial information ----

## Local Administrative Units (LAU) ----
  options(gisco_cache_dir = "C:/GISCO_spatial_data")
  dk_lau <- gisco_get_lau(year = "2019", country = "DNK") %>%
    mutate(LAU_NAME = str_conv(LAU_NAME, "UTF-8")) %>% 
    # Translate "København" to "Copenhagen"
    mutate(LAU_NAME = gsub("København", "Copenhagen", LAU_NAME))
  
## Join both datasets by (region - LAU_NAME)  
  sp_dat_migr <- dk_lau %>% 
     left_join(dat_migr, by = c("LAU_NAME" = "region")) %>% 
     st_as_sf()  

# Plot net migration rate in 2019 ----
  
  # Big cities/urban areas
  big_cities <- c("Copenhagen", "Aarhus", "Odense", "Aalborg")
  big_cities <- dk_lau %>% filter(LAU_NAME %in% big_cities)
  
  # Plot
  ggplot() +
    geom_sf(data = filter(sp_dat_migr, date == 2019),
            aes(fill = net_migr_rate),
            color = "grey",
            size = 0.05) +
    scale_fill_gradient2(name = "Rate\n[per 1000 inhabitants]",
                         low = "red",
                         mid = "white",
                         high = "blue",
                         midpoint = 0) +
    labs(title = "Net migration rate in 2019",
         subtitle = "Denmark's municipalities",
         caption = "Data source: Statistics Denmark\nAuthors: J. Elio (@Elio_Javi), C. Keßler, H.S. Hansen. Aalborg University, Department of Planning",
         x = "",
         y = "") +
    geom_sf_label_repel(data = big_cities,
                        aes(label = LAU_NAME),
                        nudge_x = c(-0.5, 1.5, 1.5, -1),
                        nudge_y = c(  -1,   0, 0.2, 0.5),
                        seed = 10) +
    theme_bw()
    
# Cumulative net migration from 2008 to 2019 ----
  ggplot() +
    geom_sf(data = filter(sp_dat_migr, date == 2019),
            aes(fill = cum_net_migr/1000),
            color = "grey",
            size = 0.05) +
    scale_fill_viridis(name = "Pop\n[x1000]", option = "magma", direction = -1) +
    labs(title = "Cummulative net migration (2008-2019)",
         subtitle = "Denmark's municipalities",
         caption = "Data source: Statistics Denmark\nAuthors: J. Elio (@Elio_Javi), C. Keßler, H.S. Hansen. Aalborg University, Department of Planning",
         x = "",
         y = "") +
    geom_sf_label_repel(data = big_cities,
                        aes(label = LAU_NAME),
                        nudge_x = c(-0.5, 1.5, 1.5, -1),
                        nudge_y = c(  -1,   0, 0.2, 0.5),
                        seed = 10) +
    theme_bw()
  
# Animation cumulative net migration ----
  ggplot() +
    geom_sf(data = sp_dat_migr,
            aes(fill = cum_net_migr/1000),
            color = NA,
            size = 0.05) +
    scale_fill_viridis(name = "Pop\n[x1000]", option = "magma", direction = -1) +
    labs(title = "Cummulative net migration (Denmark's municipalities)",
         subtitle = "{closest_state}",
         x = "", 
         y = "") +
    transition_states(date, wrap = FALSE) +
    geom_sf_label_repel(data = big_cities,
                        aes(label = LAU_NAME),
                        nudge_x = c(-0.5, 1.5, 1.5, -1),
                        nudge_y = c(  -1,   0, 0.2, 0.5),
                        seed = 10) +
    theme_bw()
  
  