###**************************************
###   Immigrants and Descendants     ####
###**************************************

  library(danstat) 
  library(tidyverse)
  library(readr)
  library(dint)
  library(patchwork)
  library(gridExtra)
  library(forcats)
  library(RColorBrewer)
  library(furrr)
  library(sf)
  library(giscoR)
  library(viridis)
  
  if(!require("devtools"))  install.packages("devtools")
  if(!require("ggsflabel")) devtools::install_github("yutannihilation/ggsflabel")
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
  
## Population data from Statistic Denmark ----
## Data from Statistic Denmark: https://www.statbank.dk/10021 
## Table: FOLK1C
## Population at the first day of the quarter by region, ancestry, and country of origin
## Subjects of interest: population and elections (02)
## Immigrants and their descendants (2402)

  id_table <- "FOLK1C"
  var_pop <- get_table_metadata(table_id = id_table, variables_only = TRUE)
  var_pop %>% select(id, text)
  var_pop$values
  
  # loop by quarter for getting the data 
  steps <- function(quarter){
    var_values <- list(id_region, id_ancestry, id_citizen, quarter)
    var_input <- purrr::map2(.x = var_codes, .y = var_values, .f = ~list(code = .x, values = .y))
    get_data(id_table, variables = var_input)
  }
  
  # Codes for var_input
  var_codes <- c("OMRÅDE", "HERKOMST", "IELAND", "Tid")
  
  # Values for var_input
  ## Region: Denmark
  id_region <- NA
  ## Ancestry (Immigrants and Descendant)
  id_ancestry <- c(4, 3)
  ## Country of origin (remove total)
  id_citizen <- as.numeric(var_pop$values[[5]]$id)
  id_citizen <- id_citizen[id_citizen > 0]
  ## Quarters
  # id_quarter <- var_pop$values[[6]]$id   # Select all quarters
  id_quarter <- "2020K4"                 # Select only 2020-Q4
  # Parallel process with {future}
  plan(multiprocess)  
  pop_migr <- id_quarter %>% future_map(steps)
  pop_migr <- bind_rows(pop_migr)
  plan("default")

  # Clean column names and format some data
  pop_migr %>% 
    rename(region = OMRÅDE,
           ancestry = HERKOMST,
           origin = IELAND,
           date = TID, 
           pop = INDHOLD) %>% 
    mutate(region = gsub("Copenhagen", "København", region)) %>% 
    mutate(date = gsub("Q", "", date),
           date = as_date_yq(as.integer(date)),
           date = first_of_quarter(date)) -> pop_migr
  
  # Total Immigrants and Descendants by region, date, and country of origin
  pop_migr %>% 
    pivot_wider(c(region, origin, date), names_from = ancestry, values_from = pop) %>% 
    mutate(Total = Immigrants + Descendant) %>% 
    pivot_longer(cols = c(Immigrants, Descendant, Total),
                 values_to = "pop",
                 names_to = "ancestry") -> pop_migr

# Spatial distribution ----

## Local Administrative Units (LAU) ----
  options(gisco_cache_dir = "C:/GISCO_spatial_data")
  dk_lau <- gisco_get_lau(year = "2019", country = "DNK") %>%
    mutate(LAU_NAME = str_conv(LAU_NAME, "UTF-8")) %>% 
    arrange(LAU_NAME)
  
  # Join Foreign population (pop_migr) and municipalities (dk_lau) 
  # Select only data from 2020-Q4
  dk_lau %>% 
    left_join(filter(pop_migr, date == as.Date("2020-10-01")),
              by = c("LAU_NAME" = "region")) %>% 
    st_as_sf() -> pop_migr_lau_2020Q4
  
  # Big cities/urban areas
  big_cities <- c("København", "Aarhus", "Odense", "Aalborg", "Frederiksberg", "Vejle")
  big_cities <- dk_lau %>% filter(LAU_NAME %in% big_cities)

# Plot total foreign citizens by LAU ----

## Total foreign population by LAU (and percentage over the total migrants)
## Binds represent the same percentage of population (~20%)

  cumsum__breaks <- c(0, 25, 50, 75, 100)
  
  pop_migr_lau_2020Q4 %>% 
    filter(ancestry == "Total") %>% 
    group_by(LAU_NAME) %>% 
    summarise(pop = sum(pop, na.rm = TRUE)) %>% 
    arrange(-pop) %>% 
    ungroup() %>% 
    # LAU groups with the same percentage of migrants (~ 20%)
    mutate(pop_pct = 100 * pop/sum(pop),
           pop_pct_cum = cumsum(pop_pct),
           pop_pct_cum_brk = cut(pop_pct_cum, breaks = cumsum__breaks)) %>% 
    select(LAU_NAME, pop, pop_pct, pop_pct_cum, pop_pct_cum_brk) -> pop_migr_lau_2020Q4_total
  
  pop_migr_lau_2020Q4_total %>% 
    ggplot() +
    geom_sf(aes(fill = pop_pct_cum_brk),
            color = "grey",
            size = 0.05) + 
    scale_fill_brewer(name = "Cumulative [%]",
                      palette = "YlGnBu",
                      direction = -1) +
    labs(title = "Where do immigrans and their descendants live in Denmark?",
         subtitle = "Denmark's municipalities (Date: 2020-Q4)",
         caption = "Data source: Statistics Denmark\nAuthors: J. Elio (@Elio_Javi), C. Keßler, H.S. Hansen. Aalborg University, Department of Planning",
         x = "",
         y = "") +
    theme_plot() +
    ylim(54.50, 58.0) +
    geom_sf_label_repel(data = big_cities,
                        aes(label = LAU_NAME),
                        force = 10,
                        nudge_y = c( 1.5,  2,  1, -1, -1, -1.5),
                        nudge_x = c(  -1,  1,  1,  1,  0, -1.5),
                        seed = 10)
  
  ggsave("Results/pop_migrs_lau_2020.png", width = 15, height = 15, units = "cm")

## Top 6 citizen groups (and percentage over the same country of origin)
  cumsum__breaks <- c(0, 20, 40.0, 60.0, 80.0, 100)
  top_migr_groups <- c("Turkey", "Poland", "Syria", "Germany", "Romania", "Iraq")
  pop_migr_lau_2020Q4 %>% 
    filter(ancestry == "Total",
           origin %in% top_migr_groups) %>% 
    group_by(origin) %>% 
    arrange(-pop) %>% 
    mutate(pop_pct = 100 * pop / sum(pop, na.rm = TRUE),
           pop_pct_cum = cumsum(pop_pct),
           pop_pct_cum_brk = cut(pop_pct_cum, breaks = cumsum__breaks)) %>% 
    select(LAU_NAME, origin, pop, pop_pct, pop_pct_cum, pop_pct_cum_brk) -> pop_migr_lau_2020Q4_top_groups
  
  # facet by top countries, adding the total number of immigrants/descendants
  sum_pop <- function(country) {
    sum(pop_migr_lau_2020Q4_top_groups[pop_migr_lau_2020Q4_top_groups$origin == country, ]$pop, na.rm = T)
  }
  levels <- top_migr_groups
  labels <- c(paste("Turkish citizens \n(" , sum_pop("Turkey") , " inhabitants)", sep = ""),
              paste("Polish citizens \n("  , sum_pop("Poland") , " inhabitants)", sep = ""),
              paste("Syrian citizens \n("  , sum_pop("Syria")  , " inhabitants)", sep = ""),
              paste("German citizens \n("  , sum_pop("Germany"), " inhabitants)", sep = ""),
              paste("Romanian citizens \n(", sum_pop("Romania"), " inhabitants)", sep = ""),
              paste("Iraqi citizens \n("   , sum_pop("Iraq"), " inhabitants)", sep = "")
  )
  
  pop_migr_lau_2020Q4_top_groups %>% 
    mutate(origin = factor(origin, levels = levels, labels = labels)) %>% 
    ggplot() +
    geom_sf(aes(fill = pop_pct_cum_brk), color = "grey", size = 0.05) +
    scale_fill_brewer(name = "Cumulative [%]",
                      palette = "YlGnBu",
                      direction = -1) +
   labs(title = "Top countries of origin for immigrants and their descendants",
         subtitle = "Denmark's municipalities (Date: 2020-Q4)",
         caption = "Data source: Statistics Denmark\nAuthors: J. Elio (@Elio_Javi), C. Keßler, H.S. Hansen. Aalborg University, Department of Planning") +
    facet_wrap(~ origin, ncol = 2) +
    theme_plot()
  
  ggsave("Results/pop_migrs_lau_top_2020.png", width = 21, height = 27.7, units = "cm")

