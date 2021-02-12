###*******************************************
###   Population changes by LAUs   (maps)  ###
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
  library(RColorBrewer)
  
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
  options(gisco_cache_dir = "C:/GISCO_spatial_data")
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

# Map ----
  
## Differentiate by ancestry
## Ancestry: Total population, Persons of Danish origin, Immigrants, and Descendants
  
  # Annotate cities only in the "Total" facet
  big_cities <- big_cities %>%  
    mutate(ancestry = "Total", 
           ancestry = factor(ancestry, levels = levels(dk_lau_pop$ancestry)))
  
  # Transform Percentage of change to factor 
  pct_breaks <- c(floor(min(dk_lau_pop$pop_pct_2008)), -10, -5, 
                  0, 5, 10, 20, 30, 50, 100, 200, max(dk_lau_pop$pop_pct_2008))
  
  dk_lau_pop %>% 
    mutate(pop_pct_2008_brk = cut(dk_lau_pop$pop_pct_2008,
                                  breaks = pct_breaks)) %>% 
    mutate(ancestry = factor(ancestry),
           ancestry = fct_relevel(ancestry, "Total", after = 0))-> dk_lau_pop
  
  # Plot
  myPallette <- c(rev(brewer.pal(3, "YlOrRd")), brewer.pal(9, "Blues"))
  ggplot() +
    geom_sf(data = filter(dk_lau_pop,
                          date == as.Date("2020-07-01")),
    aes(fill = pop_pct_2008_brk),
    color = "grey50",
    size = 0.05) +
    scale_fill_manual(name = "Percentage", 
                      values = myPallette,
                      drop = FALSE) +
    labs(title = "Danish population change by LAUs",
         subtitle = "Period: 2008 - 2020",
         caption = "Data source: Statistics Denmark\nAuthor: J. Elio (@Elio_Javi), C. Keßler, H.S. Hansen. Aalborg University, Department of Planning",
         x = "", 
         y = "") +
    theme_plot() +
    ylim(54.50, 58.0) +
    geom_sf_label_repel(data = big_cities,
                        aes(label = LAU_NAME),
                        force = 10,
                        nudge_y = 3,
                        nudge_x = 0.5,
                        seed = 10) +
    facet_wrap( ~ancestry, ncol = 2)
  
  # Export plots
  ggsave("Results/pop_growth_lau_spatial_ancestry_2008_2020.png",
         width = 30,
         height = 30,
         units = "cm")
