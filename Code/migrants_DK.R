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
#' Population at the first day of the quarter by region, ancestry, and country of origin
#' Subjects of interest: population and elections (02)
#' Immigrants and their descendants (2402)

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
  id_quarter <- var_pop$values[[6]]$id   # Select all quarters
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

# Plot changes over time for all Denmark ----
# Plot only the evolution of the top 10 countries in 2020  
  pop_migr %>%
    filter(region == "All Denmark",
           date == as.Date("2020-10-01"),
           ancestry == "Total") %>%
    slice_max(pop, n = 10) -> pop_migr_top10
  
  pop_migr %>%
    filter(region == "All Denmark", 
           origin %in% pop_migr_top10$origin) %>% 
  # Reorder factors (shorted by Total population in 2020-Q3) 
    pivot_wider(c(origin, date), names_from = ancestry, values_from = pop) %>% 
    mutate(origin = factor(origin), 
           origin = fct_reorder2(origin, date, Total)) -> pop_migr_top10 
  
  # Total
  pop_migr_top10 %>% 
    ggplot() +
    geom_line(aes(x = date, y = Total/1000, colour = origin)) +
    theme_plot() +
    labs(title = "Immigrants and descendants in Denmark", 
         subtitle = "Top 10 countries of origin in 2020",
         y = "Pop [x1000]",
         x = "") +
    scale_color_brewer(name = "Country of\norigin", palette = "Paired") -> p1
  
  # Immigrants and Descendants
  pop_migr_top10 %>% 
    select(-Total) %>% 
    pivot_longer(cols = c(Immigrants, Descendant), names_to = "ancestry", values_to = "pop") %>%
    mutate(ancestry = factor(ancestry, levels = c("Immigrants","Descendant"))) %>% 
    ggplot() +
    geom_line(aes(x = date, y = pop/1000, colour = origin)) +
    facet_grid(~ancestry) +
    theme_plot() +
    labs(subtitle = "Divided by ancestry", 
         caption = "Data source: Statistics Denmark\nAuthor: J. Elio (@Elio_Javi), C. Keßler, H.S. Hansen. Aalborg University, Department of Planning",
         y = "Pop [x1000]",
         x = "") +
    scale_color_brewer(name = "Country of\norigin", palette = "Paired") -> p2
  
  p1 / p2 + plot_layout(guides = "collect")
    
  ggsave("Results/pop_migr_countries_2008_2020.png", width = 20, height = 20, units = "cm")
