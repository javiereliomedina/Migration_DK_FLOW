###********************
###   MIGRANTS     ####
###********************

  library(danstat) 
  library(tidyverse)
  library(readr)
  library(dint)
  library(gganimate)
  library(patchwork)
  library(gridExtra)
  library(forcats)
  library(RColorBrewer)
  library(furrr)

# Load data ----
## Population by citizenship (Table FOLK1B)
## Data from Statistic Denmark: https://www.statbank.dk/10021
## Subjects of interest: Population and elections (02)
## Immigrants and their descendants (2402)
## Table FOLK1B (Population at the first day of the quarter by region, sex, age (5 years age groups) and citizenship (2008Q1-2020Q3))
## Get data without age groups 
  
# loop by quarter for getting the data 
  steps <- function(quarter){
    var_values <- list(id_muni, id_gender, id_citizen, quarter)
    var_input <- purrr::map2(.x = var_codes, .y = var_values, .f = ~list(code = .x, values = .y))
    get_data("FOLK1B", variables = var_input)
  }
  
# Codes for var_input
  var_pop <- get_table_metadata(table_id = "FOLK1B", variables_only = TRUE)
  var_pop %>% select(id, text)
  var_pop$values
 
   var_codes <- c("OMRÅDE", "KØN", "STATSB", "Tid")
  # Values for var_input
  ## Data by LAUs
  id_muni <- as.numeric(var_pop$values[[1]]$id)
  id_muni <- id_muni[id_muni > 100]
  ## Gender: Men/Women
  id_gender <- NA
  ## Data by countries (remove total)
  id_citizen <- as.numeric(var_pop$values[[4]]$id)
  id_citizen <- id_citizen[id_citizen > 0]
  ## Quarters
  id_quarter <- var_pop$values[[5]]$id 
  # Parallel process with {future}
  plan(multiprocess)  
  pop_ctzn <- id_quarter %>% future_map(steps)
  pop_ctzn <- bind_rows(pop_ctzn)
  plan("default")
  
# Clean column names and format some data
  pop_ctzn %>% 
    filter(INDHOLD > 0) %>% 
    rename(LAU_NAME = OMRÅDE,
           Gender = KØN,
           Citizen = STATSB,
           Date = TID, 
           Pop = INDHOLD) %>% 
      mutate(LAU_NAME = gsub("Copenhagen", "København", LAU_NAME)) %>% 
      mutate(Date = gsub("Q", "", Date),
             Date = as_date_yq(as.integer(Date)),
             Date = first_of_quarter(Date)) -> pop_ctzn
  
# Foreign population ---- 
  pop_ctzn %>% 
    filter(Citizen != "Denmark") %>% 
    # Add pop in percentage
    group_by(LAU_NAME, Date) %>% 
    mutate(Pop_pct = Pop / sum(Pop) * 100) %>% 
    ungroup() -> pop_migr

## Summarise data for all Denmark ----
  pop_migr %>% 
    group_by(Gender, Citizen, Date) %>% 
    summarise(Pop = sum(Pop)) %>% 
    ungroup() %>% 
    group_by(Date) %>% 
    mutate(Pop_pct = 100 * Pop/sum(Pop)) %>% 
    arrange(Citizen, Date, Pop_pct) -> pop_migr_DK
  
## Plot changes over time (by gender)
## Only plot counties with a population >8000 at any time
  pop_migr_DK %>%
    pivot_wider(c(Citizen, Date), names_from = Gender, values_from = Pop) %>% 
    group_by(Date) %>%
    filter(Total > 14000) -> pop_migr_14k
  
  pop_migr_14k %>%
    ggplot() +
    geom_line(aes(x = Date, y = Total/1000, colour = Citizen)) +
    theme_bw() +
    labs(title = "Foreign citizens in Denmark", 
         subtitle = "Population higher than 14k",
         y = "Pop [x1000]") +
    theme(axis.text = element_text(size = 8),
          axis.title = element_text(size = 11, face ="bold")) +
    scale_color_brewer(palette = "Paired") -> p1
  
  pop_migr_14k %>%
    select(-Total) %>% 
    pivot_longer(cols = -c(Citizen, Date), names_to = "Gender", values_to = "Pop") %>% 
    ggplot() +
    geom_line(aes(x = Date, y = Pop/1000, colour = Citizen)) +
    facet_grid(~Gender) +
    theme_bw() +
    labs(subtitle = "Divided by gender", 
         y = "Pop [x1000]") +
    theme(axis.text = element_text(size = 8),
          axis.title = element_text(size = 11, face ="bold")) +
    scale_color_brewer(palette = "Paired") -> p2
  
  p1 / p2 + plot_layout(guides = "collect") 
  ggsave("Results/pop_migr_countries_2008_2020.png", width = 20, height = 20, units = "cm")
  