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
## Data from Statistic Denmark: https://www.statbank.dk/10021 (load with {danstat})
  get_subjects()

## Subjects of interest (Population and elections - 02)
  subj <- get_subjects(subjects = "02")
  sub_subj <- subj$subjects %>% bind_rows()
  sub_subj

## Getting table information and variables
## immigrants and their descendants (2402)
  tables <- get_tables(subjects = "2402")
  tables %>% select(id, text, variables) 

## Metadata table of interest (e.g. KMSTA001)
  var_pop <- get_table_metadata(table_id = "FOLK1B", variables_only = TRUE)
  var_pop %>% select(id, text)
  var_pop$values

## Ged data
## API - Max. number of selected data = 100000
## loop by quarter for getting the data (without age groups)
  steps <- function(quarter){
    var_values <- list(id_muni, id_gender, id_citizen, quarter)
    var_input <- purrr::map2(.x = var_codes, .y = var_values, .f = ~list(code = .x, values = .y))
    get_data("FOLK1B", variables = var_input)
  }
  
  # Codes for var_input
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
  