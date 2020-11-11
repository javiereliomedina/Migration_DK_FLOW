###*********************************
###   AGE STRUCTURE MIGRANTS    ####
###*********************************

  library(danstat) 
  library(tidyverse)
  library(readr)
  library(dint)
  library(gganimate)
  library(patchwork)
  library(gridExtra)

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
  var_pop <- get_table_metadata(table_id = "FOLK1D", variables_only = TRUE)
  var_pop %>% select(id, text)
  var_pop$values

## pull the needed data from the API
### Variables we would like to get data
  var_input <- list(list(code = "OMRÅDE", values = "000"),
                    list(code = "KØN", values = c(1,2)),
                    list(code = "ALDER", values = seq(0, 125, 1)),
                    list(code = "STATSB", values = c("DANSK", "UDLAND")),
                    list(code = "tid", values = NA))

### Get data 
  get_data("FOLK1D", variables = var_input) %>%
    rename(Country = OMRÅDE,
           Gender = KØN, 
           Age = ALDER,
           Citizen = STATSB,
           Year = TID,
           Pop = INDHOLD) %>%  
    mutate(Country = "Denmark",
           Citizen = case_when(Citizen == "Danish citizen" ~ "Danish",
                               Citizen == "Foreign citizen" ~ "Foreign"),
           Date = gsub("Q", "", Year),
           Date = as.integer(Date),
           Date = as_date_yq(Date),
           Date = first_of_quarter(Date)
    ) %>% 
    separate(Year, c("Year", "Quarter"), sep = "Q") %>% 
    mutate(Year = as.integer(Year),
           Quarter = as.integer(Quarter)) %>% 
    mutate(Age = parse_number(Age), 
           Age = as.integer(Age)) %>% 
    # Add rows with total pop by age
    pivot_wider(names_from = "Citizen", values_from = c("Pop")) %>% 
    mutate(Total = Danish + Foreign) %>% 
    pivot_longer(cols = c(Danish, Foreign, Total), names_to = "Citizen", values_to = "Pop") %>% 
    # Add population in percentage (%)
    group_by(Date) %>% 
    mutate(Pop_per = 100 * Pop / sum(Pop)) %>%    
    ungroup() -> pop_ctzn_age 

## Total population by quarter and citizenship
  pop_ctzn_age %>% 
    group_by(Date, Citizen) %>%
    # Add summary statistics (mean and median age)
    summarise(Pop_total = sum(Pop),
              Age_mean   = weighted.mean(Age, Pop),
              Age_median = matrixStats::weightedMedian(Age, Pop)) %>%  
    ungroup() %>%  
    # Standardize it to % change with 2008-Q1 as baseline
    group_by(Citizen) %>% 
    arrange(Date, Citizen) %>% 
    mutate(pct_change_2008 = (Pop_total/first(Pop_total) - 1) * 100) %>% 
    ungroup() -> pop_ctzn_ttl_quarter

# Plots ----
  
## Population growth ----

## Plots  
  ggplot() +
    geom_line(data = pop_ctzn_ttl_quarter, aes(x = Date, y = Pop_total/1000, col = Citizen)) +
    labs(title = "Danish population (2008 - 2020)",
         subtitle = "Total population",
         y = "x1000") +
    theme_bw() -> p1
  
  ggplot() +
    geom_line(data = pop_ctzn_ttl_quarter, aes(x = Date, y = pct_change_2008, col = Citizen)) +
    labs(title = "",
         subtitle = "Percentage of change",
         y = "%") +
    theme_bw() -> p2
  
  ggplot() +
    geom_line(data = pop_ctzn_ttl_quarter, aes(x = Date,
                                               y = Age_median,
                                               colour = Citizen,
                                               linetype = "Median")) +
    geom_line(data = pop_ctzn_ttl_quarter, aes(x = Date,
                                               y = Age_mean,
                                               colour = Citizen,
                                               linetype = "Mean")) +
    scale_linetype_discrete(name = "Age") +
    labs(title = "",
         subtitle = "Age variation", 
         y = "Age") +
    theme_bw() -> p3
  
  p1 + p2 + p3
  ggsave("Results/pop_migr_growth_2008_2020.png", width = 30, height = 10, units = "cm")
  
  