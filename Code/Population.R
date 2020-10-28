#########################################
#####     Denmark: population       #####
#########################################

  library(tidyverse)
  library(readr)
  library(readxl)
  library(gganimate)
  library(dint)
  library(danstat)

# Data load ----
# Total population at the first day of the quarter (2008Q1 - 2020Q3) 
# Data from Statistic Denmark: https://www.statbank.dk/10021 (load with {danstat})
  get_subjects()

## Subjects of interest (Population and elections - 02)
  subj <- get_subjects(subjects = "02")
  sub_subj <- subj$subjects %>% bind_rows()

## Getting table information and variables
## Population and population projections (2401)
  tables <- get_tables(subjects = "2401")
  tables %>% select(id, text, variables) 

## Metadata table of interest (e.g. FOLK1A)
  var_pop <- get_table_metadata(table_id = "FOLK1A", variables_only = TRUE)
  var_pop %>% select(id, text)
  var_pop$values
  
## pull the needed data from the API
  
### Variables we would like to get data
### e.g. OMRÅDE - regions, Tid - time
  var_codes <- c("OMRÅDE", "Tid")

### Construct a list with variable code-values pairs 
### NA - extract all values for a variable code (e.g. Tid)
### ID of Municipalities - LAUs (values of OMRÅDE to extract; id > 100)
  var_pop$values[1] 
  id_muni <- as.numeric(var_pop$values[[1]]$id)
  id_muni <- id_muni[id_muni > 100]
  var_values <- list(id_muni, NA)   
  var_input <- purrr::map2(.x = var_codes, .y = var_values, .f = ~list(code = .x, values = .y))
  
### Get data 
  get_data("FOLK1A", variables = var_input) %>%
    rename(LAU_NAME = OMRÅDE,
           Year = TID, 
           Pop = INDHOLD) %>% 
    mutate(LAU_NAME = gsub("Copenhagen", "København", LAU_NAME)) %>% 
    arrange(LAU_NAME, Year) %>%
    mutate(Date = gsub("Q", "", Year),
           Date = as.integer(Date),
           Date = as_date_yq(Date),
           Date = first_of_quarter(Date)
    ) %>% 
    separate(Year, c("Year", "Quarter"), sep = "Q") %>% 
    mutate(Year = as.integer(Year),
           Quarter = as.integer(Quarter)) %>% 
    arrange(LAU_NAME, Year, Quarter) -> pop_quarter 
 
# Plots population variation by quarter ----

## Total population
  ggplot(data = pop_quarter,
         aes(x = Date,
             y = Pop / 1000)) +
    geom_line(colour = "#0072B2") +
    facet_wrap(~LAU_NAME, scales = "free") +
    theme_bw() +
    theme(axis.text = element_text(size = 7),
          axis.title = element_text(size = 11, face ="bold")) +
    labs(title = "Population of Denmark by LAUs at the first day of the quarter (2008-Q1  to 2020-Q3)") +
    scale_y_continuous(name = "x1000", labels = scales::number_format(accuracy = 0.1)) +
    scale_x_date(date_breaks = "3 year", date_labels = "%y")
  
  ggsave("Rresults/pop_lau_2008_2020_quarters.png", width = 37, height = 37, units = "cm")

## Standardize it to % change with 2008-Q1 as baseline
  pop_quarter %>% 
    group_by(LAU_NAME) %>% 
    arrange(LAU_NAME, Year, Quarter) %>% 
    mutate(pct_change_2008 = (Pop/first(Pop) - 1) * 100) %>% 
    ungroup() -> pop_quarter
  
  ggplot(data = pop_quarter,
         aes(x = Date,
             y = pct_change_2008)) +
    geom_line(colour = "#0072B2") +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey") + 
    facet_wrap(~LAU_NAME) +
    theme_bw() +
    theme(axis.text = element_text(size = 7),
          axis.title = element_text(size = 11, face = "bold")) +
    labs(title = "Population change of Denmark with 2008-Q1 as baseline") +
    scale_y_continuous(name = "[%]",
                       breaks = seq(-20, 35, 10),
                       labels = scales::number_format(accuracy = 1)) +
    scale_x_date(date_breaks = "3 year", date_labels = "%y")

  ggsave("Results/pop_lau_2008_2020_quarters_change.png", width = 37, height = 37, units = "cm")

# Spatial plots ----

## Link with LAUs
  dk_lau_pop_quarter <- dk_lau %>% 
    left_join(pop_quarter, by = "LAU_NAME") %>% 
    st_as_sf() %>% 
    arrange(LAU_NAME, Year) %>% 
    mutate(Year = as.character(Year))
  
## Change in 2020 
  ggplot() +
    geom_sf(data = filter(dk_lau_pop_quarter, Year == 2020, Quarter == 3), aes(fill = pct_change_2008)) +
    scale_fill_gradient2(name = "Change [%]",
                         low = "red",
                         mid = "white",
                         high = "blue",
                         midpoint = 0) +
    labs(title = "Population in Denmark",
         subtitle = "Change between 2008-Q1 and 2020-Q3") +
    theme_bw()
   
  ggsave("Results/sp_pop_lau_2008_2020_change.png", width = 20, height = 20, units = "cm")
  
## Animation
  anim <- ggplot() +
    # Not sure why I got an error if all the data are selected
    # and I've removed the first date (baseline: 2008-Q1)
    geom_sf(data = filter(dk_lau_pop_quarter, Date > "2008-01-01"), aes(fill = pct_change_2008)) +
    scale_fill_gradient2(name = "Change [%]",
                         low = "red",
                         mid = "white",
                         high = "blue",
                         midpoint = 0) +
    labs(title = "Population change of Denmark (baseline 2008-Q1)",
         subtitle = "{closest_state}") +
    transition_states(Date, wrap = FALSE) +
    theme_bw()
  
## export as .gif
  anim_save("Results/sp_pop_lau_2008_2020_change_anim.gif", anim)
  