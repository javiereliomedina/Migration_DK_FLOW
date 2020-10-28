#########################################
#####     Denmark: population       #####
#########################################

  library(tidyverse)
  library(readr)
  library(readxl)
  library(gganimate)
  library(dint)

# Population ----
# Population by year (2008-2020 Q1) by gender and status 
# Data download from https://www.statbank.dk/10021

# Data: Total population by quarter ----
## FOLK1A: Population at the first day of the quarter by region, sex, age and marital status
## REGION: municipalities
## SEX: NA
## AGE: NA
## MARITAL STATUS: NA
## QUARTER: select all
## Export as excel: pop_dk_year_quarter_2008_2020.xlsx
  read_xlsx("Rdata/Statistics_DK/pop_dk_year_quarter_2008_2020.xlsx", skip = 2)  %>%
    rename(LAU_NAME = "...1") %>% 
    mutate(LAU_NAME = gsub("Copenhagen", "København", LAU_NAME)) %>% 
    pivot_longer(cols = starts_with("20"),
                 names_to = "Year",
                 values_to = "Pop", 
                 values_drop_na = TRUE) %>%
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

  ggsave("Rresults/pop_lau_2008_2020_quarters_change.png", width = 37, height = 37, units = "cm")

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
   
  ggsave("Rresults/sp_pop_lau_2008_2020_change.png", width = 20, height = 20, units = "cm")
  
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
  anim_save("Rresults/sp_pop_lau_2008_2020_change_anim.gif", anim)
  