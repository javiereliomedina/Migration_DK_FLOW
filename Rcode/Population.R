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
           Date = last_of_quarter(Date)
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


# Animation population change 
## Select only first quarter of the year 

 
  
  



  

  
## Data 2 ----
## FOLK1A: Population at the first day of the quarter by region, sex, age and marital status
## REGION: municipalities
## SEX: Men - Women
## AGE: NA
## MARITAL STATUS: select all
## QUARTER: select all
## Export as excel: Pop_muni_gen_stat_2008_2020_Q1.xlsx
  pop_year <-  read_xlsx("Rdata/Statistics_DK/Pop_muni_gen_stat_2008_2020_Q1.xlsx", skip = 2) %>% 
    setNames(c("Status", "Gender", "LAU_NAME", paste("Q1", seq(2020, 2008, by = -1), sep = "_"))) %>% 
    fill(Status, Gender) %>% 
    mutate(LAU_NAME = gsub("Copenhagen", "København", LAU_NAME),
           Status = stringr::str_to_lower(Status),
           Status = gsub(" ", "_", Status)
           ) %>% 
    pivot_longer(cols = starts_with("Q1"),
                 names_to = "Year",
                 values_to = "Pop", 
                 values_drop_na = TRUE) %>% 
    separate(Year, c("Quarter", "Year"), sep = "_") %>% 
    pivot_wider(names_from = Gender, values_from = Pop) %>% 
    pivot_wider(names_from = Status, values_from = c("Total", "Men", "Women"))
  
  dk_lau_pop_year <- dk_lau %>% 
    left_join(pop_year, by = "LAU_NAME") %>% 
    st_as_sf() %>% 
    arrange(LAU_NAME, Year) %>% 
    mutate(Year = as.character(Year))  

# Total pop by LAU------
 pop_tot <- pop_year %>%
    select(LAU_NAME, Year, Total_total) %>% 
    arrange(LAU_NAME, Year) %>% 
    rename(Total = Total_total) %>% 
    mutate(Year = as.integer(Year)) 
  
## Plot population variation
  ggplot(data = pop_tot,
         aes(x = as.numeric(sprintf('%02d', Year %% 100)),
             y = Total / 1000
             )
         ) +
    geom_line() +
    facet_wrap(~LAU_NAME, scales = "free") +
    theme_bw() +
    theme(axis.text = element_text(size = 7),
          axis.title = element_text(size = 11, face ="bold")) +
    labs(title = "Population of Denmark by LAUs") +
    scale_y_continuous(name = "x1000", labels = scales::number_format(accuracy = 0.1)) +
    scale_x_continuous(name = "Year", labels = scales::number_format(accuracy = 1)) +
    ggsave("Rresults/pop_lau_2008_2020_Q1_year.png", width = 37, height = 37, units = "cm")
  
## Percentage of population changed respect the previous year
  pop_tot <- pop_tot %>% 
    group_by(LAU_NAME) %>% 
    mutate(pct_change = (Total/lag(Total) - 1) * 100) %>% 
    ungroup()
  
  
  ggplot(data = pop_tot,
         aes(x = as.numeric(sprintf('%02d', Year %% 100)),
             y = pct_change
         )
  ) +
    geom_line() +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey") +
    facet_wrap(~LAU_NAME) +
    theme_bw() +
    theme(axis.text = element_text(size = 7),
          axis.title = element_text(size = 11, face ="bold")) +
    labs(title = "Population of Denmark by LAUs") +
    scale_y_continuous(name = "x1000", labels = scales::number_format(accuracy = 0.1)) +
    scale_x_continuous(name = "Year", labels = scales::number_format(accuracy = 1)) 
  
  
  
  
  
  
  
  
  
  dk_lau_pop_tot <- dk_lau %>% 
    left_join(pop_tot, by = "LAU_NAME") %>% 
    st_as_sf() %>% 
    arrange(LAU_NAME, Year) %>% 
    mutate(Year = as.character(Year))  
  

  anim <- ggplot() +
    geom_sf(data = dk_lau_pop_tot, aes(fill = pct_change)) +
    # scale_colour_gradientn(colours = terrain.colors(10)) +
    scale_fill_gradient2(name = "Change [%]",
                         low = "red",
                         mid = "blue",
                         high = "green",
                         midpoint = 0) +
    labs(title = "{closest_state}") +
    transition_states(Year, wrap = FALSE, state_length = 2) +
    theme_bw()
  
  anim_save("Rresults/pop_lau_change_year.gif", anim)
  
  
  