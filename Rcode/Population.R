#########################################
#####     Denmark: population       #####
#########################################

  library(tidyverse)
  library(readr)
  library(readxl)
  library(gganimate)
  
# Population
## Download .csv from https://www.statbank.dk/statbank5a/SelectTable/Omrade0.asp?PLanguage=1
  pop <- read_csv2("Rdata/Statistics_DK/Population.csv", col_names = FALSE) %>% 
    setNames(c("Year", "Status", "LAU_NAME", "Total", "Men","Women")) %>% 
    separate(Year, c("Year", "Quarter"), sep = "Q") %>% 
    mutate(LAU_NAME = str_conv(LAU_NAME, "latin1"),
           LAU_NAME = gsub("Copenhagen", "København", LAU_NAME)) %>% 
    select(LAU_NAME, everything()) %>% 
    mutate(Status = stringr::str_to_lower(Status),
           Status = gsub(" ", "_", Status),
           # Status = gsub("(\\b[a-z][a-z][a-z])|.", "\\1", Status, perl=TRUE)
    ) %>%
    pivot_wider(names_from = Status, values_from = c("Total", "Men", "Women")) %>% 
    arrange(LAU_NAME)
  
  pop_dk_lau <- dk_lau %>% 
    left_join(pop, by = "LAU_NAME") %>% 
    st_as_sf() %>% 
    arrange(LAU_NAME)
  
# Population by year (2008-2020 Q1) by gender and status 
  pop_year <-  read_xlsx("Rdata/Statistics_DK/Pop_muni_gen_stat_2008_2020_Q1.xlsx", skip = 2) %>% 
    setNames(c("Status", "Gender", "LAU_NAME", paste("Q1", seq(2020, 2008, by = -1), sep = "_"))) %>% 
    fill(Status, Gender) %>% 
    mutate(LAU_NAME = gsub("Copenhagen", "København", LAU_NAME),
           Status = stringr::str_to_lower(Status),
           Status = gsub(" ", "_", Status),
           # Status = gsub("(\\b[a-z][a-z][a-z])|.", "\\1", Status, perl = TRUE)
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
    
# Animate with transition_states  
  anim1 <- ggplot() +
    geom_sf(data = dk_lau_pop_year, aes(fill = Total_total)) +
    labs(title = "{closest_state}") +
    transition_states(Year, wrap = FALSE)
  anim_save("anim2.gif", anim1)
  