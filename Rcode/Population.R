#########################################
#####     Denmark: population       #####
#########################################

  library(tidyverse)
  library(readr)
  library(readxl)
  library(gganimate)
  
# Population ----
# Population by year (2008-2020 Q1) by gender and status 
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
    ggsave("Rresults/pop_lau_year.png", width = 30, height = 30, units = "cm")
  
## Percentage of population changed respect the previous year
  pop_tot <- pop_tot %>% 
    group_by(LAU_NAME) %>% 
    mutate(pct_change = (Total/lag(Total) - 1) * 100) %>% 
    ungroup()
  
  dk_lau_pop_tot <- dk_lau %>% 
    left_join(pop_tot, by = "LAU_NAME") %>% 
    st_as_sf() %>% 
    arrange(LAU_NAME, Year) %>% 
    mutate(Year = as.character(Year))  
  

  anim <- ggplot() +
    geom_sf(data = dk_lau_pop_tot, aes(fill = pct_change)) +
    scale_fill_gradient2(name = "Change [%]",
                         low = "red",
                         mid = "white",
                         high = "blue",
                         midpoint = 0) +
    labs(title = "{closest_state}") +
    transition_states(Year, wrap = FALSE, state_length = 2) +
    theme_bw()
  
  anim_save("Rresults/pop_lau_change_year.gif", anim)
  
  
  