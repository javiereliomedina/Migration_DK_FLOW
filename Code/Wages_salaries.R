#######################################-
####   Wages and salaries Denmark   ####
#######################################-

library(danstat)
library(tidyverse)
library(gganimate)

if (!require("remotes")) install.packages("remotes")
if (!require("ggpyramid"))  remotes::install_github("javiereliomedina/ggpyramid")

library(ggpyramid)

# Load data ----

## Table 
  table <- "INDKP201"
  dat_meta <- get_table_metadata(table_id = table, variables_only = TRUE)
  dat_meta %>% select(id, text)

## Show the first values of each parameter 
  lapply(dat_meta$values, head, n = 5)

## Select variables
  variables <- list(
    # Wages and salaries etc., total 
    list(code = "INDKOMSTTYPE", values = "115"),
    # By gender (Men and Women)
    list(code = "KOEN", values = c("M", "K")),
    # All ages groups
    list(code = "ALDER", values = NA),
    # Only people with the type of income
    list(code = "POPU", values = "5020"),
    # Constant prices
    list(code = "PRISENHED", values = "005"),
    # Median income
    list(code = "ENHED", values = NA),
    # All time series
    list(code = "Tid", values = NA))
  
## get data
  dat <- get_data("INDKP201", variables)
  dat

# Clean data ----
  
  dat_clean <- dat %>%
    # Translate into English 
    rename(income = INDKOMSTTYPE,
           gender = KOEN,
           age = ALDER,
           population = POPU, 
           prc_unit = PRISENHED,
           unit = ENHED,
           date = TID,
           value = INDHOLD
    ) %>% 
    # Remove age group = "Total, 15 years and over"
    filter(age != "Total, 15 years and over") %>% 
    # Format age (consecutive levels) 
    mutate(age = gsub("80 years and over", "80-00", age),
           age = gsub(" years", "", age),
           age = factor(age, levels = subset(dat_meta$values[[3]], id != "14TOT")$id)) 
  
# Plots ----

## Wages in 2019
  brks_y <- seq(-625, 625, 250)
  lmts_y = c(min(brks_y), max(brks_y))
  dat_clean %>% 
    filter(date == 2019,
           unit != "People (Number)") %>% 
    mutate(unit = gsub("DKK", "kDKK", unit)) %>% 
    ggpyramid(age = age,
              pop = value/1000,
              gender = gender,
              men = "Men",
              women = "Women",
              fill = gender) +
    scale_fill_manual(name = "Gender", values = c("#E69F00", "#009E73")) +
    scale_y_continuous(name = NULL, breaks = brks_y, limits = lmts_y) +
    labs(title = "Wages and salaries in Denmark (2019) ",
         subtitle = "Only people with the type of income",
         y = "",
         caption = "Data source: Statistics Denmark \nAuthor: J. Elio (@Elio_Javi)\nAalborg University, Department of Planning") +
    theme_bw() +
    facet_wrap( ~ unit)
  
  ggsave("Results/wages_salaries_2019.png")
  
## Animation
  brks_y <- seq(-625, 625, 250)
  lmts_y = c(min(brks_y), max(brks_y))
  anim <- dat_clean %>% 
    filter(unit != "People (Number)") %>%
    mutate(unit = gsub("DKK", "kDKK", unit)) %>% 
    ggpyramid(age = age,
              pop = value/1000,
              gender = gender,
              men = "Men",
              women = "Women",
              fill = gender) +
    scale_fill_manual(name = "Gender", values = c("#E69F00", "#009E73")) + 
    scale_y_continuous(name = NULL, breaks = brks_y, limits = lmts_y) +
    labs(title = "Wages and salaries in Denmark (Constant prices) ",
         subtitle = "Date: {closest_state}",
         caption = "Data: Only people with the type of income\nSource: Statistics Denmark \nAuthor: J. Elio (@Elio_Javi)\nAalborg University, Department of Planning") +
    theme_bw() +
    facet_wrap( ~ unit) +
    transition_states(date)
  
  anim_save("Results/wages_salaries.gif", anim)
  
           