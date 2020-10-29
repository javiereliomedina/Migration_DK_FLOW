###********************
###   Age structure ###
###********************

  library(danstat) 
  library(tidyverse)
  library(readr)
  library(dint)
  library(gganimate)
  library(patchwork)
  
# Load data ----

# Total population at the first day of the quarter (2008Q1 - 2020Q3) 
# Data from Statistic Denmark: https://www.statbank.dk/10021 (load with {danstat})
  get_subjects()
  
## Subjects of interest (Population and elections - 02)
  subj <- get_subjects(subjects = "02")
  sub_subj <- subj$subjects %>% bind_rows()
  sub_subj
  
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
  var_input <- list(list(code = "OMRÅDE", values = "000"),
                    list(code = "KØN", values = c(1,2)),
                    list(code = "ALDER", values = seq(0,125, 1)),
                    list(code = "tid", values = NA))
  
### Get data 
  get_data("FOLK1A", variables = var_input) %>%
    rename(Country = OMRÅDE,
           Gender = KØN, 
           Age = ALDER,
           Year = TID,
           Pop = INDHOLD) %>% 
    mutate(Country = "Denmark") %>% 
    mutate(Date = gsub("Q", "", Year),
           Date = as.integer(Date),
           Date = as_date_yq(Date),
           Date = first_of_quarter(Date)
    ) %>% 
    separate(Year, c("Year", "Quarter"), sep = "Q") %>% 
    mutate(Year = as.integer(Year),
           Quarter = as.integer(Quarter)) %>% 
    mutate(Age = parse_number(Age), 
           Age = as.integer(Age)) -> pop_age 

### Estimate population percentages by age 
  pop_age %>% 
    group_by(Date) %>% 
    mutate(Pop_per = 100 * Pop / sum(Pop)) %>% 
    ungroup() -> pop_age
 
# Plots ----
  brks_x <- seq(0, 125, 10)
  brks_y <- seq(-1, 1, 0.2)
  lbls_y <- paste0(as.character(brks_y), "%")
  
  dat_pyramid_2008Q1 <- filter(pop_age, Year == 2008, Quarter == 1)
  dat_pyramid_2020Q3 <- filter(pop_age, Year == 2020, Quarter == 3)
  
  p_2008Q1 <- ggplot() + 
    geom_bar(data = filter(dat_pyramid_2008Q1, Gender == "Women"),
             aes(x = Age, y = Pop_per, fill = Gender),
             stat = "identity", 
             width = 1) + 
    geom_bar(data = filter(dat_pyramid_2008Q1, Gender == "Men"),
             aes(x = Age, y = -Pop_per, fill = Gender), 
             stat = "identity",
             width = 1) + 
    scale_y_continuous(name = NULL, breaks = brks_y, labels = lbls_y) +
    scale_x_continuous(name = "Age", breaks = brks_x, labels = brks_x, limits = c(0, 105)) + 
    coord_flip() + 
    labs(title = "Population pyramid of Denmark",
         subtitle = "Date: 2008-01-01") +
    scale_fill_manual(values = c("#0072B2", "#D55E00")) +
    theme_bw()
  
  p_2020Q3 <- ggplot() + 
    geom_bar(data = filter(dat_pyramid_2020Q3, Gender == "Women"),
             aes(x = Age, y = Pop_per, fill = Gender),
             stat = "identity", 
             width = 1) + 
    geom_bar(data = filter(dat_pyramid_2020Q3, Gender == "Men"),
             aes(x = Age, y = -Pop_per, fill = Gender), 
             stat = "identity",
             width = 1) + 
    scale_y_continuous(name = NULL, breaks = brks_y, labels = lbls_y) +
    scale_x_continuous(name = "Age", breaks = brks_x, labels = brks_x, limits = c(0, 105)) + 
    coord_flip() + 
    labs(title = "",
         subtitle = "Date: 2020-07-01") +
    scale_fill_manual(values = c("#0072B2", "#D55E00")) +
    theme_bw()
  
  p_2008Q1 + p_2020Q3 + plot_layout(guides = 'collect')  
  ggsave("Results/pop_pyramid_2008_2020.png", width = 25, height = 12, units = "cm")
  
# Animations ----
# Not sure why I got an error if all the data are selected
# and I've removed the first date (baseline: 2008-Q1)
  dat_pyramid <- filter(pop_age, Date > "2008-01-01")
  brks_x <- seq(0, 125, 15)
  brks_y <- seq(-1, 1, 0.2)
  lbls_y <- paste0(as.character(brks_y), "%")
  
  ggplot() + 
    geom_bar(data = filter(dat_pyramid, Gender == "Women"),
             aes(x = Age, y = Pop_per, fill = Gender),
             stat = "identity", 
             width = 1) + 
    geom_bar(data = filter(dat_pyramid, Gender == "Men"),
             aes(x = Age, y = -Pop_per, fill = Gender), 
             stat = "identity",
             width = 1) + 
    scale_y_continuous(name = NULL, breaks = brks_y, labels = lbls_y) + 
    scale_x_continuous(name = "Age", breaks = brks_x, labels = brks_x, limits = c(0, 105)) + 
    coord_flip() + 
    transition_states(Date, wrap = FALSE) +
    labs(title = "Population pyramid of Denmark",
         subtitle = "Date: {closest_state}") +
    scale_fill_manual(values = c("#0072B2", "#D55E00")) +
    theme_bw() -> anim
  
  anim_save("Results/pop_pyramid_2008_2020_anim.gif", anim)
  
  
  