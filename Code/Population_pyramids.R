###********************
###   Age structure ###
###********************

  library(danstat) 
  library(tidyverse)
  library(readr)
  library(dint)
  library(gganimate)
  library(patchwork)
  library(gridExtra)
  
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

# Make some calculation 
  
## Population percentages 
  pop_age %>% 
    group_by(Date) %>% 
    mutate(Pop_per = 100 * Pop / sum(Pop)) %>%    
    ungroup() -> pop_age
 
## Table with summaries statistic
  pop_age %>% 
    group_by(Date, Year, Quarter) %>%
    summarise(Pop_total  = sum(Pop),
              Age_mean   = weighted.mean(Age, Pop),
              # Age_median = median(rep(Age, times = Pop)),
              Age_median = matrixStats::weightedMedian(Age, Pop)
              ) %>% 
    ungroup() -> pop_age_sum
  
# Plots ----

## Population growth ----
  ggplot() +
    geom_line(data = pop_age_sum, aes(x = Date, y = Pop_total/1000)) +
    labs(title = "Denmark population (2008 - 2020)",
         subtitle = "Total population",
         y = "x1000") +
    theme_bw() -> p1
  
  ggplot() +
    geom_line(data = pop_age_sum, aes(x = Date, y = Age_median, colour = "Median")) +
    geom_line(data = pop_age_sum, aes(x = Date, y = Age_mean , colour = "Mean")) +
    scale_colour_discrete(name = "Value") +
    labs(title = "",
         subtitle = "Age structure", 
         y = "Age") +
    theme_bw() -> p2
  
  p1 + p2
  ggsave("Results/pop_summary_2008_2020.png", width = 25, height = 12, units = "cm")
   
## Population pyramids ----
## Aux. function for plotting population pyramids
## Add only the year and quarter (YQ) we would like to plot (e.g. 20081)
  plot_pyramid <- function(YQ) {
    brks_x <- seq(0, 125, 10)
    brks_y <- seq(-1, 1, 0.2)
    lbls_y <- paste0(as.character(brks_y), "%")
    df_plot <- filter(pop_age, Date ==  first_of_quarter(as_date_yq(YQ)))
    ggplot() + 
    geom_bar(data = filter(df_plot, Gender == "Women"),
             aes(x = Age, y = Pop_per, fill = Gender),
             stat = "identity", 
             width = 1) + 
    geom_bar(data = filter(df_plot, Gender == "Men"),
             aes(x = Age, y = -Pop_per, fill = Gender), 
             stat = "identity",
             width = 1) + 
    scale_y_continuous(name = NULL, breaks = brks_y, labels = lbls_y, limits = c(-0.85, 0.85)) +
    scale_x_continuous(name = "Age", breaks = brks_x, labels = brks_x, limits = c(0, 105)) + 
    coord_flip() + 
    labs(title = "Population pyramid of Denmark",
         subtitle = paste("Date", first_of_quarter(as_date_yq(YQ)), sep = ": ")) +
    scale_fill_manual(values = c("#0072B2", "#D55E00")) +
    theme_bw()
  }

## Baseline (2008-Q1 = 20081)
## Add a table with summary data (Totla population, mean age, median age) 
  YQ <- 20081
  table <- tribble(~Desc, ~Value,
                   "Total pop (mil)", filter(pop_age_sum, Date ==  first_of_quarter(as_date_yq(YQ)))$Pop_total/1000, 
                   "Mean age"       , filter(pop_age_sum, Date ==  first_of_quarter(as_date_yq(YQ)))$Age_mean,
                   "Median age"     , filter(pop_age_sum, Date ==  first_of_quarter(as_date_yq(YQ)))$Age_median) %>% 
    mutate(Value = round(Value, 1))

  plot_pyramid(YQ) +
    annotation_custom(tableGrob(table,
                                rows = NULL,
                                theme = ttheme_minimal(base_size = 7)),
                      xmin = 80, xmax = 110,
                      ymin = -0.8, ymax = -0.3) -> p_2008Q1

## 2020-Q3 (20203)
  YQ <- 20203
  table <- tribble(~Desc, ~Value,
                   "Total pop (mil)", filter(pop_age_sum, Date ==  first_of_quarter(as_date_yq(YQ)))$Pop_total/1000, 
                   "Mean age"       , filter(pop_age_sum, Date ==  first_of_quarter(as_date_yq(YQ)))$Age_mean,
                   "Median age"     , filter(pop_age_sum, Date ==  first_of_quarter(as_date_yq(YQ)))$Age_median) %>% 
    mutate(Value = round(Value, 1))
  
  plot_pyramid(YQ) +
    labs(title = "",
         subtitle = paste("Date", first_of_quarter(as_date_yq(YQ)), sep = ": ")) +
    annotation_custom(tableGrob(table,
                                rows = NULL,
                                theme = ttheme_minimal(base_size = 7)),
                      xmin = 80, xmax = 110,
                      ymin = -0.8, ymax = -0.3) -> p_2020Q3
  
## Plot together
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
  
  
  