###******************************
###     Population growth     ###
###******************************

  library(danstat) 
  library(tidyverse)
  library(readr)
  library(dint)
  library(gganimate)
  library(patchwork)
  library(gridExtra)
  library(furrr)
  library(forcats)

  source("Code/theme_plot.R")
  
# Load data ----
#' Data from Statistic Denmark: https://www.statbank.dk/10021 
#' Table: FOLK1C
#' Population at the first day of the quarter by region, sex, age (5 years age groups), ancestry and country of origin
#' Subjects of interest: population and elections (02)
#' Immigrants and their descendants (2402)
  
  id_table <- "FOLK1C"
  var_pop <- get_table_metadata(table_id = id_table, variables_only = TRUE)
  var_pop %>% select(id, text)
  var_pop$values
  
# loop by quarter for getting the data 
  steps <- function(quarter){
    var_values <- list(id_region, id_gender, id_age , id_ancestry, id_citizen, quarter)
    var_input <- purrr::map2(.x = var_codes, .y = var_values, .f = ~list(code = .x, values = .y))
    get_data(id_table, variables = var_input)
  }
  
# Codes for var_input
  var_codes <- c("OMRÅDE", "KØN", "ALDER", "HERKOMST", "IELAND", "Tid")
  
  # Values for var_input
  ## Region: Denmark
  id_region <- "000"
  ## Gender: Men/Women
  id_gender <- c(1, 2)
  ## Age (remove total)
  id_age <- subset(var_pop$values[[3]], id != "IALT")$id
  ## Ancestry
  id_ancestry <- c(5, 4, 3)
  ## country of origin (remove total)
  id_citizen <- as.numeric(var_pop$values[[5]]$id)
  id_citizen <- id_citizen[id_citizen > 0]
  ## Quarters
  id_quarter <- var_pop$values[[6]]$id   # Select all quarters
  # id_quarter <- c("2008K1", "2020K4")  # Select only some years (e.g. 2008-Q1 and 2020-Q4)
  # Parallel process with {future}
  plan(multiprocess)  
  pop_DK <- id_quarter %>% future_map(steps)
  pop_DK <- bind_rows(pop_DK)
  plan("default")
  
# Clean column names and format some data
  pop_DK %>% 
    filter(INDHOLD > 0) %>% 
    rename(region = OMRÅDE,
           gender = KØN,
           age = ALDER,
           ancestry = HERKOMST,
           origin = IELAND,
           date = TID, 
           pop = INDHOLD) %>% 
    mutate(date = gsub("Q", "", date),
           date = as_date_yq(as.integer(date)),
           date = first_of_quarter(date)) %>% 
    mutate(region = ifelse(region == "All Denmark",
                           "Denmark",
                           NA))  %>% 
    # Format ancestry and age
    mutate(ancestry = ifelse(ancestry == "Persons of Danish origin", "Danish", ancestry),
           ancestry = factor(ancestry), 
           ancestry = fct_relevel(ancestry, "Immigrants", after = 1)
    ) %>% 
    # Format "age"    
    mutate(age = ifelse(age == "100 years and over", "100OV", age),
           age = gsub(" years", "", age),
           age = factor(age, levels = id_age)) -> pop_DK
  
# Total population by quarter and ancestry ----
  pop_DK %>% 
    group_by(date, ancestry) %>%
    # Add total population (pop_total) 
    summarise(pop_total = sum(pop)) %>%  
    ungroup() %>%  
    # Calculate difference and standardize pop_total to % change with 2008-Q1 as baseline
    group_by(ancestry) %>% 
    arrange(date, ancestry) %>% 
    mutate(pop_change_2008 = (pop_total/first(pop_total) - 1) * 100,
           pop_diff_2008 = pop_total - first(pop_total)) %>% 
    ungroup() %>% 
    # Difference percentage over the total change
    group_by(date) %>% 
    mutate(pop_diff_2008_pct = 100 * pop_diff_2008 / sum(pop_diff_2008)) %>% 
    ungroup() -> pop_DK_quarter

# Plots ---- 
  
  ggplot() +
    geom_area(data = pop_DK_quarter, aes(x = date, y = pop_diff_2008/1000, fill = ancestry)) +
    scale_fill_manual(name = "Ancestry", values = c("#0072B2", "#F0E442", "#D55E00")) +
    labs(title = "Danish population growth (2008 - 2020)",
         subtitle = "Population difference",
         y = "x1000",
         x = "") +
    theme_plot() -> p1
  
  ggplot() +
    geom_area(data = pop_DK_quarter, aes(x = date, y = pop_diff_2008_pct, fill = ancestry)) +
    scale_fill_manual(name = "Ancestry", values = c("#0072B2", "#F0E442", "#D55E00")) +
    labs(subtitle = "Percentage over the total change",
         caption = "Source: Statistics Denmark",
         y = "%",
         x = "") +
    theme_plot() -> p2
  
  p1 + p2 + plot_layout(guides = "collect") 
  ggsave("Results/pop_growth_2008_2020.png", width = 25, height = 10, units = "cm")
  