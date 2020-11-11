###******************************
###    Population pyramid    ####
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
  # id_quarter <- var_pop$values[[6]]$id   # Select all quarters
  id_quarter <- c("2008K1", "2020K4")  # Select only some years (e.g. 2008-Q1 and 2020-Q4)
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
  
# Calculate population in percentage
  pop_DK %>% 
  group_by(date) %>% 
    mutate(pop_per = 100 * pop / sum(pop)) %>%    
    ungroup() -> pop_DK

# Pre-process ----
## Aux. function plotting population pyramids 
  ggpyramid <- function(df, fill = gender) {
    brks_y <- seq(-4, 4, 1)
    lmts_y = c(min(brks_y), max(brks_y))
    lbls_y <- paste0(as.character(abs(brks_y)), "%")
    ggplot() + 
      geom_bar(data = subset(df, gender == "Women"),
               aes(x = age,
                   y = pop_per,
                   fill = {{ fill }}), 
               stat = "identity", 
               width = 1) + 
      geom_bar(data = subset(df, gender == "Men"),
               aes(x = age,
                   y = -pop_per,
                   fill = {{ fill }}), 
               stat = "identity",
               width = 1) + 
      geom_hline(yintercept = 0, colour = "grey10") +
      annotate(geom = "text", y = 2,  x = 21, label = "Women", fontface = "bold") +
      annotate(geom = "text", y = -2, x = 21, label = "Men", fontface = "bold") +
      geom_segment(aes(x = 3.5, xend = 3.5, y = -4, yend = 4), linetype = "dashed") +
      geom_segment(aes(x = 13, xend = 13, y = -4, yend = 4), linetype = "dashed") +
      scale_y_continuous(name = NULL, breaks = brks_y, labels = lbls_y, limits = lmts_y) +
      scale_x_discrete(name = "Age") +
      coord_flip() 
  }
  
## Define theme
  theme_pyramid <- function() {
    theme_bw() +
    theme(axis.text = element_text(size = 7),
          axis.title = element_text(size = 11, face = "bold"))
  }
  
## Plot population pyramid ---- 
# Baseline (2008Q1)
  filter(pop_DK, date ==  first_of_quarter(as_date_yq(20081))) %>% 
    ggpyramid(fill = ancestry) + 
    scale_fill_manual(values = c("#0072B2", "#F0E442", "#D55E00")) + 
    labs(title = "Population pyramid of Denmark",
         subtitle = paste("Date", first_of_quarter(as_date_yq(20081)), sep = ": ")) +
    theme_pyramid() -> p1
  
# Situation in 2020-Q3
  filter(pop_DK, date ==  first_of_quarter(as_date_yq(20204))) %>% 
    ggpyramid(fill = ancestry) + 
    scale_fill_manual(values = c("#0072B2", "#F0E442", "#D55E00")) +
    theme_pyramid() +
    labs(title = "",
         subtitle = paste("Date", first_of_quarter(as_date_yq(20204)), sep = ": ")) -> p2
 
  p1 + p2 + plot_layout(guides = "collect")  
  ggsave("Results/pop_pyramid_2008_2020_migr.png", width = 25, height = 12, units = "cm")

## Animated population pyramid from 2008-2020 ----
## Not sure why I got an error if all the data are selected
## and I've removed 2008
  filter(pop_DK, date >= "2009-01-01") %>% 
    ggpyramid(fill = ancestry) + 
    scale_fill_manual(values = c("#0072B2", "#F0E442", "#D55E00")) +
    transition_states(date, wrap = FALSE) +
    labs(title = "Population pyramid of Denmark",
         subtitle = "Date: {closest_state}") -> anim
  
  anim_save("Results/pop_pyramid_2008_2020_migr_anim.gif", anim)

