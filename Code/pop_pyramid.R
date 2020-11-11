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

# Load data ----
#' Data from Statistic Denmark: https://www.statbank.dk/10021 
#' Table: FOLK1D
#' population at the first day of the quarter by region, sex, age and group of citizenship
#' Subjects of interest: population and elections (02)
#' Immigrants and their descendants (2402)

  subj <- get_subjects(subjects = "02")
  sub_subj <- subj$subjects %>% bind_rows()
  sub_subj

# Codes for var_input
  var_pop <- get_table_metadata(table_id = "FOLK1D", variables_only = TRUE)
  var_pop %>% select(id, text)
  var_pop$values

  var_input <- list(list(code = "OMRÅDE", values = "000"),
                    list(code = "KØN", values = c(1, 2)),
                    list(code = "ALDER", values = seq(0, 125, 1)),
                    list(code = "STATSB", values = c("DANSK", "UDLAND")),
                    list(code = "tid", values = NA))

# Get data 
  get_data("FOLK1D", variables = var_input) %>%
    rename(country = OMRÅDE,
           gender = KØN, 
           age = ALDER,
           citizen = STATSB,
           date = TID,
           pop = INDHOLD) %>%  
    mutate(country = "Denmark",
           citizen = case_when(citizen == "Danish citizen" ~ "Danish",
                               citizen == "Foreign citizen" ~ "Foreign", 
                               TRUE ~ citizen)) %>% 
    mutate(date = gsub("Q", "", date),
           date = as_date_yq(as.integer(date)),
           date = first_of_quarter(date)) %>% 
    mutate(age = parse_number(age), 
           age = as.integer(age)) %>% 
    # Add population in percentage (%)
    group_by(date) %>% 
    mutate(pop_per = 100 * pop / sum(pop)) %>%    
    ungroup() -> pop_ctzn_age 

# Pre-process ----
  
## Aux. function plotting population pyramids 
  geom_pyramid <- function(df, fill = gender) {
    brks_y <- seq(-4, 4, 1)
    lmts_y = c(min(brks_y), max(brks_y))
    lbls_y <- paste0(as.character(abs(brks_y)), "%")
    ggplot() + 
      geom_bar(data = subset(df, gender == "Women"),
               aes(x = cut_interval(age, length = 5, right = FALSE),
                   y = pop_per,
                   fill = {{ fill}}), 
               stat = "identity", 
               width = 1) + 
      geom_bar(data = subset(df, gender == "Men"),
               aes(x = cut_interval(age, length = 5, right = FALSE),
                   y = -pop_per,
                   fill = {{ fill }}), 
               stat = "identity",
               width = 1) + 
      geom_hline(yintercept = 0, colour = "grey10") +
      annotate(geom = "text", y = 2,  x = 25, label = "Women", fontface = "bold") +
      annotate(geom = "text", y = -2, x = 25, label = "Men", fontface = "bold") +
      geom_segment(aes(x = 3.5, xend = 3.5, y = -4, yend = 4), linetype = "dashed") +
      geom_segment(aes(x = 13, xend = 13, y = -4, yend = 4), linetype = "dashed") +
      scale_y_continuous(name = NULL, breaks = brks_y, labels = lbls_y, limits = lmts_y) +
      scale_x_discrete(name = "Age", drop = TRUE) +
      coord_flip() + 
      scale_fill_manual(values = c("#0072B2", "#D55E00"))
  }
  
## Define theme
  theme_pyramid <- function() {
    theme_bw() +
    theme(axis.text = element_text(size = 7),
          axis.title = element_text(size = 11, face = "bold"))
  }
  
## Plot population pyramid ---- 
# Baseline (2008Q1)
  filter(pop_ctzn_age, date ==  first_of_quarter(as_date_yq(20081))) %>% 
    geom_pyramid(fill = citizen) + 
    labs(title = "Population pyramid of Denmark",
         subtitle = paste("Date", first_of_quarter(as_date_yq(20081)), sep = ": ")) +
    theme_pyramid() -> p_2008Q1
  
# Situation in 2020-Q3
  filter(pop_ctzn_age, date ==  first_of_quarter(as_date_yq(20203))) %>% 
    geom_pyramid(fill = citizen) +
    theme_pyramid() +
    labs(title = "",
         subtitle = paste("Date", first_of_quarter(as_date_yq(20203)), sep = ": ")) -> p_2020Q3
  
  p_2008Q1 + p_2020Q3 + plot_layout(guides = 'collect')  
  ggsave("Results/pop_pyramid_2008_2020_migr.png", width = 25, height = 12, units = "cm")

## Animated population pyramid from 2008-2020 ----
## Not sure why I got an error if all the data are selected
## and I've removed the first date (baseline: 2008-Q1)
  filter(pop_ctzn_age, date > "2008-01-01") %>% 
    geom_pyramid() + 
    transition_states(date, wrap = FALSE) +
    labs(title = "Population pyramid of Denmark",
         subtitle = "Date: {closest_state}") -> anim
  
  anim_save("Results/pop_pyramid_2008_2020_migr_anim.gif", anim)

