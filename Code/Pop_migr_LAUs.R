###********************
###   MIGRANTS     ####
###********************

  library(danstat) 
  library(tidyverse)
  library(readr)
  library(dint)
  library(gganimate)
  library(patchwork)
  library(gridExtra)
  library(forcats)
  library(RColorBrewer)
  library(furrr)

# Load data ----
## Data from Statistic Denmark: https://www.statbank.dk/10021 (load with {danstat})
  get_subjects()

## Subjects of interest (Population and elections - 02)
  subj <- get_subjects(subjects = "02")
  sub_subj <- subj$subjects %>% bind_rows()
  sub_subj

## Getting table information and variables
## immigrants and their descendants (2402)
  tables <- get_tables(subjects = "2402")
  tables %>% select(id, text, variables) 

## Metadata table of interest (e.g. KMSTA001)
  var_pop <- get_table_metadata(table_id = "FOLK1B", variables_only = TRUE)
  var_pop %>% select(id, text)
  var_pop$values

## Ged data
## API - Max. number of selected data = 100000
## loop by quarter for getting the data (without age groups)
  steps <- function(quarter){
    var_values <- list(id_muni, id_gender, id_citizen, quarter)
    var_input <- purrr::map2(.x = var_codes, .y = var_values, .f = ~list(code = .x, values = .y))
    get_data("FOLK1B", variables = var_input)
  }
  
  # Codes for var_input
  var_codes <- c("OMRÅDE", "KØN", "STATSB", "Tid")
  # Values for var_input
  ## Data by LAUs
  id_muni <- as.numeric(var_pop$values[[1]]$id)
  id_muni <- id_muni[id_muni > 100]
  ## Gender: Men/Women
  id_gender <- c(1, 2)
  ## Data by countries (remove total)
  id_citizen <- as.numeric(var_pop$values[[4]]$id)
  id_citizen <- id_citizen[id_citizen > 0]
  ## Quarters
  id_quarter <- var_pop$values[[5]]$id 
  # Parallel process with {future}
  plan(multiprocess)  
  pop_ctzn <- id_quarter %>% future_map(steps)
  pop_ctzn <- bind_rows(pop_ctzn)
  plan("default")
  
# Clean column names and format some data
  pop_ctzn %>% 
    rename(LAU_NAME = OMRÅDE,
           Gender = KØN,
           Citizen = STATSB,
           Date = TID, 
           Pop = INDHOLD) %>% 
      mutate(LAU_NAME = gsub("Copenhagen", "København", LAU_NAME)) %>% 
      mutate(Date = gsub("Q", "", Date),
             Date = as_date_yq(as.integer(Date)),
             Date = first_of_quarter(Date)) -> pop_ctzn
  
# Foreign population ---- 
  pop_ctzn %>% 
    filter(Citizen != "Denmark", Pop > 0) %>% 
    # Add pop in percentage
    group_by(LAU_NAME, Date) %>% 
    mutate(Pop_pct = Pop / sum(Pop) * 100) %>% 
    ungroup() -> pop_migr

## Summarise data for all Denmark ----
  pop_migr %>% 
    group_by(Gender, Citizen, Date) %>% 
    summarise(Pop = sum(Pop)) %>% 
    ungroup() %>% 
    group_by(Date) %>% 
    mutate(Pop_pct = 100 * Pop/sum(Pop)) %>% 
    # New column with the total % of migration (Women + Men)    
    group_by(Date, Citizen) %>% 
    mutate(Pop_pct_total = sum(Pop_pct)) %>%
    ungroup() %>% 
    arrange(Citizen, Date, Pop_pct_total) -> pop_migr_DK
  
## Bar charts ranking origin countries ---- 
## Aux. function (bar charts- Top 20 countries)
  plot_rank_countries <- function(date) {
    p <- filter(pop_migr_DK, Date == as.Date(date)) %>% 
      top_n(Pop_pct_total, n = 40) %>% 
      ggplot(aes(y = fct_reorder(stringr::str_wrap(Citizen, 15), Pop_pct_total),
                 x = Pop/1000, fill = Gender)) + 
      geom_bar(stat = "identity") +
      scale_x_continuous(limits = c(0, 45)) +
      labs(title = "Top 20 migration countries by origin", 
           subtitle = paste("Date", date, sep = ": "),
           y = "", 
           x = "Pop [x1000]") +
      theme_bw() +
      theme(axis.text = element_text(size = 8),
            axis.title = element_text(size = 11, face ="bold"))
    print(p)
  }
  
  # Baseline (2008-Q1)
  p1 <- plot_rank_countries("2008-01-01")
  # 2012
  p2 <- plot_rank_countries("2012-01-01") +
    labs(title = "")
  # 2016
  p3 <- plot_rank_countries("2016-01-01") +
    labs(title = "")
  # Actual stage (2020)
  p4 <- plot_rank_countries("2020-01-01") +
    labs(title = "")
   
  (p1 + p2) / (p3 + p4) + plot_layout(guides = "collect") 
  ggsave("Results/top_20_migr_countries_2008_2020.png", width = 35, height = 25, units = "cm")
  
# Animation
  dates <- unique(pop_ctzn$Date)
  gif <- function() {lapply(dates, function(i) {plot_rank_countries(i)})}
  animation::saveGIF(gif(), interval = .5, movie.name = "top_20_mifr_countries_2008_2020.gif")

## Plot changes over time (by gender)
## Only plot counties with a population >8000 at any time
  pop_migr_DK %>%
    group_by(Date) %>%
    filter(Pop > 8000) %>%
    ggplot() +
    geom_line(aes(x = Date, y = Pop/1000, colour = Citizen)) +
    facet_grid(~Gender) +
    theme_bw() +
    labs(title = "Foreign citizens by gender in Denmark", 
         subtitle = "Population higher than 8000 at any time",
         y = "Pop [x1000]") +
    theme(axis.text = element_text(size = 8),
          axis.title = element_text(size = 11, face ="bold")) +
    scale_color_brewer(palette = "Paired")
  ggsave("Results/pop_migr_countries_2008_2020.png", width = 18, height = 10, units = "cm")
  