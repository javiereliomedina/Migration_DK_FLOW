###*********************************
###   AGE STRUCTURE MIGRANTS    ####
###*********************************

  library(danstat) 
  library(tidyverse)
  library(readr)
  library(dint)
  library(gganimate)
  library(patchwork)
  library(gridExtra)

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
  var_pop <- get_table_metadata(table_id = "FOLK1D", variables_only = TRUE)
  var_pop %>% select(id, text)
  var_pop$values

## pull the needed data from the API
### Variables we would like to get data
  var_input <- list(list(code = "OMRÅDE", values = "000"),
                    list(code = "KØN", values = c(1,2)),
                    list(code = "ALDER", values = seq(0, 125, 1)),
                    list(code = "STATSB", values = c("DANSK", "UDLAND")),
                    list(code = "tid", values = NA))

### Get data 
  get_data("FOLK1D", variables = var_input) %>%
    rename(Country = OMRÅDE,
           Gender = KØN, 
           Age = ALDER,
           Citizen = STATSB,
           Year = TID,
           Pop = INDHOLD) %>%  
    mutate(Country = "Denmark",
           Citizen = case_when(Citizen == "Danish citizen" ~ "Danish",
                               Citizen == "Foreign citizen" ~ "Foreign"),
           Date = gsub("Q", "", Year),
           Date = as.integer(Date),
           Date = as_date_yq(Date),
           Date = first_of_quarter(Date)
    ) %>% 
    separate(Year, c("Year", "Quarter"), sep = "Q") %>% 
    mutate(Year = as.integer(Year),
           Quarter = as.integer(Quarter)) %>% 
    mutate(Age = parse_number(Age), 
           Age = as.integer(Age),
           Age_cut = cut(Age, breaks = seq(0, 130, 5), right = FALSE)) %>% 
    group_by(Date) %>% 
    mutate(Pop_per = 100 * Pop / sum(Pop)) %>%    
    ungroup()-> pop_migr_age 
  
## Population pyramids ----
## Aux. function for plotting population pyramids 

    plot_pyramid <- function(df) {
      brks_y <- seq(-4, 4, 1)
      lmts_y = c(min(brks_y), max(brks_y))
      lbls_y <- paste0(as.character(abs(brks_y)), "%")
      ggplot() + 
        geom_bar(data = filter(df, Gender == "Women"),
                 aes(x = cut_interval(Age, length = 5, right = FALSE),
                     y = Pop_per,
                     fill =  Citizen), 
                 stat = "identity", 
                 width = 1) + 
        geom_bar(data = filter(df, Gender == "Men"),
                 aes(x = cut_interval(Age, length = 5, right = FALSE),
                     y = -Pop_per,
                     fill = Citizen), 
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
        labs(title = "Population pyramid of danish immigrans",
             subtitle = paste("Date", first(df$Date), sep = ": ")) +
        scale_fill_manual(values = c("#0072B2", "#D55E00")) +
        theme_bw() +
        theme(axis.text = element_text(size = 7),
              axis.title = element_text(size = 11, face ="bold"))
    }

## Plot baseline (2008Q1 - 20081)
  filter(pop_migr_age, Date ==  first_of_quarter(as_date_yq(20081))) %>% 
    plot_pyramid() -> p_2008Q1

## Situation 2020Q3
  filter(pop_migr_age, Date ==  first_of_quarter(as_date_yq(20203))) %>% 
    plot_pyramid() +
    labs(title = "") -> p_2020Q3
  
## Plot together
  p_2008Q1 + p_2020Q3 + plot_layout(guides = 'collect')  
  ggsave("Results/pop_pyramid_2008_2020_migr.png", width = 25, height = 12, units = "cm")

# Animation 2008-2020 ----
# Not sure why I got an error if all the data are selected
# and I've removed the first date (baseline: 2008-Q1)

  plot_pyramid(filter(pop_migr_age, Date > "2008-01-01")) + 
    transition_states(Date, wrap = FALSE) +
    labs(title = "Population pyramid of Denmark",
         subtitle = "Date: {closest_state}") -> anim
  
  anim_save("Results/pop_pyramid_2008_2020_migr_anim.gif", anim)
  
  