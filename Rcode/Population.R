#########################################
#####     Denmark: population       #####
#########################################

  library(tidyverse)
  library(readr)
  library(SnowballC)
  
# Population
## Download .csv from https://www.statbank.dk/statbank5a/SelectTable/Omrade0.asp?PLanguage=1
  read_csv2("Rdata/Statistics_DK/Population.csv", col_names = FALSE) %>% 
    setNames(c("Year", "Status", "LAU_NAME", "Total", "Men","Women")) %>% 
    separate(Year, c("Year", "Quarter"), sep = "Q") %>% 
    mutate(LAU_NAME = str_conv(LAU_NAME, "latin1"),
           LAU_NAME = gsub("Copenhagen", "København", LAU_NAME)) %>% 
    select(LAU_NAME, everything()) %>% 
    mutate(Status = stringr::str_to_lower(Status),
           Status = gsub(" ", "_", Status),
           # Status = gsub("(\\b[a-z][a-z][a-z])|.", "\\1", Status, perl=TRUE)
           ) %>%
    pivot_wider(names_from = Status, values_from = c("Total", "Men", "Women")) -> pop_muni
  
  
# Join with municipalities (dk_muni)
  left_join(pop_muni, dk_muni, by = "LAU_NAME") %>% 
    st_as_sf() -> pop_muni
  
  
  mapview::mapview(pop_muni, zcol = "Women_divorced")
  
  