#####################################
##########     Denmark     ##########
#####################################

  library(tidyverse)
  library(sf)

# Administrative units ----
## Data from https://ec.europa.eu/eurostat/web/gisco/geodata/reference-data/administrative-units-statistical-units/lau
## Scale 1:1 Million
## EN: ©EuroGeographics for the administrative boundaries

## Download data (Only run once)
## Load data
## Plots
  plot(st_geometry(dk_country), border = "blue", col = "grey", main = "Country")
  plot(st_geometry(dk_regions), border = "blue", col = "grey", main = "Regions")
  plot(st_geometry(dk_provinces), border = "blue", col = "grey", main = "Provinces")
  plot(st_geometry(dk_muni), border = "blue", col = "grey", main = "Municipalities")
  plot(st_geometry(dk_comm), border = "blue", col = "grey", main = "Communes")
  
# Population ----
## Download .csv from https://www.statbank.dk/statbank5a/SelectTable/Omrade0.asp?PLanguage=1
  read_csv2("Rdata/Statistics_DK/Population.csv", col_names = FALSE) %>% 
    setNames(c("Total", "Year", "Status", "LAU_NAME", "Men","Women")) %>% 
    separate(Year, c("Year", "Quarter"), sep = "Q") %>% 
    mutate(LAU_NAME = str_conv(LAU_NAME, "latin1"),
           LAU_NAME = gsub("Copenhagen", "København", LAU_NAME)) %>% 
    select(-Total) %>% 
    select(LAU_NAME, everything()) %>% 
    left_join(dk_muni, by = "LAU_NAME") %>% 
    st_as_sf() -> pop_muni
  

  
  
  