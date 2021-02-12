#****************************************
#####     Denmark: GISCO data   #########
#****************************************

## Scale 1:1 Million
## EN: ©EuroGeographics for the administrative boundaries
## Create a local repository for caching gisco big files
## e.g. C:/Users/FU53VP/OneDrive - Aalborg Universitet/Dokumenter/GISCO_spatial_data

  library(tidyverse)
  library(sf)
  library(giscoR)

  options(gisco_cache_dir = "C:/GISCO_spatial_data")


# Nomenclature of Territorial Units for Statistics (NUTS) ----
  dk_country <- gisco_get_countries(resolution = "01", year = "2016", country = "DNK")
  
  dk_regions <- gisco_get_nuts(resolution = "01", year = "2016", country = "DNK", nuts_level = "2") %>% 
    mutate(NAME_LATN = str_conv(NAME_LATN, "UTF-8"),
           NUTS_NAME = str_conv(NUTS_NAME, "UTF-8"))
    
  dk_provinces <- gisco_get_nuts(resolution = "01", year = "2016", country = "DNK", nuts_level = "3") %>% 
    mutate(NAME_LATN  = str_conv(NUTS_NAME, "UTF-8"),
           NUTS_NAME  = str_conv(NUTS_NAME, "UTF-8"))
  
# Local Administrative Units (LAU) ----
  dk_lau <- gisco_get_lau(year = "2019", country = "DNK") %>%
    mutate(LAU_NAME = str_conv(LAU_NAME, "UTF-8")) %>% 
    arrange(LAU_NAME)

# Communes ----
  dk_communes <- gisco_get_communes(year = "2016", country = "DNK") %>% 
    mutate(COMM_NAME  = str_conv(COMM_NAME, "UTF-8"))
  
# Plots ----
  plot(st_geometry(dk_country), border = "blue", col = "grey", main = "Country")
  plot(st_geometry(dk_regions), border = "blue", col = "grey", main = "Regions")
  plot(st_geometry(dk_provinces), border = "blue", col = "grey", main = "Provinces")
  plot(st_geometry(dk_lau), border = "blue", col = "grey", main = "Municipalities")
  plot(st_geometry(dk_communes), border = "blue", col = "grey", main = "Communes")
  