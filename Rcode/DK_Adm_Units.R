#****************************************
#####     Denmark: GISCO data   #########
#****************************************

# # Scale 1:1 Million
# # EN: ©EuroGeographics for the administrative boundaries
# # Create a local repository for caching gisco big files
# options(gisco_cache_dir = "C:/Users/FU53VP/OneDrive - Aalborg Universitet/Dokumenter/GISCO_spatial_data")

  library(tidyverse)
  library(sf)
  library(giscoR)

# Nomenclature of Territorial Units for Statistics (NUTS) ----
  dk_country <- gisco_get_countries(resolution = "01", year = "2016", country = "DNK")
  dk_regions <- gisco_get_nuts(resolution = "01", year = "2016", country = "DNK", nuts_level = "2")
  dk_provinces <- gisco_get_nuts(resolution = "01", year = "2016", country = "DNK", nuts_level = "3")
  
# Local Administrative Units (LAU) ----
  dk_lau <- gisco_get_lau(year = "2019", country = "DNK")

# Communes ----
  dk_communes <- gisco_get_communes(year = "2016", country = "DNK")
  
# Plots ----
  plot(st_geometry(dk_country), border = "blue", col = "grey", main = "Country")
  plot(st_geometry(dk_regions), border = "blue", col = "grey", main = "Regions")
  plot(st_geometry(dk_provinces), border = "blue", col = "grey", main = "Provinces")
  plot(st_geometry(dk_lau), border = "blue", col = "grey", main = "Municipalities")
  plot(st_geometry(dk_communes), border = "blue", col = "grey", main = "Communes")
  