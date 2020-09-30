############################################
#####     Denmark: Eurostat data       #####
############################################
# Data from https://ec.europa.eu/eurostat/web/gisco/geodata/reference-data/administrative-units-statistical-units/lau
# Scale 1:1 Million
# EN: ©EuroGeographics for the administrative boundaries

  library(tidyverse)
  library(sf)

# Nomenclature of Territorial Units for Statistics (NUTS) ----
  NUTS2021_URL <- "http://gisco-services.ec.europa.eu/distribution/v2/nuts/download/ref-nuts-2021-01m.shp.zip"
  download.file(NUTS2021_URL, destfile = "Rdata/NUTS2021_Eurostat.zip")
  unzip(zipfile = "Rdata/NUTS2021_Eurostat.zip", exdir   = "Rdata/NUTS2021_Eurostat")

  ## NUTS Level 1
  unzip(zipfile = "Rdata/NUTS2021_Eurostat/NUTS_RG_01M_2021_4326_LEVL_1.shp.zip",
        exdir   = "Rdata/NUTS2021_LEVL_1") 
  dk_country <- read_sf("Rdata/NUTS2021_LEVL_1/NUTS_RG_01M_2021_4326_LEVL_1.shp") %>%
    filter(CNTR_CODE == "DK")

  ## NUTS level 2
  unzip(zipfile = "Rdata/NUTS2021_Eurostat/NUTS_RG_01M_2021_4326_LEVL_2.shp.zip",
        exdir   = "Rdata/NUTS2021_LEVL_2") 
  dk_regions <- read_sf("Rdata/NUTS2021_LEVL_2/NUTS_RG_01M_2021_4326_LEVL_2.shp") %>%
    filter(CNTR_CODE == "DK")

  ## NUTS Level 3  
  unzip(zipfile = "Rdata/NUTS2021_Eurostat/NUTS_RG_01M_2021_4326_LEVL_3.shp.zip",
        exdir   = "Rdata/NUTS2021_LEVL_3") 
  dk_provinces <- read_sf("Rdata/NUTS2021_LEVL_3/NUTS_RG_01M_2021_4326_LEVL_3.shp") %>%
    filter(CNTR_CODE == "DK")

# Local Administrative Units (LAU) ----
  LAU2019_URL <- "http://gisco-services.ec.europa.eu/distribution/v2/lau/download/ref-lau-2019-01m.shp.zip"
  download.file(LAU2019_URL, destfile = "Rdata/LAU2019.zip")
  unzip(zipfile = "Rdata/LAU2019.zip", exdir   = "Rdata/LAU2019")
  unzip(zipfile = "Rdata/LAU2019/LAU_RG_01M_2019_4326.shp.zip",
        exdir   = "Rdata/LAU_RG_01M_2019_4326") 
  dk_muni <- read_sf("Rdata/LAU_RG_01M_2019_4326/LAU_RG_01M_2019_4326.shp") %>%
    filter(CNTR_CODE == "DK")

# Communes ----
  Communes2016_URL <- "http://gisco-services.ec.europa.eu/distribution/v2/communes/download/ref-communes-2016-01m.shp.zip"
  download.file(Communes2016_URL, destfile = "Rdata/Communes2016.zip")
  unzip(zipfile = "Rdata/Communes2016.zip", exdir   = "Rdata/Communes2016")
  unzip(zipfile = "Rdata/Communes2016/COMM_RG_01M_2016_4326.shp.zip",
        exdir   = "Rdata/COMM_RG_01M_2016_4326") 
  dk_comm <- read_sf("Rdata/COMM_RG_01M_2016_4326/COMM_RG_01M_2016_4326.shp") %>%
    filter(CNTR_CODE == "DK")
  

# Plots ----
  plot(st_geometry(dk_country), border = "blue", col = "grey", main = "Country")
  plot(st_geometry(dk_regions), border = "blue", col = "grey", main = "Regions")
  plot(st_geometry(dk_provinces), border = "blue", col = "grey", main = "Provinces")
  plot(st_geometry(dk_muni), border = "blue", col = "grey", main = "Municipalities")
  plot(st_geometry(dk_comm), border = "blue", col = "grey", main = "Communes")
  