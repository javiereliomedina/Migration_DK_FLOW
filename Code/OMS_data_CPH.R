################################################-
###     OMS data Copenhagen   ##################
################################################-

  library(tidyverse)
  library(sf)
  library(ggspatial)
  library(osmdata)
  library(giscoR)

# Based on: https://rspatialdata.github.io/osm.html
# Available features: https://wiki.openstreetmap.org/wiki/Map_features

# Bounding box ----
  box <- c(xmin = 12.44, xmax = 12.70, ymin = 55.60, ymax = 55.74)

# Administrative units ----
# Create a local repository for caching gisco big files
  options(gisco_cache_dir = "C:/GISCO_spatial_data") 
  
  CPH <- gisco_get_lau(year = "2019", country = "DNK") %>%
    mutate(LAU_NAME = str_conv(LAU_NAME, "UTF-8")) %>% 
    arrange(LAU_NAME) %>%
    st_crop(box)
  
# Retrieve data from OMS ----
  CPH_bb <- getbb("Copenhagen")

## rivers and canals 
  CPH_rivers <- CPH_bb %>%
    opq() %>%
    add_osm_feature(key = "waterway", value = c("river", "canal")) %>%
    osmdata_sf()

## Lakes 
  CPH_water <- CPH_bb %>%
    opq() %>%
    add_osm_feature(key = "natural", value = "water") %>%
    osmdata_sf()

## Green areas 
  CPH_parks <- CPH_bb %>%
    opq() %>%
    add_osm_feature(key = "leisure", value = c("park", "common")) %>%
    osmdata_sf()
  
  CPH_forest <- CPH_bb %>%
    opq() %>%
    add_osm_feature(key = "landuse", value = c("meadow", "allotments", "forest")) %>%
    osmdata_sf()
  
  CPH_scrub <- CPH_bb %>%
    opq() %>% 
    add_osm_feature(key = "natural", value = "scrub") %>% 
    osmdata_sf()
  
## Streets 
  CPH_streets <- CPH_bb %>%
    opq() %>%
    add_osm_feature("highway", c("motorway", "primary", "secondary", "tertiary")) %>%
    osmdata_sf()

## Hospitals 
  CPH_hospitals <- CPH_bb %>%
    opq() %>%
    add_osm_feature("amenity", "hospital") %>%
    osmdata_sf()

## Airport 
  CPH_airport <- CPH_bb %>%
    opq() %>%
    add_osm_feature("aeroway", "aerodrome") %>%
    osmdata_sf()
  
  CPH_airport_runway <- CPH_bb %>%
    opq() %>%
    add_osm_feature("aeroway", "runway") %>%
    osmdata_sf()

## Historic
  CPH_historic <- CPH_bb %>%
    opq() %>%
    add_osm_feature("historic", "fort") %>%
    osmdata_sf()

# Visualization ----

  ggplot() +
    geom_sf(data = CPH, fill = "grey95") +
    geom_sf(data = CPH_rivers$osm_lines, inherit.aes = FALSE, colour = "#7fc0ff") +
    geom_sf(data = CPH_water$osm_polygons, inherit.aes = FALSE, colour = NA, fill = "#7fc0ff") +
    geom_sf(data = CPH_parks$osm_polygons, colour = NA, fill = "palegreen1",  alpha = .3) +
    geom_sf(data = CPH_forest$osm_polygons, colour = NA, fill = "palegreen2",  alpha = .3) +
    geom_sf(data = CPH_scrub$osm_polygons, colour = NA, fill = "palegreen3",  alpha = .3) +
    geom_sf(data = CPH_streets$osm_lines, inherit.aes = FALSE, color = "#ffbe7f", size = .4, alpha = .8) +
    geom_sf(data = CPH_hospitals$osm_polygons, inherit.aes = FALSE, colour = NA, fill = "#D55E00") +
    geom_sf(data = CPH_historic$osm_polygons, fill = "palegreen4", colour = NA) +
    geom_sf(data = CPH_airport$osm_polygons, colour = NA, fill = "grey88") +
    geom_sf(data = CPH_airport_runway$osm_lines, colour = "grey", size = 3) +
    coord_sf(crs = sf::st_crs(CPH),
             xlim = c(12.44, 12.70),
             ylim = c(55.60, 55.74),
             expand = FALSE) +
    labs(title = "Hospitals in Copenhagen",
         caption = "Source: Open Street Map\nAuthor: J. Elio (@Elio_Javi), C. Keßler, H.S. Hansen. Aalborg University, Department of Planning") +
    ggthemes::theme_map() +
    theme(panel.background = element_rect(fill = "#7fc0ff")) +
    annotation_scale(location = "tr",
                     pad_y = unit(1.5, "cm")
                     ) +
    annotation_north_arrow(location = "tr",
                           pad_x = unit(2, "cm"), 
                           which_north = "true",
                           height = unit(1, "cm"),
                           width = unit(0.75, "cm"))
  
  ggsave("Results/CPH_Hospitals.png")

  
