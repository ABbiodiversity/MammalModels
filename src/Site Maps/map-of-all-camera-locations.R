#-----------------------------------------------------------------------------------------------------------------------

# Title: Leaflet map for ABMI camera locations

# Attach packages
library(leaflet)
library(leaflet.extras)
library(sf)
library(dplyr)
library(readr)

root <- "G:/Shared drives/ABMI Camera Mammals/"

# Load data.

# Alberta province polygon
sf_ab <- st_read(paste0(root, "data/spatial/ab_prov.shp")) %>% st_transform(4326)

# ABMI camera sites (a mix of 'true' and public buffered sites)
sf_abmi <- st_read("./data/processed/abmi_cam_locations_new.shp")

# CMU, NWSAR, BG
sf_cmu <- st_read(paste0(root, "data/lookup/locations/spatial/cmu_2017-2020.shp"))
sf_bg <- st_read(paste0(root, "data/lookup/locations/spatial/bg_2016.shp"))
sf_nwsar <- st_read(paste0(root, "data/lookup/locations/spatial/nwsar_2021.shp"))

# Camera Icons
cam_icon <- makeAwesomeIcon(
  icon = "camera",
  iconColor = "black",
  library = "ion",
  markerColor = "white"
)

sf_lu <- st_read(paste0(root, "projects/osm-badr-site-selection/spatial/LU_2021_2022.shp")) %>% st_transform(4326)

# Public ABMI sites
sf_abmi_sites_all <- st_read(paste0(root, "data/spatial/abmi-sites-all.shp")) %>% st_transform(4326)

sf_osm_2022 <- st_read(paste0(root, "projects/osm-badr-site-selection/spatial/camaru_proposed_sites_osm_2022_projected.shp")) %>%
  st_transform(4326)

sf_osm_2021 <- st_read(paste0(root, "projects/osm-badr-site-selection/spatial/osm_camera_locations_2021.shp")) %>%
  st_transform(4326)

# ABMI Off-Grid.
sf_abmi_og <- st_read(paste0(root, "data/spatial/og.shp")) %>% st_transform(4326)

#-----------------------------------------------------------------------------------------------------------------------

# Map

map <- sf_ab %>%
  leaflet() %>%
  addTiles() %>%
  addProviderTiles("Esri.WorldImagery", group = "Satellite Imagery") %>%
  addFullscreenControl() %>%
  addResetMapButton() %>%
  addScaleBar(position = "bottomleft",
              options = scaleBarOptions(imperial = FALSE)) %>%
  addMeasure(primaryLengthUnit = "meters", primaryAreaUnit = "sqmeters",
             secondaryLengthUnit = "kilometers", secondaryAreaUnit = "sqkilometers",
             activeColor = "cornflowerblue", completedColor = "cornflowerblue",
             position = "topleft") %>%
  addDrawToolbar(position = "topleft", polylineOptions = FALSE, polygonOptions = FALSE,
                 circleOptions = FALSE, rectangleOptions = FALSE, circleMarkerOptions = FALSE,
                 markerOptions = drawMarkerOptions(repeatMode = TRUE, markerIcon = cam_icon),
                 editOptions = editToolbarOptions(edit = TRUE, remove = TRUE)) %>%

  # Alberta Polygon
  addPolygons(color = "#070707", weight = 1, smoothFactor = 0.2, opacity = 2,
              fill = FALSE) %>%

  #addPolygons(data = sf_lu, color = "white", weight = 1, smoothFactor = 0.2, opacity = 2, fill = FALSE,
  #            group = "Landscape Unit") %>%

  addCircleMarkers(data = sf_abmi, group = "ABMI EH (True)", radius = 4, color = "orange",
                   popup = paste0(sf_abmi$location)) |> #clusterOptions = markerClusterOptions()) %>%

  #addCircleMarkers(data = sf_abmi_sites_all, radius = 6, color = "blue",
  #                 popup = paste0(sf_abmi_sites_all$site), fillOpacity = 1, group = "ABMI EH (Public)") %>%

  addCircleMarkers(data = sf_cmu, radius = 4, color = "yellow",
                   popup = paste0(sf_cmu$location), fillOpacity = 1, group = "CMU/NWSAR Grids") %>%

  addCircleMarkers(data = sf_nwsar, radius = 4, color = "yellow",
                   popup = paste0(sf_nwsar$location), fillOpacity = 1, group = "CMU/NWSAR Grids") %>%

  #addCircleMarkers(data = sf_bg, radius = 4, color = "cornflowerblue",
  #                 popup = paste0(sf_bg$location), fillOpacity = 1, group = "Big Grids") %>%

  #addCircleMarkers(data = sf_osm_2022, radius = 4, color = "red",
  #                 popup = paste0(sf_osm_2022$OBJECTID), fillOpacity = 1, group = "OSM 2021-22") %>%

  #addCircleMarkers(data = sf_osm_2021, radius = 6, color = "darkred",
  #                 popup = paste0(sf_osm_2021$Site_ID), fillOpacity = 1, group = "OSM 2021-22") %>%

  addCircleMarkers(data = sf_abmi_og, radius = 6, color = "orange",
                   popup = paste0(sf_abmi_og$Site_ID), fillOpacity = 1, group = "ABMI OG") %>%

  # Layers control
  addLayersControl(overlayGroups = c("ABMI EH (True)", "CMU/NWSAR Grids", "Big Grids",
                                     "Satellite Imagery", "Landscape Unit", "ABMI EH (Public)",
                                     "ABMI OG",
                                     "OSM 2021-22"),
                   options = layersControlOptions(collapsed = FALSE),
                   position = "topright") %>%
  hideGroup(c("ABMI EH (Public)", "CMU/NWSAR Grids", "Big Grids"))

map
