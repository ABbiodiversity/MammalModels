# Tolko Moose Analysis

# Attach packages
library(leaflet)
library(leaflet.extras)
library(sf)
library(dplyr)
library(readr)
library(stringr)

g_drive <- "G:/Shared drives/ABMI Camera Mammals/"

# Alberta province polygon
sf_ab <- st_read(paste0(root, "data/spatial/ab_prov.shp")) |> st_transform(4326)

# ABMI camera sites (a mix of 'true' and public buffered sites)
sf_abmi <- st_read("C:/Users/mabec/Documents/R/abmi-shiny-app/data/processed/abmi_cam_locations_new.shp")

# NWSAR
sf_nwsar <- st_read(paste0(root, "data/lookup/locations/spatial/nwsar_2021.shp"))

sf_abmi_og <- st_read(paste0(root, "data/spatial/og.shp")) %>% st_transform(4326)

# Camera Icons
cam_icon <- makeAwesomeIcon(
  icon = "camera",
  iconColor = "black",
  library = "ion",
  markerColor = "white"
)

sf_tolko <- st_read(paste0(g_drive, "data/spatial/ab_fma.shp")) |>
  st_transform(4326) |>
  filter(str_detect(FMA_NAME, "Tolko"))

map <- sf_ab |>
  leaflet() |>
  addTiles() |>
  addProviderTiles("Esri.WorldImagery", group = "Satellite Imagery") |>
  addFullscreenControl() |>
  addResetMapButton() |>
  addScaleBar(position = "bottomleft",
              options = scaleBarOptions(imperial = FALSE)) %>%
  addMeasure(primaryLengthUnit = "meters", primaryAreaUnit = "sqmeters",
             secondaryLengthUnit = "kilometers", secondaryAreaUnit = "sqkilometers",
             activeColor = "cornflowerblue", completedColor = "cornflowerblue",
             position = "topleft") |>
  addDrawToolbar(position = "topleft", polylineOptions = FALSE, polygonOptions = FALSE,
                 circleOptions = FALSE, rectangleOptions = FALSE, circleMarkerOptions = FALSE,
                 markerOptions = drawMarkerOptions(repeatMode = TRUE, markerIcon = cam_icon),
                 editOptions = editToolbarOptions(edit = TRUE, remove = TRUE)) |>

  # Alberta Polygon
  addPolygons(color = "#070707", weight = 1, smoothFactor = 0.2, opacity = 2,
              fill = FALSE) |>

  addPolygons(data = sf_tolko, color = "white", weight = 2, smoothFactor = 0.2, opacity = 2,
              fill = FALSE) |>

  addCircleMarkers(data = sf_abmi, group = "ABMI EH (True)", radius = 4, color = "orange",
                   popup = paste0(sf_abmi$location)) |> #clusterOptions = markerClusterOptions()) %>%

  addCircleMarkers(data = sf_nwsar, radius = 4, color = "yellow",
                   popup = paste0(sf_nwsar$location), fillOpacity = 1, group = "NWSAR Grids") |>

  addCircleMarkers(data = sf_abmi_og, radius = 6, color = "orange",
                   popup = paste0(sf_abmi_og$Site_ID), fillOpacity = 1, group = "ABMI OG") |>

  # Layers control
  addLayersControl(overlayGroups = c("ABMI EH (True)", "NWSAR Grids", "ABMI OG",
                                     "Satellite Imagery"),
                   options = layersControlOptions(collapsed = FALSE),
                   position = "topright")

map







