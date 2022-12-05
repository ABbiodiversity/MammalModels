#-----------------------------------------------------------------------------------------------------------------------

# Title:        OSM BADR Camera/ARU Site Map
# Description:  Create leaflet map for display of oilsands monitoring (OSM) before-after dose-reponse (BADR) camera and
#               ARU sites, JEMs, and Landscape Units from 2021-2023.

# Author:       Marcus Becker
# Last Updated: December 2022

#-----------------------------------------------------------------------------------------------------------------------

# Attach required packages
library(readr)
library(sf)
library(leaflet)
library(leaflet.extras)
library(dplyr)
library(stringr)
library(tidyr)
library(janitor)

# Shared Google Drive path
g_drive <- "G:/Shared drives/ABMI Camera Mammals/projects/"

#-----------------------------------------------------------------------------------------------------------------------

# Alberta
sf_ab <- st_read(paste0(g_drive, "osm-badr-site-selection/spatial/ab_prov.shp")) |>
  st_transform(4326)

# Landscape Units (LUs)
sf_lu <- st_read(paste0(g_drive, "OSM-BADR/LU_3YR/LU_3YR.shp")) |>
  st_transform(4326) |>
  clean_names() |>
  select(lu, label, year, deciles, shape_area)

# JEM units for 2023 (from Jenet)
# LUs in 2023-2024: 9, 14, 16, 22
sf_jem <- st_read(paste0(g_drive, "osm-badr-site-selection/spatial/2023_24_JEMS_draft.shp")) |>
  st_transform(4326) |>
  clean_names() |>
  select(lu, site_name, type, treatment) |>
  mutate(year = 2023)

# Add buffer to JEMs (will do in Arc - but do here for now to view.)
sf_jem_buffer <- st_buffer(sf_jem, 1500)

# Icons
cam_abmi_2021 <- makeAwesomeIcon(
  icon = "camera",
  iconColor = "white",
  library = "ion",
  markerColor = "lightgray"
)

cam_abmi_2022 <- makeAwesomeIcon(
  icon = "camera",
  iconColor = "black",
  library = "ion",
  markerColor = "white"
)

#-----------------------------------------------------------------------------------------------------------------------

# Create interactive map for viewing

map <- sf_ab |>
  leaflet() |>
  addTiles() |>
  addProviderTiles("Esri.WorldImagery",
                   group = "Satellite Imagery") |>
  addFullscreenControl() |>
  addResetMapButton() |>
  addScaleBar(position = "bottomleft",
              options = scaleBarOptions(imperial = FALSE)) |>
  addMeasure(position = "topleft",
             primaryLengthUnit = "meters",
             primaryAreaUnit = "sqmeters",
             secondaryLengthUnit = "kilometers",
             secondaryAreaUnit = "sqkilometers",
             activeColor = "cornflowerblue",
             completedColor = "cornflowerblue") |>
  addDrawToolbar(position = "topleft",
                 polylineOptions = FALSE,
                 polygonOptions = FALSE,
                 circleOptions = FALSE,
                 rectangleOptions = FALSE,
                 circleMarkerOptions = FALSE,
                 markerOptions = drawMarkerOptions(repeatMode = TRUE, markerIcon = cam_abmi_2022),
                 editOptions = editToolbarOptions(edit = TRUE, remove = TRUE)) |>
  addMapPane(name = "Boundaries LU", zIndex = 410) |>
  addMapPane(name = "Boundaries JEM", zIndex = 415) |>
  addMapPane(name = "Habitat Treatment Data", zIndex = 420) |>

  # Add polygon layers:

  # Alberta provincial polygon (note: Let's grab an OSR layer instead)
  addPolygons(color = "#070707",
              weight = 1,
              smoothFactor = 0.2,
              opacity = 2,
              fill = FALSE,
              group = "None",
              options = leafletOptions(pane = "Boundaries LU")) |>

  # Landscape Units (2021-2023)
  addPolygons(data = sf_lu,
              color = "white",
              weight = 1,
              smoothFactor = 0.2,
              opacity = 1,
              fillOpacity = 0.05,
              popup = paste("Treatment: ", "<b>", sf_lu$label, "</b>",
                            "<br>",
                            "Sampling Year: ", "<b>", sf_lu$year, "</b>",
                            "<br>",
                            "LU Code: ", "<b>", sf_lu$lu, "</b>")) |>

  # JEM sites + 1500m buffer
  addPolygons(data = sf_jem_buffer,
              color = "#6baed6",
              fillColor = "black",
              weight = 1,
              smoothFactor = 0.2,
              opacity = 1,
              fillOpacity = 0.1,
              group = "JEM Sites",
              options = leafletOptions(pane = "Boundaries JEM"),
              popup = paste("Habitat Target: ", "<b>", sf_jem_buffer$type, "</b>", "<br>",
                            "<br>",
                            "Treatment Target: ", "<b>", sf_jem_buffer$treatment, "</b>", "<br>",
                            "<br>",
                            "JEM Site Code: ", "<b>", sf_jem_buffer$site_name, "</b>"),
              highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE)) |>

  # Layers control
  addLayersControl(overlayGroups = c("Landscape Units",
                                     "JEM Sites",
                                     "Satellite Imagery"),
                   baseGroups = c("None"),
                   options = layersControlOptions(collapsed = FALSE),
                   position = "topright")

map






