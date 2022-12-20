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
sf_lu <- st_read(paste0(g_drive, "osm-badr-site-selection/spatial/LU_3Yr_Aug2022/LU3YR_Aug22.shp")) |>
  st_transform(4326) |>
  clean_names() |>
  select(lu, lu_treatment = lu_treatmnt, label, year, deciles, shape_area)

# JEM units for 2023 (from Jenet)
# LUs in 2023-2024: 9, 14, 16, 22
sf_jem <- st_read(paste0(g_drive, "osm-badr-site-selection/spatial/jem_2023_buffer_1000m.shp")) |>
  st_transform(4326) |>
  clean_names() |>
  select(lu, site_name, type, treatment, buff_dist) |>
  mutate(year = 2023)

# Habitat + Treatment layers
sf_hab_treat <- st_read(paste0(g_drive, "osm-badr-site-selection/spatial/veg-treatment-join-clip-to-2023-jems_v2.shp")) |>
  st_transform(4326) |>
  clean_names() |>
  select(type, treatment)

treedlow <- sf_hab_treat |> filter(type == "TreedLow20")
decidmix <- sf_hab_treat |> filter(type == "DecidMix40")

# Palettes
pal_decid <- colorFactor(
  palette = "Dark2",
  domain = decidmix$treatment
)

pal_treedlow <-colorFactor(
  palette = "Dark2",
  domain = treedlow$treatment
)

# Icons
cam_abmi <- makeAwesomeIcon(
  icon = "camera",
  iconColor = "white",
  library = "ion",
  markerColor = "lightgray"
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
                 markerOptions = drawMarkerOptions(repeatMode = TRUE, markerIcon = cam_abmi),
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
  addPolygons(data = sf_jem,
              color = "#6baed6",
              fillColor = "black",
              weight = 1,
              smoothFactor = 0.2,
              opacity = 1,
              fillOpacity = 0.1,
              group = "JEM Sites",
              options = leafletOptions(pane = "Boundaries JEM"),
              popup = paste("Habitat Target: ", "<b>", sf_jem$type, "</b>", "<br>",
                            "<br>",
                            "Treatment Target: ", "<b>", sf_jem$treatment, "</b>", "<br>",
                            "<br>",
                            "JEM Site Code: ", "<b>", sf_jem$site_name, "</b>"),
              highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE)) |>

  # Decidmix40 + HF treatments layer
  addPolygons(data = decidmix, color = "grey50", fillColor = ~ pal_decid(treatment),
              weight = 1, smoothFactor = 0.2, opacity = 1, fillOpacity = 0.5, group = "Habitat: DecidMix40+",
              options = leafletOptions(pane = "Habitat Treatment Data"),
              popup = paste("Treatment: ", "<b>", decidmix$treatment, "</b>"),
              highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE)) |>

  # Treedlow + HF treatments layer
  addPolygons(data = treedlow, color = "grey50", fillColor = ~ pal_treedlow(treatment),
              weight = 1, smoothFactor = 0.2, opacity = 1, fillOpacity = 0.5, group = "Habitat: TreedLow20+",
              options = leafletOptions(pane = "Habitat Treatment Data"),
              popup = paste("Treatment: ", "<b>", treedlow$treatment, "</b>"),
              highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE)) |>

  # Layers control
  addLayersControl(overlayGroups = c("Landscape Units",
                                     "JEM Sites",
                                     "Satellite Imagery"),
                   baseGroups = c("None", "Habitat: DecidMix40+", "Habitat: TreedLow20+"),
                   options = layersControlOptions(collapsed = FALSE),
                   position = "topright") |>

  # Legend
  addLegend(data = decidmix, position = "topright", pal = pal_decid,
            values = ~ treatment,
            opacity = 1)

map

#-----------------------------------------------------------------------------------------------------------------------






