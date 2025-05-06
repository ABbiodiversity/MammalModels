#-----------------------------------------------------------------------------------------------------------------------

# Title:        BDT Camera/ARU Site Map
# Description:  Create leaflet map for display of Biodiversity Trajectories camera locations

# Author:       Marcus Becker
# Last Updated: June 2024

#-----------------------------------------------------------------------------------------------------------------------

# Attach required packages
library(readr)
library(sf)
library(leaflet)
library(leaflet.extras)
library(dplyr)
library(stringr)
library(tidyr)

# Shared Google Drive path to relevant spatial data
g_drive <- "G:/Shared drives/ABMI Camera Mammals/Projects/Biodiversity Trajectories/"

new_locations <- read_csv(paste0(g_drive, "New BDT Locations.csv")) |>
  st_as_sf(coords = c("POINT_X", "POINT_Y"), crs = 4326) |>
  select(ABMI_ID, Treatment = Fire_Harve)

old_locations <- read_csv(paste0(g_drive, "ABMI_Biodiversity_Trajectories_2023_location_report.csv")) |>
  filter(!is.na(latitude)) |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
  select(location)

ab_fma <- st_read(paste0(g_drive, "ab_fma.shp")) |>
  st_transform(4326) |>
  select(FMA_NAME)

#-----------------------------------------------------------------------------------------------------------------------

# Create interactive map for viewing

cam_new <- makeAwesomeIcon(
  icon = "camera",
  iconColor = "black",
  library = "ion",
  markerColor = "white"
)

cam_old <- makeAwesomeIcon(
  icon = "camera",
  iconColor = "white",
  library = "ion",
  markerColor = "grey40"
)

map <- ab_fma |>
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
                 markerOptions = drawMarkerOptions(repeatMode = TRUE, markerIcon = cam_new),
                 editOptions = editToolbarOptions(edit = TRUE, remove = TRUE)) |>

  # Add polygon layers:

  # FMA polygons
  addPolygons(color = "#070707",
              weight = 1,
              smoothFactor = 0.2,
              opacity = 2,
              fill = FALSE,
              group = "None") |>

  addAwesomeMarkers(data = new_locations,
                    icon = cam_old,
                    group = "2025 Camera Locations",
                    popup = paste("Location: ", "<b>", new_locations$ABMI_ID, "</b>",
                                  "<br>",
                                  "Treatment: ", new_locations$Treatment)) |>

  addAwesomeMarkers(data = old_locations,
                    icon = cam_new,
                    group = "2023 Camera Locations",
                    popup = paste("Location: ", "<b>", old_locations$location, "</b>",
                                  "<br>",
                                  "Treatment: "))

map












