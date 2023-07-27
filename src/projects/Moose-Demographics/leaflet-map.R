#-----------------------------------------------------------------------------------------------------------------------

# Project:          Moose Demographics

# Title:            Map of sites/deployments/grid used
# Description:

# Author:           Marcus Becker

# Previous scripts: None

#-----------------------------------------------------------------------------------------------------------------------

# Import packages
library(readr)
library(mapview)
library(sf)
library(dplyr)
library(leaflet)
library(leaflet.extras)

# Google drive
g_drive <- "G:/Shared drives/ABMI Camera Mammals/"

# CMU deployment locations
cmu_loc <- read_csv(paste0(g_drive, "data/lookup/locations/all-projects_all-years_locations-for-habitat-modeling.csv")) |>
  filter(str_detect(project, "CMU"),
         !str_detect(location, "T$")) |>
  select(-project) |>
  # A few MAL locations are missing coordinates
  filter(!is.na(longitude)) |>
  distinct() |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

# Quick map
mapview(cmu_loc)

# Join detections data
cmu_loc_det <- cmu_loc |>
  left_join(detections, by = "location") |>
  mutate(category = case_when(
    n_fem > 0 & n_juv > 0 ~ "Calf/Cow",
    n_fem > 0 & n_juv < 1 ~ "Cow Only",
    TRUE ~ "None"
  )) |>
  mutate(category = factor(category, levels = c("None", "Cow Only", "Calf/Cow")))

# Let's load the caribou ranges (which may become relevant)
sf_caribou <- st_read(paste0(g_drive, "data/spatial/ab_caribou_ranges.shp")) |>
  st_transform(4326) |>
  filter(str_detect(LOCALRANGE, "Athabasca|Rich"))

# Create leaflet map with a bit more interactive control

cat_pal <- colorFactor(
  palette = c("Black", "Orange", "Red"),
  domain = cmu_loc_det$category
)

map <- sf_caribou |>
  leaflet() |>
  addTiles() |>
  addProviderTiles("CartoDB.Positron", group = "cartoDB") |>
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
                 markerOptions = FALSE,
                 editOptions = editToolbarOptions(edit = TRUE, remove = TRUE)) |>
  addMapPane(name = "Caribou Ranges", zIndex = 410) |>
  addMapPane(name = "CMU Deployments", zIndex = 420) |>

  # Add Polygon Layers

  addPolygons(color = "#6baed6",
              weight = 1,
              smoothFactor = 0.2,
              opacity = 1,
              fillColor = "black",
              fillOpacity = 0.1,
              group = "Caribou Ranges",
              popup = paste("Local Range: ", "<b>", sf_caribou$LOCALRANGE, "</b>", "<br>",
                          "Subunit: ", "<b>", sf_caribou$SUBUNIT, "</b>"),
              highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
              options = leafletOptions(pane = "Caribou Ranges")) |>

  addCircles(data = cmu_loc_det,
             color = ~ cat_pal(category),
             radius = 15,
             weight = 15,
             popup = paste("Location: ", cmu_loc_det$location, "<br>",
                           "Number of Cow Detections: ", cmu_loc_det$n_fem, "<br>",
                           "Number of Calf Detections: ", cmu_loc_det$n_juv),
             options = leafletOptions(pane = "CMU Deployments")) |>

  addLegend(data = cmu_loc_det,
            position = "topright",
            pal = cat_pal,
            values = ~ category,
            opacity = 1)



map





