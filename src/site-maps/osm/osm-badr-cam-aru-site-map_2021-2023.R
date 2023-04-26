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

# OSR
sf_osr <- st_read(paste0(g_drive, "osm-badr-site-selection/spatial/AB_OSR.shp")) |>
  st_transform(4326)

# Landscape Units (LUs) for all 3 years.
sf_lu <- st_read(paste0(g_drive, "osm-badr-site-selection/spatial/LU_3Yr_Aug2022/LU3YR_Aug22.shp")) |>
  st_transform(4326) |>
  clean_names() |>
  select(lu, lu_treatment = lu_treatmnt, label, year, deciles, shape_area)

# New JEMs
sf_jem <- read_csv(paste0(g_drive, "osm-badr-site-selection/jems_2023_v4.csv")) |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
  st_transform(3400) |>
  st_buffer(dist = 1500, nQuadSegs = 100)

# Proposed 2023 Camera/ARU sites:
sf_sites_2023 <- st_read(paste0(g_drive, "osm-badr-site-selection/spatial/camaru_osm_sites_2023_v4.shp")) |>
  st_transform(4326)

high_insitu <- sf_sites_2023 |>
  filter(tretmnt == "High Activity Insitu Well Pads")
dense_linear <- sf_sites_2023 |>
  filter(tretmnt == "Dense Linear Features")
low_wellpads <- sf_sites_2023 |>
  filter(tretmnt == "Low Activity Well Pads")
reference <- sf_sites_2023 |>
  filter(tretmnt == "Low Disturbance/Reference")
roads <- sf_sites_2023 |>
  filter(tretmnt == "Roads")
plant_mine <- sf_sites_2023 |>
  filter(tretmnt == "Plant/Mine")
pre_insitu <- sf_sites_2023 |>
  filter(tretmnt == "Pre Insitu")

# Proposed vascular plant transect layer
#sf_vp <- st_read(paste0(g_drive, "osm-badr-site-selection/vascular-plants/VPTransects_2023_V2/doc.kml")) |>
#  st_transform(4326)

# All treatment and habitat areas
sf_all <- st_read(paste0(g_drive, "osm-badr-site-selection/spatial/treat-hab-all-2023-lus.shp")) |>
  clean_names() |>
  select(type, treatment) |>
  st_intersection(sf_jem) |>
  st_transform(4326)

# TreedLow20 layer
treedlow <- sf_all |>
  filter(type == "TreedLow20")

# DecidMix40 layer
decidmix <- sf_all |>
  filter(type == "DecidMix40") |>
  rename(Treatment = treatment)

# Change projection of JEMs
sf_jem <- st_transform(sf_jem, crs = 4326)

# Palettes
pal_decid <- colorFactor(
  palette = "Dark2",
  domain = decidmix$Treatment
)

pal_treedlow <-colorFactor(
  palette = "Dark2",
  domain = treedlow$treatment
)

# Icons
cam <- makeAwesomeIcon(
  icon = "camera",
  iconColor = "black",
  library = "ion",
  markerColor = "white"
)

vp <- makeAwesomeIcon(
  icon = "leaf",
  iconColor = "white",
  library = "ion",
  markerColor = "green"
)

#-----------------------------------------------------------------------------------------------------------------------

# Create interactive map for viewing

map <- sf_osr |>
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
                 markerOptions = drawMarkerOptions(repeatMode = TRUE, markerIcon = cam),
                 editOptions = editToolbarOptions(edit = TRUE, remove = TRUE)) |>
  addMapPane(name = "Boundaries LU", zIndex = 410) |>
  addMapPane(name = "Boundaries JEM", zIndex = 415) |>
  addMapPane(name = "Habitat Treatment Data", zIndex = 420) |>
  addMapPane(name = "2023 Camera Sites", zIndex = 430) |>

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
              group = "Landscape Units",
              options = leafletOptions(pane = "Boundaries LU"),
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
  addPolygons(data = decidmix, color = "grey50", fillColor = ~ pal_decid(Treatment),
              weight = 1, smoothFactor = 0.2, opacity = 1, fillOpacity = 0.5, group = "Habitat: DecidMix40+",
              options = leafletOptions(pane = "Habitat Treatment Data"),
              popup = paste("Treatment: ", "<b>", decidmix$Treatment, "</b>"),
              highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE)) |>

  # Treedlow + HF treatments layer
  addPolygons(data = treedlow, color = "grey50", fillColor = ~ pal_treedlow(treatment),
              weight = 1, smoothFactor = 0.2, opacity = 1, fillOpacity = 0.5, group = "Habitat: TreedLow20+",
              options = leafletOptions(pane = "Habitat Treatment Data"),
              popup = paste("Treatment: ", "<b>", treedlow$treatment, "</b>"),
              highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE)) |>

  # 2023 Camera Sites
  addAwesomeMarkers(data = dense_linear,
                    icon = cam,
                    group = "Cam/ARU (Dense Linear Features)",
                    options = leafletOptions(pane = "2023 Camera Sites"),
                    popup = paste("Location: ", "<b>", dense_linear$camera, "</b>",
                                  "<br>", "<br>",
                                  "Notes:", "<br>",
                                  dense_linear$cmr_nts)) |>

  addAwesomeMarkers(data = high_insitu,
                    icon = cam,
                    group = "Cam/ARU (High Activity Insitu Well Pads)",
                    options = leafletOptions(pane = "2023 Camera Sites"),
                    popup = paste("Location: ", "<b>", high_insitu$camera, "</b>",
                                  "<br>", "<br>",
                                  "Notes:", "<br>",
                                  high_insitu$cmr_nts)) |>

  addAwesomeMarkers(data = low_wellpads,
                    icon = cam,
                    group = "Cam/ARU (Low Activity Well Pads)",
                    options = leafletOptions(pane = "2023 Camera Sites"),
                    popup = paste("Location: ", "<b>", low_wellpads$camera, "</b>",
                                  "<br>", "<br>",
                                  "Notes:", "<br>",
                                  low_wellpads$cmr_nts)) |>

  addAwesomeMarkers(data = reference,
                    icon = cam,
                    group = "Cam/ARU (Low Disturbance/Reference)",
                    options = leafletOptions(pane = "2023 Camera Sites"),
                    popup = paste("Location: ", "<b>", reference$camera, "</b>",
                                  "<br>", "<br>",
                                  "Notes:", "<br>",
                                  reference$cmr_nts)) |>

  addAwesomeMarkers(data = plant_mine,
                    icon = cam,
                    group = "Cam/ARU (Plant/Mine)",
                    options = leafletOptions(pane = "2023 Camera Sites"),
                    popup = paste("Location: ", "<b>", plant_mine$camera, "</b>",
                                  "<br>", "<br>",
                                  "Notes:", "<br>",
                                  plant_mine$cmr_nts)) |>

  addAwesomeMarkers(data = roads,
                    icon = cam,
                    group = "Cam/ARU (Roads)",
                    options = leafletOptions(pane = "2023 Camera Sites"),
                    popup = paste("Location: ", "<b>", roads$camera, "</b>",
                                  "<br>", "<br>",
                                  "Notes:", "<br>",
                                  roads$cmr_nts)) |>

  addAwesomeMarkers(data = pre_insitu,
                    icon = cam,
                    group = "Cam/ARU (Pre Insitu)",
                    options = leafletOptions(pane = "2023 Camera Sites"),
                    popup = paste("Location: ", "<b>", pre_insitu$camera, "</b>",
                                  "<br>", "<br>",
                                  "Notes:", "<br>",
                                  pre_insitu$cmr_nts)) |>

  #addAwesomeMarkers(data = sf_vp,
  #                  icon = vp,
  #                  group = "Vascular Plant Transects",
  #                  options = leafletOptions(pane = "2023 Camera Sites"),
  #                  popup = paste("Location: ", "<b>", sf_vp$Name)) |>

  # Layers control
  addLayersControl(overlayGroups = c("Satellite Imagery",
                                     "Landscape Units",
                                     "JEM Sites",
                                     "Vascular Plant Transects",
                                     "Cam/ARU (Dense Linear Features)",
                                     "Cam/ARU (High Activity Insitu Well Pads)",
                                     "Cam/ARU (Low Activity Well Pads)",
                                     "Cam/ARU (Low Disturbance/Reference)",
                                     "Cam/ARU (Plant/Mine)",
                                     "Cam/ARU (Roads)",
                                     "Cam/ARU (Pre Insitu)"),
                   baseGroups = c("None", "Habitat: DecidMix40+", "Habitat: TreedLow20+"),
                   options = layersControlOptions(collapsed = FALSE),
                   position = "topright") |>

  # Legend
  addLegend(data = decidmix, position = "topright", pal = pal_decid,
            values = ~ Treatment,
            opacity = 1)

# View map
map

#-----------------------------------------------------------------------------------------------------------------------

# Save map
htmlwidgets::saveWidget(map, file = "./docs/osm_cam-aru-vp_site_map.html", selfcontained = FALSE)

#-----------------------------------------------------------------------------------------------------------------------
