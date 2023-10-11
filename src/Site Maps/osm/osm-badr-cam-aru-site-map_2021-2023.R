#-----------------------------------------------------------------------------------------------------------------------

# Title:        OSM BADR Camera/ARU Site Map
# Description:  Create leaflet map for display of oilsands monitoring (OSM) before-after dose-reponse (BADR) camera and
#               ARU sites, JEMs, and Landscape Units from 2021-2023.

# Author:       Marcus Becker
# Last Updated: September 2023

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

# Shared Google Drive path to relevant spatial data
g_drive <- "G:/Shared drives/ABMI Camera Mammals/Projects/OSM BADR/"

#-----------------------------------------------------------------------------------------------------------------------

# OSR
sf_osr <- st_read(paste0(g_drive, "Data/Spatial/AB_OSR.shp")) |>
  st_transform(4326)

# Landscape Units (LUs) for all 3 years.
sf_lu <- st_read(paste0(g_drive, "Data/Spatial/LU3YR_Aug22.shp")) |>
  st_transform(4326) |>
  clean_names() |>
  mutate(year = as.numeric(paste0("20", str_sub(year, 1, 2)))) |>
  select(lu, lu_treatment = lu_treatmnt, label, year, deciles, shape_area)

# LUs from 2021
sf_lu_2021 <- sf_lu |>
  filter(year == "2021") |>
  st_transform(3400)

# 2023 JEMS (Joint Environmental Monitoring Sites)
sf_jem_2023 <- read_csv(paste0(g_drive, "Data/jems_2023_v4.csv")) |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
  st_transform(3400) |>
  st_buffer(dist = 1500, nQuadSegs = 100) |>
  select(lu, year, veg_type = type, hf_treatment = treatment, site_name)

sf_jem <- st_read(paste0(g_drive, "Data/Spatial/JEMs_2021_2022_1000m_buffer.shp")) |>
  st_transform(3400) |>
  # Clean
  clean_names() %>%
  mutate(site_name = ifelse(is.na(site_name), site_name_3, site_name),
         lu = as.numeric(str_extract(site_name, "[^-]+")),
         veg_type = case_when(
           str_detect(site_name, "DM") ~ "DecidMix40",
           str_detect(site_name, "TL") ~ "TreedLow20",
           TRUE ~ type),
         hf_treatment = case_when(
           str_detect(site_name, "-C") ~ "Low Disturbance/Reference",
           str_detect(site_name, "-Rd") ~ "Roads",
           str_detect(site_name, "-SL") ~ "Dense Linear Features",
           str_detect(site_name, "-HW") ~ "High Activity Insitu Well Pads",
           str_detect(site_name, "-LW") ~ "Low Activity Well Pads",
           str_detect(site_name, "-PM") ~ "Plant/Mine",
           str_detect(site_name, "-PI") ~ "Pre-Insitu",
           TRUE ~ treatment),
         year = as.numeric(ifelse(lu == "2" | lu == "3" | lu == "4" | lu == "8", "2021", "2022"))) %>%
  select(lu, year, veg_type, hf_treatment, site_name) |>
  bind_rows(sf_jem_2023) |>
  mutate(hf_treatment = ifelse(str_detect(hf_treatment, "Plant/Mine"), "Plant/Mine Buffer", hf_treatment))

# Camera/ARU sites - from Cris, September 2023
sf_camaru <- st_read(paste0(g_drive, "Data/Spatial/OSM_TBM_Cam_2021_23.shp")) |>
  st_transform(4326)

# NEW 2024 SITES
sf_camaru <- st_read(paste0(g_drive, "Data/proposed_camaru_locations_osm_2024.shp")) |>
  st_transform(4326)

#st_write(sf_camaru, paste0(g_drive, "Data/proposed_camaru_locations_osm_2024.shp"))

check <- sf_camaru |>
  filter(Status_2024 == "Re-Visit" | Status_2024 == "New Location") |>
  filter(!str_detect(Comments, "0.5m")) |>
  group_by(Treatment) |>
  tally()

unique(check$geometry)

sf_camaru <- read_csv(paste0(g_drive, "osm_badr_2024_camaru.csv")) |>
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) |>
  st_transform(3400)

st_write(sf_camaru, paste0(g_drive, "Data/proposed_camaru_locations_osm_2024_v2.shp"))

sf_camaru <- read_csv(paste0(g_drive, "osm_badr_2024_camaru.csv"))

# All treatment and habitat areas
sf_all <- st_read(paste0(g_drive, "Data/Spatial/Veg_Treatment_Clip_to_LU.shp")) |>
  clean_names() |>
  select(type, treatment) |>
  filter(!treatment == "Plant/Mine Buffer") |>
  #st_intersection(sf_jem) |>
  st_transform(4326)

sf_all_2021 <- st_read(paste0(g_drive, "Data/Spatial/Veg_Treatment_Clip_to_LU.shp")) |>
  clean_names() |>
  select(type, treatment) |>
  filter(!treatment == "Plant/Mine Buffer") |>
  st_intersection(sf_lu_2021) |>
  st_transform(4326) |>
  select(type, treatment, lu, year) |>
  st_cast("POLYGON")

# TreedLow20 layer
treedlow <- sf_all |>
  filter(type == "TreedLow20")

# DecidMix40 layer
decidmix <- sf_all |>
  filter(type == "DecidMix40") |>
  rename(Treatment = treatment)

# Change projection of JEMs
sf_jem_map <- st_transform(sf_jem, crs = 4326)

# Palettes
pal_decid <- colorFactor(
  palette = "Dark2",
  domain = decidmix$Treatment
)

pal_treedlow <-colorFactor(
  palette = "Dark2",
  domain = treedlow$hf_treatment
)

# Icons
cam <- makeAwesomeIcon(
  icon = "camera",
  iconColor = "black",
  library = "ion",
  markerColor = "white"
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

  # Alberta provincial polygon
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
  addPolygons(data = sf_jem_map,
              color = "#6baed6",
              fillColor = "black",
              weight = 1,
              smoothFactor = 0.2,
              opacity = 1,
              fillOpacity = 0.1,
              group = "JEM Sites",
              options = leafletOptions(pane = "Boundaries JEM"),
              popup = paste("Habitat Target: ", "<b>", sf_jem$veg_type, "</b>", "<br>",
                            "<br>",
                            "Treatment Target: ", "<b>", sf_jem$hf_treatment, "</b>", "<br>",
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

  addAwesomeMarkers(data = sf_camaru,
                    icon = cam,
                    group = "Cam/ARU",
                    options = leafletOptions(pane = "2023 Camera Sites"),
                    popup = paste("Location: ", "<b>", sf_camaru$Site_ID, "</b>",
                                  "<br>", "<br>",
                                  "Notes:", "<br>"
                                  )) |>

  # Layers control
  addLayersControl(overlayGroups = c("Satellite Imagery",
                                     "Landscape Units",
                                     "JEM Sites",
                                     "Cam/ARU"),
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
                                  pre_insitu$cmr_nts))


#-----------------------------------------------------------------------------------------------------------------------

check <- sf_camaru |>
  filter(Year == "2021") |>
  filter(!Project == "OSM-TBM-Fisher") |>
  mutate(LU = str_sub(Site_ID, 1, 1)) |>
  select(Site_ID, LU, deployment, Year, Latitude, Longitude, Project, Comments) |>
  st_set_geometry(NULL)

write_csv(check, paste0(g_drive, "Projects/OSM BADR/osm_badr_2024_camaru.csv"))






