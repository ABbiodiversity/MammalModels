#-----------------------------------------------------------------------------------------------------------------------

# Title:       Process camera locations
# Description: Estimate ABMI camera site locations (2013-2021) from site centers provided by the Geospatial Centre.
#              Clean up other layers, and compile for interactive map.

# Date:        January, 2022
# Author:      Marcus Becker

#-----------------------------------------------------------------------------------------------------------------------

# Attach packages
library(dplyr)
library(tidyr)
library(sf)
library(readr)
library(stringr)

g_drive <- "G:/Shared drives/ABMI Camera Mammals/"

# Read in actual locations for surveyed sites (2013-2021) - From Cris, Jan 2022.
# Stored in personal Drive.
sf_site_actual <- st_read("G:/My Drive/ABMI/data/spatial/sites/ABMI_Actual_CameraSites_2013_2021.shp")

#-----------------------------------------------------------------------------------------------------------------------

# Represent camera locations

st_crs(sf_site_actual) # Should be EPSG 3400 (NAD83 / Alberta 10-TM (Forest))

# First, draw a square buffer:
sf_buf_site_actual <- sf_site_actual |>
  # Distance (dist) represented in meters; this is the radius when doing buffering.
  st_buffer(dist = 300, nQuadSegs = 1, endCapStyle = "SQUARE")

# List sites, expand four times for the number of cameras at each (NW, NE, SW, SE)
dep <- sf_buf_site_actual |>
  st_set_geometry(NULL) |>
  select(site = ABMI) |>
  uncount(4)

# Predict locations of the four cameras
sf_cam_pred <- sf_buf_site_actual |>
  # Obtain coordinates of each 'point' of the geometry
  st_coordinates() |>
  # Convert from Matrix to dataframe
  data.frame() |>
  # Remove the duplicate (there's always a duplicate to trace back to the 'first' point)
  distinct() |>
  # Add deployments
  bind_cols(dep) |>
  # We need to be able to tell which is NW, NE, SW, SE.
  group_by(site) |>
  # Simple way to identify which point on the square is in the top right, top left, etc.
  mutate(north = ifelse(abs(Y) > mean(abs(Y)), 1, 0),
         east = ifelse(abs(X) > mean(abs(X)), 1, 0)) |>
  ungroup() |>
  mutate(quadrant = case_when(
    north == 1 & east == 1 ~ "NE",
    north == 1 & east == 0 ~ "NW",
    north == 0 & east == 1 ~ "SE",
    north == 0 & east == 0 ~ "SW"
  )) |>
  mutate(location = paste0(site, "-", quadrant)) |>
  # Convert back to spatial object (CRS = NAD83 / Alberta 10-TM Forest)
  st_as_sf(coords = c("X", "Y"), crs = 3400) |>
  select(site, quadrant, location) |>
  # Transform to EPSG 4326 for mapping in leaflet
  st_transform(4326)

# These are the deployments that we have 'true' (i.e., predicated) locations for:
true_cam <- sf_cam_pred |>
  st_set_geometry(NULL) |>
  select(location) |>
  mutate(actual_loc = 1)

#-----------------------------------------------------------------------------------------------------------------------

# Now we do the rest of the ABMI, and stitch together.

# OK, so there's a lot:

# 1. ABMI EH 2014-2021
# -----> Note: I won't have the 'restricted' sites from these, which are mostly in the south. Will have to default to
#        the public locations.
# 2. ABMI OG 2015, 2017, 2018
# 3. ABMI Southern (2019, 2021) and Northern (2019, 2020) Focal Areas
# 4. ABMI OSM sites (2021, 2022)
# 5. CMU grids (2017-2021)
# 6. NWSAR grids (2021, 2022)
# 7. Big Grid (2016)

df_cam_all <- read_csv(paste0(g_drive, "data/lookup/locations/abmi-cmu-nwsar-bg_public-locations_2022-01-13.csv"))

# Let's grab  EH 2021, too.
library(wildRtrax)
library(keyring)
Sys.setenv(WT_USERNAME = key_get("WT_USERNAME", keyring = "wildtrax"),
           WT_PASSWORD = key_get("WT_PASSWORD", keyring = "wildtrax"))
wt_auth()

# Project ID for EH 2021
proj_id <- wt_get_download_summary(sensor_id = "CAM") |>
  filter(str_detect(project, "Ecosystem Health 2021")) |>
  select(project_id) |>
  pull() |>
  unlist()

# Public locations of EH 2021 (honestly, I should probably have all the true locations from Cris)
df_loc_2021 <- wt_download_report(project_id = proj_id, sensor_id = "CAM") |>
  select(project, location, longitude, latitude) |>
  distinct()

#-----------------------------------------------------------------------------------------------------------------------

# CMU
sf_loc_cmu <- df_loc_all |>
  filter(str_detect(project, "CMU")) |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

st_write(sf_loc_cmu, paste0(root, "data/lookup/locations/spatial/cmu_2017-2020.shp"))

# NWSAR
sf_loc_nwsar <- df_loc_all |>
  filter(str_detect(project, "Northwest")) |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

st_write(sf_loc_nwsar, paste0(root, "data/lookup/locations/spatial/nwsar_2021.shp"))

# Big Grid
sf_loc_bg <- df_loc_all |>
  filter(str_detect(project, "Big Grid")) |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

st_write(sf_loc_bg, paste0(root, "data/lookup/locations/spatial/bg_2016.shp"))

#-----------------------------------------------------------------------------------------------------------------------

# ABMI
df_loc_abmi <- df_cam_all |>
  filter(str_detect(project, "ABMI")) |>
  filter(!project == "ABMI Adopt-a-Camera 2017",
         !project == "ABMI Amphibian Monitoring 2020",
         # Remove OG's for now ... Cris gave these to me, too.
         !str_detect(location, "^OG"),
         !str_detect(location, "-CL$|-CCL$")) |>
  # Add 2021
  bind_rows(df_loc_2021) |>
  # Let's set a marker for deployments where we have an actual location
  left_join(true_cam, by = "location") |>
  mutate(actual_loc = replace_na(actual_loc, 0))

# Now we need to add the 'project' field to the true locations from above.
actual_loc_proj <- df_loc_abmi |>
  filter(actual_loc == 1) |>
  select(project, location, actual_loc)

# True locations w/ project field
sf_proj_cam_pred <- sf_cam_pred |>
  left_join(actual_loc_proj, by = c("location")) |>
  filter(!is.na(project)) |>
  select(project, location, actual_loc)

# Now for the public buffered locations
sf_loc_abmi_public <- df_loc_abmi |>
  filter(actual_loc == 0) |>
  filter(!is.na(longitude)) |>
  separate(location, into = c("site", "quadrant", "extras"), sep = "-", remove = TRUE) |>
  select(project, site, longitude, latitude) |>
  distinct() |>
  # This was weird. There appears to be different public locations for revisited sites ... typo in WT?
  group_by(project, site) |>
  # Just take the first one if there's multiple. Will probably have to revisit at some point.
  filter(row_number() == 1) |>
  ungroup() |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
  # Transform to EPSG 3400 for buffering
  st_transform(3400)

# Square buffer, 600m x 600m
sf_spread <- st_buffer(sf_loc_abmi_public, dist = 300, nQuadSegs = 1, endCapStyle = "SQUARE")

# List sites, expand four times for the number of cameras at each (NW, NE, SW, SE)
pub_dep <- sf_spread |>
  st_set_geometry(NULL) |>
  uncount(4)

# Predict locations of the four cameras; remember, not a real prediction. Just spreading it out for mapping purposes.
sf_public_pred <- sf_spread |>
  # Obtain coordinates of each 'point' of the geometry
  st_coordinates() |>
  data.frame() |>
  # Remove the duplicate (there's always a duplicate to trace back to the 'first' point)
  distinct() |>
  # Add deployments
  bind_cols(pub_dep) |>
  group_by(project, site) |>
  # We need to be able to tell which is NW, NE, SW, SE.
  mutate(north = ifelse(abs(Y) > mean(abs(Y)), 1, 0),
         east = ifelse(abs(X) > mean(abs(X)), 1, 0)) |>
  ungroup() |>
  mutate(quadrant = case_when(
    north == 1 & east == 1 ~ "NE",
    north == 1 & east == 0 ~ "NW",
    north == 0 & east == 1 ~ "SE",
    north == 0 & east == 0 ~ "SW"
  )) |>
  mutate(location = paste0(site, "-", quadrant)) |>
  # Convert back to spatial object (CRS = NAD83 / Alberta 10-TM Forest)
  st_as_sf(coords = c("X", "Y"), crs = 3400) |>
  select(project, location) |>
  mutate(actual_loc = 0) |>
  st_transform(4326)

# Combine together, keeping the 'actual_loc' field so we know which are 'true' (predicted).
sf_all <- bind_rows(sf_proj_cam_pred, sf_public_pred)

# Save as shapefile
st_write(sf_all, "./data/processed/abmi_cam_locations_new.shp")

#-----------------------------------------------------------------------------------------------------------------------
