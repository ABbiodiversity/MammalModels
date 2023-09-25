#-----------------------------------------------------------------------------------------------------------------------

# Title:            Calculate EVI
# Description:      Pull MODIS data using GEE and compute EVI at ABMI sites

# Authors:          Marcus Becker
# Date:             September 2022

# Previous Scripts: None

#-----------------------------------------------------------------------------------------------------------------------

# Attach packages
library(rgee)
library(googleCloudStorageR)
library(jsonlite)

# Initialize Earth Engine
#ee_install()
ee_Initialize(user = "mabecker", drive = TRUE, gcs = TRUE)

library(sf)
library(raster)
library(dplyr)
library(readr)
library(stringr)
library(tidyr)

select <- dplyr::select

# Directories for saving raster objects
g_drive <- "G:/Shared drives/ABMI Camera Mammals/"
my_dir <- paste0(g_drive, "data/spatial/rgee")

# Load functions
source("./src/functions/delta-evi-rgee.R")

# Load Alberta polygon as AOI
roi_big <- st_read(paste0(g_drive, "data/spatial/ab_prov.shp")) |>
  st_bbox() |>
  st_as_sfc() |>
  sf_as_ee()

# ABMI Ecosystem Health sites
# Read in actual locations for surveyed sites (2013-2021) - From CG, Jan 2022.
# Stored in MB personal Drive.
sf_sites <- st_read("G:/My Drive/ABMI/data/spatial/sites/ABMI_Actual_CameraSites_2013_2021.shp") |>
  # Buffer by 1000 metres (1km) - Encompasses all 4 cameras at each site.
  st_buffer(dist = 1000)

# This function loads ALL of the existing MOD13A1 data
modis_all <- ee$ImageCollection("MODIS/006/MOD13A1")

# For visualization:
VizParams <- list(palette = c(
  'FFFFFF', 'CE7E45', 'DF923D', 'F1B555', 'FCD163', '99B718', '74A901',
  '66A000', '529400', '3E8601', '207401', '056201', '004C00', '023B01',
  '012E01', '011D01', '011301'
))

#-----------------------------------------------------------------------------------------------------------------------

# Now map the function over the image collection so that each image is processed.
# We don't have to worry about the fact that our roi is a different projection
# than MODIS data. GEE figures this out for you and when you download the final
# rasters you can specify the projection you want

# Summarize the data into median EVI in summer and fall

# Years - let's do 2015, 2016, 2017, and 2018

# 2014

# Summer: July 1 - August 1 (per M. Dickie)
summer_2014 <- modis_all$
  filter(ee$Filter$date("2014-07-01", "2014-08-01"))$
  map(MOD13Q1_clean)$
  reduce(ee$Reducer$median())

# Fall: Sept 1 - October 1
fall_2014 <- modis_all$
  filter(ee$Filter$date("2014-09-01", "2014-10-01"))$
  map(MOD13Q1_clean)$
  reduce(ee$Reducer$median())

# Delta - Difference between summer and fall
delta_2014 <- summer_2014$subtract(fall_2014)

# Map for visualization
Map$addLayer(delta_2014, VizParams)

# Download a single image
# When you use a reducer function you have to specify the scale and projection when you
# download the layer or you end up with the default - WGS-84 projection with 1-degree resolution
# which is huge and useless
ee_as_raster(delta_2014,
             region = roi_big,
             dsn = file.path(my_dir, "delta_2014.tif"),
             via = "drive",
             crs = "SR-ORG:6974",
             scale = 231.6564)

# 2015

# Summer: July 1 - August 1 (per M. Dickie)
summer_2015 <- modis_all$
  filter(ee$Filter$date("2015-07-01", "2015-08-01"))$
  map(MOD13Q1_clean)$
  reduce(ee$Reducer$median())

# Fall: Sept 1 - October 1
fall_2015 <- modis_all$
  filter(ee$Filter$date("2015-09-01", "2015-10-01"))$
  map(MOD13Q1_clean)$
  reduce(ee$Reducer$median())

# Delta - Difference between summer and fall
delta_2015 <- summer_2015$subtract(fall_2015)

# Map for visualization
Map$addLayer(delta_2015, VizParams)

# Download a single image
# When you use a reducer function you have to specify the scale and projection when you
# download the layer or you end up with the default - WGS-84 projection with 1-degree resolution
# which is huge and useless
ee_as_raster(delta_2015,
             region = roi_big,
             dsn = file.path(my_dir, "delta_2015.tif"),
             via = "drive",
             crs = "SR-ORG:6974",
             scale = 231.6564)

# 2016

# Summer: July 1 - August 1 (per M. Dickie)
summer_2016 <- modis_all$
  filter(ee$Filter$date("2016-07-01", "2016-08-01"))$
  map(MOD13Q1_clean)$
  reduce(ee$Reducer$median())

# Fall: Sept 1 - October 1
fall_2016 <- modis_all$
  filter(ee$Filter$date("2016-09-01", "2016-10-01"))$
  map(MOD13Q1_clean)$
  reduce(ee$Reducer$median())

# Delta - Difference between summer and fall
delta_2016 <- summer_2016$subtract(fall_2016)

# Map for visualization
Map$addLayer(delta_2016, VizParams)

# Download a single image
# When you use a reducer function you have to specify the scale and projection when you
# download the layer or you end up with the default - WGS-84 projection with 1-degree resolution
# which is huge and useless
ee_as_raster(delta_2016,
             region = roi_big,
             dsn = file.path(my_dir, "delta_2016.tif"),
             via = "drive",
             crs = "SR-ORG:6974",
             scale = 231.6564)

# 2017

# Summer: July 1 - August 1 (per M. Dickie)
summer_2017 <- modis_all$
  filter(ee$Filter$date("2017-07-01", "2017-08-01"))$
  map(MOD13Q1_clean)$
  reduce(ee$Reducer$median())

# Fall: Sept 1 - October 1
fall_2017 <- modis_all$
  filter(ee$Filter$date("2017-09-01", "2017-10-01"))$
  map(MOD13Q1_clean)$
  reduce(ee$Reducer$median())

# Delta - Difference between summer and fall
delta_2017 <- summer_2017$subtract(fall_2017)

# Map for visualization
Map$addLayer(delta_2017, VizParams)

# Download a single image
# When you use a reducer function you have to specify the scale and projection when you
# download the layer or you end up with the default - WGS-84 projection with 1-degree resolution
# which is huge and useless
ee_as_raster(delta_2017,
             region = roi_big,
             dsn = file.path(my_dir, "delta_2017.tif"),
             via = "drive",
             crs = "SR-ORG:6974",
             scale = 231.6564)

# 2018

# Summer: July 1 - August 1 (per M. Dickie)
summer_2018 <- modis_all$
  filter(ee$Filter$date("2018-07-01", "2018-08-01"))$
  map(MOD13Q1_clean)$
  reduce(ee$Reducer$median())

# Fall: Sept 1 - October 1
fall_2018 <- modis_all$
  filter(ee$Filter$date("2018-09-01", "2018-10-01"))$
  map(MOD13Q1_clean)$
  reduce(ee$Reducer$median())

# Delta - Difference between summer and fall
delta_2018 <- summer_2018$subtract(fall_2018)

# Map for visualization
Map$addLayer(delta_2018, VizParams)

# Download a single image
# When you use a reducer function you have to specify the scale and projection when you
# download the layer or you end up with the default - WGS-84 projection with 1-degree resolution
# which is huge and useless
ee_as_raster(delta_2018,
             region = roi_big,
             dsn = file.path(my_dir, "delta_2018.tif"),
             via = "drive",
             crs = "SR-ORG:6974",
             scale = 231.6564)

#-----------------------------------------------------------------------------------------------------------------------

# Load rasters

ras_delta_2014 <- raster(paste0(my_dir, "/delta_2014.tif"))
ras_delta_2015 <- raster(paste0(my_dir, "/delta_2015.tif"))
ras_delta_2016 <- raster(paste0(my_dir, "/delta_2016.tif"))
ras_delta_2017 <- raster(paste0(my_dir, "/delta_2017.tif"))
ras_delta_2018 <- raster(paste0(my_dir, "/delta_2018.tif"))
crs <- crs(ras_delta_2015)

# Project home ranges into same prj as raster
sf_sites_transform <- sf_sites |>
  st_transform(crs = crs)

# Extract mean delta EVI for each site for each year
sites_evi_2014 <- sf_sites_transform |>
  mutate(evi_2014 = raster::extract(ras_delta_2014, sf_sites_transform, fun = mean, na.rm = TRUE)) |>
  st_set_geometry(NULL) |>
  mutate(evi_2014 = evi_2014[,1])

sites_evi_2015 <- sf_sites_transform |>
  mutate(evi_2015 = raster::extract(ras_delta_2015, sf_sites_transform, fun = mean, na.rm = TRUE)) |>
  st_set_geometry(NULL) |>
  mutate(evi_2015 = evi_2015[,1])

sites_evi_2016 <- sf_sites_transform |>
  mutate(evi_2016 = raster::extract(ras_delta_2016, sf_sites_transform, fun = mean, na.rm = TRUE)) |>
  st_set_geometry(NULL) |>
  mutate(evi_2016 = evi_2016[,1])

sites_evi_2017 <- sf_sites_transform |>
  mutate(evi_2017 = raster::extract(ras_delta_2017, sf_sites_transform, fun = mean, na.rm = TRUE)) |>
  st_set_geometry(NULL) |>
  mutate(evi_2017 = evi_2017[,1])

sites_evi_2018 <- sf_sites_transform |>
  mutate(evi_2018 = raster::extract(ras_delta_2018, sf_sites_transform, fun = mean, na.rm = TRUE)) |>
  st_set_geometry(NULL) |>
  mutate(evi_2018 = evi_2018[,1])

# Join together
sites_evi_all <- sf_sites_transform |>
  st_set_geometry(NULL) |>
  left_join(sites_evi_2014) |>
  left_join(sites_evi_2015) |>
  left_join(sites_evi_2016) |>
  left_join(sites_evi_2017) |>
  left_join(sites_evi_2018) |>
  mutate(site = as.character(ABMI)) |>
  dplyr::select(-c(ABMI, Longitude, Latitude))

#-----------------------------------------------------------------------------------------------------------------------

# Bring in Moose density data

min_season_days <- 20
min_total_days <- 50

lure_effect <- 1.250844

df_lure <- read_csv(paste0(g_drive, "data/lookup/lure/abmi-cmu_all-years_lure_2021-10-07.csv")) |>
  filter(str_detect(project, "Health 2015|Health 2016|Health 2017|Health 2018"),
         !str_detect(location, "OG"))

df_dens_evi <- read_csv(paste0(g_drive, "results/density/deployments/abmi_all-years_density_long_2021-10-07.csv")) |>
  filter(common_name == "Moose",
         str_detect(project, "Health 2015|Health 2016|Health 2017|Health 2018"),
         !str_detect(location, "OG")) |>
  # Minimum seasonal requirements
  filter(!is.na(density_km2),
         total_season_days >= min_season_days) |>
  group_by(project, location, common_name) |>
  mutate(total_days = sum(total_season_days)) |>
  # Remove deployments that don't meet minimum requirement for total days:
  filter(total_days >= min_total_days) |>
  # Density considering both seasons together:
  summarise(full_density_km2 = mean(density_km2)) |>
  ungroup() |>
  left_join(df_lure, by = c("location", "project")) |>
  mutate(full_density_km2 = ifelse(lure == "Yes", full_density_km2 / lure_effect, full_density_km2)) |>
  # Average density across the 4 cameras of a site
  separate(location, into = c("site", "station"), sep = "-", remove = TRUE) |>
  group_by(project, site, common_name) |>
  summarise(site_density = mean(full_density_km2)) |>
  left_join(sites_evi_all, by = "site") |>
  # Remove sites without EVI information
  filter(!is.na(evi_2015)) |>
  # Take proper year of EVI
  mutate(evi = case_when(
    str_detect(project, "2015") ~ evi_2014,
    str_detect(project, "2016") ~ evi_2015,
    str_detect(project, "2017") ~ evi_2016,
    str_detect(project, "2018") ~ evi_2017
  )) |>
  select(project, site, common_name, site_density, evi)

check <- df_dens_evi |>
  filter(site_density < 4,
         site_density > 0)

ggplot(check) +
  geom_point(aes(x = site_density, y = evi)) +
  ylab("Delta EVI") +
  xlab("Moose Density")



df_dens_evi <- read_csv(paste0(g_drive, "results/density/deployments/abmi_all-years_density_long_2021-10-07.csv")) |>
  filter(common_name == "Moose",
         str_detect(project, "Health 2015|Health 2016|Health 2017|Health 2018"),
         !str_detect(location, "OG")) |>
  # Minimum seasonal requirements
  filter(!is.na(density_km2),
         total_season_days >= min_season_days,
         season == "summer") |>
  left_join(df_lure, by = c("location", "project")) |>
  mutate(summer_density_km2 = ifelse(lure == "Yes", density_km2 / lure_effect, density_km2)) |>
  # Average density across the 4 cameras of a site
  separate(location, into = c("site", "station"), sep = "-", remove = TRUE) |>
  group_by(project, site, common_name) |>
  summarise(summer_site_density = mean(summer_density_km2)) |>
  left_join(sites_evi_all, by = "site") |>
  # Remove sites without EVI information
  filter(!is.na(evi_2015)) |>
  # Take proper year of EVI
  mutate(evi = case_when(
    str_detect(project, "2015") ~ evi_2015,
    str_detect(project, "2016") ~ evi_2016,
    str_detect(project, "2017") ~ evi_2017,
    str_detect(project, "2018") ~ evi_2018
  )) |>
  select(project, site, common_name, summer_site_density, evi)

check <- df_dens_evi |>
  filter(summer_site_density < 4)

ggplot(check) +
  geom_point(aes(x = summer_site_density, y = evi)) +
  ylab("Delta EVI") +
  xlab("Moose Density")



