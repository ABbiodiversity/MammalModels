#-----------------------------------------------------------------------------------------------------------------------

# Project:          ABMI (Oilsands Monitoring)

# Title:            Calculate density of species by project/location
# Description:      Process raw ABMI (OSM) camera tag data from WildTrax and estimate density using the time in front of
#                   method. Includes the ACME camera deployments.

# Author:           Marcus Becker

# Previous scripts: None

# Last updated:     August 28 2023

#-----------------------------------------------------------------------------------------------------------------------

# Attach packages

library(wildRtrax) # To download data
library(keyring)   # For storing credentials safely

# Set path to Shared Google Drive (G Drive)
g_drive <- "G:/Shared drives/ABMI Camera Mammals/"

# Source functions for TIFC workflow
source("./src/Functions/estimate-density-tifc.R")

# Write and archive function
source("./src/Functions/write-and-archive.R")

# Species character strings
load(paste0(g_drive, "data/lookup/wt_cam_sp_str.RData"))

# Project
proj <- "osm"

# Years
years <- "_all-years"

# Date
date <- Sys.Date()

#-----------------------------------------------------------------------------------------------------------------------

# Download data
# Set environment variables (username/password)
Sys.setenv(WT_USERNAME = key_get("WT_USERNAME", keyring = "wildtrax"),
           WT_PASSWORD = key_get("WT_PASSWORD", keyring = "wildtrax"))

# Authenticate into WildTrax
wt_auth()

# Pull OSM project IDs (both ABMI & ACME)
osm_proj <- wt_get_download_summary(sensor_id = "CAM") |>
  filter(str_detect(project, "ABMI OSM")) |>
  mutate(across(everything(), unlist)) |>
  select(project, project_id)

osm_proj_ids <- osm_proj$project_id

# Download tag and image reports using IDs
main_reports <- map_df(.x = osm_proj_ids,
                      .f = ~ wt_download_report(
                        project_id = .x,
                        sensor_id = "CAM",
                        report = "main",
                        weather_cols = FALSE))

main_reports_simp <- main_reports |>
  left_join(osm_proj, by = "project_id") |>
  filter(species_common_name != "NONE")
  filter(field_of_view == "WITHIN")
  select(project, location, image_date_time, )

# Don't think I need to download image reports here anymore.
image_reports <- map_df(.x = osm_proj_ids,
                        .f = ~ wt_download_report(
                          project_id = .x,
                          sensor_id = "CAM",
                          report = "image",
                          weather_cols = FALSE))

# Strip it down to include only relevant information (trigger, field of view)
image_fov_trigger <- image_reports |>
  select(project, location, date_detected, trigger, field_of_view)

# Clean up tags - i.e., consolidate tags, remove tags that are not within the fov, strip down number of columns
tags_clean <- tag_reports |>
  # Consolidate tags of same species in same image into one row
  consolidate_tags() |>
  left_join(image_fov_trigger, by = c("project", "location", "date_detected")) |>
  mutate(date_detected = ymd_hms(date_detected)) |>
  filter(field_of_view == "WITHIN") |>
  select(project, location, date_detected, common_name, age_class, sex, number_individuals)

#-----------------------------------------------------------------------------------------------------------------------

# Save cleaned and binded data to Shared Google Drive
# (so we don't have to re-download from WildTrax each time)

# Last done: December 6, 2022

# Simple image reports
image_fov_trigger |>
  write_csv(paste0(g_drive, "data/lookup/images/", proj, "_all-years_image-report_simple.csv"))

# Only tags of species
tags_clean |>
  # Remove all non-native mammal images
  filter(common_name %in% native_sp) |>
  write_csv(paste0(g_drive, "data/base/clean/", proj, "_all-years_native-sp_clean_", Sys.Date(), ".csv"))

# Save (all) cleaned data
tags_clean |>
  write_csv(paste0(g_drive, "data/base/clean/", proj, "_all-years_all-data_clean_", Sys.Date(), ".csv"))

#-----------------------------------------------------------------------------------------------------------------------

# If needed (not re-downloading from WildTrax), import data:

image_fov_trigger <- read_csv(paste0(g_drive, "data/lookup/images/", proj, years, "_image-report_simple.csv"))

# Find appropriate tag data file
file <- list.files(path = paste0(g_drive, "data/base/clean"), full.names = TRUE) |>
  str_subset(pattern = paste0(proj, years, "_all-data_clean"))
# Import
tags_clean <- read_csv(file)

#-----------------------------------------------------------------------------------------------------------------------

# Summarise time-by-day for each camera deployment in the ABMI & ACME OSM projects.

df_tbd_summary <- get_operating_days(
  image_report = image_fov_trigger,
  # Keep project
  include_project = TRUE,
  # Summarise
  summarise = TRUE,
  # Include ABMI seasons
  .abmi_seasons = TRUE
)

# Write new results and archive old results
write_and_archive(
  data = df_tbd_summary,
  type = "tbd",
  project = proj,
  years = years
)

#-----------------------------------------------------------------------------------------------------------------------

# Calculate time in front of camera (TIFC)

df_tt <- tags_clean |>
  # First calculate time by series
  calculate_time_by_series() |>
  # Next calculate tifc by location, deployment, species
  sum_total_time(tbd = df_tbd_summary)

# Write new results and archive old results
write_and_archive(
  data = df_tt,
  type = "tt",
  project = proj,
  years = years
)

#-----------------------------------------------------------------------------------------------------------------------

# Calculate density at each location

# VegHF information for OSM deployments
# Note: This is just the GIS output - probably worth going through and fixing manually.
df_vegdetdist <- read_csv(paste0(g_drive, "data/lookup/veghf/osm_2021_veghf-point_2022-04-26.csv")) |>
  make_vegfordetdist() |>
  unite("project_location", project, location, sep = "_", remove = TRUE)

# Calculate density (long and wide)
df_density_long <- calc_density_by_loc(tt = df_tt,
                                       veg = df_vegdetdist,
                                       cam_fov_angle = 40,
                                       format = "long")

df_density_wide <- calc_density_by_loc(tt = df_tt,
                                       veg = df_vegdetdist,
                                       cam_fov_angle = 40,
                                       format = "wide")

# Write new results and archive old results

write_and_archive(
  data = df_density_long,
  type = "dl",
  project = proj,
  years = years
)

write_and_archive(
  data = df_density_wide,
  type = "dw",
  project = proj,
  years = years
)

#-----------------------------------------------------------------------------------------------------------------------
