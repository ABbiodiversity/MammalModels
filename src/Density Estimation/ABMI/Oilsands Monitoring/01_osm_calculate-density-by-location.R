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
years <- "_2021-2022"

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
# Pull OSM project IDs (just ABMI for now)
osm_proj <- wt_get_download_summary(sensor_id = "CAM") |>
  filter(str_detect(project, "ABMI OSM"))

# Project IDs as vector
osm_proj_ids <- osm_proj$project_id

# Download main reports using IDs
main_reports <- map_df(.x = osm_proj_ids,
                       .f = ~ wt_download_report(
                         project_id = .x,
                         sensor_id = "CAM",
                         report = "main",
                         weather_cols = FALSE))

# Strip it down to only include relevant information
main_reports_clean <- main_reports |>
  left_join(osm_proj, by = "project_id") |>
  # Consolidate tags of same species in same image into one row
  consolidate_tags() |>
  mutate(image_date_time = ymd_hms(image_date_time)) |>
  filter(image_fov == "WITHIN") |>
  select(project, location, image_date_time, image_fov, species_common_name, individual_count, age_class, sex_class)

#-----------------------------------------------------------------------------------------------------------------------

# Save cleaned and binded data to Shared Google Drive
# (so we don't have to re-download from WildTrax each time)

# Last done:

# Simple image reports
image_fov_trigger |>
  write_csv(paste0(g_drive, "data/lookup/images/", proj, "_all-years_image-report_simple.csv"))

# Last done: August 31, 2023

# Only tags of species
main_reports_clean |>
  # Keep only species tags
  filter(species_common_name %in% native_sp) |>
  write_csv(paste0(g_drive, "data/base/clean/", proj, years, "_native-sp_clean_", Sys.Date(), ".csv"))

# Save (all) cleaned data
main_reports_clean |>
  write_csv(paste0(g_drive, "data/base/clean/", proj, years, "_all-data_clean_", Sys.Date(), ".csv"))

#-----------------------------------------------------------------------------------------------------------------------

# If needed (not re-downloading from WildTrax), import data:

# Note: Downloaded and cleaned manually until wildRtrax function is fixed.
image_fov_trigger <- read_csv(paste0(g_drive, "data/lookup/image-reports/", proj, years, "_image-report_simple.csv"))

# Find appropriate tag data file
file <- list.files(path = paste0(g_drive, "data/base/clean"), full.names = TRUE) |>
  str_subset(pattern = paste0(proj, years, "_all-data_clean"))
# Import
main_reports_clean <- read_csv(file[[2]])

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

df_tt <- main_reports_clean |>
  # First calculate time by series
  calculate_time_by_series() |>
  # Next calculate TIFC by location, deployment, species
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
df_vegdetdist_21 <- read_csv(paste0(g_drive, "data/lookup/veghf/osm_2021_veghf-point_2022-04-26.csv")) |>
  make_vegfordetdist() |>
  unite("project_location", project, location, sep = "_", remove = TRUE) |>
  select(project_location, VegForDetectionDistance)

df_vdd <- read.csv(paste0(g_drive, "data/lookup/veghf/VegForDetectionDistance/osm2022.csv")) |>
  select(project, location, VegForDetectionDistance) |>
  unite("project_location", project, location, sep = "_", remove = TRUE) |>
  bind_rows(df_vegdetdist_21)

# Calculate density (long and wide)
df_density_long <- calc_density_by_loc(tt = df_tt,
                                       veg = df_vdd,
                                       cam_fov_angle = 40,
                                       format = "long")

df_density_wide <- calc_density_by_loc(tt = df_tt,
                                       veg = df_vdd,
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

# Re-estimate density with NEW EDD predictions

df_vdd_new <- read_csv(paste0(g_drive, "data/lookup/veghf/VegForDetectionDistance/New/EDD (Re-) Modeling OSM.csv")) |>
                         select(1, 2, 5:8) |>
  mutate(secondary_category = paste0(primary_category, "_", secondary_category))

