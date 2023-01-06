#-----------------------------------------------------------------------------------------------------------------------

# Project:          ABMI (Ecosystem Health 2019, 2020, 2021, 2022)

# Title:            Calculate density of species by project/location
# Description:      Process raw ABMI Ecosystem Health camera tag data from WildTrax and estimate density using the time
#                   in front of camera method.
# Author:           Marcus Becker

# Previous scripts: None

#-----------------------------------------------------------------------------------------------------------------------

# Attach packages

library(wildRtrax) # To download data
library(keyring)   # For storing credentials safely

# Set path to Shared Google Drive (G Drive)
g_drive <- "G:/Shared drives/ABMI Camera Mammals/"

# Source functions for TIFC workflow
source("./src/functions/estimate-density-tifc.R")

# Species character strings
load(paste0(g_drive, "data/lookup/wt_cam_sp_str.RData"))

# Project
proj <- "eh"

# Years
years <- "_19-20-21-22"

#-----------------------------------------------------------------------------------------------------------------------

# Download data
# Set environment variables (username/password)
Sys.setenv(WT_USERNAME = key_get("WT_USERNAME", keyring = "wildtrax"),
           WT_PASSWORD = key_get("WT_PASSWORD", keyring = "wildtrax"))

# Authenticate into WildTrax
wt_auth()

# Pull EH project IDs - 2019, 2020, 2021, and 2022.
eh_proj_ids <- wt_get_download_summary(sensor_id = "CAM") |>
  filter(str_detect(project, "Health 2019|Health 2020|Health 2021|Health 2022")) |>
  pull(project_id) |>
  unlist()

# Download tag and image reports using IDs
tag_reports <- map_df(.x = eh_proj_ids,
                      .f = ~ wt_download_report(
                        project_id = .x,
                        sensor_id = "CAM",
                        report = "tag",
                        weather_cols = FALSE))

image_reports <- map_df(.x = eh_proj_ids,
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

# Last done: January 5, 2023

# Simple image reports
image_fov_trigger |>
  write_csv(paste0(g_drive, "data/lookup/images/", proj, "_19-20-21-22_image-report_simple.csv"))

# Only tags of species
tags_clean |>
  # Remove all non-native mammal images
  filter(common_name %in% native_sp) |>
  write_csv(paste0(g_drive, "data/base/clean/", proj, "_19-20-21-22_native-sp_clean_", Sys.Date(), ".csv"))

# Save (all) cleaned data
tags_clean |>
  write_csv(paste0(g_drive, "data/base/clean/", proj, "_19-20-21-22_all-data_clean_", Sys.Date(), ".csv"))

#-----------------------------------------------------------------------------------------------------------------------

# Summarise time-by-day for each camera deployment in the four EH projects (19, 20, 21, 22).

df_tbd_summary <- get_operating_days(
  image_report = image_fov_trigger,
  # Keep project
  include_project = TRUE,
  # Summarise
  summarise = TRUE,
  # Include ABMI seasons
  .abmi_seasons = TRUE
)

# Write results
write_csv(df_tbd_summary, paste0(g_drive, "data/processed/time-by-day/", proj, "_19-20-21-22_tbd-summary_", Sys.Date(), ".csv"))

#-----------------------------------------------------------------------------------------------------------------------

# Calculate time in front of camera (TIFC)

df_tt <- tags_clean |>
  # First calculate time by series
  calculate_time_by_series() |>
  # Next calculate tifc by location, deployment, species
  sum_total_time(tbd = df_tbd_summary)

# Write results
# Full results long:
write_csv(df_tt, paste0(g_drive, "data/processed/time-in-cam-fov/", proj, "_19-20-21-22_fov-time_long_", Sys.Date(), ".csv"))

#-----------------------------------------------------------------------------------------------------------------------

# Calculate density at each location

# Veg/HF lookup

library(googlesheets4)
library(googledrive)

# Get VegForDetectionDistance information from Google Sheets.
# Note: Only 2019-2022 available here.
veghf_sheets <- drive_find(type = "spreadsheet", shared_drive = "ABMI Camera Mammals") |>
  filter(str_detect(name, "Ecosystem Health")) |>
  select(id) |>
  pull()

# Dataframe for use in density function
df_vegdetdist <- map_df(.x = veghf_sheets,
                              .f = ~ read_sheet(ss = .x)) |>
  select(project, location, VegForDetectionDistance) |>
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

# Write results

write_csv(df_density_long, paste0(g_drive, "results/density/deployments/", proj, "_19-20-21-22_density_long_", Sys.Date(), ".csv"))

write_csv(df_density_wide, paste0(g_drive, "results/density/deployments/", proj, "_19-20-21-22_density_wide_", Sys.Date(), ".csv"))

#-----------------------------------------------------------------------------------------------------------------------
