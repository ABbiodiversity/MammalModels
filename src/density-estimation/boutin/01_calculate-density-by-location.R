#-----------------------------------------------------------------------------------------------------------------------

# Project:          BOUTIN

# Title:            Process Raw Data
# Description:      Process raw BOUTIN camera tag data from WildTrax in preparation for downstream density estimation,
#                   as well as extract NONE gap class information.
# Author:           Marcus Becker
# Date:             November 2022

# Previous scripts: None

#-----------------------------------------------------------------------------------------------------------------------

# Attach packages

library(wildRtrax) # To download data
library(keyring)   # For storing credentials safely

# Set path to Shared Google Drive (G Drive)
g_drive <- "G:/Shared drives/ABMI Camera Mammals/"

# Source functions for TIFC workflow
source("./src/functions/tifc_workflow.R")

# Species character strings
load(paste0(g_drive, "data/lookup/wt_cam_sp_str.RData"))

# Project
proj <- "boutin"

# Options for milliseconds
options(digits.secs = 2)

#-----------------------------------------------------------------------------------------------------------------------

# Download BOUTIN data

# Set environment variables (username/password)
Sys.setenv(WT_USERNAME = key_get("WT_USERNAME", keyring = "wildtrax"),
           WT_PASSWORD = key_get("WT_PASSWORD", keyring = "wildtrax"))

# Authenticate into WildTrax
wt_auth()

# Pull select BOUTIN camera project IDs
boutin_proj_ids <- wt_get_download_summary(sensor_id = "CAM") |>
  # Filter for just BOUTIN
  filter(str_detect(project, "Boutin")) |>
  pull(project_id) |>
  unlist()

# Download tag and image reports using IDs
tag_reports <- map_df(.x = boutin_proj_ids,
               .f = ~ wt_download_report(
                 project_id = .x,
                 sensor_id = "CAM",
                 report = "tag",
                 weather_cols = FALSE))

image_reports <- map_df(.x = boutin_proj_ids,
                 .f = ~ wt_download_report(
                   project_id = .x,
                   sensor_id = "CAM",
                   report = "image",
                   weather_cols = FALSE
                 ))

# Strip it down to include on relevant information (trigger, field of view)
image_fov_trigger <- image_reports |>
  select(project, location, serial_number, date_detected, trigger, field_of_view) |>
  mutate(date_detected = ymd_hms(date_detected))

# Clean up tags - i.e., consolidate tags, remove tags that are not within the fov, strip down number of columns
tags_clean <- tag_reports |>
  select(project, location, date_detected, common_name, age_class, sex, number_individuals) |>
  # Covert to date_detected to date time, but preserve milliseconds (ugh)
  mutate(date_detected = ymd_hms(date_detected)) |>
  left_join(image_fov_trigger, by = c("project", "location", "date_detected")) |>
  distinct() |>
  unite("location", location, serial_number, sep = "-", remove = TRUE) |>
  # Consolidate tags of same species in same image into one row
  consolidate_tags() |>
  filter(field_of_view == "WITHIN") |>
  select(project, location, date_detected, common_name, age_class, sex, number_individuals)

#-----------------------------------------------------------------------------------------------------------------------

# Save cleaned and binded data to Shared Google Drive
# (so we don't have to re-download from WildTrax each time)

# Last done: December 2, 2022

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

# Summarise time-by-day for each camera deployment in the two NWSAR projects.

df_tbd_summary <- image_fov_trigger |>
  unite("location", location, serial_number, sep = "-", remove = TRUE) |>
  get_operating_days(
  # Keep project
  include_project = TRUE,
  # Summarise
  summarise = TRUE,
  # Include ABMI seasons
  .abmi_seasons = TRUE)

# Write results
write_csv(df_tbd_summary, paste0(g_drive, "data/processed/time-by-day/", proj, "_all-years_tbd-summary_", Sys.Date(), ".csv"))

#-----------------------------------------------------------------------------------------------------------------------

# Calculate time in front of camera (TIFC)

df_tt <- tags_clean |>
  # First calculate time by series
  calculate_time_by_series() |>
  # Next calculate tifc by location, deployment, species
  sum_total_time(tbd = df_tbd_summary)

# Write results
# Full results long:
write_csv(df_tt, paste0(g_drive, "data/processed/time-in-cam-fov/", proj, "_all-years_fov-time_long_", Sys.Date(), ".csv"))

#-----------------------------------------------------------------------------------------------------------------------

# Calculate density at each location

# Dataframe for use in density function
df_vegdetdist <- df_tt |>
  select(project, location) |>
  distinct() |>
  mutate(VegForDetectionDistance = "Conif") |>
  unite("project_location", project, location, sep = "_", remove = TRUE)

# Only interested in certain species
sp <- c("Black Bear", "Canada Lynx", "Coyote", "Fisher", "Gray Wolf", "Marten", "Moose", "Snowshoe Hare", "White-tailed Deer",
        "Wolverine")

# Calculate density (long and wide)
df_density_long <- calc_density_by_loc(tt = df_tt,
                                       veg = df_vegdetdist,
                                       cam_fov_angle = 40,
                                       format = "long") |>
  filter(common_name %in% sp)

# Write results

write_csv(df_density_long, paste0(g_drive, "results/density/deployments/", proj, "_all-years_density_long_", Sys.Date(), ".csv"))

#-----------------------------------------------------------------------------------------------------------------------








