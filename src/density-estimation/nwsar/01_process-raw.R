#-----------------------------------------------------------------------------------------------------------------------

# Project:          NWSAR

# Title:            Process Raw Camera Data
# Description:      Process raw NWSAR camera tag data from WildTrax in preparation for downstream density estimation,
#                   as well as extract NONE gap class information.
# Author:           Marcus Becker

# Previous scripts: None

#-----------------------------------------------------------------------------------------------------------------------

# Attach packages

library(wildRtrax) # To download data
library(keyring)   # For storing credentials safely

library(stringr)
library(dplyr)
library(purrr)

# Set path to Shared Google Drive (G Drive)
g_drive <- "G:/Shared drives/ABMI Camera Mammals/"

# Source helper functions - most of these will probably be included in wildRtrax in the near future.
source("./src/functions/helper_fns.R")

# Species character strings
load(paste0(g_drive, "data/lookup/wt_cam_sp_str.RData"))

#-----------------------------------------------------------------------------------------------------------------------

# Download data
# Set environment variables (username/password)
Sys.setenv(WT_USERNAME = key_get("WT_USERNAME", keyring = "wildtrax"),
           WT_PASSWORD = key_get("WT_PASSWORD", keyring = "wildtrax"))

# Authenticate into WildTrax
wt_auth()

# Pull NWSAR project IDs
nwsar_proj_ids <- wt_get_download_summary(sensor_id = "CAM") |>
  filter(str_detect(project, "Northwest")) |>
  pull(project_id) |>
  unlist()

# Download tag and image reports using IDs
tag_reports <- map_df(.x = nwsar_proj_ids,
                      .f = ~ wt_download_report(
                        project_id = .x,
                        sensor_id = "CAM",
                        report = "tag",
                        weather_cols = FALSE))

image_reports <- map_df(.x = nwsar_proj_ids,
                        .f = ~ wt_download_report(
                          project_id = .x,
                          sensor_id = "CAM",
                          report = "image",
                          weather_cols = FALSE))

# Strip it down to include on relevant information (trigger, field of view)
image_fov_trigger <- image_reports |>
  select(project, location, date_detected, trigger, field_of_view)

# Clean up tags
tags_clean <- tag_reports |>
  # Consolidate tags of same species in same image into one row
  wt_consolidate_tags() |>
  left_join(image_fov, by = c("project", "location", "date_detected")) |>
  filter(field_of_view == "WITHIN") |>
  select(project, location, date_detected, common_name, age_class, sex, number_individuals)

# Add 'N' gap class to images following a 'NONE' image
df_gap_nones <- add_gap_class_n(tags_clean)

#-----------------------------------------------------------------------------------------------------------------------

# Save cleaned and binded data to Shared Google Drive
# (so we don't have to re-download from WildTrax each time)

# Simple image reports
image_fov_trigger |>
  write_csv(paste0(g_drive, "data/lookup/images/nwsar_all-years_image-report_simple.csv"))

# Only tags of species
tags_clean |>
  # Remove all non-native mammal images
  filter(common_name %in% native_sp) |>
  write_csv(paste0(g_drive, "data/base/clean/nwsar_all-years_native-sp_clean_", Sys.Date(), ".csv"))

# Save (all) cleaned data
tags_clean |>
  write_csv(paste0(g_drive, "data/base/clean/nwsar_all-years_all-data_clean_", Sys.Date(), ".csv"))

# NONE gaps
df_gap_nones |>
  write_csv(paste0(g_drive, "data/processed/probabilistic-gaps/nwsar_all-years_gap-class-nones_", Sys.Date(), ".csv"))

#-----------------------------------------------------------------------------------------------------------------------
