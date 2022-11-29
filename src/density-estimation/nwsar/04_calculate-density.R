#-----------------------------------------------------------------------------------------------------------------------

# Project:          NWSAR

# Title:            Calculate density by deployment and grid (?)
# Description:
# Authors:          Marcus Becker

# Previous scripts: 01_process-raw, 02_days-of-operation, 03_calculate-tifc

#-----------------------------------------------------------------------------------------------------------------------

# Attach packages
library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(purrr)

# Set path to Google Drive
g_drive <- "G:/Shared drives/ABMI Camera Mammals/"

library(googlesheets4)
library(googledrive)

# Get VegForDetectionDistance information from Google Sheets.
veghf_sheets <- drive_find(type = "spreadsheet", shared_drive = "ABMI Camera Mammals") |>
  filter(str_detect(name, "Northwest")) |>
  select(id) |>
  pull()

df_vegdetdist <- map_df(.x = veghf_sheets,
                        .f = ~ read_sheet(
                          ss = .x
                        )) |>
  select(project, location, VegForDetectionDistance) |>
  unite("project_location", project, location, sep = "_", remove = TRUE)

# Effective detection distance (EDD) predictions lookup
df_edd <- read_csv(paste0(g_drive, "data/processed/detection-distance/predictions/edd_veghf_season.csv"))

# EDD species groups
df_dist_groups <- read_csv(paste0(g_drive, "data/lookup/species-distance-groups.csv"))

# Set parameters:

# Camera field of view angle
cam_fov_angle <- 40

# Total time in front of the camera (previous script)

df_tt <- read_csv(paste0(g_drive, "data/processed/time-in-cam-fov/nwsar_all-years_fov-time_long_2022-11-29.csv"))

#-----------------------------------------------------------------------------------------------------------------------

# Test new function

df_density_long <- calc_density_by_loc(x = df_tt,
                                       y = df_vegdetdist,
                                       cam_fov_angle = 40,
                                       format = "long")

df_density_wide <- calc_density_by_loc(x = df_tt,
                                       y = df_vegdetdist,
                                       cam_fov_angle = 40,
                                       format = "wide")

#-----------------------------------------------------------------------------------------------------------------------

# Write results

write_csv(df_density_long, paste0(g_drive, "results/density/deployments/nwsar_all-years_density_long_", Sys.Date(), ".csv"))

write_csv(df_density_wide, paste0(g_drive, "results/density/deployments/nwsar_all-years_density_wide_", Sys.Date(), ".csv"))


