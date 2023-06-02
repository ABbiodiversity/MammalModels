#-----------------------------------------------------------------------------------------------------------------------

# Project:          ABMI (Off-Grid)

# Title:            Calculate density of species by project/location
# Description:      Process raw ABMI Off-Grid camera tag data from WildTrax and estimate density using the time in front of
#                   camera method.
# Author:           Marcus Becker

# Previous scripts: None

#-----------------------------------------------------------------------------------------------------------------------

# Attach packages

library(wildRtrax) # To download data
library(keyring)   # For storing credentials safely
library(fs) # File management

# Set path to Shared Google Drive (G Drive)
g_drive <- "G:/Shared drives/ABMI Camera Mammals/"

# Source functions for TIFC workflow
source("./src/functions/estimate-density-tifc.R")

# Write and archive function
source("./src/functions/write-and-archive.R")

# Species character strings
load(paste0(g_drive, "data/lookup/wt_cam_sp_str.RData"))

# Project
proj <- "og"

# Years
years <- "_all-years"

#-----------------------------------------------------------------------------------------------------------------------

# Download data
# Set environment variables (username/password)
Sys.setenv(WT_USERNAME = key_get("WT_USERNAME", keyring = "wildtrax"),
           WT_PASSWORD = key_get("WT_PASSWORD", keyring = "wildtrax"))

# Authenticate into WildTrax
wt_auth()

# Pull OG project IDs
og_proj_ids <- wt_get_download_summary(sensor_id = "CAM") |>
  filter(str_detect(project, "Off-Grid|Focal|ABMI Amphib|Edge")) |>
  pull(project_id) |>
  unlist()

# Download tag and image reports using IDs
tag_reports <- map_df(.x = og_proj_ids,
                      .f = ~ wt_download_report(
                        project_id = .x,
                        sensor_id = "CAM",
                        report = "tag",
                        weather_cols = FALSE))

image_reports <- map_df(.x = og_proj_ids,
                        .f = ~ wt_download_report(
                          project_id = .x,
                          sensor_id = "CAM",
                          report = "image",
                          weather_cols = FALSE) |>
                          mutate_if(is.numeric, as.character))

# Strip it down to include only relevant information (trigger, field of view)
image_fov_trigger <- image_reports |>
  select(-c(latitude, longitude, image_sequence, is_human_blurred))

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

image_fov_trigger <- read_csv(paste0(g_drive, "data/lookup/images/", proj, years, "_all-years_image-report_simple.csv"))

# Find appropriate tag data file
file <- list.files(path = paste0(g_drive, "data/base/clean"), full.names = TRUE) |>
  str_subset(pattern = paste0(proj, years, "_all-data_clean"))
# Import
tags_clean <- read_csv(file)

#-----------------------------------------------------------------------------------------------------------------------

# Summarise time-by-day for each camera deployment in the two NWSAR projects.

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

# Write new results and archive old results:
write_and_archive(
  data = df_tt,
  type = "tt",
  project = proj,
  years = years
)

#-----------------------------------------------------------------------------------------------------------------------

# Calculate density at each location

# Veg/HF lookup

library(googlesheets4)
library(googledrive)

# Get VegForDetectionDistance information from Google Sheets.
veghf_sheets <- drive_find(type = "spreadsheet", shared_drive = "ABMI Camera Mammals") |>
  filter(str_detect(name, "Focal|Amphibian")) |>
  select(id) |>
  pull()

# Dataframe for use in density function
df_vegdetdist_focal <- map_df(.x = veghf_sheets,
                              .f = ~ read_sheet(ss = .x)) |>
  select(project, location, VegForDetectionDistance) |>
  unite("project_location", project, location, sep = "_", remove = TRUE)

# Bring in information form 2013-2018 contained in lookup file (manually checked in the past)
df_vegdetdist <- read_csv(paste0(g_drive, "data/lookup/veghf/abmi-cmu_2013-2018_vegsoilhf-detdistveg_2022-12-06.csv")) |>
  # This lookup contains only OG projects and the Edge-Interior project
  filter(str_detect(project, "Off-Grid|Edge")) |>
  select(project, location, VegForDetectionDistance) |>
  unite("project_location", project, location, sep = "_", remove = TRUE) |>
  bind_rows(df_vegdetdist_focal)

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
