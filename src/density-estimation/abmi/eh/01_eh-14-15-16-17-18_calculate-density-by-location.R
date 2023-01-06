#-----------------------------------------------------------------------------------------------------------------------

# Project:          ABMI (Ecosystem Health 2014, 2015, 2016, 2017, 2018)

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

# Write and archive function
source("./src/functions/write-and-archive.R")

# Species character strings
load(paste0(g_drive, "data/lookup/wt_cam_sp_str.RData"))

# Project
proj <- "eh"

# Years
years <- "_14-15-16-17-18"

#-----------------------------------------------------------------------------------------------------------------------

# Download data
# Set environment variables (username/password)
Sys.setenv(WT_USERNAME = key_get("WT_USERNAME", keyring = "wildtrax"),
           WT_PASSWORD = key_get("WT_PASSWORD", keyring = "wildtrax"))

# Authenticate into WildTrax
wt_auth()

# Pull EH project IDs - 2014, 2015, 2016, 2017, and 2018.
eh_proj_ids <- wt_get_download_summary(sensor_id = "CAM") |>
  filter(str_detect(project, "Health 2014|Health 2015|Health 2016|Health 2017|Health 2018")) |>
  pull(project_id) |>
  unlist()

# Download tag and image reports using IDs
tag_reports <- map_df(.x = eh_proj_ids,
                      .f = ~ wt_download_report(
                        project_id = .x,
                        sensor_id = "CAM",
                        report = "tag",
                        weather_cols = FALSE))

image_reports <- map_df(.x = id,
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

# Last done: NEEDS TO BE DONE

# Simple image reports
image_fov_trigger |>
  write_csv(paste0(g_drive, "data/lookup/images/", proj, "_image-report_simple.csv"))

# Only tags of species
tags_clean |>
  # Remove all non-native mammal images
  filter(common_name %in% native_sp) |>
  write_csv(paste0(g_drive, "data/base/clean/", proj, "_native-sp_clean_", Sys.Date(), ".csv"))

# Save (all) cleaned data
tags_clean |>
  write_csv(paste0(g_drive, "data/base/clean/", proj, "_all-data_clean_", Sys.Date(), ".csv"))

# This is the temporary solution - use data processed previously.
tags_clean <- read_csv(paste0(g_drive, "data/base/clean/abmi-cmu_all-years_all-data_clean_2021-10-07.csv")) |>
  # Filter for appropriate projects
  filter(str_detect(project, "Health 2014|Health 2015|Health 2016|Health 2017|Health 2018")) |>
  # Remove OG deployments
  filter(!str_detect(location, "OG")) |>
  select(project, location, date_detected, common_name, age_class, sex, number_individuals)

#-----------------------------------------------------------------------------------------------------------------------

# Summarise time-by-day for each camera deployment in the five EH projects (14, 15, 16, 17, 18).

# HAS NOT BEEN DONE
df_tbd_summary <- get_operating_days(
  image_report = image_fov_trigger,
  # Keep project
  include_project = TRUE,
  # Summarise
  summarise = TRUE,
  # Include ABMI seasons
  .abmi_seasons = TRUE
)

# Write results - HAS NOT BEEN DONE
write_csv(df_tbd_summary, paste0(g_drive, "data/processed/time-by-day/", proj, "_tbd-summary_", Sys.Date(), ".csv"))

# This is the temporary solution - use data processed previously.
df_tbd_summary <- read_csv(paste0(g_drive, "data/processed/time-by-day/abmi-cmu_all-years_tbd-summary_2021-10-07.csv")) |>
  # Filter for appropriate projects
  filter(str_detect(project, "Health 2014|Health 2015|Health 2016|Health 2017|Health 2018")) |>
  # Remove OG deployments
  filter(!str_detect(location, "OG")) |>
  unite("project_location", project, location, sep = "_")

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

# VegHF information for EH deployments
df_vegdetdist <- read_csv(paste0(g_drive, "data/lookup/veghf/abmi-cmu_2013-2018_vegsoilhf-detdistveg_2022-12-06.csv")) |>
  filter(str_detect(project, "ABMI")) |>
  filter(!str_detect(location, "OG")) |>
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
