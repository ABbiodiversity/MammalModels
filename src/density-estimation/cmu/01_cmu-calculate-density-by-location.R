#-----------------------------------------------------------------------------------------------------------------------

# Project:          CMU (2017, 2018, 2019, 2020, 2021)

# Title:            Calculate density of species by project/location
# Description:      Process raw CMU camera tag data from WildTrax and estimate density using the time in front of camera
#                   method.
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
proj <- "cmu"

# Years
years <- "_17-18-19-20-21"

#-----------------------------------------------------------------------------------------------------------------------

# Download data
# Set environment variables (username/password)
Sys.setenv(WT_USERNAME = key_get("WT_USERNAME", keyring = "wildtrax"),
           WT_PASSWORD = key_get("WT_PASSWORD", keyring = "wildtrax"))

# Authenticate into WildTrax
wt_auth()

# Pull CMU project IDs
cmu_proj_ids <- wt_get_download_summary(sensor_id = "CAM") |>
  filter(str_detect(project, "CMU"),
         # Remove the Lemming project
         !str_detect(project, "Lemming")) |>
  pull(project_id) |>
  unlist()

cmu_proj_ids <- 904

# Download tag and image reports using IDs
tag_reports <- map_df(.x = cmu_proj_ids,
                      .f = ~ wt_download_report(
                        project_id = .x,
                        sensor_id = "CAM",
                        report = "tag",
                        weather_cols = FALSE))

# Note: There is an issue now - a column has been added, image_exif_temperature
image_reports <- map_df(.x = cmu_proj_ids,
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
  mutate(date_detected = ymd_hms(date_detected)) |>
  left_join(image_fov_trigger, by = c("project", "location", "date_detected")) |>
  mutate(date_detected = ymd_hms(date_detected)) |>
  filter(field_of_view == "WITHIN") |>
  select(project, location, date_detected, common_name, age_class, sex, number_individuals)

#-----------------------------------------------------------------------------------------------------------------------

# Save cleaned and binded data to Shared Google Drive
# (so we don't have to re-download from WildTrax each time)

# Last done:

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

image_fov_trigger <- read_csv(paste0(g_drive, "data/lookup/images/", proj, "_all-years_image-report_simple.csv"))

# Find appropriate tag data file
file <- list.files(path = paste0(g_drive, "data/base/clean"), full.names = TRUE) |>
  str_subset(pattern = paste0(proj, "_all-years_all-data_clean"))
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

project <- c("CMU 2017",
              "CMU 2018",
              "CMU Ecosystem Monitoring Camera Program 2019",
              "CMU Ecosystem Monitoring Camera Program 2020",
              "CMU Ecosystem Monitoring Camera Program 2021")

# VegForDetectionDistance lookups:
df_veghf <- read_csv(paste0(g_drive, "data/lookup/veghf/abmi-cmu_all-years_veghf-soilhf-detdistveg_2021-10-06.csv")) |>
  filter(str_detect(project, "^CMU")) |>
  # Note: Purely using location, not project. Locations should be consistent project-to-project.
  select(location, VegForDetectionDistance) |>
  distinct()

# Updated values from database:
# Create database connection to CMU VegHF Checks (SQLite db)
cvc <- DBI::dbConnect(
  drv = RSQLite::SQLite(),
  paste0(g_drive, "database/cmu-veghf-checks.db")
)

# Tables
tables <- DBI::dbListTables(conn = cvc) |> str_subset("updates")

# Pull updated data (from visual classification)
updated_veghf <- map_df(.x = tables, .f = ~ DBI::dbReadTable(conn = cvc, name = .x)) |>
  filter(!VegForDetectionDistance_updated == "")

# Update VegHF
df_veghf_updated <- df_veghf |>
  left_join(updated_veghf, by = "location") |>
  mutate(VegForDetectionDistance = ifelse(!is.na(VegForDetectionDistance_updated),
                                          VegForDetectionDistance_updated,
                                          VegForDetectionDistance)) |>
  select(1, 2) |>
  crossing(project) |>
  unite("project_location", project, location, sep = "_", remove = TRUE)

DBI::dbDisconnect(cvc)

# Calculate density
df_density_long <- calc_density_by_loc(tt = df_tt,
                                       veg = df_veghf_updated,
                                       cam_fov_angle = 40,
                                       format = "long")

df_density_wide <- calc_density_by_loc(tt = df_tt,
                                       veg = df_veghf_updated,
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

# Pull out Trail (and paired) deployments

# CHR 2018, CHR 2019, ADE 2020

trail <- df_density_wide |>
  filter(str_detect(location, "CHR") & str_detect(project, "2018|2019") | str_detect(location, "ADE") & str_detect(project, "2020"))

#-----------------------------------------------------------------------------------------------------------------------
