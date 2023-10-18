#-----------------------------------------------------------------------------------------------------------------------

# Project:          Biodiversity Trajectories 2023

# Title:            Calculate density of species by project/location
# Description:      Process raw ABMI (OSM) camera tag data from WildTrax and estimate density using the time in front of
#                   method. Includes the ACME camera deployments.

# Author:           Marcus Becker

# Previous scripts: None

# Last updated:

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
proj <- "bdt"

# Years
years <- "_2023"

# Date
date <- Sys.Date()

#-----------------------------------------------------------------------------------------------------------------------

# Download data
# Set environment variables (username/password)
Sys.setenv(WT_USERNAME = key_get("WT_USERNAME", keyring = "wildtrax"),
           WT_PASSWORD = key_get("WT_PASSWORD", keyring = "wildtrax"))

# Authenticate into WildTrax
wt_auth()

# Pull BDT project ID
bdt_proj <- wt_get_download_summary(sensor_id = "CAM") |>
  filter(str_detect(project, "Trajectories")) |>
  select(project, project_id)

bdt_proj_id <- bdt_proj$project_id

# Download main and image reports using ID
main_report <- wt_download_report(
  project_id = bdt_proj_id,
  sensor_id = "CAM",
  reports = "main")

# Strip it down to only include relevant information
main_reports_clean <- main_report |>
  left_join(bdt_proj, by = "project_id") |>
  # Consolidate tags of same species in same image into one row
  consolidate_tags() |>
  mutate(image_date_time = ymd_hms(image_date_time)) |>
  filter(image_fov == "WITHIN") |>
  # Note: May want to use project_id, location_id, and image_id going forward?
  select(project, location, image_date_time, image_fov, species_common_name, individual_count, age_class, sex_class)

# Image report
image_report <- wt_download_report(
  project_id = bdt_proj_id,
  sensor_id = "CAM",
  reports = "image_report")

# Strip it down to include only relevant information (trigger, field of view)
image_fov_trigger <- image_report |>
  left_join(bdt_proj, by = "project_id") |>
  select(project, location, image_id, image_date_time, image_trigger_mode, image_fov, media_url)

#-----------------------------------------------------------------------------------------------------------------------

# Save cleaned and binded data to Shared Google Drive
# (so we don't have to re-download from WildTrax each time)

# Last done: October 13, 2023

# Simple image reports
image_fov_trigger |>
  write_csv(paste0(g_drive, "data/lookup/image-reports/", proj, years, "_image-report_simple.csv"))

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
tags_clean <- read_csv(file)

#-----------------------------------------------------------------------------------------------------------------------

# Summarise time-by-day for each camera deployment in the ABMI & ACME OSM projects.

df_tbd_summary <- image_fov_trigger |>
  get_operating_days(
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

# VegHF information for BDT deployments
s_drive <- "S:/samba/abmisc/GC_eric/FromEric/To_Marcus/"

library(veghfsoil)
library(RSQLite)
library(DBI)

con <- DBI::dbConnect(RSQLite::SQLite(), paste0(s_drive, "20231006_BDT_LandscapeSummary/summaries_20231011_rev00.sqlite"))
DBI::dbListTables(con)

# Read in landscape summary
df <- read_summary(
  summary_path = paste0(s_drive, "20231006_BDT_LandscapeSummary/summaries_20231011_rev00.sqlite"),
  table = "landscape_summary_camaru_pts_2023_2023"
)

# Obtain long summary
d_long <- make_veghf_long(
  d = df,
  col.label = "Site_ID",
  col.veg = "Combined_ChgByCWCS",
  col.baseyear = 2019,
  col.hfyear = "YEAR",
  col.soil = "Soil_Type_1",
  unround = FALSE,
  hf_fine = TRUE) |>
  make_vegfordetdist() |>
  mutate(project_location = paste0("Biodiversity Trajectories 2023_", Site_ID)) |>
  select(project_location, VEGHFAGEclass, VegForDetectionDistance)

urls <- image_fov_trigger |>
  # Remove lower camera deployments
  filter(!str_detect(location, "-M$")) |>
  filter(image_trigger_mode == "Time Lapse") |>
  mutate(julian = yday(image_date_time),
         hour = hour(image_date_time)) |>
  filter(julian == "166",
         hour == "12") |>
  select(location, media_url)

manual_check <- d_long |>
  separate(project_location, into = c("project", "location"), sep = "_") |>
  left_join(urls, by = "location") |>
  select(location, VEGHFAGEclass, VegForDetectionDistance, media_url)

write_csv(manual_check, paste0(g_drive, "data/lookup/veghf/manual_checking/", "VegHF Checks for Biodiversity Trajectories 2023.csv"))

# Load in manual checks
library(googlesheets4)
library(googledrive)

veghf <- drive_find(type = "spreadsheet", shared_drive = "ABMI Camera Mammals") |>
  filter(str_detect(name, "Biodiversity Trajectories")) |>
  select(id) |>
  pull()

veghf_corr <- read_sheet(ss = veghf) |>
  mutate(VEGHFAGEclass = ifelse(is.na(VegFromImage), VEGHFAGEclass, str_extract(VegFromImage, "^[^ ]+"))) |>
  make_vegfordetdist() |>
  select(location, VegForDetectionDistance)

# Handle the "-M" deployments
d_long <- df_tt |>
  select(location) |>
  distinct() |>
  mutate(low = ifelse(str_detect(location, "-M$"), TRUE, FALSE)) |>
  mutate(location = str_remove(location, "-M$")) |>
  left_join(veghf_corr, by = "location") |>
  mutate(location = ifelse(low == TRUE, paste0(location, "-M"), location)) |>
  mutate(project_location = paste0("Biodiversity Trajectories 2023_", location)) |>
  select(-c(low, location))

# Calculate density (long and wide)
df_density_long <- calc_density_by_loc(tt = df_tt,
                                       veg = d_long,
                                       cam_fov_angle = 40,
                                       format = "long")

df_density_wide <- calc_density_by_loc(tt = df_tt,
                                       veg = d_long,
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

# 150-m vegetation summary

# Read in landscape summary
df <- read_summary(
  summary_path = paste0(s_drive, "20231006_BDT_LandscapeSummary/summaries_20231011_rev00.sqlite"),
  table = "landscape_summary_camaru_2023_2023"
)

# Obtain long summary
d_long <- make_veghf_long(
  d = df,
  col.label = "Site_ID",
  col.veg = "Combined_ChgByCWCS",
  col.baseyear = 2019,
  col.hfyear = "YEAR",
  col.soil = "Soil_Type_1",
  unround = FALSE,
  hf_fine = TRUE)

library(dplyr)
library(tidyr)
library(stringr)

veghf_150 <- d_long |>
  filter(deployment == "CAM" | deployment == "BOTH") |>
  mutate(location = Site_ID) |>
  group_by(location, Section, VEGHFAGEclass) |>
  summarise(area = sum(Shape_Area)) |>
  ungroup() |>
  mutate(VEGHFAGEclass = str_replace(VEGHFAGEclass, "0$", "9")) |>
  mutate(project = "Biodiversity Trajectories 2023") |>
  pivot_wider(id_cols = c(location, project, Section), names_from = VEGHFAGEclass, values_from = area, values_fill = 0)

write_csv(veghf_150, paste0(g_drive, "data/lookup/veghf/bdt_2023_veghf-buffer-summary_2023-10-17.csv"))


