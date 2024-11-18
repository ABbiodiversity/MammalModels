#-----------------------------------------------------------------------------------------------------------------------

# Project:          ABMI - Oilsands Monitoring - BADR Mammals

# Title:            Calculate density of species by project/location for the OSM BADR cameras
# Description:      Process raw ABMI (OSM) camera tag data from WildTrax and estimate density using the time in front of
#                   method. Includes the ACME camera deployments.

# Author:           Marcus Becker

# Previous scripts: None

# Last updated:     September 2024

#-----------------------------------------------------------------------------------------------------------------------

# Attach packages

library(wildrtrax) # To download data
library(keyring)   # For storing credentials safely

# Set path to Shared Google Drive (G Drive) - note: NEW for OSM BADR data
g_drive <- "G:/Shared drives/OSM BADR Mammals/"

# Older Google Drive
g_drive_old <- "G:/Shared drives/ABMI Camera Mammals/"

# Source functions for TIFC workflow
source("./src/Functions/estimate-density-tifc.R")

# Write and archive function
source("./src/Functions/write-and-archive.R")

# Species character strings
load(paste0(g_drive_old, "data/lookup/wt_cam_sp_str.RData"))

# Project
proj <- "OSM BADR"

# Years
years <- "2021-2023"

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
  filter(str_detect(project, "OSM"),
         organization == "ABMI")

# Project IDs as a vector
osm_proj_ids <- osm_proj$project_id

# Download tag and image reports using IDs
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
  filter(image_fov == "WITHIN") |>
  select(project, location, image_date_time, image_fov, species_common_name, individual_count, age_class, sex_class)

# Don't think I need to download image reports here anymore.
image_reports <- map_df(.x = osm_proj_ids,
                        .f = ~ wt_download_report(
                          project_id = .x,
                          sensor_id = "CAM",
                          report = "image_report",
                          weather_cols = FALSE))

# Strip it down to include only relevant information (trigger, field of view)
image_fov_trigger <- image_reports |>
  left_join(osm_proj, by = "project_id") |>
  select(project, location, image_date_time, image_trigger_mode, image_fov)

#-----------------------------------------------------------------------------------------------------------------------

# Save cleaned data to Shared Google Drive
# (so we don't have to re-download from WildTrax each time)

# Last done: August 12, 2024

# Simple image reports
image_fov_trigger |>
  write_csv(paste0(g_drive, "Data/OSM BADR ABMI ACME 2021-2023 Image Reports Simple.csv"))

# Last done: August 12, 2024

# Only tags of species
main_reports_clean |>
  # Keep only species tags
  filter(species_common_name %in% native_sp) |>
  write_csv(paste0(g_drive, "Data/OSM BADR ABMI ACME 2021-2023 Main Reports Clean Native Sp.csv"))

# Save (all) cleaned data
main_reports_clean |>
  write_csv(paste0(g_drive, "Data/OSM BADR ABMI ACME 2021-2023 Main Reports Clean.csv"))

#-----------------------------------------------------------------------------------------------------------------------

# If needed (not re-downloading from WildTrax), import data:

image_fov_trigger <- read_csv(paste0(g_drive, "Data/OSM BADR ABMI ACME 2021-2023 Image Reports Simple.csv"))

main_reports_clean <- read_csv(paste0(g_drive, "Data/OSM BADR ABMI ACME 2021-2023 Main Reports Clean.csv"))

#-----------------------------------------------------------------------------------------------------------------------

# Summarise time-by-day for each camera deployment in the ABMI & ACME OSM projects.

df_tbd <- get_operating_days(
  image_report = image_fov_trigger,
  # Keep project
  include_project = TRUE,
  # Summarise
  summarise = FALSE,
  # Include ABMI seasons
  .abmi_seasons = TRUE
)

# New season cutoff Julian days
spring.start <- 99
summer.start <- 143
winter.start <- 288

# Note: Will integrate this new approach into the get_operating_days() function eventually.
df_tbd_summary <- df_tbd |>
  mutate(operating = 1) |>
  # Create variable for Julian date
  mutate(julian = as.numeric(format(ymd(date), "%j"))) |>
  # Create variable for new season definitions
  mutate(season_new = as.factor(case_when(
    julian >= spring.start & julian <= summer.start ~ "Spring",
    julian > summer.start & julian <= winter.start ~ "Summer",
    TRUE ~ "Winter"))) |>
  mutate_at(c("project_location", "season_new"), factor) |>
  group_by(project_location, season_new, .drop = FALSE) |>
  summarise(operating_days = sum(operating)) |>
  ungroup() |>
  mutate_if(is.factor, as.character) |>
  group_by(project_location) |>
  mutate(total_days = sum(operating_days)) |>
  pivot_wider(id_cols = c(project_location, total_days), names_from = season_new, values_from = operating_days) |>
  ungroup()

#-----------------------------------------------------------------------------------------------------------------------

# Calculate time in front of camera (TIFC)

df_tt <- main_reports_clean |>
  # Turn 'Deer' tags into White-tailed Deer (most likely)
  mutate(species_common_name = ifelse(species_common_name == "Deer", "White-tailed Deer", species_common_name)) |>
  # First calculate time by series
  calculate_time_by_series() |>
  # Next calculate TIFC by location, deployment, species
  sum_total_time(tbd = df_tbd_summary)

write_csv(df_tt, paste0(g_drive, "Data/OSM BADR ABMI ACME 2021-2023 TIFC By Species Season.csv"))

#-----------------------------------------------------------------------------------------------------------------------

# Calculate density at each location

# Veg Detection Distance Categories for OSM deployments

library(googlesheets4)
library(googledrive)

# Sheet ID for OSM EDD categories
sheet_id <- drive_find(type = "spreadsheet",
                       shared_drive = "ABMI Mammals") |>
  filter(str_detect(name, "OSM")) |>
  select(id) |>
  pull()

# Read in data
edd_cat <- read_sheet(ss = sheet_id) |>
  select(project, location, primary_category, secondary_category) |>
  mutate(overall_category = paste0(primary_category, "_", secondary_category)) |>
  unite("project_location", project, location, sep = "_", remove = TRUE) |>
  select(project_location, overall_category)

# Calculate density (long)
df_density_long <- calc_density_by_loc(tt = df_tt,
                                       veg = edd_cat,
                                       cam_fov_angle = 40,
                                       format = "long")

# Summarise density
df_density_sum <- df_density_long |>
  # Remove seasons with operating days less than 20
  filter(total_season_days >= 20) |>
  # Remove Black Bear and Winter
  filter(!(species_common_name == "Black Bear" & season_new == "Winter")) |>
  # Summarise density
  group_by(project, location, species_common_name) |>
  summarise(density_km2 = weighted.mean(density_km2, w = total_season_days),
            total_days = sum(total_season_days))

# Save results
write_csv(df_density_sum, paste0(g_drive, "Results/OSM BADR ABMI ACME 2021-2023 Species Density By Deployment.csv"))

#-----------------------------------------------------------------------------------------------------------------------
