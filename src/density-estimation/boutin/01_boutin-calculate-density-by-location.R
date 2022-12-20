#-----------------------------------------------------------------------------------------------------------------------

# Project:          BOUTIN

# Title:            Calculate density of species by project/location
# Description:      Process raw BOUTIN camera tag data from WildTrax and estimate density using the time in front of
#                   method.
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
source("./src/functions/estimate-density-tifc.R")

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

# Strip it down to include only relevant information (trigger, field of view)
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

# Read data
image_fov_trigger <- read_csv(paste0(g_drive, "data/lookup/images/", proj, "_all-years_image-report_simple.csv"))

# Summarise time-by-day for each camera deployment Boutin projects.
# Note this is the old way to do it.
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
# write_csv(df_tbd_summary, paste0(g_drive, "data/processed/time-by-day/", proj, "_all-years_tbd-summary_", Sys.Date(), ".csv"))

# New way: Retrieve days of operation
df_days <- image_fov_trigger |>
  # Remove a couple weird dates
  filter(date_detected > as.Date("2014-01-01")) |>
  unite("location", location, serial_number, sep = "-", remove = TRUE) |>
  get_operating_days(
    # Don't keep project
    include_project = FALSE,
    # Don't summarise
    summarise = FALSE) |>
  mutate(operating = 1)
  # Nice, this seemed to work well. Yay for functions.

# Custom monitoring periods (per Stan)
monitoring_periods <- data.frame(
  "monitoring_period" = c(
    "spring_2014",
    "fall_2014",
    "spring_2015",
    "fall_2015",
    "spring_2016",
    "fall_2016",
    "spring_2017",
    "fall_2017",
    "spring_2018",
    "fall_2018",
    "spring_2019",
    "fall_2019",
    "spring_2020",
    "fall_2020",
    "spring_2021",
    "fall_2021"),
  "date_start" = c(
    as.Date("2014-04-01"),
    as.Date("2014-09-01"),
    as.Date("2015-04-01"),
    as.Date("2015-09-01"),
    as.Date("2016-04-01"),
    as.Date("2016-09-01"),
    as.Date("2017-04-01"),
    as.Date("2017-09-01"),
    as.Date("2018-04-01"),
    as.Date("2018-09-01"),
    as.Date("2019-04-01"),
    as.Date("2019-09-01"),
    as.Date("2020-04-01"),
    as.Date("2020-09-01"),
    as.Date("2021-04-01"),
    as.Date("2021-09-01")),
  "date_end" = c(
    as.Date("2014-06-30"),
    as.Date("2014-11-30"),
    as.Date("2015-06-30"),
    as.Date("2015-11-30"),
    as.Date("2016-06-30"),
    as.Date("2016-11-30"),
    as.Date("2017-06-30"),
    as.Date("2017-11-30"),
    as.Date("2018-06-30"),
    as.Date("2018-11-30"),
    as.Date("2019-06-30"),
    as.Date("2019-11-30"),
    as.Date("2020-06-30"),
    as.Date("2020-11-30"),
    as.Date("2021-06-30"),
    as.Date("2021-11-30")))

# Summarise operating days by MP
df_summary <- monitoring_periods |>
  mutate(number_of_days = map2(.x = date_start,
                               .y = date_end,
                               .f = ~ summarise_op_days_by_mp(
                                 x = df_days,
                                 date_start = .x,
                                 date_end = .y,
                                 season = FALSE))) |>
  unnest(cols = c(number_of_days)) |>
  select(monitoring_period, location, total_days)

#-----------------------------------------------------------------------------------------------------------------------

# Read in data
tags_clean <- read_csv(paste0(g_drive, "data/base/clean/", proj, "_all-years_all-data_clean_2022-12-02.csv"))

# Calculate time in front of camera (TIFC)
# Note: this is the standard way.
df_tt <- tags_clean |>
  # First calculate time by series
  calculate_time_by_series() |>
  # Next calculate tifc by location, deployment, species
  sum_total_time(tbd = df_tbd_summary)

# Now we're doing it by Monitoring Period
# Expand monitoring periods dataframe
df_mp_expanded <- monitoring_periods |>
  mutate(date = map2(.x = date_start, .y = date_end, .f = ~ seq.Date(from = .x, to = .y, by = "day"))) |>
  unnest(cols = c(date)) |>
  select(monitoring_period, date)

# Calculate time by series
tt <- tags_clean |>
  calculate_time_by_series() |>
  mutate(date = as.Date(series_start)) |>
  # Join monitoring periods
  left_join(df_mp_expanded, by = "date") |>
  # Remove any series that aren't part of a monitoring period
  filter(!is.na(monitoring_period)) |>
  mutate_at(c("monitoring_period", "location", "common_name"), factor) |>
  group_by(monitoring_period, location, common_name, .drop = FALSE) |>
  # Sum up the total time in front of the camera
  summarise(total_duration = sum(series_total_time)) |>
  ungroup() |>
  mutate_if(is.factor, as.character) |>
  # New time-by-day summary that has been generated in the previous script
  left_join(df_summary, by = c("location", "monitoring_period")) |>
  # This is where the lack of 0 combos in df_summary manifests as an issue
  mutate(total_days = ifelse(is.na(total_days), 0, total_days))

# Unique species seen
sp <- as.character(sort(unique(tt$common_name)))

tt_nn <- df_summary |>
  # Retrieve only those that had no images of animals
  anti_join(tt, by = c("location", "monitoring_period")) |>
  crossing(common_name = sp) |>
  # Add total_duration column, which is zero in these cases
  mutate(total_duration = 0)

tt_full <- tt |>
  bind_rows(tt_nn) |>
  arrange(monitoring_period, location, common_name) |>
  select(monitoring_period, location, common_name, total_days, total_duration)

# Write results
# Full results long:
write_csv(tt_full, paste0(g_drive, "data/processed/time-in-cam-fov/", proj, "_monitoring-periods_fov-time_long_", Sys.Date(), ".csv"))

#-----------------------------------------------------------------------------------------------------------------------

# Calculate density at each location

# Dataframe for use in density function
df_vegdetdist <- tt_full |>
  select(location) |>
  distinct() |>
  mutate(VegForDetectionDistance = "Conif")

# Only interested in certain species
sp <- c("Snowshoe Hare", "Canada Lynx")

cam_fov_angle <- 40

# Effective detection distance (EDD) predictions lookup
edd <- read_csv(paste0(g_drive, "data/processed/detection-distance/predictions/edd_veghf_season.csv"))
# EDD species groups
dist_groups <- read_csv(paste0(g_drive, "data/lookup/species-distance-groups.csv"))

d <- tt_full |>
  # Make 'seasons' (to match to EDD values) - not perfect but close enough
  mutate(season = case_when(
    str_detect(monitoring_period, "fall") ~ "winter",
    str_detect(monitoring_period, "spring") ~ "summer"
  )) |>
  # Join species EDD groups
  left_join(dist_groups, by = "common_name") |>
  # Join detection distance vegetation values
  left_join(df_vegdetdist, by = "location") |>
  # Join EDD predictions
  left_join(edd, by = c("dist_group", "season", "VegForDetectionDistance")) |>
  # Remove random species (mostly birds) <- something to check on though.
  filter(!is.na(detdist)) |>
  # Calculate density
  mutate(effort = total_days * (detdist ^ 2 * pi * (cam_fov_angle / 360)) / 100,
         # Catch per unit effort
         cpue = total_duration / effort,
         # Catch per unit effort in km2
         cpue_km2 = cpue / 60 / 60 / 24 * 10000) |>
  # All the NaNs are just combinations where the total_seasons_days is 0.
  select(monitoring_period, location, common_name, total_days, total_duration, density_km2 = cpue_km2)

# Write results

write_csv(d, paste0(g_drive, "results/density/deployments/", proj, "_monitoring-periods_density_long_", Sys.Date(), ".csv"))

#-----------------------------------------------------------------------------------------------------------------------
