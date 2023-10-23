#-----------------------------------------------------------------------------------------------------------------------

# Title:       Estimate time in front of camera for paired heights analysis
# Description: At each of the 10 paired sites with two cameras at different heights (1m vs 0.5m), estimate the total
#              time spent in front of the camera for each mammal species.

# Author:      Marcus A Becker
# Date:        July 28, 2022

#-----------------------------------------------------------------------------------------------------------------------

# Root directory (Shared Google Drive)
g_drive <- "G:/Shared drives/ABMI Camera Mammals/"

# Attach packages
library(wildRtrax) # Just obtaining data
library(keyring) # Storing credentials

library(tidyverse)

# Native species tags in WildTrax
load(paste0(g_drive, "data/lookup/wt_cam_sp_str.RData"))

# Probabilistic gaps probability of leaving predictions
df_leave_prob_pred <- read_csv(paste0(g_drive, "data/processed/probabilistic-gaps/gap-leave-prob_predictions_2021-10-05.csv"))
# Gap groups
df_gap_groups <- read_csv(paste0(g_drive, "data/lookup/species-gap-groups.csv"))
# Average time between images per species
df_tbp <- read_csv(paste0(g_drive, "data/processed/time-btwn-images/abmi-cmu_all-years_tbp_2021-06-25.csv"))

# Seasonal start/end dates (julian day):
summer.start.j <- 106 # April 16
summer.end.j <- 288 # October 15

#-----------------------------------------------------------------------------------------------------------------------

# Load data

# Set up credentials for WildTrax
Sys.setenv(WT_USERNAME = key_get("WT_USERNAME", keyring = "wildtrax"),
           WT_PASSWORD = key_get("WT_PASSWORD", keyring = "wildtrax"))

# Authenticate
wt_auth()

# Obtain database project IDs
proj <- wt_get_download_summary(sensor_id = "CAM") |>
  filter(str_detect(project, "Ecosystem Health 2022|Height|ABMI OSM 2022|Trajectories")) |>
  select(project, project_id)

proj_ids <- proj$project_id

data <- map_df(.x = proj_ids,
               .f = ~ wt_download_report(
                 project_id = .x,
                 sensor_id = "CAM",
                 weather_cols = FALSE,
                 reports = "main")) |>
  left_join(proj, by = "project_id")

# Locations that had paired camera heights for pulling out of EH and OSM
osm_eh_loc <- data |>
  filter(str_detect(project, "Height")) |>
  select(location) |>
  distinct() |>
  pull(location)

# Locations that had paired camera heights for pulling out of BDT
bdt_loc <- data |>
  filter(str_detect(project, "Trajectories"),
         str_detect(location, "-M$")) |>
  mutate(location = str_remove(location, "-M$")) |>
  select(location) |>
  distinct() |>
  pull()

bdt_data <- data |>
  filter(str_detect(project, "Trajectories")) |>
  mutate(height = ifelse(str_detect(location, "-M$"), "0.5m", "1m"),
         location = str_remove(location, "-M$")) |>
  filter(location %in% bdt_loc)

data_all <- data |>
  filter(location %in% osm_eh_loc) |>
  mutate(height = ifelse(str_detect(project, "Height"), "0.5m", "1m")) |>
  bind_rows(bdt_data) |>
  filter(image_fov == "WITHIN") |>
  # Don't need project, just keep the lower column
  select(location, height, image_date_time, species_common_name, individual_count)

#-----------------------------------------------------------------------------------------------------------------------

# Adjustments required:

# Ecosystem Health 2022 (1m cameras):
# - 793-NE failed August 6, 2021
# - 793-SW failed May 18, 2022

# OSM 2022 (1m cameras):
# - 1-1A2-CA1 failed December 19, 2022
# - 1-1A3-CA3 * Note that this deployment is a bit wonky, with no timelapse images. Will keep in though.

# Heights (0.5m cameras):
# - 792-SW failed July 1, 2022
# - 1-1A3-CA3 -> failed on February 12, 2023

# BDT 2023 (1m cameras):
# - 724-3-NE failed April 8, 2023
# - 761-1-SE -> Need to adjust time. Seems to be ahead by half a day?
# - 602-2-SW -> needs to have dates pushed ahead by 5 days

# BDT 2023 (0.5m cameras)

# - 637-2-NE-M -> Actual start date is March 12, 2023
# - 724-4-SE-M -> Actual start date is March 2, 2023
# - 761-3-NE-M -> Actual start date is March 9, 2023


# Camera pairs to remove from analysis:
# - 1-1A2-CA2 -> 1m camera didn't take reliable images (cute baby lynx photos though)
# - 1-1A2-CA3 -> Neither camera took reliable images
# - 1-1A2-CA4 -> Neither camera took reliable images (cute baby bear images though)
# - 1-1A2-CA5 -> Neither camera took reliable images
# - 1-2A2-CA1 -> Awkward camera angle / suspicious height.

remove <- c("1-1A2-CA2", "1-1A2-CA3", "1-1A2-CA4", "1-1A2-CA5", "1-2A2-CA1")

data_subset <- data_all |>
  # Remove deployment pairs that did not collect reliable data
  filter(!location %in% remove) |>
  mutate(image_date_time = ymd_hms(image_date_time)) |>
  # Truncate dates so that only data operating during a common period is used (only 3/10 locations impacted)
  filter(!(image_date_time > as.Date("2021-08-07 00:00:00") & location == "793-NE"),
         !(image_date_time > as.Date("2022-05-19 00:00:00") & location == "793-SW"),
         !(image_date_time > as.Date("2022-07-02 00:00:00") & location == "792-SW"),
         !(image_date_time > as.Date("2022-12-19 00:00:00") & location == "1-1A2-CA1"),
         !(image_date_time > as.Date("2023-02-12 00:00:00") & location == "1-1A3-CA3"),
         !(image_date_time > as.Date("2023-04-08 00:00:00") & location == "OG-ALPAC-724-3-NE"),
         !(image_date_time < as.Date("2023-03-12 00:00:00") & location == "OG-ALPAC-637-2-NE" & height == "0.5m"),
         !(image_date_time < as.Date("2023-03-02 00:00:00") & location == "OG-ALPAC-724-4-SE" & height == "0.5m"),
         !(image_date_time < as.Date("2023-03-09 00:00:00") & location == "OG-ALPAC-761-3-NE" & height == "0.5m")) |>
  # Adjust date/times that were set wrong
  mutate(image_date_time = case_when(
    location == "OG-ALPAC-761-1-SE" & height == "1m" ~ image_date_time %m-% hours(12),
    location == "OG-ALPAC-602-2-SW" & height == "1m" ~ image_date_time %m+% days(5),
    TRUE ~ image_date_time))

#-----------------------------------------------------------------------------------------------------------------------

# Assess where NONE images split detections
df_gap_nones <- data_subset |>
  select(location, height, image_date_time, species_common_name) |>
  arrange(location, height, image_date_time) |>
  # Create gap class column
  mutate(species_common_name_next = lead(species_common_name),
         gap_class = ifelse(species_common_name != "NONE" & species_common_name_next == "NONE", "N", NA)) |>
  filter(gap_class == "N") |>
  select(-c(species_common_name_next))

# Identify independent detections ('series')
df_series <- data_subset |>
  filter(species_common_name %in% native_sp,
         !individual_count == "VNA") |>
  # Identify where NONEs occurred
  left_join(df_gap_nones, by = c("location", "height", "image_date_time", "species_common_name")) |>
  unite("location", c(location, height), sep = "_") |>
  # Order observations
  arrange(location, image_date_time, species_common_name) |>
  # Identify series
  mutate(series_num = 0,
         # Lagged time stamp
         image_date_time_previous = lag(image_date_time),
         # Lead time stamp
         image_date_time_next = lead(image_date_time),
         # Calculate difference in time between ordered images
         diff_time_previous = as.numeric(image_date_time - image_date_time_previous),
         diff_time_next = as.numeric(abs(image_date_time - image_date_time_next)),
         # Lagged species
         species_common_name_previous = lag(species_common_name),
         # Was is a different species?
         diff_sp = ifelse(species_common_name != species_common_name_previous, TRUE, FALSE),
         # Lagged deployment
         location_previous = lag(location),
         # Was is a different deployment?
         diff_location = ifelse(location != location_previous, TRUE, FALSE),
         # Flag gaps that will need checking
         gap_check = ifelse(diff_location == FALSE & diff_sp == FALSE & (diff_time_previous <= 120 & diff_time_previous >= 20), 1, 0),
         # Lagged gap class
         gap_class_previous = replace_na(lag(gap_class), ""),
         # Identify new series, based on being different deployment, species, greater than 120 seconds, and approp gaps
         diff_series = ifelse(diff_location == TRUE | diff_sp == TRUE | diff_time_previous > 120 | (gap_class_previous == "L" | gap_class_previous == "N"), 1, 0),
         # Number series
         series_num = c(1, cumsum(diff_series[-1]) + 1),
         # Flag gaps that require probabilistic time assignment
         gap_prob = replace_na(ifelse(gap_check == 1 & (gap_class_previous == "" | gap_class_previous == "U"), 1, 0), 0)) |>
  group_by(series_num) |>
  mutate(diff_time_previous = ifelse(row_number() == 1, 0, diff_time_previous),
         diff_time_next = ifelse(row_number() == n(), 0, diff_time_next)) |>
  ungroup() |>
  # Join gap group lookup table
  left_join(df_gap_groups, by = "species_common_name") |>
  # Join gap leaving predictions
  left_join(df_leave_prob_pred, by = c("gap_group", "diff_time_previous" = "diff_time")) |>
  # Adjust time difference between ordered images that require probabilistic time assignment
  mutate(pred = replace_na(pred, 1),
         diff_time_previous_adj = ifelse(gap_prob == 1, diff_time_previous * (1 - pred), diff_time_previous),
         diff_time_next_adj = ifelse(lead(gap_prob == 1), diff_time_next * (1 - lead(pred)), diff_time_next))

# Vector of all project-locations
dep <- df_series |>
  select(location) |>
  distinct() |>
  pull()

# Total time for each detection/series
df_tts <- df_series |>
  left_join(df_tbp, by = "species_common_name") |>
  mutate(individual_count = as.numeric(individual_count)) |>
  group_by(series_num) |>
  mutate(# Check whether the image was first or last in a series
    bookend = ifelse(row_number() == 1 | row_number() == n(), 1, 0),
    # Calculate time for each individual image
    image_time = ifelse(bookend == 1,
                        ((diff_time_previous_adj + diff_time_next_adj) / 2) + (tbp / 2),
                        (diff_time_previous_adj + diff_time_next_adj) / 2),
    # Multiply image time by the number of animals present
    image_time_ni = image_time * individual_count) |>
  # Group by common name as well to add it as a variable to output
  group_by(species_common_name, location, .add = TRUE) |>
  # Calculate total time and number of images for each series
  summarise(n_images = n(),
            series_total_time = sum(image_time_ni)) |>
  ungroup() |>
  # Just adjust a single WTD series
  mutate(series_total_time = if_else(is.na(series_total_time), 22, series_total_time)) |>
  # Double the series time of single-series images (halved in an earlier step when it shouldn't be)
  mutate(series_total_time = ifelse(n_images < 2, series_total_time * 2, series_total_time)) |>
  select(series_num, species_common_name, n_images, series_total_time)

# Calculate total time in front of the camera, by deployment, project, and species
df_tt <- df_series |>
  group_by(series_num) |>
  arrange(image_date_time, .by_group = TRUE) |>
  filter(row_number() == 1) |>
  left_join(df_tts, by = c("series_num", "species_common_name")) |>
  select(location, image_date_time, species_common_name, series_num, series_total_time) |>
  ungroup() |>
  mutate(julian = as.numeric(format(image_date_time, "%j")),
         season = ifelse(julian >= summer.start.j & julian <= summer.end.j, "summer", "winter")) |>
  mutate_at(c("location", "species_common_name", "season"), factor) |>
  # Note: didn't include season here - not enough sample size at this point (I think ...). TBD.
  group_by(location, species_common_name, .drop = FALSE) |>
  summarise(total_duration = sum(series_total_time)) |>
  ungroup() |>
  mutate_if(is.factor, as.character)

#-----------------------------------------------------------------------------------------------------------------------

# Save results
write_csv(df_tt, paste0(g_drive, "data/processed/time-in-cam-fov/camera-heights_fov-time-long_", Sys.Date(), ".csv"))

#-----------------------------------------------------------------------------------------------------------------------
