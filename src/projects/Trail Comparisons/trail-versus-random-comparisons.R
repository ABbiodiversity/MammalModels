#-----------------------------------------------------------------------------------------------------------------------

# Project:          Game trail vs Random Comparisons

# Title:            Game trail vs Random Comparisons
# Description:

# Author:           Marcus Becker

# Previous scripts: None

#-----------------------------------------------------------------------------------------------------------------------

library(readr)
library(dplyr)
library(stringr)
library(tidyr)

# Set path to Shared Google Drive (G Drive)
g_drive <- "G:/Shared drives/ABMI Camera Mammals/"

#-----------------------------------------------------------------------------------------------------------------------

# Calculate densities at these cameras only during common operation periods.

cmu_dep_days <- read_csv(paste0(g_drive, "data/processed/time-by-day/cmu_dep_operation_by_day_2022-08-22.csv")) |>
  filter(str_detect(location, "CHR|ADE"))

# Trail cameras
trail <- cmu_dep_days |>
  filter(str_detect(location, "T$")) |>
  filter(str_detect(location, "CHR|ADE")) |>
  mutate(location_trail = location,
         location = str_remove(location, "T$")) |>
  rename(operating_trail = operating)

# Random cameras
random <- cmu_dep_days |>
  filter(str_detect(location, "CHR|ADE"),
         !str_detect(location, "T$")) |>
  rename(operating_random = operating)

# Pair together to evaluate common operation days
paired <- random |>
  left_join(trail, by = c("location", "date", "julian", "season")) |>
  filter(operating_random == 1 & operating_trail == 1) |>
  pivot_longer(cols = c(location, location_trail), values_to = "location") |>
  select(location, date, julian, season) |>
  mutate(operating = 1) |>
  # Something funky going on with start date on a few trail cameras.
  filter(date > as.Date("2018-09-17"))

beginning <- min(paired$date)
ending <- max(paired$date)

summarise_number_of_op_days <- function(x, date_start, date_end) {

  # Convert date_start and date_end to Date
  date_start <- as.Date(date_start)
  date_end <- as.Date(date_end)

  # Summarise based on custom date range
  y <- x |>
    filter(date >= date_start,
           date <= date_end) |>
    mutate_at(c("location", "season"), factor) |>
    group_by(location, season, .drop = FALSE) |>
    summarise(operating_days = sum(operating)) |>
    ungroup() |>
    pivot_wider(id_cols = location, names_from = season, values_from = operating_days)

  if("winter" %in% names(y)) {
    y
  } else {
    y <- y |> mutate(winter = 0)
  }

  if("summer" %in% names(y)) {
    y
  } else {
    y <- y |> mutate(summer = 0)
  }

  z <- y |>
    mutate(total_days = summer + winter) |>
    select(location, total_days, total_summer_days = summer, total_winter_days = winter)

  return(z)

}

tbd <- summarise_number_of_op_days(x = paired, date_start = beginning, date_end = ending)

#-----------------------------------------------------------------------------------------------------------------------

# Now figure out density in just that unique date range.

# Previously processed data:

# 1. Probabilistic gaps
df_leave_prob_pred <- read_csv(paste0(g_drive, "data/processed/probabilistic-gaps/gap-leave-prob_predictions_2021-10-05.csv"))
# 2. Time between photos
df_tbp <- read_csv(paste0(g_drive, "data/processed/time-btwn-images/abmi-cmu_all-years_tbp_2021-06-25.csv"))
# 3. Gap classes
df_gap <- read_csv(paste0(g_drive, "data/processed/probabilistic-gaps/abmi-cmu_all-years_gap-class-raw_2021-10-07.csv"))

# Lookup:

# Native species
load(paste0(g_drive, "data/lookup/wt_cam_sp_str.RData"))
# Gap groups
df_gap_groups <- read_csv(paste0(g_drive, "data/lookup/species-gap-groups.csv"))

# Parameters:

# Seasonal start/end dates (julian day):
summer.start.j <- 106 # April 16
summer.end.j <- 288 # October 15

# Load (cleaned) tag data
df_all <- read_csv(paste0(g_drive, "data/base/clean/cmu_all-years_all-data_clean_2022-08-22.csv")) |>
  filter(str_detect(location, "CHR|ADE"))

df_series <- df_all |>
  # Only consider images within the field of view and of native species
  filter(field_of_view == "WITHIN",
         common_name %in% native_sp) |>
  # Join gap class
  left_join(df_gap, by = c("location", "project", "date_detected", "common_name")) |>
  # Remove project
  select(-project) |>
  # Order observations
  arrange(location, date_detected, common_name) |>
  # Identify series and gaps requiring probabilistic time assignment
  mutate(series_num = 0,
         # Lagged time stamp
         date_detected_previous = lag(date_detected),
         # Lead time stamp
         date_detected_next = lead(date_detected),
         # Calculate difference in time between ordered images
         diff_time_previous = as.numeric(date_detected - date_detected_previous),
         diff_time_next = as.numeric(abs(date_detected - date_detected_next)),
         # Lagged species
         common_name_previous = lag(common_name),
         # Was is a different species?
         diff_sp = ifelse(common_name != common_name_previous, TRUE, FALSE),
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
         series_num = c(0, cumsum(diff_series[-1])),
         # Flag gaps that require probabilistic time assignment
         gap_prob = replace_na(ifelse(gap_check == 1 & (gap_class_previous == "" | gap_class_previous == "U"), 1, 0), 0)) |>
  group_by(series_num) |>
  mutate(diff_time_previous = ifelse(row_number() == 1, 0, diff_time_previous),
         diff_time_next = ifelse(row_number() == n(), 0, diff_time_next)) |>
  ungroup() |>
  # Join gap group lookup table
  left_join(df_gap_groups, by = "common_name") |>
  # Join gap leaving predictions
  left_join(df_leave_prob_pred, by = c("gap_group", "diff_time_previous" = "diff_time")) |>
  # Adjust time difference between ordered images that require probabilistic time assignment
  mutate(pred = replace_na(pred, 1),
         diff_time_previous_adj = ifelse(gap_prob == 1, diff_time_previous * (1 - pred), diff_time_previous),
         diff_time_next_adj = ifelse(lead(gap_prob == 1), diff_time_next * (1 - lead(pred)), diff_time_next))

# Calculate total time in front of the camera, by series (tts = Total Time by Series)

df_tts <- df_series |>
  left_join(df_tbp, by = "common_name") |>
  group_by(series_num) |>
  mutate(# Check whether the image was first or last in a series
    bookend = ifelse(row_number() == 1 | row_number() == n(), 1, 0),
    # Calculate time for each individual image
    image_time = ifelse(bookend == 1,
                        ((diff_time_previous_adj + diff_time_next_adj) / 2) + (tbp / 2),
                        (diff_time_previous_adj + diff_time_next_adj) / 2),
    # Multiply image time by the number of animals present
    image_time_ni = image_time * number_individuals) |>
  # Group by common name as well to add it as a variable to output
  group_by(common_name, .add = TRUE) |>
  # Calculate total time and number of images for each series
  summarise(n_images = n(),
            series_total_time = sum(image_time_ni)) |>
  ungroup()

# Calculate total time in front of camera, by deployment and species (tt = total time)

# Note: We're now doing this according to a customized date range; i.e., the days in which both random and trail cameras
# were operating.

# Days operating
days <- paired |>
  select(date, location) |>
  distinct() |>
  mutate(monitoring_period = "common")

df_tt <- df_series |>
  group_by(series_num) |>
  arrange(date_detected, .by_group = TRUE) |>
  filter(row_number() == 1) |>
  left_join(df_tts, by = c("series_num", "common_name")) |>
  select(location, date_detected, common_name, series_num, series_total_time) |>
  ungroup() |>
  mutate(julian = as.numeric(format(date_detected, "%j")),
         season = ifelse(julian >= summer.start.j & julian <= summer.end.j, "summer", "winter"),
         date = as.Date(date_detected)) |>
  # Join monitoring periods
  left_join(days, by = c("date", "location")) |>
  # Remove any series that aren't part of a monitoring period
  filter(!is.na(monitoring_period)) |>
  mutate_at(c("monitoring_period", "location", "common_name", "season"), factor) |>
  group_by(monitoring_period, location, common_name, season, .drop = FALSE) |>
  # Sum up the total time in front of the camera
  summarise(total_duration = sum(series_total_time)) |>
  ungroup() |>
  mutate_if(is.factor, as.character) |>
  # New time-by-day summary that has been generated in the previous script
  left_join(tbd, by = c("location"))

# For deployments with no images of native animals (nn = no natives):

# Unique species seen
sp <- as.character(sort(unique(df_tt$common_name)))

# Monitoring periods
mp <- "common"

df_tt_nn <- tbd |>
  # Retrieve only those that had no images of native species
  anti_join(df_tt, by = "location") |>
  crossing(season = c("summer", "winter"), common_name = sp, monitoring_period = mp) |>
  # Add total_duration column, which is zero in these cases
  mutate(total_duration = 0) |>
  arrange(monitoring_period, location, common_name, season)

# Bind together
df_tt_full <- df_tt |>
  bind_rows(df_tt_nn) |>
  arrange(monitoring_period, location, common_name, season) |>
  mutate(total_season_days = ifelse(season == "summer", total_summer_days, total_winter_days)) |>
  select(monitoring_period:season, total_season_days, total_duration)

# VegHF categories lookup:
df_veghf <- read_csv(paste0(g_drive, "data/lookup/veghf/abmi-cmu_all-years_veghf-soilhf-detdistveg_2021-10-06.csv")) |>
  filter(str_detect(project, "^CMU")) |>
  select(location, VegForDetectionDistance) |>
  distinct()

# Effective detection distance (EDD) predictions lookup
df_edd <- read_csv(paste0(g_drive, "data/processed/detection-distance/predictions/edd_veghf_season.csv"))

# EDD species groups
df_dist_groups <- read_csv(paste0(g_drive, "data/lookup/species-distance-groups.csv"))

# Set parameters:

# Camera field of view angle
cam_fov_angle <- 40

# Update VegHF

# Create database connection to CMU VegHF Checks (SQLite db)
cvc <- DBI::dbConnect(
  drv = RSQLite::SQLite(),
  paste0(g_drive, "database/cmu-veghf-checks.db")
)

# Tables
tables <- DBI::dbListTables(conn = cvc) |> str_subset("updates")

# Pull updated data (from visual classification)
updated_veghf <- purrr::map_df(.x = tables, .f = ~ DBI::dbReadTable(conn = cvc, name = .x)) |>
  filter(!VegForDetectionDistance_updated == "")

# Update VegHF
df_veghf_updated <- df_veghf |>
  left_join(updated_veghf, by = "location") |>
  mutate(VegForDetectionDistance = ifelse(!is.na(VegForDetectionDistance_updated),
                                          VegForDetectionDistance_updated,
                                          VegForDetectionDistance)) |>
  select(-3)

# Append all the ingredients needed for density estimation, then estimate density at each individual deployment
# (by species and season)

df_dep_density <- df_tt_full |>
  # Join species EDD groups
  left_join(df_dist_groups, by = "common_name") |>
  # Join detection distance vegetation values
  left_join(df_veghf_updated, by = "location") |>
  # Join EDD predictions
  left_join(df_edd, by = c("dist_group", "season", "VegForDetectionDistance")) |>
  # Remove random species (mostly birds) <- something to check on though.
  filter(!is.na(detdist)) |>
  # Calculate density
  mutate(effort = total_season_days * (detdist ^ 2 * pi * (cam_fov_angle / 360)) / 100,
         # Catch per unit effort
         cpue = total_duration / effort,
         # Catch per unit effort in km2
         cpue_km2 = cpue / 60 / 60 / 24 * 10000) |>
  # All the NaNs are just combinations where the total_seasons_days is 0.
  select(monitoring_period, location, common_name, season, total_season_days, total_duration, density_km2 = cpue_km2)

# Wide version
d <- df_dep_density |>
  select(monitoring_period, location, season, total_season_days) |>
  distinct() |>
  pivot_wider(id_cols = c(location, monitoring_period), names_from = season, values_from = total_season_days)

d2 <- df_dep_density |>
  pivot_wider(id_cols = c(location, monitoring_period), names_from = c(common_name, season), values_from = density_km2) |>
  left_join(d, by = c("location", "monitoring_period")) |>
  select(monitoring_period, location, summer, winter, everything())

write_csv(d2, paste0(g_drive, "results/density/deployments/trail/cmu_trail-random_density_wide_2022-12-09.csv"))

