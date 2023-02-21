# Investigating Alice's data in the Yukon

library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(janitor)
library(lubridate)

g_drive <- "G:/Shared drives/ABMI Camera Mammals/data/"

# Source functions for TIFC workflow
source("./src/functions/estimate-density-tifc.R")

# Species character strings
load(paste0(g_drive, "lookup/wt_cam_sp_str.RData"))

# Gap groups
gap_groups <- read_csv(paste0(g_drive, "lookup/species-gap-groups.csv"))
# Leaving probability predictions
leave_prob_pred <- read_csv(paste0(g_drive, "processed/probabilistic-gaps/gap-leave-prob_predictions_2021-10-05.csv"))
# Time between images
tbi <- read_csv(paste0(g_drive, "processed/time-btwn-images/abmi-cmu_all-years_tbp_2021-06-25.csv"))


# Import data
d <- read_csv(paste0(g_drive, "base/other-projects/lynx 2017 Sep-Nov.csv")) |>
  clean_names() |>
  mutate(date_photo = dmy(date_photo),
         date_detected = ymd_hms(paste0(date_photo, time_photo)),
         number_individuals = rowSums(across(10:14)),
         common_name = "Canada Lynx",
         project = "Alice Kenney") |>
  select(project, location = detector, date_detected, common_name, number_individuals)

# Calculate time in front of camera (TIFC)
# Note: this is the standard way.
series <- d |>
  # Order observations
  arrange(project, location, date_detected, common_name) |>
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
         # Identify new series, based on being different deployment, species, greater than 120 seconds, and approp gaps
         diff_series = ifelse(diff_location == TRUE | diff_sp == TRUE | diff_time_previous > 120, 1, 0),
         # Number series
         series_num = c(1, cumsum(diff_series[-1]) + 1),
         # Flag gaps that require probabilistic time assignment
         gap_prob = replace_na(ifelse(gap_check == 1, 1, 0), 0)) |>
  group_by(series_num) |>
  mutate(diff_time_previous = ifelse(row_number() == 1, 0, diff_time_previous),
         diff_time_next = ifelse(row_number() == n(), 0, diff_time_next)) |>
  ungroup() |>
  # Join gap group lookup table
  left_join(gap_groups, by = "common_name") |>
  # Join gap leaving predictions
  left_join(leave_prob_pred, by = c("gap_group", "diff_time_previous" = "diff_time")) |>
  # Adjust time difference between ordered images that require probabilistic time assignment
  mutate(pred = replace_na(pred, 1),
         diff_time_previous_adj = ifelse(gap_prob == 1, diff_time_previous * (1 - pred), diff_time_previous),
         diff_time_next_adj = ifelse(lead(gap_prob == 1), diff_time_next * (1 - lead(pred)), diff_time_next),
         diff_time_next_adj = ifelse(is.na(diff_time_next_adj), 0, diff_time_next_adj)) |>


# Calculate total time in front of the camera, by series
tts <- series %>%
  left_join(tbi, by = "common_name") |>
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
  group_by(project, location, common_name, .add = TRUE) |>
  # Calculate total time and number of images for each series
  summarise(n_images = n(),
            series_total_time = sum(image_time_ni),
            series_start = min(date_detected),
            series_end = max(date_detected)) |>
  ungroup() |>
  select(project, location, series_num, common_name, series_start, series_end, series_total_time, n_images)

tbd <- read_csv(paste0(g_drive, "base/other-projects/lynx 2017 Sep-Nov_sessionDates.csv")) |>
  clean_names() |>
  mutate(start_date = dmy(deployment_date),
         end_date = dmy(retrieval_date)) |>
  select(location = detector, start_date, end_date) |>
  group_by(location) |>
  summarise(start_date = min(start_date),
            end_date = max(end_date)) |>
  filter(!is.na(end_date)) |>
  group_by(location) |>
  mutate(date = list(seq(from = start_date, to = end_date, by = "day"))) |>
  unnest(date) |>
  ungroup() |>
  select(location, date) |>
  mutate(operating = 1) |>
  mutate(month = month(date)) |>
  filter(month == "9" | month == "10" | month == "11") |>
  select(-month) |>
  group_by(location) |>
  summarise(total_days = sum(operating)) |>
  ungroup() |>
  mutate(project_location = paste0("Alice Kenney_", location)) |>
  select(-location)

# Summarise total time
tt <- tts |>
  unite("project_location", project, location, sep = "_", remove = TRUE) |>
  mutate_at(c("project_location", "common_name"), factor) |>
  group_by(project_location, common_name, .drop = FALSE) |>
  summarise(total_duration = sum(series_total_time)) |>
  ungroup() |>
  mutate_if(is.factor, as.character) |>
  left_join(tbd, by = c("project_location")) |>
  mutate(VegForDetectionDistance = "Conif") |>
  filter(!is.na(total_days))

# Effective detection distance (EDD) predictions lookup
edd <- read_csv(paste0(g_drive, "processed/detection-distance/predictions/edd_veghf_season.csv"))
# EDD species groups
dist_groups <- read_csv(paste0(g_drive, "lookup/species-distance-groups.csv"))

cam_fov_angle <- 40

dens_v2 <- tt |>
  mutate(season = "winter") |>
  # Join species EDD groups
  left_join(dist_groups, by = "common_name") |>
  # Join EDD predictions
  left_join(edd, by = c("dist_group", "season", "VegForDetectionDistance")) |>
  # Calculate density
  mutate(effort = total_days * 18.6 / 100,
         # Catch per unit effort
         cpue = total_duration / effort,
         # Catch per unit effort in km2
         cpue_km2 = cpue / 60 / 60 / 24 * 10000) |>
  # All the NaNs are just combinations where the total_seasons_days is 0.
  select(project_location, common_name, total_days, total_duration, density_km2 = cpue_km2)

# Source functions for summarising density
source("./src/functions/summarise-density.R")

# Estimate density
sum <- dens_v2 |>
  mutate(grid = "grid") |>
  # Use custom function (loaded above)
  summarise_density(
    # Group by project
    group_id = grid,
    # Don't aggregate over sampling periods (monitoring_periods)
    agg_samp_per = TRUE,
    # `common_name` is the species column
    species_col = common_name,
    # `density_km2` is the column containing density value
    dens_col = density_km2,
    # Choose confidence level (set to 90%)
    conflevel = 0.9
  )

y <- tt |>
  separate(project_location, into = c("project", "detector"), sep = "_") |>
  mutate(species = "lynx") |>
  select(-c(project, common_name, VegForDetectionDistance)) |>
  select(detector, species, total_duration, total_days)

write_csv(y, paste0(g_drive, "base/other-projects/lookup.csv"))


