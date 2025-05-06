#-----------------------------------------------------------------------------------------------------------------------

# Project:          Game Trail Versus Random

# Title:            Summarise time in front of camera
# Description:      The purpose of this script is to summarise the data from the game trail and random paired camera

# Author:           Marcus Becker

# Previous scripts: None

#-----------------------------------------------------------------------------------------------------------------------

library(tidyverse)
library(wildRtrax)
library(keyring)

# Google Drive
g_drive <- "G:/Shared drives/ABMI Camera Mammals/"

Sys.setenv(WT_USERNAME = key_get("WT_USERNAME", keyring = "wildtrax"),
           WT_PASSWORD = key_get("WT_PASSWORD", keyring = "wildtrax"))

# Authenticate into WildTrax
wt_auth()

# Native species tags in WildTrax
load(paste0(g_drive, "data/lookup/wt_cam_sp_str.RData"))

# Probabilistic gaps (i.e., probability of leaving predictions)
df_leave_prob_pred <- read_csv(paste0(g_drive, "data/processed/probabilistic-gaps/gap-leave-prob_predictions_2021-10-05.csv"))
# Gap groups
df_gap_groups <- read_csv(paste0(g_drive, "data/lookup/species-gap-groups.csv"))
# Average time between images per species
df_tbp <- read_csv(paste0(g_drive, "data/processed/time-btwn-images/abmi-cmu_all-years_tbp_2021-06-25.csv"))

# Seasonal start/end dates (julian day):
summer.start.j <- 106 # April 16
summer.end.j <- 288 # October 15

#-----------------------------------------------------------------------------------------------------------------------

# Let's load data from the Feral Horse Project (S. Meyhoff)

# Metadata
meta <- read_csv(paste0(g_drive, "Projects/Game Trail Versus Random/fhp_location_metadata.csv")) |>
  # Interested in paired
  filter(paired == "yes") |>
  select(location, trail_random, cam_start, cam_end, days_active) |>
  arrange(location)

locs <- meta$location

# Let's figure out what dates we want to remove
meta_new <- meta |>
  # I think we want to just deal with the 'A's
  filter(!str_detect(location, "-B$|-C$")) |>
  mutate(location = str_remove(location, "-GT$|-R$|-GT|-R")) |>
  pivot_wider(id_cols = location, names_from = trail_random, values_from = days_active) |>
  mutate(difference = `game trail` - random)

# Locations to remove (just 2)
remove <- c("G2810R22", "S3110O22")

# Locations to remove certain time periods
adjust <- meta_new |>
  filter(abs(difference) < 250,
         abs(difference) > 5) |>
  pull(location)

meta_2 <- meta |>
  mutate(location2 = str_remove(location, "-GT$|-R$|-GT|-R")) |>
  #filter(location2 %in% adjust) |>
  group_by(location2) |>
  mutate(cam_start_max = max(cam_start),
            cam_end_min = min(cam_end),
            days_active = min(days_active)) |>
  ungroup() |>
  select(location, trail_random, cam_start_max, cam_end_min)

# Image data from WildTrax
proj <- 1954

data_raw <- wt_download_report(project_id = proj,
                           sensor_id = "CAM",
                           reports = "main",
                           weather_cols = FALSE)

data <- data_raw |>
  filter(location %in% locs) |>
  # Remove a couple that don't have adequate paired data
  mutate(location2 = str_remove(location, "-GT$|-R$|-GT|-R")) |>
  filter(!location2 %in% remove) |>
  select(-location2) |>
  # Now let's remove the Bs and Cs (for now)
  filter(!str_detect(location, "-B$|-C$")) |>
  # Now let's chop off periods of some locations
  left_join(meta_2) |>
  # Just use within range images for now ... something to flag, though.
  filter(image_fov == "WITHIN") |>
  mutate(image_date_time = ymd_hms(image_date_time) +
           case_when(
             location == "G297H22-GT-A" ~ days(12),
             location == "G298R22-GT-A" ~ days(249),
             location == "G309F22-GT"   ~ days(1),
             TRUE                       ~ seconds(0)  # Default case, add 0 seconds
           )) |>
  mutate(image_date_time = ymd_hms(image_date_time) +
           case_when(
             location == "G278L22"        ~ minutes(60),
             location == "G299F22-R-2-A"    ~ minutes(64),
             location == "G288R22-GT-B"     ~ minutes(9),
             location == "G309O22-GT-B"     ~ minutes(64),
             location == "G277L22-C"        ~ minutes(5),
             location == "G277H22-R-2-C"      ~ minutes(59),
             location == "G298H22-GT-C"     ~ minutes(61),
             location == "G306O22-R"      ~ minutes(-5),
             location == "G298R22-GT-A"   ~ minutes(44),
             location == "G308O22-GT-B"   ~ minutes(61),
             location == "G3010R22-GT-C"  ~ minutes(60),
             location == "G3010F22-GT-B"  ~ minutes(4),
             location == "G3010L22-A"     ~ minutes(60),
             location == "G299F22-R-2-B"  ~ minutes(61),
             location == "G288H22-GT-C"   ~ minutes(61),
             location == "G298H22-GT-B"   ~ minutes(60),
             location == "G277L22-B"      ~ minutes(64),
             location == "G298O22-GT-B"   ~ minutes(62),
             TRUE                         ~ minutes(0)  # Default case
           )) |>
  select(location, image_date_time, species_common_name, individual_count, cam_start_max, cam_end_min) |>
  filter(image_date_time > cam_start_max,
         image_date_time < cam_end_min)

# Assess where NONE images split detections
df_gap_nones <- data |>
  select(location, image_date_time, species_common_name) |>
  arrange(location, image_date_time) |>
  # Create gap class column
  mutate(species_common_name_next = lead(species_common_name),
         gap_class = ifelse(species_common_name != "NONE" & species_common_name_next == "NONE", "N", NA)) |>
  filter(gap_class == "N") |>
  select(-c(species_common_name_next))

# Species of interest
sp <- c("White-tailed Deer", "Mule Deer", "Horse", "Black Bear", "Grizzly Bear",
        "Cougar", "Elk (wapiti)", "Canada Lynx", "Gray Wolf", "Red Fox", "Coyote",
        "Moose", "Marten")

# Identify independent detections ('series')
df_series <- data |>
  filter(species_common_name %in% sp,
         !individual_count == "VNA") |>
  # Identify where NONEs occurred
  left_join(df_gap_nones, by = c("location", "image_date_time", "species_common_name")) |>
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
  # Give Horse 'Moose' gap group
  mutate(gap_group = ifelse(is.na(gap_group), "Moose", gap_group)) |>
  # Join gap leaving predictions
  left_join(df_leave_prob_pred, by = c("gap_group", "diff_time_previous" = "diff_time")) |>
  # Adjust time difference between ordered images that require probabilistic time assignment
  mutate(pred = replace_na(pred, 1),
         diff_time_previous_adj = ifelse(gap_prob == 1, diff_time_previous * (1 - pred), diff_time_previous),
         diff_time_next_adj = ifelse(lead(gap_prob == 1), diff_time_next * (1 - lead(pred)), diff_time_next))

write_csv(df_series, paste0(g_drive, "data/processed/series-summary/gametrailrandom-series.csv"))

# Vector of all locations (only 42)
dep <- df_series |>
  select(location) |>
  distinct() |>
  pull()

# Total time for each detection/series
df_tts <- df_series |>
  left_join(df_tbp, by = "species_common_name") |>
  # Give Horse a tbp value
  mutate(tbp = ifelse(is.na(tbp), 4.5, tbp)) |>
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
  mutate(series_total_time = if_else(is.na(series_total_time), 2.5, series_total_time)) |>
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

locs <- df_tt |>
  select(location) |>
  distinct() |>
  mutate(location = str_remove(location, "-A$"))

#-----------------------------------------------------------------------------------------------------------------------

# Let's work on the OSM ABMI/JF pairs

# Note: Did I not remove the times when one camera wasn't operating properly?
# It appears that I did. Very subtle. Test this out on 1-0-MAT26A.

proj <- wt_get_download_summary(sensor_id = "CAM") |>
  filter(str_detect(project, "ACME")) |>
  select(project_id) |>
  pull()

data_raw <- map_df(.x = proj,
                   .f = ~ wt_download_report(
                         project_id = .x,
                         sensor_id = "CAM",
                         report = "main",
                         weather_cols = FALSE))

locations <- data_raw |>
  select(project_id, location, latitude, longitude) |>
  distinct()

# Don't re-run this lol.
#write_csv(locations, paste0(g_drive, "Projects/Game Trail Versus Random/ACME EDD.csv"))

data_image_raw <- map_df(.x = proj,
                   .f = ~ wt_download_report(
                     project_id = .x,
                     sensor_id = "CAM",
                     report = "image_report",
                     weather_cols = FALSE))

data_image <- data_image_raw |>
  #filter(image_trigger_mode == "Time Lapse") |>
  group_by(project_id, location) |>
  summarise(last_image = max(image_date_time)) |>
  ungroup() |>
  # Adjust the names of a few locations to match each other better
  mutate(location = case_when(
    location == "2-1B1-CA5" ~ "2-1B1-MA21A",
    location == "3-2B1-CA5" ~ "3-2B1-MA38A",
    location == "3-2F2-CA4" ~ "3-2F2-MA4A",
    TRUE ~ location)) |>
  # Remove a few single locations without a partner
  filter(!str_detect(location, "3-0-MA35B|3-0-MA9A|13-0-MAAA14|13-0-MAAA10|13-0-MAZ15|13-0-MAAA16")) |>
  separate(location, into = c("site", "station"), sep = "(?=.$)") |>
  group_by(site) |>
  summarise(last_image_min = min(last_image)) |>
  ungroup() |>
  # Add a day
  mutate(last_image_min = last_image_min %m+% days(1))

data <- data_raw |>
  # Adjust the names of a few locations to match each other better
  mutate(location = case_when(
    location == "2-1B1-CA5" ~ "2-1B1-MA21A",
    location == "3-2B1-CA5" ~ "3-2B1-MA38A",
    location == "3-2F2-CA4" ~ "3-2F2-MA4A",
    TRUE ~ location)) |>
  # Remove a few single locations without a partner
  filter(!str_detect(location, "3-0-MA35B|3-0-MA9A|13-0-MAAA14|13-0-MAAA10|13-0-MAZ15|13-0-MAAA16")) |>
  select(location, image_date_time, species_common_name, individual_count) |>
  separate(location, into = c("site", "station"), sep = "(?=.$)", remove = FALSE) |>
  left_join(data_image, by = "site") |>
  filter(image_date_time < last_image_min) |>
  select(-c(last_image_min, site, station)) |>
  mutate(species_common_name = ifelse(species_common_name == "Deer", "White-tailed Deer", species_common_name))

# Note: Suspicious pairs

# 1-0-MAAH34A -> No staff setup or timelapse; potentially funky.
# 1-0-MAAK34B -> No staff setup or timelapse; potentially funky.
# 1-0-MAAM33A -> Camera did not operate properly.
# 1-0-MAAU28B -> Camera did not operate properly.
# 1-0-MAR28B -> No staff setup or timelapse; potentially funky.
# 13-0-MAAA10A -> Camera did not operate properly.
# 13-0-MAAA14A/B -> Neither camera operated properly.
# 13-0-MAAA15B -> Short operation.
# 13-0-MAAA16B -> Neither camera operated properly.
# 13-0-MAZ15A -> Camera did not operate properly.
# 13-0-MAZ16A -> Camera did not work properly.
# 13-0-MAD14A -> Camera did not work properly
# 13-0-MAP17A -> Camera did not work properly
# 13-0-MAR17A -> Camera did not work properly
# 1-0-MAAS28

unique(data$location)

# Remove those problematic locations above

data <- data |>
  filter(!str_detect(location, "1-0-MAAH34|1-0-MAAK34|1-0-MAAM33|1-0-MAAU28|1-0-MAR28|1-0-MAAS28")) |>
  filter(!str_detect(location, "13-0-MAAA10|13-0-MAAA14|13-0-MAAA15|13-0-MAAA16|13-0-MAZ15|13-0-MAZ16|13-0-MAD14|13-0-MAP17|13-0-MAR17"))

# Assess where NONE images split detections
df_gap_nones <- data |>
  select(location, image_date_time, species_common_name) |>
  arrange(location, image_date_time) |>
  # Create gap class column
  mutate(species_common_name_next = lead(species_common_name),
         gap_class = ifelse(species_common_name != "NONE" & species_common_name_next == "NONE", "N", NA)) |>
  filter(gap_class == "N") |>
  select(-c(species_common_name_next))

check <- data |>
  group_by(species_common_name) |>
  tally() |>
  arrange(desc(n))

unique(data$species_common_name)

# Species of interest
sp <- c("White-tailed Deer", "Moose", "Black Bear", "Snowshoe Hare", "Woodland Caribou",
        "Coyote", "Canada Lynx")

# Note: Leaving off Red Fox, Gray Wolf, Fisher and Marten - virtually no images.

# Identify independent detections ('series')
df_series <- data |>
  filter(species_common_name %in% sp,
         !individual_count == "VNA") |>
  # Identify where NONEs occurred
  left_join(df_gap_nones, by = c("location", "image_date_time", "species_common_name")) |>
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

write_csv(df_series, paste0(g_drive, "data/processed/series-summary/osm-gametrailrandom-series.csv"))

# Vector of all locations (85)
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
  mutate(series_total_time = if_else(is.na(series_total_time), 2.5, series_total_time)) |>
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

#-------------------------------

# Let's get TBD for each deployment

tbd <- data |>
  group_by(location) |>
  summarise(first = min(image_date_time),
            last = max(image_date_time)) |>
  ungroup() |>
  separate(location, into = c("site", "station"), sep = "(?=.$)", remove = FALSE) |>
  group_by(site) |>
  mutate(min = min(first),
         max = max(last)) |>
  ungroup() |>
  select(location, min, max)

# Source functions for TIFC workflow
source("./src/Functions/estimate-density-tifc.R")

summer_green <- 143

snow_start <- 289
snow_end <- 106
snow_end_2 <- 96

df_tbd <- tbd |>
  mutate(min = as.Date(min),
         max = as.Date(max)) |>
  group_by(location) |>
  mutate(date = list(seq(from = min, to = max, by = "day"))) |>
  unnest(date) |>
  ungroup() |>
  select(location, date) |>
  mutate(operating = 1) |>
  mutate(julian = yday(date)) |>
  mutate(season_new1 = ifelse((julian > snow_start) | (julian < snow_end), "snow", "nonsnow")) |>
  mutate(season_new2 = ifelse((julian > snow_start) | (julian < snow_end_2), "snow", "nonsnow")) |>
  mutate(season_new3 = case_when(
    julian >= summer_green & julian <= snow_start ~ "summer",
    julian > snow_start | julian < summer_green ~ "winter",
    # Note: There's actually no spring days in these data
    julian < summer_green & julian > snow_end ~ "spring"
  ))

# New season 1

df_tbd_new1 <- df_tbd |>
  select(location, date, operating, season_new1) |>
  mutate_at(c("location", "season_new1"), factor) |>
  group_by(location, season_new1, .drop = FALSE) |>
  summarise(operating_days = sum(operating)) |>
  ungroup() |>
  mutate_if(is.factor, as.character) |>
  group_by(location) |>
  mutate(total_days = sum(operating_days)) |>
  pivot_wider(id_cols = c(location, total_days), names_from = season_new1, values_from = operating_days) |>
  ungroup()

df_tbd_new2 <- df_tbd |>
  select(location, date, operating, season_new2) |>
  mutate_at(c("location", "season_new2"), factor) |>
  group_by(location, season_new2, .drop = FALSE) |>
  summarise(operating_days = sum(operating)) |>
  ungroup() |>
  mutate_if(is.factor, as.character) |>
  group_by(location) |>
  mutate(total_days = sum(operating_days)) |>
  pivot_wider(id_cols = c(location, total_days), names_from = season_new2, values_from = operating_days) |>
  ungroup()

df_tbd_new3 <- df_tbd |>
  select(location, date, operating, season_new3) |>
  mutate_at(c("location", "season_new3"), factor) |>
  group_by(location, season_new3, .drop = FALSE) |>
  summarise(operating_days = sum(operating)) |>
  ungroup() |>
  mutate_if(is.factor, as.character) |>
  group_by(location) |>
  mutate(total_days = sum(operating_days)) |>
  pivot_wider(id_cols = c(location, total_days), names_from = season_new3, values_from = operating_days) |>
  ungroup()

df_tbd_ns <- df_tbd |>
  select(location, date, operating) |>
  mutate_at(c("location"), factor) |>
  group_by(location, .drop = FALSE) |>
  summarise(total_days = sum(operating)) |>
  ungroup() |>
  mutate_if(is.factor, as.character)

df_tt <- df_series |>
  group_by(series_num) |>
  arrange(image_date_time, .by_group = TRUE) |>
  filter(row_number() == 1) |>
  left_join(df_tts, by = c("series_num", "species_common_name")) |>
  select(location, image_date_time, species_common_name, series_num, series_total_time) |>
  ungroup() |>
  mutate(julian = yday(image_date_time)) |>
  mutate(season_new1 = ifelse((julian > snow_start) | (julian < snow_end), "snow", "nonsnow")) |>
  mutate(season_new2 = ifelse((julian > snow_start) | (julian < snow_end_2), "snow", "nonsnow")) |>
  mutate(season_new3 = case_when(
    julian >= summer_green & julian <= snow_start ~ "summer",
    julian > snow_start | julian < summer_green ~ "winter",
    # Note: There's actually no spring days in these data
    julian < summer_green & julian > snow_end ~ "spring"
  )) |>
  select(-c(julian))

# New Season 1
df_tt_new1 <- df_tt |>
  mutate_at(c("location", "species_common_name", "season_new1"), factor) |>
  group_by(location, species_common_name, season_new1, .drop = FALSE) |>
  summarise(total_duration = sum(series_total_time)) |>
  ungroup() |>
  mutate_if(is.factor, as.character) |>
  left_join(df_tbd_new1, by = c("location"))

df_tt_nn_new1 <- df_tbd_new1 |>
  # Retrieve only those that had no images of animals
  anti_join(df_tt_new1, by = "location") |>
  crossing(season_new1 = c("snow", "nonsnow"), species_common_name = sp) |>
  # Add total_duration column, which is zero in these cases
  mutate(total_duration = 0)

df_tt_full_new1 <- df_tt_new1 |>
  bind_rows(df_tt_nn_new1) |>
  arrange(location, species_common_name, season_new1) |>
  mutate(total_season1_days = ifelse(season_new1 == "snow", snow, nonsnow)) |>
  select(location, species_common_name, season_new1, total_season1_days, total_duration)

# New Season 2
df_tt_new2 <- df_tt |>
  mutate_at(c("location", "species_common_name", "season_new2"), factor) |>
  group_by(location, species_common_name, season_new2, .drop = FALSE) |>
  summarise(total_duration = sum(series_total_time)) |>
  ungroup() |>
  mutate_if(is.factor, as.character) |>
  left_join(df_tbd_new2, by = c("location"))

df_tt_nn_new2 <- df_tbd_new2 |>
  # Retrieve only those that had no images of animals
  anti_join(df_tt_new2, by = "location") |>
  crossing(season_new2 = c("snow", "nonsnow"), species_common_name = sp) |>
  # Add total_duration column, which is zero in these cases
  mutate(total_duration = 0)

df_tt_full_new2 <- df_tt_new2 |>
  bind_rows(df_tt_nn_new2) |>
  arrange(location, species_common_name, season_new2) |>
  mutate(total_season2_days = ifelse(season_new2 == "snow", snow, nonsnow)) |>
  select(location, species_common_name, season_new2, total_season2_days, total_duration)

# New Season 3
df_tt_new3 <- df_tt |>
  mutate_at(c("location", "species_common_name", "season_new3"), factor) |>
  group_by(location, species_common_name, season_new3, .drop = FALSE) |>
  summarise(total_duration = sum(series_total_time)) |>
  ungroup() |>
  mutate_if(is.factor, as.character) |>
  left_join(df_tbd_new3, by = c("location"))

df_tt_nn_new3 <- df_tbd_new3 |>
  # Retrieve only those that had no images of animals
  anti_join(df_tt_new3, by = "location") |>
  crossing(season_new3 = c("summer", "winter"), species_common_name = sp) |>
  # Add total_duration column, which is zero in these cases
  mutate(total_duration = 0)

df_tt_full_new3 <- df_tt_new3 |>
  bind_rows(df_tt_nn_new3) |>
  arrange(location, species_common_name, season_new3) |>
  mutate(total_season3_days = ifelse(season_new3 == "winter", winter, summer)) |>
  select(location, species_common_name, season_new3, total_season3_days, total_duration)

# For species where there are no seasonal effects on EDD
df_tt_ns <- df_tt |>
  mutate_at(c("location", "species_common_name"), factor) |>
  group_by(location, species_common_name, .drop = FALSE) |>
  summarise(total_duration = sum(series_total_time)) |>
  ungroup() |>
  mutate_if(is.factor, as.character) |>
  left_join(df_tbd_ns, by = c("location"))

df_tt_nn_ns <- df_tbd_ns |>
  # Retrieve only those that had no images of animals
  anti_join(df_tt_ns, by = "location") |>
  crossing(species_common_name = sp) |>
  # Add total_duration column, which is zero in these cases
  mutate(total_duration = 0)

df_tt_full_ns <- df_tt_ns |>
  bind_rows(df_tt_nn_ns) |>
  arrange(location, species_common_name) |>
  select(location, species_common_name, total_days, total_duration)

# Calculate density at each location

df_vegdetdist <- read_csv(paste0(g_drive, "Projects/Game Trail Versus Random/ACME EDD Updated.csv")) |>
  select(location, primary_category, secondary_category) |>
  filter(!is.na(primary_category))

cam_fov_angle <- 40

# New Season 1
# Only Snowshoe Hare
hare <- read_csv(paste0(g_drive, "data/processed/detection-distance/predictions/New/Hare.csv"))

df_dens_seas1 <- df_tt_full_new1 |>
  filter(species_common_name == "Snowshoe Hare") |>
  # Join new categories
  left_join(df_vegdetdist) |>
  mutate(secondary_category = paste0(primary_category, "_", secondary_category)) |>
  left_join(hare, by = c("secondary_category", "season_new1")) |>
  select(-c(primary_category, dist_group)) |>
  # Calculate density
  mutate(effort = total_season1_days * (prediction ^ 2 * pi * (cam_fov_angle / 360)) / 100,
         # Catch per unit effort
         cpue = total_duration / effort,
         # Catch per unit effort in km2
         cpue_km2 = cpue / 60 / 60 / 24 * 10000) |>
  # All the NaNs are just combinations where the total_seasons_days is 0.
  select(location, species_common_name, season_new1, total_season1_days, total_duration, density_km2 = cpue_km2) |>
  group_by(location, species_common_name) |>
  summarise(density_km2 = weighted.mean(density_km2, w = total_season1_days),
            total_days = sum(total_season1_days))

# New Season 2
# Only Lynx
lynx <- read_csv(paste0(g_drive, "data/processed/detection-distance/predictions/New/Lynx.csv"))

df_dens_seas2 <- df_tt_full_new2 |>
  filter(species_common_name == "Canada Lynx") |>
  # Join new categories
  left_join(df_vegdetdist) |>
  mutate(secondary_category = paste0(primary_category, "_", secondary_category)) |>
  left_join(lynx, by = c("secondary_category", "season_new2")) |>
  select(-c(primary_category, dist_group)) |>
  # Calculate density
  mutate(effort = total_season2_days * (prediction ^ 2 * pi * (cam_fov_angle / 360)) / 100,
         # Catch per unit effort
         cpue = total_duration / effort,
         # Catch per unit effort in km2
         cpue_km2 = cpue / 60 / 60 / 24 * 10000) |>
  # All the NaNs are just combinations where the total_seasons_days is 0.
  select(location, species_common_name, season_new2, total_season2_days, total_duration, density_km2 = cpue_km2) |>
  group_by(location, species_common_name) |>
  summarise(density_km2 = weighted.mean(density_km2, w = total_season2_days),
            total_days = sum(total_season2_days))

# New Season 3
coyote <- read_csv(paste0(g_drive, "data/processed/detection-distance/predictions/New/Coyote.csv"))
ungulates <- read_csv(paste0(g_drive, "data/processed/detection-distance/predictions/New/LargeUngulates.csv"))

# Create value for Coyote F-C-C Winter
coyote_cc_winter <- coyote |>
  filter(secondary_category == "Forested_Closed-Closed") |>
  group_by(secondary_category, dist_group) |>
  summarise(prediction = mean(prediction)) |>
  mutate(season_new3 = "winter")

# Create value for Large Ungulates F-C-C and O-O Winter
lu_cc_oo_winter <- ungulates |>
  filter(secondary_category == "Forested_Closed-Closed" | secondary_category == "Open_Open") |>
  group_by(secondary_category, dist_group) |>
  summarise(prediction = mean(prediction)) |>
  mutate(season_new3 = "winter")

coy_ung <- bind_rows(coyote, ungulates, coyote_cc_winter, lu_cc_oo_winter)

scn_1 <- c("Coyote", "Moose", "Woodland Caribou")

df_dens_seas3 <- df_tt_full_new3 |>
  filter(species_common_name %in% scn_1) |>
  # Join new categories
  left_join(df_vegdetdist) |>
  mutate(secondary_category = paste0(primary_category, "_", secondary_category),
         dist_group = ifelse(str_detect(species_common_name, "Coyote"), "Coyote", "LargeUngulates")) |>
  left_join(coy_ung, by = c("secondary_category", "season_new3", "dist_group")) |>
  select(-c(primary_category, dist_group)) |>
  # Calculate density
  mutate(effort = total_season3_days * (prediction ^ 2 * pi * (cam_fov_angle / 360)) / 100,
         # Catch per unit effort
         cpue = total_duration / effort,
         # Catch per unit effort in km2
         cpue_km2 = cpue / 60 / 60 / 24 * 10000) |>
  # All the NaNs are just combinations where the total_seasons_days is 0.
  select(location, species_common_name, season_new3, total_season3_days, total_duration, density_km2 = cpue_km2) |>
  # Combine with weighted mean
  group_by(location, species_common_name) |>
  summarise(density_km2 = weighted.mean(density_km2, w = total_season3_days),
            total_days = sum(total_season3_days))

# Now species with no seasonal effects
# Bears, Deer, Gray Wolf, Small Mustelids
bears <- read_csv(paste0(g_drive, "data/processed/detection-distance/predictions/New/Bear.csv"))
deer <- read_csv(paste0(g_drive, "data/processed/detection-distance/predictions/New/Deer.csv"))
wolf <- read_csv(paste0(g_drive, "data/processed/detection-distance/predictions/New/Wolf.csv"))
mustelids <- read_csv(paste0(g_drive, "data/processed/detection-distance/predictions/New/SmallMustelids.csv"))

must_pred <- mean(mustelids$prediction)

mustelids_all <- data.frame(
  dist_group = "SmallMustelids",
  secondary_category = c("Open_Open", "Shrubby_Open", "Shrubby_Closed"),
  prediction = must_pred) |>
  bind_rows(mustelids)

bdwm <- bind_rows(bears, deer, wolf, mustelids_all)

scn_2 <- c("Black Bear", "White-tailed Deer", "Gray Wolf", "Fisher", "Marten")

# Snow for Bears (we don't want to count the days during the winter)
df_snow_bears <- df_tbd_new1 |>
  select(location, snow)

df_dens_ns <- df_tt_full_ns |>
  filter(species_common_name %in% scn_2) |>
  # Join new categories
  left_join(df_vegdetdist) |>
  mutate(secondary_category = paste0(primary_category, "_", secondary_category),
         dist_group = case_when(
           species_common_name == "Black Bear" ~ "Bear",
           species_common_name == "Gray Wolf" ~ "Gray Wolf",
           species_common_name == "White-tailed Deer" ~ "Deer",
           str_detect(species_common_name, "Fisher|Marten") ~ "SmallMustelids")) |>
  left_join(bdwm, by = c("secondary_category", "dist_group")) |>
  left_join(df_snow_bears, by = c("location")) |>
  mutate(total_days = ifelse(species_common_name == "Black Bear", total_days - snow, total_days)) |>
  select(-c(primary_category, dist_group, snow)) |>
  # Calculate density
  mutate(effort = total_days * (prediction ^ 2 * pi * (cam_fov_angle / 360)) / 100,
         # Catch per unit effort
         cpue = total_duration / effort,
         # Catch per unit effort in km2
         cpue_km2 = cpue / 60 / 60 / 24 * 10000) |>
  # All the NaNs are just combinations where the total_seasons_days is 0.
  select(location, species_common_name, total_days, density_km2 = cpue_km2)

dens_all <- bind_rows(df_dens_seas1,
                      df_dens_seas2,
                      df_dens_seas3,
                      df_dens_ns) |>
  arrange(location, species_common_name)



