#-----------------------------------------------------------------------------------------------------------------------

# Title:       Estimate time in front of camera for paired model analysis
# Description: At each of the X paired sites with two cameras at different heights (PC900 vs HF2s), estimate the total
#              time spent in front of the camera for each mammal species.

# Author:      Marcus A Becker
# Date:        August 2024

#-----------------------------------------------------------------------------------------------------------------------

# Google Drive
g_drive <- "G:/Shared drives/ABMI Camera Mammals/"

# Attach packages
library(tidyverse)

# Native species tags in WildTrax
load(paste0(g_drive, "data/lookup/wt_cam_sp_str.RData"))

source("src/Functions/estimate-density-tifc.R")

# Gap groups
df_gap_groups <- read_csv(paste0(g_drive, "data/lookup/species-gap-groups.csv"))
# Probabilistic gaps
df_leave_prob_pred <- read_csv(paste0(g_drive, "data/processed/probabilistic-gaps/gap-leave-prob_predictions_2021-10-05.csv"))
# 2. Time between photos
df_tbp <- read_csv(paste0(g_drive, "data/processed/time-btwn-images/abmi-cmu_all-years_tbp_2021-06-25.csv"))

# Authenticate into WildTrax to retrieve data
library(keyring)
library(wildrtrax)

# Set environment variables
Sys.setenv(WT_USERNAME = key_get("WT_USERNAME", keyring = "wildtrax"),
           WT_PASSWORD = key_get("WT_PASSWORD", keyring = "wildtrax"))

# Authenticate
wt_auth()

#-----------------------------------------------------------------------------------------------------------------------

# List model comparison projects
comp_projects <- wt_get_download_summary(sensor_id = "CAM") |>
  filter(str_detect(project, "Model Comparison")) |>
  select(project, project_id)

project_ids <- comp_projects$project_id

# Obtain data from these projects
# Notes:

# 2018 is nothing but CUDDEBACK cameras - not interested in.
# 2019 has a few CUDDEBACK but mostly HF2; from the EH 2019 project
# 2020 has both PC900s and HF2 deployments - so only 8 total sites, not 16. Annoying. Ask Brett B to fix.
#   Another thing to note: The Amphibian Monitoring 2020 Project (where these sites come from) have PC900 cameras from May-August 2020,
#   but are labelled as the HF2 sites in the model comparison project (August 2020 - April 2021). To note when combining in the future.
# 2021 has just HF2, from the OSM 2021 project. 19 sites.
# 2022 has just HF2, from the EH 2022 project. 14 sites.

# Download image report to know which image came from which camera model
image_data <- map_df(.x = project_ids,
                  .f = ~ wt_download_report(
                    project_id = .x,
                    sensor_id = "CAM",
                    report = "image_report",
                    weather_cols = FALSE
                  )) |>
  left_join(comp_projects) |>
  select(project, location, image_id, equipment_make, equipment_model)

# Download tag data from comparison projects
tag_data <- map_df(.x = project_ids,
                   .f = ~ wt_download_report(
                     project_id = .x,
                     sensor_id = "CAM",
                     report = "main",
                     weather_cols = FALSE
                   ))

# Hyperfire 2 tags
df_tags_hp2x <- tag_data |>
  left_join(comp_projects) |>
  # Join image id tags
  left_join(image_data) |>
  # Remove Cuddeback and PC900s
  filter(equipment_model == "HF2 PRO COVERT") |>
  select(project, location, image_date_time, image_fov, species_common_name, individual_count,
         age_class, sex_class, equipment_model)

# PC900 tags from Comp 2020
df_tags_pc_amphib <- tag_data |>
  left_join(comp_projects) |>
  # Join image id tags
  left_join(image_data) |>
  filter(equipment_model == "PC900 PROFESSIONAL") |>
  mutate(project = "ABMI Amphibian Monitoring 2020") |>
  # Change location names
  mutate(location = str_replace(location, "-(\\d+)$", "-71-\\1"))

# Comparison locations
comp_loc <- df_tags_hp2x |>
  # Dont worry about 2020 - have all we need already
  filter(!project == "ABMI Camera Model Comparison 2020") |>
  select(location) |>
  distinct() |>
  pull()

# Download projects with remaining PC900 data

# ABMI EH 2019, ABMI OSM 2021, ABMI EH 2022
ids <- c(214, 947, 1097)

projects_pc <- wt_get_download_summary(sensor_id = "CAM") |>
  filter(project_id %in% ids) |>
  select(project, project_id)

tag_data_pc <- map_df(.x = ids,
                   .f = ~ wt_download_report(
                     project_id = .x,
                     sensor_id = "CAM",
                     report = "main"
                   ))

df_tags_pc <- tag_data_pc |>
  left_join(projects_pc) |>
  filter(location %in% comp_loc) |>
  # Join the tags from above
  bind_rows(df_tags_pc_amphib) |>
  mutate(equipment_model = "PC900 PROFESSIONAL") |>
  select(project, location, image_date_time, image_fov, species_common_name, individual_count,
         age_class, sex_class, equipment_model)

# All tags
df_tags <- bind_rows(df_tags_pc, df_tags_hp2x)

#-----------------------------------------------------------------------------------------------------------------------

# Let's just do it year by year again

# Let's start with 2020. Seems cleaner. No issues with time range, out of field of view photos, etc.

df_series_20 <- df_tags |>
  filter(str_detect(project, "2020")) |>
  # Let's append camera model to location
  mutate(location_cam = paste0(location, "_", equipment_model)) |>
  # Amalgamate tags of same species in same image
  mutate(individual_count = as.numeric(ifelse(individual_count == "VNA", 1, individual_count))) |>
  group_by(location_cam, image_date_time, species_common_name) |>
  mutate(individual_count = sum(individual_count)) |>
  distinct(location_cam, image_date_time, species_common_name, individual_count, .keep_all = TRUE) |>
  ungroup() |>
  # Just native species.
  filter(species_common_name %in% native_sp) |>
  # Order observations
  arrange(location_cam, image_date_time, species_common_name) |>
  # Assess series
  mutate(series_num = 0,
         image_date_time_lag = lag(image_date_time),
         diff_time = as.numeric(image_date_time - image_date_time_lag),
         species_common_name_lag = lag(species_common_name),
         diff_sp = ifelse(species_common_name != species_common_name_lag, TRUE, FALSE),
         location_cam_lag = lag(location_cam),
         diff_loc = ifelse(location_cam != location_cam_lag, TRUE, FALSE),
         gap_check = ifelse(diff_loc == FALSE & diff_sp == FALSE & (diff_time <= 120 & diff_time >= 20), 1, 0),
         diff_series = ifelse(diff_loc == TRUE | diff_sp == TRUE | diff_time > 120, 1, 0),
         series_num = c(0, cumsum(diff_series[-1]))) |>
  left_join(df_gap_groups, by = "species_common_name") |>
  left_join(df_leave_prob_pred, by = c("gap_group", "diff_time")) |>
  mutate(pred = replace_na(pred, 1),
         diff_time_adj = round(ifelse(gap_check == 1, diff_time * (1 - pred), diff_time), digits = 2))

# Calculate total time in front of the camera, by series

# Start with series that have >1 image
df_tts_multiple <- df_series_20 |>
  # Remove first image from dataframe (as to not include diff_time in time totals for the series)
  mutate(series_num_previous = lag(series_num)) |>
  filter(series_num_previous == series_num) |>
  group_by(series_num) |>
  summarise(n_images = n() + 1,
            total_time = sum(diff_time_adj))

# Next, single-image series'
df_tts_single <- df_series_20 |>
  group_by(series_num) |>
  summarise(n_images = n()) |>
  # Keep only series with 1 image
  filter(n_images == 1) |>
  mutate(total_time = 0)

# Bind together
df_tts_all <- df_tts_multiple |>
  bind_rows(df_tts_single) |>
  arrange(series_num)

# Add time between photos, accounting for the average number of animals in each photo of a series
df_tts_final <- df_series_20 |>
  mutate(individual_count = as.numeric(ifelse(individual_count == "VNA", 1, individual_count))) |>
  # Join in average tbp, by species; note that this is slightly different from results calculated above (incl. non-ABMI)
  left_join(df_tbp, by = "species_common_name") |>
  select(series_num, species_common_name, individual_count, tbp) |>
  group_by(series_num) |>
  # Number of individuals in a series
  mutate(avg_individuals = mean(individual_count)) |>
  ungroup() |>
  select(-individual_count) |>
  distinct() |>
  left_join(df_tts_all, by = "series_num") |>
  mutate(series_total_time = (total_time + tbp) * avg_individuals) |>
  select(series_num, species_common_name, n_images, series_total_time)

# Calculate total time in front of camera, by deployment, year, and species (tt = total time)

# Seasonal start/end dates (julian day):
summer.start.j <- 106 # April 16
summer.end.j <- 288 # October 15

df_tt <- df_series_20 |>
  group_by(series_num) |>
  arrange(image_date_time) |>
  filter(row_number() == 1) |>
  left_join(df_tts_final, by = c("series_num", "species_common_name")) |>
  select(series_num, n_images, location_cam, image_date_time, species_common_name, series_total_time) |>
  ungroup() |>
  mutate(julian = as.numeric(format(image_date_time, "%j")),
         season = ifelse(julian >= summer.start.j & julian <= summer.end.j, "summer", "winter")) |>
  mutate_at(c("location_cam", "species_common_name", "season"), factor) |>
  group_by(location_cam, species_common_name, season, .drop = FALSE) |>
  summarise(total_duration = sum(series_total_time),
            total_images = sum(n_images),
            total_series = n()) |>
  ungroup() |>
  mutate_if(is.factor, as.character)

# Write summary of series
write_csv(df_tt, "G:/Shared drives/ABMI Mammals/Data/Model Comparisons/Summary of Series 2020.csv")

#---------------------------------------------------------------------------------------------------

# Now, let's try to figure out the 2019 stuff. A little messier, a little less pleasant. Sigh.
# Note: I like what Dave did - have the series summaries already done, that he could read in.

# Bring 2019 data together into single dataframe.

df_2019 <- df_tags |>
  filter(str_detect(project, "2019")) |>
  mutate(location_cam = paste0(location, "_", equipment_model)) |>
  select(location_cam, image_date_time, image_fov, species_common_name, age_class, sex_class, individual_count) |>
  # Amalgamate tags of same species in same image
  mutate(individual_count = as.numeric(ifelse(individual_count == "VNA", 1, individual_count))) |>
  group_by(location_cam, image_date_time, species_common_name) |>
  mutate(individual_count = sum(individual_count),
         age_class = paste0(age_class, collapse = ", "),
         sex_class = paste0(sex_class, collapse = ", ")) |>
  distinct(location_cam, image_date_time, species_common_name, individual_count, .keep_all = TRUE) |>
  ungroup()

# Let's investigate the operating time ranges.
df_2019_ranges <- df_2019 |>
  group_by(location_cam) |>
  summarise(start_date_time = min(image_date_time),
            end_date_time = max(image_date_time))

# Notes:
# 739 should be removed from the comparison altogether - HF2 basically never worked.
# 896 PC ended early. Adjust according. Truncate both cameras to July 15, 2019.

# All of the out-of-range images are in 788-NW PC900. Starts on January 27, 2019.
out_of_range <- df_2019 |>
  filter(!image_fov == "WITHIN") |>
  select(location_cam) |>
  distinct()

# Remove non-overlap periods. Kind of a hacky way to do it, but oh well.
df_2019_adj <- df_2019 |>
  # Remove 739.
  filter(!str_detect(location_cam, "^739")) |>
  # End 896 early.
  filter(!(str_detect(location_cam, "^896") & image_date_time > as.Date("2019-07-14 23:59:59"))) |>
  # End 788 early as well.
  filter(!(str_detect(location_cam, "^788") & image_date_time > as.Date("2019-01-26 23:59:59")))

# Now let's calculate series.

df_series_19 <- df_2019_adj |>
  # Just native species.
  filter(species_common_name %in% native_sp) |>
  # Let's append camera model to location
  mutate(image_date_time = ymd_hms(image_date_time)) |>
  # Order observations
  arrange(location_cam, image_date_time, species_common_name) |>
  # Assess series
  mutate(series_num = 0,
         image_date_time_lag = lag(image_date_time),
         diff_time = as.numeric(image_date_time - image_date_time_lag),
         species_common_name_lag = lag(species_common_name),
         diff_sp = ifelse(species_common_name != species_common_name_lag, TRUE, FALSE),
         location_cam_lag = lag(location_cam),
         diff_loc = ifelse(location_cam != location_cam_lag, TRUE, FALSE),
         gap_check = ifelse(diff_loc == FALSE & diff_sp == FALSE & (diff_time <= 120 & diff_time >= 20), 1, 0),
         diff_series = ifelse(diff_loc == TRUE | diff_sp == TRUE | diff_time > 120, 1, 0),
         series_num = c(0, cumsum(diff_series[-1]))) |>
  left_join(df_gap_groups, by = "species_common_name") |>
  left_join(df_leave_prob_pred, by = c("gap_group", "diff_time")) |>
  mutate(pred = replace_na(pred, 1),
         diff_time_adj = round(ifelse(gap_check == 1, diff_time * (1 - pred), diff_time), digits = 2))

# Calculate total time in front of the camera, by series

# Start with series that have >1 image
df_tts_multiple <- df_series_19 |>
  # Remove first image from dataframe (as to not include diff_time in time totals for the series)
  mutate(series_num_previous = lag(series_num)) |>
  filter(series_num_previous == series_num) |>
  group_by(series_num) |>
  summarise(n_images = n() + 1,
            total_time = sum(diff_time_adj))

# Next, single-image series'
df_tts_single <- df_series_19 |>
  group_by(series_num) |>
  summarise(n_images = n()) |>
  # Keep only series with 1 image
  filter(n_images == 1) |>
  mutate(total_time = 0)

# Bind together
df_tts_all <- df_tts_multiple |>
  bind_rows(df_tts_single) |>
  arrange(series_num)

# Add time between photos, accounting for the average number of animals in each photo of a series
df_tts_final <- df_series_19 |>
  mutate(individual_count = as.numeric(ifelse(individual_count == "VNA", 1, individual_count))) |>
  # Join in average tbp, by species; note that this is slightly different from results calculated above (incl. non-ABMI)
  left_join(df_tbp, by = "species_common_name") |>
  select(series_num, species_common_name, individual_count, tbp) |>
  group_by(series_num) |>
  # Number of individuals in a series
  mutate(avg_individuals = mean(individual_count)) |>
  ungroup() |>
  select(-individual_count) |>
  distinct() |>
  left_join(df_tts_all, by = "series_num") |>
  mutate(series_total_time = (total_time + tbp) * avg_individuals) |>
  select(series_num, species_common_name, n_images, series_total_time)

# Calculate total time in front of camera, by deployment, year, and species (tt = total time)

df_tt <- df_series_19 |>
  group_by(series_num) |>
  arrange(image_date_time) |>
  filter(row_number() == 1) |>
  left_join(df_tts_final, by = c("series_num", "species_common_name")) |>
  select(series_num, n_images, location_cam, image_date_time, species_common_name, series_total_time) |>
  ungroup() |>
  mutate(julian = as.numeric(format(image_date_time, "%j")),
         season = ifelse(julian >= summer.start.j & julian <= summer.end.j, "summer", "winter")) |>
  mutate_at(c("location_cam", "species_common_name", "season"), factor) |>
  group_by(location_cam, species_common_name, season, .drop = FALSE) |>
  summarise(total_duration = sum(series_total_time),
            total_images = sum(n_images),
            total_series = n()) |>
  ungroup() |>
  mutate_if(is.factor, as.character)

write_csv(df_tt, "G:/Shared drives/ABMI Mammals/Data/Model Comparisons/Summary of Series 2019.csv")

#-----------------------------------------------------------------------------------------------------------------------

# Let's grab what I can for 2021 too :)

# Let's look at time ranges.
ranges_21 <- df_tags |>
  filter(str_detect(project, "2021")) |>
  mutate(location_cam = paste0(location, "_", equipment_model)) |>
  group_by(location_cam) |>
  summarise(start_date_time = min(image_date_time),
            end_date_time = max(image_date_time))
# Seems good except for 3-1F1-CA2. PC900 ends early. 2021-06-09.

out_of_range <- df_tags |>
  filter(str_detect(project, "2021")) |>
  filter(!image_fov == "WITHIN") |>
  select(location, equipment_model, image_fov, image_date_time) |>
  distinct() |>
  filter(!image_fov == "Out of Range")

# Funky ones:
# 2-2A1-CA1 - both models end early. 2021-05-25.
# 3-2A1-CA1 HP2 - ends 2021-11-15 and starts again 2021-11-22
# 3-2B1-CA3 HP2 - ends 2021-11-16 and starts again 2021-11-19.

# Remove non-overlap periods. Kind of a hacky way to do it, but oh well.
df_2021_adj <- df_tags |>
  filter(str_detect(project, "2021")) |>
  mutate(location_cam = paste0(location, "_", equipment_model)) |>
  # End 3-1F1-CA2 early.
  filter(!(str_detect(location_cam, "^3-1F1-CA2") & image_date_time > as.Date("2021-06-08 23:59:59"))) |>
  # End 2-2A1-CA1 early.
  filter(!(str_detect(location_cam, "^2-2A1-CA1") & image_date_time > as.Date("2021-05-24 23:59:59"))) |>
  # End 3-2A1-CA1 early, and restart it a week later.
  filter(!(str_detect(location_cam, "^3-2A1-CA1") & (image_date_time > as.Date("2021-11-14 23:59:59") & image_date_time < as.Date("2021-11-23 00:00:01")))) |>
  # End 3-2B1-CA3 early, and restart it a few days later.
  filter(!(str_detect(location_cam, "^3-2B1-CA3") & (image_date_time > as.Date("2021-11-15 23:59:59") & image_date_time < as.Date("2021-11-20 00:00:01"))))

# Now let's calculate series.

df_series_21 <- df_2021_adj |>
  # Just native species.
  filter(species_common_name %in% native_sp) |>
  # Let's append camera model to location
  mutate(image_date_time = ymd_hms(image_date_time)) |>
  # Order observations
  arrange(location_cam, image_date_time, species_common_name) |>
  # Assess series
  mutate(series_num = 0,
         image_date_time_lag = lag(image_date_time),
         diff_time = as.numeric(image_date_time - image_date_time_lag),
         species_common_name_lag = lag(species_common_name),
         diff_sp = ifelse(species_common_name != species_common_name_lag, TRUE, FALSE),
         location_cam_lag = lag(location_cam),
         diff_loc = ifelse(location_cam != location_cam_lag, TRUE, FALSE),
         gap_check = ifelse(diff_loc == FALSE & diff_sp == FALSE & (diff_time <= 120 & diff_time >= 20), 1, 0),
         diff_series = ifelse(diff_loc == TRUE | diff_sp == TRUE | diff_time > 120, 1, 0),
         series_num = c(0, cumsum(diff_series[-1]))) |>
  left_join(df_gap_groups, by = "species_common_name") |>
  left_join(df_leave_prob_pred, by = c("gap_group", "diff_time")) |>
  mutate(pred = replace_na(pred, 1),
         diff_time_adj = round(ifelse(gap_check == 1, diff_time * (1 - pred), diff_time), digits = 2))

# Calculate total time in front of the camera, by series

# Start with series that have >1 image
df_tts_multiple <- df_series_21 |>
  # Remove first image from dataframe (as to not include diff_time in time totals for the series)
  mutate(series_num_previous = lag(series_num)) |>
  filter(series_num_previous == series_num) |>
  group_by(series_num) |>
  summarise(n_images = n() + 1,
            total_time = sum(diff_time_adj))

# Next, single-image series'
df_tts_single <- df_series_21 |>
  group_by(series_num) |>
  summarise(n_images = n()) |>
  # Keep only series with 1 image
  filter(n_images == 1) |>
  mutate(total_time = 0)

# Bind together
df_tts_all <- df_tts_multiple |>
  bind_rows(df_tts_single) |>
  arrange(series_num)

# Add time between photos, accounting for the average number of animals in each photo of a series
df_tts_final <- df_series_21 |>
  mutate(individual_count = as.numeric(ifelse(individual_count == "VNA", 1, individual_count))) |>
  # Join in average tbp, by species; note that this is slightly different from results calculated above (incl. non-ABMI)
  left_join(df_tbp, by = "species_common_name") |>
  select(series_num, species_common_name, individual_count, tbp) |>
  group_by(series_num) |>
  # Number of individuals in a series
  mutate(avg_individuals = mean(individual_count)) |>
  ungroup() |>
  select(-individual_count) |>
  distinct() |>
  left_join(df_tts_all, by = "series_num") |>
  mutate(series_total_time = (total_time + tbp) * avg_individuals) |>
  select(series_num, species_common_name, n_images, series_total_time)

# Calculate total time in front of camera, by deployment, year, and species (tt = total time)

df_tt <- df_series_21 |>
  group_by(series_num) |>
  arrange(image_date_time) |>
  filter(row_number() == 1) |>
  left_join(df_tts_final, by = c("series_num", "species_common_name")) |>
  select(series_num, n_images, location_cam, image_date_time, species_common_name, series_total_time) |>
  ungroup() |>
  mutate(julian = as.numeric(format(image_date_time, "%j")),
         season = ifelse(julian >= summer.start.j & julian <= summer.end.j, "summer", "winter")) |>
  mutate_at(c("location_cam", "species_common_name", "season"), factor) |>
  group_by(location_cam, species_common_name, season, .drop = FALSE) |>
  summarise(total_duration = sum(series_total_time),
            total_images = sum(n_images),
            total_series = n()) |>
  ungroup() |>
  mutate_if(is.factor, as.character)

write_csv(df_tt, "G:/Shared drives/ABMI Mammals/Data/Summary of Series 2021.csv")

#-----------------------------------------------------------------------------------------------------------------------

# Now finally 2022

# Let's look at time ranges.
ranges_22 <- df_tags |>
  filter(str_detect(project, "2022")) |>
  mutate(location_cam = paste0(location, "_", equipment_model)) |>
  group_by(location_cam) |>
  summarise(start_date_time = min(image_date_time),
            end_date_time = max(image_date_time))
# Seems reasonable

# Now let's calculate series.

df_series_22 <- df_tags |>
  filter(str_detect(project, "2022")) |>
  mutate(location_cam = paste0(location, "_", equipment_model)) |>
  # Just native species.
  filter(species_common_name %in% native_sp) |>
  # Let's append camera model to location
  mutate(image_date_time = ymd_hms(image_date_time)) |>
  # Order observations
  arrange(location_cam, image_date_time, species_common_name) |>
  # Assess series
  mutate(series_num = 0,
         image_date_time_lag = lag(image_date_time),
         diff_time = as.numeric(image_date_time - image_date_time_lag),
         species_common_name_lag = lag(species_common_name),
         diff_sp = ifelse(species_common_name != species_common_name_lag, TRUE, FALSE),
         location_cam_lag = lag(location_cam),
         diff_loc = ifelse(location_cam != location_cam_lag, TRUE, FALSE),
         gap_check = ifelse(diff_loc == FALSE & diff_sp == FALSE & (diff_time <= 120 & diff_time >= 20), 1, 0),
         diff_series = ifelse(diff_loc == TRUE | diff_sp == TRUE | diff_time > 120, 1, 0),
         series_num = c(0, cumsum(diff_series[-1]))) |>
  left_join(df_gap_groups, by = "species_common_name") |>
  left_join(df_leave_prob_pred, by = c("gap_group", "diff_time")) |>
  mutate(pred = replace_na(pred, 1),
         diff_time_adj = round(ifelse(gap_check == 1, diff_time * (1 - pred), diff_time), digits = 2))

# Calculate total time in front of the camera, by series

# Start with series that have >1 image
df_tts_multiple <- df_series_22 |>
  # Remove first image from dataframe (as to not include diff_time in time totals for the series)
  mutate(series_num_previous = lag(series_num)) |>
  filter(series_num_previous == series_num) |>
  group_by(series_num) |>
  summarise(n_images = n() + 1,
            total_time = sum(diff_time_adj))

# Next, single-image series'
df_tts_single <- df_series_22 |>
  group_by(series_num) |>
  summarise(n_images = n()) |>
  # Keep only series with 1 image
  filter(n_images == 1) |>
  mutate(total_time = 0)

# Bind together
df_tts_all <- df_tts_multiple |>
  bind_rows(df_tts_single) |>
  arrange(series_num)

# Add time between photos, accounting for the average number of animals in each photo of a series
df_tts_final <- df_series_22 |>
  mutate(individual_count = as.numeric(ifelse(individual_count == "VNA", 1, individual_count))) |>
  # Join in average tbp, by species; note that this is slightly different from results calculated above (incl. non-ABMI)
  left_join(df_tbp, by = "species_common_name") |>
  select(series_num, species_common_name, individual_count, tbp) |>
  group_by(series_num) |>
  # Number of individuals in a series
  mutate(avg_individuals = mean(individual_count)) |>
  ungroup() |>
  select(-individual_count) |>
  distinct() |>
  left_join(df_tts_all, by = "series_num") |>
  mutate(series_total_time = (total_time + tbp) * avg_individuals) |>
  select(series_num, species_common_name, n_images, series_total_time)

# Calculate total time in front of camera, by deployment, year, and species (tt = total time)

df_tt <- df_series_22 |>
  group_by(series_num) |>
  arrange(image_date_time) |>
  filter(row_number() == 1) |>
  left_join(df_tts_final, by = c("series_num", "species_common_name")) |>
  select(series_num, n_images, location_cam, image_date_time, species_common_name, series_total_time) |>
  ungroup() |>
  mutate(julian = as.numeric(format(image_date_time, "%j")),
         season = ifelse(julian >= summer.start.j & julian <= summer.end.j, "summer", "winter")) |>
  mutate_at(c("location_cam", "species_common_name", "season"), factor) |>
  group_by(location_cam, species_common_name, season, .drop = FALSE) |>
  summarise(total_duration = sum(series_total_time),
            total_images = sum(n_images),
            total_series = n()) |>
  ungroup() |>
  mutate_if(is.factor, as.character)

write_csv(df_tt, "G:/Shared drives/ABMI Mammals/Data/Summary of Series 2022.csv")




