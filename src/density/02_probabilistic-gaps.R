#-----------------------------------------------------------------------------------------------------------------------

# Title:            Probabilistic Gaps
# Description:      Model the probability of animals leaving the camera field of view based on length of gaps between
#                   images. Data is based on tagging done in the old ABMI cameras website from 2013 to 2018.
# Author:           Marcus Becker, David Huggard
# Date:             August 2022

# Previous scripts: 01_process-raw.R

#-----------------------------------------------------------------------------------------------------------------------

# Attach packages
library(readr)
library(tidyr)
library(dplyr)
library(purrr)

# Set path to Shared Google Drive
g_drive <- "G:/Shared drives/ABMI Camera Mammals/"

#-----------------------------------------------------------------------------------------------------------------------

# Gap analysis data (2013-2018)
df_gap <- read_csv(paste0(g_drive, "data/processed/probabilistic-gaps/abmi_2013-2018_gap-class-raw.csv"))

# Species group for gap analysis
df_gap_groups <- read_csv(paste0(g_drive, "data/lookup/species-gap-groups.csv"))

# Load tag data
df_all <- read_csv(paste0(g_drive, "data/base/clean/abmi_all-years_all-data_clean_2021-10-05.csv"))

# Native species common names in WildTrax
load(paste0(g_drive, "data/lookup/wt_native_sp.RData"))

#-----------------------------------------------------------------------------------------------------------------------

# Probabilistic Gaps

# Set up data for modeling leaving probability using observations from gap analysis and time difference between images:
df_leave_prob <- df_all |>
  # Filter only images WITHIN the field of view
  filter(field_of_view == "WITHIN",
         common_name %in% native_sp) |>
  # Join gap class
  left_join(df_gap, by = c("location", "date_detected", "common_name")) |>
  # Order observations
  arrange(location, date_detected, common_name, project) |>
  # Identify series and gaps requiring probabilistic time assignment
  # Lagged time stamp
  mutate(date_detected_previous = lag(date_detected),
         # Calculate difference in time between ordered images
         diff_time = as.numeric(date_detected - date_detected_previous),
         # Lagged species
         common_name_previous = lag(common_name),
         # Was it a different species?
         diff_sp = ifelse(common_name != common_name_previous, TRUE, FALSE),
         # Lagged deployment
         location_previous = lag(location),
         # Was is a different deployment?
         diff_location = ifelse(location != location_previous, TRUE, FALSE),
         # Flag gaps that will need checking
         gap_check = ifelse(diff_location == FALSE & diff_sp == FALSE & (diff_time <= 120 & diff_time >= 20), 1, 0),
         # Lagged gap class
         gap_class_previous = lag(gap_class)) |>
  # Join gap group lookup table
  left_join(df_gap_groups, by = "common_name") |>
  # Isolate observations requiring a gap check
  filter(!is.na(gap_class_previous) & gap_check == "1") |>
  select(common_name, gap_group, gap_class_previous, diff_time) |>
  # Determine whether the animal left the field of view or not (i.e. tagged as 'P' (present) by gap analysis)
  mutate(left = ifelse(gap_class_previous == "P", 0, 1)) |>
  select(-gap_class_previous)

# Modeling by gap group, followed by predictions using those models:
df_leave_prob_pred <- df_leave_prob |>
  filter(!is.na(gap_group)) |>
  group_by(gap_group) |>
  nest() |>
  # Define model for each gap group, and then make predictions
  mutate(model = map(.x = data, ~ smooth.spline(x = .$diff_time, y = .$left, df = 3)),
         pred = map(.x = model, ~ predict(., x = 20:120))) |>
  select(gap_group, pred) |>
  unnest_wider(pred) |>
  unnest(cols = c(x, y)) |>
  rename(diff_time = x, pred = y) |>
  ungroup()

#-----------------------------------------------------------------------------------------------------------------------

# Write results

write_csv(df_leave_prob,
          paste0(g_drive, "data/processed/probabilistic-gaps/gap-leave-prob_raw-data.csv"))

write_csv(df_leave_prob_pred,
          paste0(g_drive, "data/processed/probabilistic-gaps/gap-leave-prob_predictions.csv"))

#-----------------------------------------------------------------------------------------------------------------------
