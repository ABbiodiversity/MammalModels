#-----------------------------------------------------------------------------------------------------------------------

# Project:          BOUTIN

# Title:            Process Raw Data
# Description:      Process raw BOUTIN camera tag data from WildTrax in preparation for downstream density estimation,
#                   as well as extract NONE gap class information.
# Author:           Marcus Becker
# Date:             November 2022

# Previous scripts: None

#-----------------------------------------------------------------------------------------------------------------------

# Attach packages

library(wildRtrax) # To download data
library(keyring)   # For storing credentials safely

library(readr)
library(dplyr)
library(purrr)
library(stringr)
library(fs)
library(lubridate)

# Set path to Shared Google Drive (G Drive)
g_drive <- "G:/Shared drives/ABMI Camera Mammals/"

# Native species strings
load(paste0(g_drive, "data/lookup/wt_native_sp.RData"))

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
tags <- map_df(.x = boutin_proj_ids,
               .f = ~ wt_download_report(
                 project_id = .x,
                 sensor_id = "CAM",
                 report = "tag",
                 weather_cols = FALSE))

images <- map_df(.x = boutin_proj_ids,
                 .f = ~ wt_download_report(
                   project_id = .x,
                   sensor_id = "CAM",
                   report = "image",
                   weather_cols = FALSE
                 ))

images_simple <- images |>
  select(project, location, date_detected, trigger, field_of_view, serial_number) |>
  # Duplicated image records - not sure why.
  distinct()

# Remove images that are not in the field of view - this is an annoying change.
tags_within <- tags |>
  left_join(images_simple, by = c("location", "project", "date_detected")) |>
  filter(field_of_view == "WITHIN")

#-----------------------------------------------------------------------------------------------------------------------

# Clean data

df_native_clean <- tags_within |>
  # Only keep native species tags, takes way too long otherwise; remove VNAs from number_individuals (mostly non-mammals)
  filter(common_name %in% native_sp,
         !number_individuals == "VNA") |>
  # Convert number individuals column to numeric
  mutate(number_individuals = as.numeric(number_individuals)) |>
  # Amalgamate tags of same species in the same image
  group_by(project, location, date_detected, common_name) |>
  mutate(number_individuals = sum(number_individuals),
         age_class = paste0(age_class, collapse = ", "),
         sex = paste0(sex, collapse = ", ")) |>
  distinct(project, location, date_detected, common_name, number_individuals, .keep_all = TRUE) |>
  ungroup()
# Note:  ^ This code chunk will soon have a dedicated wildRtrax function.

# Join the remainder non-native mammal images back in
df_all_clean <- tags_within |>
  filter(!common_name %in% native_sp) |>
  mutate(number_individuals = as.numeric(ifelse(number_individuals == "VNA", 1, number_individuals))) |>
  bind_rows(df_native_clean) |>
  # Keep only relevant columns
  select(project, location, date_detected, common_name, age_class, sex, number_individuals) |>
  mutate(date_detected = ymd_hms(date_detected))

#-----------------------------------------------------------------------------------------------------------------------

# Add 'N' gap class to images following a 'NONE' image
# Note: a 'NONE' image is used to demarcate when a series should be truncated because an animal left the field of view.
df_gap_nones <- df_all_clean |>
  select(project, location, date_detected, common_name) |>
  arrange(project, location, date_detected) |>
  # Create gap class column
  mutate(common_name_next = lead(common_name),
         gap_class = ifelse(common_name != "NONE" & common_name_next == "NONE", "N", NA)) |>
  # Include only N gap class for native mammals
  filter(gap_class == "N",
         common_name %in% native_sp) |>
  select(-c(common_name_next))

#-----------------------------------------------------------------------------------------------------------------------

# Save Timelapse images for reference
images |>
  filter(trigger == "Time Lapse") |>
  write_csv(paste0(g_drive, "data/lookup/timelapse/boutin_all-timelapse_all-years.csv"))

#-----------------------------------------------------------------------------------------------------------------------

# Save cleaned and binded data to Shared Google Drive
# (so we don't have to re-download from WildTrax each time)

# Only tags of native species
df_all_clean |>
  # Remove all non-native mammal images
  filter(common_name %in% native_sp) |>
  write_csv(paste0(g_drive, "data/base/clean/boutin_all-years_native-sp_clean_", Sys.Date(), ".csv"))

# Save (all) cleaned data
df_all_clean |>
  write_csv(paste0(g_drive, "data/base/clean/boutin_all-years_all-data_clean_", Sys.Date(), ".csv"))

# NONE gaps
df_gap_nones |>
  write_csv(paste0(g_drive, "data/processed/probabilistic-gaps/boutin_all-years_gap-class-nones_", Sys.Date(), ".csv"))

#-----------------------------------------------------------------------------------------------------------------------
