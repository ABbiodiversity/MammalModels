#-----------------------------------------------------------------------------------------------------------------------

# Project:          ABMI

# Title:            Evaluating Spatial and Temporal Trends in Winter Tick Infestation Rates on Moose in Alberta
# Description:
# Author:           Marcus Becker

# Previous scripts:

#-----------------------------------------------------------------------------------------------------------------------

# Attach packages
library(tidyverse)
library(wildRtrax)
library(keyring)

# Download data
# Set environment variables (username/password)
Sys.setenv(WT_USERNAME = key_get("WT_USERNAME", keyring = "wildtrax"),
           WT_PASSWORD = key_get("WT_PASSWORD", keyring = "wildtrax"))

# Authenticate into WildTrax
wt_auth()

# Load data

# Set path to Shared Google Drive (G Drive)
g_drive <- "G:/Shared drives/ABMI Camera Mammals/"

# Source functions that may be needed (?)
# source("./src/Functions/estimate-density-tifc.R")

consolidate_tags <- function(main_report) {

  # Species tags w/ full information.
  y <- main_report |>
    mutate(age_class = trimws(strrep(str_c(age_class, ", "), individual_count), whitespace = ", "),
           sex_class = trimws(strrep(str_c(sex_class, ", "), individual_count), whitespace = ", "),
           tag_comments = trimws(strrep(str_c(sex_class, ", "), individual_count), whitespace = ", ")) |>
    # Now grouping my project_id, location_id, and image_id
    group_by(project_id, location_id, image_id, species_common_name) |>
    mutate(individual_count = sum(individual_count),
           age_class = paste0(age_class, collapse = ", "),
           sex_class = paste0(sex_class, collapse = ", ")) |>
    distinct(project_id, location_id, image_id, species_common_name, .keep_all = TRUE) |>
    ungroup()

  return(y)

}

#-----------------------------------------------------------------------------------------------------------------------

# TAGGING KEY

# 'Ticks' (under health_diseases)
# - 1 -> No ticks
# - 2 -> Slight (0-15%)
# - 3 -> Moderate (15-40%)
# - 4 -> Extreme (40-80%)
# - 5 -> Ghost (80%+)

# - UNKN (Not visible due to image quality)

#-----------------------------------------------------------------------------------------------------------------------

# Download data
# Project: "Prevalence of Winter Ticks on Moose
proj <- 2154

# Main report
data <- wt_download_report(project_id = proj,
                           sensor_id = "CAM",
                           reports = "main") |>
  mutate(tag_comments = ifelse(str_detect(tag_comments, ", "), str_replace(tag_comments, ",.*", ""), tag_comments)) |>
  filter(!is.na(tag_comments)) |>
  # Select relevant columns for analysis
  select(location, image_id, latitude, longitude, image_date_time, species_common_name,
         individual_count, age_class, sex_class,
         health_diseases, tag_comments, tag_needs_review)

threshold <- 120

# Adult Females
adult_female <- data |>
  filter(age_class == "Adult",
         sex_class == "Female") |>
  select(-c(latitude, longitude, image_id)) |>
  # Order the dataframe
  arrange(location, image_date_time, species_common_name) |>
  # Calculate the time difference between subsequent images
  mutate(interval = int_length(image_date_time %--% lag(image_date_time))) |>
  # Is this considered a new detection?
  mutate(new_detection = ifelse(is.na(interval) | abs(interval) >= threshold, TRUE, FALSE)) |>
  # Number independent detections
  mutate(detection = c(1, cumsum(new_detection[-1]) + 1))

adult_female_unkns <- adult_female |>
  filter(tag_comments == "UNKN") |>
  group_by(location, detection) |>
  summarise(n_individuals = max(individual_count),
            start = min(image_date_time)) |>
  mutate(hli = "UNKN")

adult_female_kns <- adult_female |>
  filter(!tag_comments == "UNKN") |>
  mutate(tag_comments = as.numeric(tag_comments)) |>
  group_by(location, detection) |>
  summarise(n_individuals = max(individual_count),
            start = min(image_date_time),
            hli = mean(tag_comments, na.rm = TRUE)) |>
  # Sigh
  mutate(hli = round(hli, digits = 0)) |>
  mutate(hli = as.character(hli))

adult_female_all <- bind_rows(adult_female_kns, adult_female_unkns) |>
  mutate(date = date(start)) |>
  ungroup() |>
  distinct(location, n_individuals, hli, date) |>
  mutate(class = "Adult Female")

# Adult Males
adult_male <- data |>
  filter(age_class == "Adult",
         sex_class == "Male") |>
  select(-c(latitude, longitude, image_id)) |>
  # Order the dataframe
  arrange(location, image_date_time, species_common_name) |>
  # Calculate the time difference between subsequent images
  mutate(interval = int_length(image_date_time %--% lag(image_date_time))) |>
  # Is this considered a new detection?
  mutate(new_detection = ifelse(is.na(interval) | abs(interval) >= threshold, TRUE, FALSE)) |>
  # Number independent detections
  mutate(detection = c(1, cumsum(new_detection[-1]) + 1))

adult_male_unkns <- adult_male |>
  filter(tag_comments == "UNKN") |>
  group_by(location, detection) |>
  summarise(n_individuals = max(individual_count),
            start = min(image_date_time)) |>
  mutate(hli = "UNKN")

adult_male_kns <- adult_male |>
  filter(!tag_comments == "UNKN") |>
  mutate(tag_comments = as.numeric(tag_comments)) |>
  group_by(location, detection) |>
  summarise(n_individuals = max(individual_count),
            start = min(image_date_time),
            hli = mean(tag_comments, na.rm = TRUE)) |>
  # Sigh
  mutate(hli = round(hli, digits = 0)) |>
  mutate(hli = as.character(hli))

adult_male_all <- bind_rows(adult_male_kns, adult_male_unkns) |>
  mutate(date = date(start)) |>
  ungroup() |>
  distinct(location, n_individuals, hli, date) |>
  mutate(class = "Adult Male")

# Adult Unkns
adult_unkn <- data |>
  filter(age_class == "Adult",
         sex_class == "Unkn") |>
  select(-c(latitude, longitude, image_id)) |>
  # Order the dataframe
  arrange(location, image_date_time, species_common_name) |>
  # Calculate the time difference between subsequent images
  mutate(interval = int_length(image_date_time %--% lag(image_date_time))) |>
  # Is this considered a new detection?
  mutate(new_detection = ifelse(is.na(interval) | abs(interval) >= threshold, TRUE, FALSE)) |>
  # Number independent detections
  mutate(detection = c(1, cumsum(new_detection[-1]) + 1))

adult_unkn_unkns <- adult_unkn |>
  filter(tag_comments == "UNKN") |>
  group_by(location, detection) |>
  summarise(n_individuals = max(individual_count),
            start = min(image_date_time)) |>
  mutate(hli = "UNKN")

adult_unkn_kns <- adult_unkn |>
  filter(!tag_comments == "UNKN") |>
  mutate(tag_comments = as.numeric(tag_comments)) |>
  group_by(location, detection) |>
  summarise(n_individuals = max(individual_count),
            start = min(image_date_time),
            hli = mean(tag_comments, na.rm = TRUE)) |>
  # Sigh
  mutate(hli = round(hli, digits = 0)) |>
  mutate(hli = as.character(hli))

adult_unkn_all <- bind_rows(adult_unkn_kns, adult_unkn_unkns) |>
  mutate(date = date(start)) |>
  ungroup() |>
  distinct(location, n_individuals, hli, date) |>
  mutate(class = "Adult Unkns")

# Juveniles
juv <- data |>
  filter(age_class == "Juv") |>
  select(-c(latitude, longitude, image_id)) |>
  # Order the dataframe
  arrange(location, image_date_time, species_common_name) |>
  # Calculate the time difference between subsequent images
  mutate(interval = int_length(image_date_time %--% lag(image_date_time))) |>
  # Is this considered a new detection?
  mutate(new_detection = ifelse(is.na(interval) | abs(interval) >= threshold, TRUE, FALSE)) |>
  # Number independent detections
  mutate(detection = c(1, cumsum(new_detection[-1]) + 1))

juv_unkns <- juv |>
  filter(tag_comments == "UNKN") |>
  group_by(detection, location) |>
  summarise(n_individuals = max(individual_count),
            start = min(image_date_time)) |>
  mutate(hli = "UNKN")

juv_kns <- juv |>
  filter(!tag_comments == "UNKN") |>
  mutate(tag_comments = as.numeric(tag_comments)) |>
  group_by(detection, location) |>
  summarise(n_individuals = max(individual_count),
            start = min(image_date_time),
            hli = mean(tag_comments, na.rm = TRUE)) |>
  # Sigh
  mutate(hli = round(hli, digits = 0)) |>
  mutate(hli = as.character(hli))

juv_all <- bind_rows(juv_kns, juv_unkns) |>
  mutate(date = date(start)) |>
  ungroup() |>
  distinct(location, n_individuals, hli, date) |>
  mutate(class = "Juv")

#-----------------------------------------------------------------------------------------------------------------------

all <- bind_rows(juv_all, adult_male_all, adult_female_all, adult_unkn_all)

#-----------------------------------------------------------------------------------------------------------------------

monthly_trend <- all |>
  filter(!hli == "UNKN") |>
  mutate(hli = as.numeric(hli)) |>
  mutate(month = month(date, label = TRUE)) |>
  group_by(month, class) |>
  summarise(wmean_hli = weighted.mean(hli, w = n_individuals))

#-----------------------------------------------------------------------------------------------------------------------

biweekly_trend <- all |>
  filter(!hli == "UNKN") |>
  mutate(hli = as.numeric(hli)) |>
  mutate(yday = yday(date)) |>
  mutate(case_when(
    yday < 16 ~ "Jan 1-15",
    yday > 16 & yday < 31 ~ "Jan 16-31",
    yday > 31 & yday < 47 ~ "Feb 1-15",
    yday > 47 & yday < 59 ~ "Feb 16-28",
    yday > 59 &
  ))
  group_by(month, class) |>
  summarise(wmean_hli = weighted.mean(hli, w = n_individuals))


