#-----------------------------------------------------------------------------------------------------------------------

# Project: Tracking moose demographics with camera data

# Title:
# Description:

# Author: Marcus Becker

# Previous scripts: None

#-----------------------------------------------------------------------------------------------------------------------

check_image <- image_fov_trigger |>
  filter(project == "CMU 2017",
         location == "FMM-6") |>
  mutate(date_detected = ymd_hms(date_detected))

check_tag <- tag_reports |>
  filter(project == "CMU 2017",
         location == "FMM-6") |>
  consolidate_tags() |>
  mutate(date_detected = ymd_hms(date_detected)) |>
  select(project, location, date_detected, common_name) |>
  left_join(check_image, by = c("project", "location", "date_detected"))

#-----------------------------------------------------------------------------------------------------------------------

library(readr)
library(dplyr)
library(stringr)
library(tidyr)

# Google drive
g_drive <- "G:/Shared drives/ABMI Camera Mammals/"

# Import data
proj <- "cmu"

# Find appropriate tag data file
file <- list.files(path = paste0(g_drive, "data/base/clean"), full.names = TRUE) |>
  str_subset(pattern = paste0(proj, "_all-years_all-data_clean"))
# Import
tags_clean <- read_csv(file)

# Moose only!
# Tags have already been consolidated, and out of range images removed.
moose <- tags_clean |>
  filter(common_name == "Moose")
