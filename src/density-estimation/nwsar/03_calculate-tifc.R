#-----------------------------------------------------------------------------------------------------------------------

# Project:          NWSAR

# Title:            Calculate camera deployment operating time ranges by day
# Description:
# Authors:          Marcus Becker, David J. Huggard

# Previous scripts: 01_process-raw

#-----------------------------------------------------------------------------------------------------------------------

# Attach packages
library(readr)
library(dplyr)
library(tidyr)
library(stringr)

# Set path to Google Drive
g_drive <- "G:/Shared drives/ABMI Camera Mammals/"

# Load NWSAR tag data
# Note: Clean data has already been filtered to be 'WITHIN' the field of view.
df_all <- read_csv(paste0(g_drive, "data/base/clean/nwsar_all-years_all-data_clean_2022-11-24.csv"))

# Previously processed data:

# 1. Probabilistic gaps
df_leave_prob_pred <- read_csv(paste0(g_drive, "data/processed/probabilistic-gaps/gap-leave-prob_predictions_2021-10-05.csv"))
# 2. Time between photos
df_tbp <- read_csv(paste0(g_drive, "data/processed/time-btwn-images/Table of time between photos within series by species incl 2019 May 2020.csv")) %>%
  rename(common_name = Species)
# 3. Time by day summary
df_tbd <- read_csv(paste0(g_drive, "data/processed/time-by-day/nwsar_all-years_tbd-summary_2022-11-28.csv")) %>%
  # Unite location and project into one column
  unite(col = "location_project", location, project, sep = "_", remove = TRUE)
# 4. None gaps
df_gap_nones <- read_csv(paste0(g_drive, "data/processed/probabilistic-gaps/nwsar_all-years_gap-class-nones_2022-11-24.csv"))

# Gap groups
df_gap_groups <- read_csv(paste0(g_drive, "data/lookup/species-gap-groups.csv"))

# Species strings
load(paste0(g_drive, "data/lookup/wt_cam_sp_str.RData"))

# Parameters:

# Seasonal start/end dates (julian day):
summer.start.j <- 106 # April 16
summer.end.j <- 288 # October 15

#-----------------------------------------------------------------------------------------------------------------------

# Group into series






