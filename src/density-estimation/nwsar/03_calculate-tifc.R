#-----------------------------------------------------------------------------------------------------------------------

# Project:          NWSAR

# Title:            Calculate camera deployment operating time ranges by day
# Description:
# Authors:          Marcus Becker, David J. Huggard

# Previous scripts: 01_process-raw, 02_days-of-operation

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

# 3. Time by day summary
df_tbd <- read_csv(paste0(g_drive, "data/processed/time-by-day/nwsar_all-years_tbd-summary_2022-11-29.csv"))

# Parameters:

# Seasonal start/end dates (julian day):
summer.start.j <- 106 # April 16
summer.end.j <- 288 # October 15

#-----------------------------------------------------------------------------------------------------------------------

# Test new function to calculate total time by series.

df_tbs <- calculate_time_by_series(x = df_all)

# Looks like it works!

# Now we need to calculate total time in front of the camera, by deployment, species, etc.

df_tt <- sum_total_time(x = df_tbs,
                        y = df_tbd)

#-----------------------------------------------------------------------------------------------------------------------

# Write results

# Full results long:
write_csv(df_tt, paste0(g_drive, "data/processed/time-in-cam-fov/nwsar_all-years_fov-time_long_", Sys.Date(), ".csv"))
