#-----------------------------------------------------------------------------------------------------------------------

# Project:          NWSAR

# Title:            Calculate camera deployment operating time ranges by day
# Description:
# Authors:          Marcus Becker, David J. Huggard

# Previous scripts: 01_process-raw

#-----------------------------------------------------------------------------------------------------------------------

library(readr)
library(dplyr)
library(lubridate)

g_drive <- "G:/Shared drives/ABMI Camera Mammals/"

# Read image report data
image_fov_trigger <- read_csv(paste0(g_drive, "data/lookup/images/nwsar_all-years_image-report_simple.csv"))

# Read in latest tag data
df_all <- read_csv(paste0(g_drive, "data/base/clean/nwsar_all-years_all-data_clean_2022-11-24.csv"))

#-----------------------------------------------------------------------------------------------------------------------

# We may only need the image report data now. That's cool.

# Use the new function to summarise time-by-day for each camera deployments.

check <- summarise_time_by_day(x = image_fov_trigger)

# Summarise time-by-day for each camera deployment






