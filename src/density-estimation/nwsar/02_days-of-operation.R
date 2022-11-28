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
library(tidyverse)

g_drive <- "G:/Shared drives/ABMI Camera Mammals/"

# Read image report data
image_fov_trigger <- read_csv(paste0(g_drive, "data/lookup/images/nwsar_all-years_image-report_simple.csv"))

#-----------------------------------------------------------------------------------------------------------------------

# Summarise time-by-day for each camera deployment in the two NWSAR projects.

df_tbd_summary <- get_operating_days(
  x = image_fov_trigger,
  # Keep project
  include_project = TRUE,
  # Summarise
  summarise = TRUE,
  # Include ABMI seasons
  .abmi_seasons = TRUE
)

