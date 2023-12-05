#-----------------------------------------------------------------------------------------------------------------------

# Project:          ABMI

# Title:            Pull Winter Moose Series' Images
# Description:
# Author:           Marcus Becker

# Previous scripts: None

#-----------------------------------------------------------------------------------------------------------------------

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


proj <- 2154

data <- wt_download_report(project_id = proj,
                           sensor_id = "CAM",
                           reports = "main") |>
  select(location, image_date_time, species_common_name,
         individual_count, age_class, sex_class,
         health_diseases, tag_comments, tag_needs_review)





