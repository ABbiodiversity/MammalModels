#-----------------------------------------------------------------------------------------------------------------------

# Project:          OSM

# Title:
# Description:
# Author:           Marcus Becker

# Previous scripts:

#-----------------------------------------------------------------------------------------------------------------------

library(tidyverse)

g_drive <- "G:/Shared drives/ABMI Camera Mammals/"

# Source functions for TIFC workflow
source("./src/Functions/estimate-density-tifc.R")

# Project
proj <- "osm"

# Years
years <- "_2021-2022"

# Find appropriate tag data file
file <- list.files(path = paste0(g_drive, "data/base/clean"), full.names = TRUE) |>
  str_subset(pattern = paste0(proj, years, "_all-data_clean"))
# Import
osm_clean <- read_csv(file)

osm_tbd <- read_csv(paste0(g_drive, "data/processed/time-by-day/osm_2021-2022_tbd-summary_2023-08-31.csv"))

df_snow <- read_csv(paste0(g_drive, "data/lookup/veghf/VegForDetectionDistance/New/EDD (Re-) Modeling OSM.csv")) |>
  select(project, location, snow_start, snow_gone) |>
  mutate_at(c("snow_start", "snow_gone"), as.Date)

# Calculate time in front of camera (TIFC)

df_tt <- osm_clean |>
  # First calculate time by series
  calculate_time_by_series() |>
  # Attach snow start and snow gone information
  left_join(df_snow, by = c("project", "location"))
  # Next calculate TIFC by location, deployment, species
  sum_total_time(tbd = df_tbd_summary)
