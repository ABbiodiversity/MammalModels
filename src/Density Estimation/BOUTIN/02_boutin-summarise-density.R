#-----------------------------------------------------------------------------------------------------------------------

# Project:          BOUTIN

# Title:            Summarise density by project
# Description:

# Author:           Marcus Becker
# Date:             December 2022

# Previous scripts: 01_boutin-calculate-density-by-location

#-----------------------------------------------------------------------------------------------------------------------

# Set path to Shared Google Drive (G Drive)
g_drive <- "G:/Shared drives/ABMI Camera Mammals/"

# Source functions for summarising density
source("./src/functions/summarise-density.R")

# Attach packages
library(readr)
library(fs)
library(dplyr)
library(stringr)

proj <- "boutin"

#-----------------------------------------------------------------------------------------------------------------------

# Import density by location data
files <- dir_ls(path = paste0(g_drive, "results/density/deployments"), glob = "*.csv")

# Species of interest
sp <- c("Snowshoe Hare", "Canada Lynx")

min_days <- 15

df_dep_density <- read_csv(paste0(g_drive, "results/density/deployments/boutin_monitoring-periods_density_long_2022-12-16.csv"))

# Clean and subset the deployment density data
df_dep_density_subset <- df_dep_density |>
  # Only species of interest
  filter(common_name %in% sp) |>
  # Remove deployment seasonal periods that don't meet minimum requirement:
  filter(!is.na(density_km2),
         total_days >= min_days)

# Estimate density by monitoring period
df_mp_density <- df_dep_density_subset |>
  # Use custom function (loaded above)
  summarise_density(
    # Group by project
    group_id = monitoring_period,
    # Don't aggregate over sampling periods (monitoring_periods)
    agg_samp_per = TRUE,
    # `common_name` is the species column
    species_col = common_name,
    # `density_km2` is the column containing density value
    dens_col = density_km2,
    # Choose confidence level (set to 90%)
    conflevel = 0.9
  ) |>
  arrange(monitoring_period, common_name)

#-----------------------------------------------------------------------------------------------------------------------

write_csv(df_mp_density, paste0(g_drive, "results/density/areas/", proj, "_density-by-monitoring-period_", Sys.Date(), ".csv"))

#-----------------------------------------------------------------------------------------------------------------------
