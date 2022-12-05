#-----------------------------------------------------------------------------------------------------------------------

# Project:          BOUTIN

# Title:            Summarise density by project
# Description:

# Author:           Marcus Becker
# Date:             December 2022

# Previous scripts: 01_calculate-density-by-location.R

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

# Minimum seasonal days requirement
min_season_days <- 20
# Minimum total days requirement
min_total_days <- 40

df_dep_density <- read_csv(paste0(g_drive, "results/density/deployments/boutin_all-years_density_long_2022-12-02.csv"))

# Clean and subset the deployment density data
df_dep_density_subset <- df_dep_density |>
  # Only species of interest, and no T (trail) deployments
  filter(common_name %in% sp,
         !str_detect(location, "T$")) |>
  # Remove deployment seasonal periods that don't meet minimum requirement:
  filter(!is.na(density_km2),
         total_season_days >= min_season_days) |>
  group_by(project, location, common_name) |>
  mutate(total_days = sum(total_season_days)) |>
  # Remove deployments that don't meet minimum requirement for total days:
  filter(total_days >= min_total_days) |>
  # Density considering both seasons together:
  summarise(full_density_km2 = mean(density_km2)) |>
  ungroup()

# Estimate density by project (for now - not sure what Stan will ultimately want.)
df_proj_density <- df_dep_density_subset |>
  # Use custom function (loaded above)
  summarise_density(
    # Group by project
    group_id = project,
    # Don't aggregate over sampling periods (monitoring_periods)
    agg_samp_per = TRUE,
    # `common_name` is the species column
    species_col = common_name,
    # `full_density_km2` is the column containing density value
    dens_col = full_density_km2,
    # Choose confidence level (set to 90%)
    conflevel = 0.9
  ) |>
  arrange(project, common_name)

#-----------------------------------------------------------------------------------------------------------------------

write_csv(df_proj_density, paste0(g_drive, "results/density/areas/", proj, "_density-by-project_", Sys.Date(), ".csv"))





