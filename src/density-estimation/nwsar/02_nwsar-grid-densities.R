#-----------------------------------------------------------------------------------------------------------------------

# Project:          NWSAR

# Title:            Calculate density of species by grid
# Description:
# Author:           Marcus Becker

# Previous scripts: 01_nwsar-calculate-density-by-location

#-----------------------------------------------------------------------------------------------------------------------

library(readr)
library(dplyr)
library(stringr)
library(tidyr)

g_drive <- "G:/Shared drives/ABMI Camera Mammals/"

# Summarise density function
source("src/functions/summarise-density.R")

# Import data
df_dep_density <- read_csv(paste0(g_drive, "results/density/deployments/nwsar_20-21_density_long_2023-01-19.csv"))

#-----------------------------------------------------------------------------------------------------------------------

# Species of interest
sp <- c("White-tailed Deer", "Black Bear", "Moose", "Woodland Caribou", "Gray Wolf",
        "Canada Lynx", "Grizzly Bear", "Wolverine", "Mule Deer", "Cougar", "Elk (wapiti)",
        "Snowshoe Hare")

# Minimum seasonal days requirement
min_season_days <- 20
# Minimum total days requirement
min_total_days <- 40

# Clean and subset the deployment density data
df_dep_density_subset <- df_dep_density |>
  # Only species of interest, and no T (trail) deployments
  filter(common_name %in% sp,
         !str_detect(location, "T$")) |>
  # Remove black bear in winter
  filter(!(common_name == "Black Bear" & season == "winter")) |>
  # Remove deployment seasonal periods that don't meet minimum requirement:
  filter(!is.na(density_km2),
         total_season_days >= min_season_days) |>
  group_by(project, location, common_name) |>
  mutate(total_days = sum(total_season_days)) |>
  # Remove deployments that don't meet minimum requirement for total days:
  filter(total_days >= min_total_days) |>
  # Density considering both seasons together:
  summarise(full_density_km2 = mean(density_km2)) |>
  ungroup() |>
  # Parse out grids
  separate(location, into = c("grid", "station"), sep = "-", remove = TRUE) |>
  select(project, grid, station, common_name, full_density_km2)

lynx <- df_dep_density_subset |>
  filter(common_name == "Gray Wolf") |>
  filter(grid == "RUN")

# Estimate density at the grid scale, by sampling period (i.e., project)
df_grid_density <- df_dep_density_subset |>
  # Use custom function (loaded above)
  summarise_density(
    # Group by grid
    group_id = grid,
    # Don't aggregate over sampling periods (monitoring_periods)
    agg_samp_per = FALSE,
    # `project` is the column referring to sampling period
    samp_per_col = project,
    # `common_name` is the species column
    species_col = common_name,
    # `full_density_km2` is the column containing density value
    dens_col = full_density_km2,
    # Choose confidence level (set to 90%)
    conflevel = 0.9
  ) |>
  arrange(common_name, grid, project)

#-----------------------------------------------------------------------------------------------------------------------

# Save results
write_csv(df_grid_density, paste0(g_drive, "results/density/areas/nwsar_grid-density-by-project_", Sys.Date(), ".csv"))

#-----------------------------------------------------------------------------------------------------------------------
