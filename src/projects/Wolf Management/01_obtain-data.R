#-----------------------------------------------------------------------------------------------------------------------

# Title:       Pull data for analysis
# Description:

# Author:      Marcus A Becker
# Date:        October 2023

#-----------------------------------------------------------------------------------------------------------------------

library(tidyverse)

g_drive <- "G:/Shared drives/ABMI Camera Mammals/"

# Load data
df_data <- read_csv(paste0(g_drive, "results/density/deployments/all-projects_all-years_wide-density-for-habitat-modeling.csv"))

load(paste0(g_drive, "data/lookup/R Dataset SpTable for ABMI North mammal coefficients 2022.RData"))

loc <- st_read(paste0(g_drive, "data/lookup/locations/spatial/west_atha.shp")) |>
  st_set_geometry(NULL) |>
  mutate(west_athabasca = TRUE)

projects <- paste0(c("eh", "cmu", "og", "nwsar", "bg"), collapse = "|")

# List files
files <- list.files(path = paste0(g_drive, "results/density/deployments"), full.names = TRUE) |>
  str_subset(pattern = projects) |>
  # Remove wide form
  str_subset(pattern = "wide", negate = TRUE)

sp <- c("White-tailed Deer", "Moose", "Woodland Caribou", "Gray Wolf")

min_season_days <- 20
min_total_days <- 40

# Read in density data
dens <- map_df(.x = files, .f = read_csv) |>
  filter(common_name %in% sp) |>
  left_join(loc, by = c("location", "project")) |>
  mutate(athabasca = ifelse(is.na(west_athabasca), "East", "West")) |>
  select(-west_athabasca) |>
  filter(!is.na(density_km2),
         total_season_days >= min_season_days) |>
  group_by(project, location, common_name, athabasca) |>
  mutate(total_days = sum(total_season_days)) |>
  # Remove deployments that don't meet minimum requirement for total days:
  filter(total_days >= min_total_days) |>
  # Density considering both seasons together:
  summarise(full_density_km2 = mean(density_km2)) |>
  ungroup()









