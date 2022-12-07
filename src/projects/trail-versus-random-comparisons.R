#-----------------------------------------------------------------------------------------------------------------------

# Project:          Game trail vs Random Comparisons

# Title:            Game trail vs Random Comparisons
# Description:

# Author:           Marcus Becker

# Previous scripts: None

#-----------------------------------------------------------------------------------------------------------------------

library(readr)
library(dplyr)
library(stringr)

# Set path to Shared Google Drive (G Drive)
g_drive <- "G:/Shared drives/ABMI Camera Mammals/"

# Read in data
osm_dens_wide <- read_csv(paste0(g_drive, "results/density/deployments/osm_all-years_density_wide_2022-12-06.csv"))
cmu_dens_wide <- read_csv(paste0(g_drive, "results/density/deployments/cmu_all-years_density_wide_2022-12-07.csv"))

#-----------------------------------------------------------------------------------------------------------------------

# Filter for only trail/random comparison deployments:
osm_trail <- osm_dens_wide |>
  filter(str_detect(location, "A$|B$")) |>
  arrange(location)

cmu_trail <- cmu_dens_wide |>
  filter(str_detect(location, "CHR") & str_detect(project, "2018|2019") | str_detect(location, "ADE") & str_detect(project, "2020"))

#-----------------------------------------------------------------------------------------------------------------------

# Write
write_csv(osm_trail, paste0(g_drive, "results/density/deployments/trail/osm_trail-random_density_wide_2022-12-07.csv"))
write_csv(cmu_trail, paste0(g_drive, "results/density/deployments/trail/cmu_trail-random_density_wide_2022-12-07.csv"))





