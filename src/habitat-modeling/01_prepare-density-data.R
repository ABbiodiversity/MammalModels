#-----------------------------------------------------------------------------------------------------------------------

# Project(s):      ABMI EH, ABMI OG, CMU, BG, NWSAR

# Title:            Prepare data for habitat modeling
# Description:      Prepare animal density, point veghf, and location data for habitat modeling.
# Author:           Marcus Becker, David J. Huggard

# Previous scripts:

#-----------------------------------------------------------------------------------------------------------------------

# Load packages
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(janitor)

# Root directory (Google Drive)
g_drive <- "G:/Shared drives/ABMI Camera Mammals/"

#-----------------------------------------------------------------------------------------------------------------------

# Import density data (long form):
df_dens_abmicmu <- read_csv(paste0(g_drive, "results/density/deployments/abmi-cmu_all-years_density_long_2021-10-07.csv"))
df_dens_nwsar   <- read_csv(paste0(g_drive, "results/density/deployments/nwsar_all-years_density_long_2021-07-23.csv"))
df_dens_bg      <- read_csv(paste0(g_drive, "results/density/deployments/bg_all-years_density_long_2021-10-31.csv"))

# All species (presumably all are in ABMI/CMU)
species <- df_dens_abmicmu |>
  select(common_name) |>
  distinct()

# Missing species in NWSAR and BG
missing_nwsar <- species |>
  anti_join(df_dens_nwsar, by = "common_name")
missing_bg <- species |>
  anti_join(df_dens_bg, by = "common_name")

# Create dataframes of NWSAR and BG deployments with missing species (0 or NA for density)
df_dens_nwsar_missing <- df_dens_nwsar |>
  select(location, project, season, total_season_days) |>
  distinct() |>
  crossing(missing_nwsar) |>
  arrange(location, common_name) |>
  # Density is NA if total season days is 0
  mutate(density_km2 = ifelse(total_season_days > 0, 0, NA))

df_dens_bg_missing <- df_dens_bg |>
  select(location, project, season, total_season_days) |>
  distinct() |>
  crossing(missing_bg) |>
  arrange(location, common_name) |>
  # Density is NA if total season days is 0
  mutate(density_km2 = ifelse(total_season_days > 0, 0, NA))

# Bind together
df_dens_all <- df_dens_abmicmu |>
  # NWSAR, including missing
  bind_rows(df_dens_nwsar, df_dens_nwsar_missing) |>
  # BG, including missing
  bind_rows(df_dens_bg, df_dens_bg_missing)

# Widen
df_season_days <- df_dens_all |>
  select(location, project, season, total_season_days) |>
  distinct() |>
  pivot_wider(id_cols = c(location, project), names_from = season, values_from = total_season_days)

df_dens_all_wide <- df_dens_all |>
  pivot_wider(id_cols = c(location, project), names_from = c(common_name, season), values_from = density_km2) |>
  left_join(df_season_days, by = c("location", "project")) |>
  select(location, project, summer, winter, everything()) |>
  # Clean up column names
  clean_names() |>
  # Remove SK CMU grids: LRN, AUB, MCC, DEW
  filter(!str_detect(location, "LRN|AUB|MCC|DEW"))

# Save file
write_csv(df_dens_all_wide,
          paste0(g_drive, "results/density/deployments/abmi-cmu-bg-nwsar_all-years_density_wide_", Sys.Date(), ".csv"))

#-----------------------------------------------------------------------------------------------------------------------

# Point VegHF (from GIS)
df_pveghf_abmicmu <- read_csv(paste0(g_drive, "data/lookup/veghf/abmi-cmu_all-years_veghf-soilhf-detdistveg_2021-10-06.csv"))
# Confusingly, NWSAR also contains 3 grids of CMU 2020.
df_pveghf_nwsar   <- read_csv(paste0(g_drive, "data/lookup/veghf/nwsar-cmu_2020_veghf-point_2021-10-31.csv"))
df_pveghf_bg      <- read_csv(paste0(g_drive, "data/lookup/veghf/bg_2016_veghf-point_2021-10-31.csv"))

# I wonder what we need the detdistveg for. Maybe I will leave that out for now. I'm sure it will come up, Dave was
# pretty adamant about needing it.

# Combine
df_pveghf_all <- df_pveghf_abmicmu |>
  select(1:3) |>
  bind_rows(df_pveghf_bg, df_pveghf_nwsar) |>
  # Remove SK grids
  filter(!str_detect(location, "LRN|AUB|MCC|DEW"))
  # Still quite a few NAs: random CMU, Amphibian Monitoring, Southern Focal, etc.

# 150m VegHF (from GIS)
df_150veghf_abmi  <- read_csv(paste0(g_drive, "data/lookup/veghf/abmi_all-years_veghf-150m-buffer_2021-11-25.csv"))
# Note: CMU is missing project. Why, Marcus, why?
# Well, we can create a crossing of every CMU project, and then join to the density estimates.
# Even if some of the grids weren't placed in every year, the join will eliminate those non-existent combos.
# But ... did HF change at all during those 4 years? Probably. Hmm. Something to look into.
df_150veghf_cmu   <- read_csv(paste0(g_drive, "data/lookup/veghf/cmu_all-years_veghf-150m-buffer_2021-10-29.csv")) |>
  crossing(project = c("CMU 2017",
                       "CMU 2018",
                       "CMU Ecosystem Monitoring Camera Program 2019",
                       "CMU Ecosystem Monitoring Camera Program 2020")) |>
  select(location, project, everything())
df_150veghf_nwsar <- read_csv(paste0(g_drive, "data/lookup/veghf/nwsar_2020_veghf-150m-buffer_2021-11-25.csv"))
df_150veghf_bg    <- read_csv(paste0(g_drive, "data/lookup/veghf/bg_2016_veghf-150m-buffer_2021-10-31.csv"))

# Combine
df_150veghf_all <- bind_rows(
  df_150veghf_abmi,
  df_150veghf_cmu,
  df_150veghf_nwsar,
  df_150veghf_bg
)

# Land facets - we'll include this some other time. I'm really only focused on the north models right now.

# Save files
write_csv(df_pveghf_all, paste0(g_drive, "data/lookup/veghf/abmi-cmu-nwsar-bg_all-years_veghf_", Sys.Date(), ".csv"))

#-----------------------------------------------------------------------------------------------------------------------

# Locations - nearest ABMI site. Seems like this will be handled at a later step.

df_loc_abmicmu <- read_csv(paste0(g_drive, "data/lookup/locations/abmi-cmu_public-locations_2021-10-20.csv"))

df_loc_nwsar <- read_csv(paste0(g_drive, "data/lookup/locations/nwsar_public-locations_2021-09-12.csv"))

df_loc_bg <- read_csv(paste0(g_drive, "data/lookup/locations/bg_public-locations_2022-01-10.csv"))

# Bind together and save
bind_rows(df_loc_abmicmu, df_loc_nwsar, df_loc_bg) |>
  write_csv(paste0(g_drive, "data/lookup/locations/abmi-cmu-nwsar-bg_public-locations_", Sys.Date(), ".csv"))

#-----------------------------------------------------------------------------------------------------------------------
