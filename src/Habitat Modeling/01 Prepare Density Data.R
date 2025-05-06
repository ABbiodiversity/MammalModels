#-----------------------------------------------------------------------------------------------------------------------

# Project(s):       EH, OG, OSM, CMU, BG, NWSAR

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
library(purrr)

# Root directory (Google Drive)
g_drive <- "G:/Shared drives/ABMI Camera Mammals/"

#-----------------------------------------------------------------------------------------------------------------------

# Import density data (long form):

projects <- paste0(c("eh", "cmu", "osm", "og", "nwsar", "bg"), collapse = "|")

# List files
files <- list.files(path = paste0(g_drive, "results/density/deployments"), full.names = TRUE) |>
  str_subset(pattern = projects) |>
  # Remove wide form
  str_subset(pattern = "wide", negate = TRUE)

# Read in density data
dens <- map_df(.x = files, .f = read_csv)

# All species
sp <- unique(dens$common_name) # 39 total have been detected in these projects

# In order to combine, need to fill out missing species x location x season combinations
missing <- dens |>
  select(project, location, season, total_season_days) |>
  distinct() |>
  crossing(common_name = sp) |>
  anti_join(dens, by = c("project", "location", "season", "total_season_days", "common_name")) |>
  arrange(location, common_name) |>
  # Density is NaN if totaly season days is 0
  mutate(density_km2 = ifelse(total_season_days > 0, 0, NaN))

# Bind together
dens_all <- dens |>
  bind_rows(missing)

# Check
check <- dens_all |>
  group_by(project, location, season) |>
  tally() # Should have 39 records (one for each species)

# Widen for modeling
season_days <- dens_all |>
  select(project, location, season, total_season_days) |>
  distinct() |>
  pivot_wider(id_cols = c(project, location), names_from = season, values_from = total_season_days)

dens_all_wide <- dens_all |>
  pivot_wider(id_cols = c(project, location), names_from = c(common_name, season), values_from = density_km2) |>
  left_join(season_days, by = c("project", "location")) |>
  select(project, location, summer, winter, everything()) |>
  # Remove SK CMU grids: LRN, AUB, MCC, DEW
  filter(!str_detect(location, "LRN|AUB|MCC|DEW"))

# Write new data
write_csv(dens_all_wide, paste0(g_drive, "results/density/deployments/all-projects_all-years_wide-density-for-habitat-modeling.csv"))

#-----------------------------------------------------------------------------------------------------------------------

# Point VegHF (from GIS and subsequent manual checking)

library(googlesheets4)
library(googledrive)

# Get VegHF information from Google Sheets for these projects (2019 and more recent):
# - EH 2019-2022
# - OG 2019 & 2020 (i.e., Focal Areas)
# - NWSAR
# Note: Does not include any CMU

# Sheet IDs
veghf_sheets <- drive_find(type = "spreadsheet", shared_drive = "ABMI Camera Mammals") |>
  filter(str_detect(name, "VegHF Checks")) |>
  select(id) |>
  pull()

# Read in relevant sheets
veghf_recent <- map_df(.x = veghf_sheets, .f = ~ read_sheet(ss = .x)) |>
  # Correct VegHF based on manual checking - done by Dave H.
  mutate(VEGHFAGEclass = ifelse(is.na(CheckedVEGHFAGEclass), VEGHFAGEclass, CheckedVEGHFAGEclass)) |>
  select(project, location, VEGHFAGEclass)

# More recent CMU projects (2019-2021) - prepared from prepare-veghf script.
veghf_recent_cmu <- read_csv(paste0(g_drive, "data/lookup/veghf/abmi-cmu-nwsar_2019-2022_vegsoilhf_raw_2022-12-06.csv")) |>
  filter(str_detect(project, "CMU")) |>
  select(project, location, VEGHFAGEclass)

# Older projects - prepared from prepare-veghf script:
# - EH 2015-2018
# - CMU 2017 & 2018
# - OG 2015-2018
veghf_older <- read_csv(paste0(g_drive, "data/lookup/veghf/abmi-cmu_2013-2018_vegsoilhf-detdistveg_2022-12-06.csv")) |>
  select(project, location, VEGHFAGEclass)

unique(veghf_older$project)

# Big Grid
veghf_bg <- read_csv(paste0(g_drive, "data/lookup/veghf/bg_2016_veghf-point_2021-10-31.csv")) |>
  select(project, location, VEGHFAGEclass)

# OSM (no ACME included here)

# Verify using metadata specifically for certain OSM deployments
osm_metadata <- read_csv(paste0(g_drive, "projects/osm-badr-site-selection/osm_2021_deployment-metadata.csv")) |>
  select(project, location, treatment, fine_scale) |>
  # On-footprint for the following treatments: dense linear features, high activity in situ, low activity well pads
  filter(str_detect(treatment, "dense|activity"),
         fine_scale == "On") |>
  mutate(VEGHFAGEclass_upd = case_when(
    str_detect(treatment, "dense") ~ "SeismicLineNarrow",
    str_detect(treatment, "low") ~ "WellSite",
    str_detect(treatment, "high") ~ "IndustrialSiteRural"
  )) |>
  select(1, 2, 5)

veghf_osm <- read_csv(paste0(g_drive, "data/lookup/veghf/abmi-cmu-nwsar_2019-2022_vegsoilhf_raw_2022-12-06.csv")) |>
  filter(str_detect(project, "OSM")) |>
  select(project, location, VEGHFAGEclass) |>
  left_join(osm_metadata) |>
  # Updated GIS information if needed
  mutate(VEGHFAGEclass = ifelse(is.na(VEGHFAGEclass_upd), VEGHFAGEclass, VEGHFAGEclass_upd)) |>
  select(-VEGHFAGEclass_upd)

# Combine all projects together
veghf_all <- bind_rows(
  veghf_recent,
  veghf_recent_cmu,
  veghf_older,
  veghf_bg,
  veghf_osm) |>
  # Remove the SK grids from the CMU projects
  filter(!str_detect(location, "LRN|AUB|MCC|DEW"))

# All the projects
unique(veghf_all$project)

# Write
write_csv(veghf_all, paste0(g_drive, "data/lookup/veghf/all-projects_all-years_veghfageclass-for-habitat-modeling.csv"))

#-----------------------------------------------------------------------------------------------------------------------

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

# Locations - nearest ABMI site.

#-----------------------------------------------------------------------------------------------------------------------
