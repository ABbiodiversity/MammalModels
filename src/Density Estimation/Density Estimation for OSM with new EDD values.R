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

summer_green <- 143

# Find appropriate tag data file
file <- list.files(path = paste0(g_drive, "data/base/clean"), full.names = TRUE) |>
  str_subset(pattern = paste0(proj, years, "_all-data_clean"))
# Import
osm_clean <- read_csv(file)

# OSM image report (simple)
image_fov_trigger <- read_csv(paste0(g_drive, "data/lookup/image-reports/", proj, years, "_image-report_simple.csv"))

# We have to import the snow information for each deployment because that impacts the season designations
df_snow <- read_csv(paste0(g_drive, "data/lookup/veghf/VegForDetectionDistance/New/EDD (Re-) Modeling OSM.csv")) |>
  select(project, location, snow_start, snow_gone) |>
  mutate_at(c("snow_start", "snow_gone"), as.Date)

# No snow information (these are deployments I didn't redo the EDD categories for)
df_no_snow <- df_snow |>
  filter(is.na(snow_start) & is.na(snow_gone)) |>
  select(project, location) |>
  distinct()

#-----------------------------------------------------------------------------------------------------------------------

# Re-do TBD processing
df_tbd <- get_operating_days(
  image_report = image_fov_trigger,
  # Keep project
  include_project = TRUE,
  # Summarise is now FALSE
  summarise = FALSE) |>
  mutate(operating = 1) |>
  separate(project_location, into = c("project", "location"), sep = "_") |>
  anti_join(df_no_snow, by = c("project", "location")) |>
  left_join(df_snow, by = c("project", "location")) |>
  mutate(season_new1 = ifelse(date > snow_start, "snow", "nonsnow")) |>
  # Snow gone early for season 2 - doesn't matter for OSM
  mutate(snow_gone_early = snow_gone %m-% days(10)) |>
  mutate(season_new2 = ifelse(date > snow_start, "snow", "nonsnow")) |>
  mutate(julian = as.numeric(format(date, "%j"))) |>
  # I think this is done funky - something to look into another time.
  mutate(season_new3 = case_when(
    date >= snow_start ~ "winter",
    date < snow_start & julian > summer_green ~ "summer",
    julian <= summer_green ~ "spring"
  )) |>
  select(project, location, date, operating, season_new1, season_new2, season_new3) |>
  # Need to consider project and location together because otherwise group_by will create phantom combinations with .drop = FALSE
  unite("project_location", c(project, location), sep = "_")

# New Season 1
df_tbd_new1 <- df_tbd |>
  select(project_location, date, operating, season_new1) |>
  mutate_at(c("project_location", "season_new1"), factor) |>
  group_by(project_location, season_new1, .drop = FALSE) |>
  summarise(operating_days = sum(operating)) |>
  ungroup() |>
  mutate_if(is.factor, as.character) |>
  group_by(project_location) |>
  mutate(total_days = sum(operating_days)) |>
  pivot_wider(id_cols = c(project_location, total_days), names_from = season_new1, values_from = operating_days) |>
  ungroup()

# New Season 2
df_tbd_new2 <- df_tbd |>
  select(project_location, date, operating, season_new2) |>
  mutate_at(c("project_location", "season_new2"), factor) |>
  group_by(project_location, season_new2, .drop = FALSE) |>
  summarise(operating_days = sum(operating)) |>
  ungroup() |>
  mutate_if(is.factor, as.character) |>
  group_by(project_location) |>
  mutate(total_days = sum(operating_days)) |>
  pivot_wider(id_cols = c(project_location, total_days), names_from = season_new2, values_from = operating_days) |>
  ungroup()

# New Season 3
df_tbd_new3 <- df_tbd |>
  select(project_location, date, operating, season_new3) |>
  mutate_at(c("project_location", "season_new3"), factor) |>
  group_by(project_location, season_new3, .drop = FALSE) |>
  summarise(operating_days = sum(operating)) |>
  ungroup() |>
  mutate_if(is.factor, as.character) |>
  group_by(project_location) |>
  mutate(total_days = sum(operating_days)) |>
  pivot_wider(id_cols = c(project_location, total_days), names_from = season_new3, values_from = operating_days) |>
  ungroup()

# No seasonal effects
df_tbd_ns <- df_tbd |>
  select(project_location, date, operating) |>
  mutate_at(c("project_location"), factor) |>
  group_by(project_location, .drop = FALSE) |>
  summarise(total_days = sum(operating)) |>
  ungroup() |>
  mutate_if(is.factor, as.character)

#-----------------------------------------------------------------------------------------------------------------------

# Calculate time in front of camera (TIFC)

df_tt <- osm_clean |>
  # First calculate time by series
  calculate_time_by_series() |>
  # Attach snow start and snow gone information
  left_join(df_snow, by = c("project", "location")) |>
  # Remove cameras with no snow information (that I didn't do new EDD categories for)
  anti_join(df_no_snow, by = c("project", "location")) |>
  mutate(season_new1 = ifelse(series_start > snow_start, "snow", "nonsnow")) |>
  # Snow gone early for season 2 - doesn't matter for OSM
  mutate(snow_gone_early = snow_gone %m-% days(10)) |>
  mutate(season_new2 = ifelse(series_start > snow_start, "snow", "nonsnow")) |>
  mutate(julian = as.numeric(format(series_start, "%j"))) |>
  # I think this is done funky - something to look into another time.
  mutate(season_new3 = case_when(
    series_start >= snow_start ~ "winter",
    series_start < snow_start & julian > summer_green ~ "summer",
    julian <= summer_green ~ "spring")) |>
  select(-c(snow_start, snow_gone, snow_gone_early, julian)) |>
  # Need to consider project and location together because otherwise group_by will create phantom combinations with .drop = FALSE
  unite("project_location", c(project, location), sep = "_")

# Unique species seen
sp <- as.character(sort(unique(df_tt$species_common_name)))

# New Season 1
df_tt_new1 <- df_tt |>
  mutate_at(c("project_location", "species_common_name", "season_new1"), factor) |>
  group_by(project_location, species_common_name, season_new1, .drop = FALSE) |>
  summarise(total_duration = sum(series_total_time)) |>
  ungroup() |>
  mutate_if(is.factor, as.character) |>
  left_join(df_tbd_new1, by = c("project_location"))

df_tt_nn_new1 <- df_tbd_new1 |>
  # Retrieve only those that had no images of animals
  anti_join(df_tt_new1, by = "project_location") |>
  crossing(season_new1 = c("snow", "nonsnow"), species_common_name = sp) |>
  # Add total_duration column, which is zero in these cases
  mutate(total_duration = 0)

df_tt_full_new1 <- df_tt_new1 |>
  bind_rows(df_tt_nn_new1) |>
  arrange(project_location, species_common_name, season_new1) |>
  mutate(total_season1_days = ifelse(season_new1 == "snow", snow, nonsnow)) |>
  separate(project_location, into = c("project", "location"), sep = "_", remove = TRUE, extra = "merge") |>
  select(project, location, species_common_name, season_new1, total_season1_days, total_duration)

# New Season 2
df_tt_new2 <- df_tt |>
  mutate_at(c("project_location", "species_common_name", "season_new2"), factor) |>
  group_by(project_location, species_common_name, season_new2, .drop = FALSE) |>
  summarise(total_duration = sum(series_total_time)) |>
  ungroup() |>
  mutate_if(is.factor, as.character) |>
  left_join(df_tbd_new2, by = c("project_location"))

df_tt_nn_new2 <- df_tbd_new2 |>
  # Retrieve only those that had no images of animals
  anti_join(df_tt_new2, by = "project_location") |>
  # Same two seasons as new season 1
  crossing(season_new2 = c("snow", "nonsnow"), species_common_name = sp) |>
  # Add total_duration column, which is zero in these cases
  mutate(total_duration = 0)

df_tt_full_new2 <- df_tt_new2 |>
  bind_rows(df_tt_nn_new2) |>
  arrange(project_location, species_common_name, season_new2) |>
  mutate(total_season2_days = ifelse(season_new2 == "snow", snow, nonsnow)) |>
  separate(project_location, into = c("project", "location"), sep = "_", remove = TRUE, extra = "merge") |>
  select(project, location, species_common_name, season_new2, total_season2_days, total_duration)

# New Season 3
df_tt_new3 <- df_tt |>
  mutate_at(c("project_location", "species_common_name", "season_new3"), factor) |>
  group_by(project_location, species_common_name, season_new3, .drop = FALSE) |>
  summarise(total_duration = sum(series_total_time)) |>
  ungroup() |>
  mutate_if(is.factor, as.character) |>
  left_join(df_tbd_new3, by = c("project_location"))

df_tt_nn_new3 <- df_tbd_new3 |>
  # Retrieve only those that had no images of animals
  anti_join(df_tt_new3, by = "project_location") |>

  crossing(season_new3 = c("spring", "summer", "winter"), species_common_name = sp) |>
  # Add total_duration column, which is zero in these cases
  mutate(total_duration = 0)

df_tt_full_new3 <- df_tt_new3 |>
  bind_rows(df_tt_nn_new3) |>
  arrange(project_location, species_common_name, season_new3) |>
  mutate(total_season3_days = case_when(
    season_new3 == "spring" ~ spring,
    season_new3 == "summer" ~ summer,
    season_new3 == "winter" ~ winter
  )) |>
  separate(project_location, into = c("project", "location"), sep = "_", remove = TRUE, extra = "merge") |>
  select(project, location, species_common_name, season_new3, total_season3_days, total_duration)

# For species where there are no seasonal effects on EDD
df_tt_ns <- df_tt |>
  mutate_at(c("project_location", "species_common_name"), factor) |>
  group_by(project_location, species_common_name, .drop = FALSE) |>
  summarise(total_duration = sum(series_total_time)) |>
  ungroup() |>
  mutate_if(is.factor, as.character) |>
  left_join(df_tbd_ns, by = c("project_location"))

df_tt_nn_ns <- df_tbd_ns |>
  # Retrieve only those that had no images of animals
  anti_join(df_tt_ns, by = "project_location") |>
  # Same two seasons as new season 1
  crossing(species_common_name = sp) |>
  # Add total_duration column, which is zero in these cases
  mutate(total_duration = 0)

df_tt_full_ns <- df_tt_ns |>
  bind_rows(df_tt_nn_ns) |>
  arrange(project_location, species_common_name) |>
  separate(project_location, into = c("project", "location"), sep = "_", remove = TRUE, extra = "merge") |>
  select(project, location, species_common_name, total_days, total_duration)

#-----------------------------------------------------------------------------------------------------------------------

# Bears -> No season, secondary category
# Coyote -> season_new3, secondary category
# Deer -> No season, secondary category
# Gray Wolf -> No season, secondary category
# Hare -> season_new1, secondary category
# Large Ungulates -> season_new3, secondary category
# Lynx -> season_new2, secondary category
# Small Mustelids -> No season, secondary category

#-----------------------------------------------------------------------------------------------------------------------

# Calculate density at each location

df_vegdetdist <- read_csv(paste0(g_drive, "data/lookup/veghf/VegForDetectionDistance/New/EDD (Re-) Modeling OSM.csv")) |>
  select(project, location, primary_category, secondary_category) |>
  filter(!is.na(primary_category))

cam_fov_angle <- 40

# New Season 1
# Only Snowshoe Hare
hare <- read_csv(paste0(g_drive, "data/processed/detection-distance/predictions/New/Hare.csv"))

df_dens_seas1 <- df_tt_full_new1 |>
  filter(species_common_name == "Snowshoe Hare") |>
  # Join new categories
  left_join(df_vegdetdist) |>
  mutate(secondary_category = paste0(primary_category, "_", secondary_category)) |>
  left_join(hare, by = c("secondary_category", "season_new1")) |>
  select(-c(primary_category, dist_group)) |>
  # Calculate density
  mutate(effort = total_season1_days * (prediction ^ 2 * pi * (cam_fov_angle / 360)) / 100,
         # Catch per unit effort
         cpue = total_duration / effort,
         # Catch per unit effort in km2
         cpue_km2 = cpue / 60 / 60 / 24 * 10000) |>
  # All the NaNs are just combinations where the total_seasons_days is 0.
  select(project, location, species_common_name, season_new1, total_season1_days, total_duration, density_km2 = cpue_km2) |>
  group_by(project, location, species_common_name) |>
  summarise(density_km2 = weighted.mean(density_km2, w = total_season1_days),
            total_days = sum(total_season1_days))

# New Season 2
# Only Lynx
lynx <- read_csv(paste0(g_drive, "data/processed/detection-distance/predictions/New/Lynx.csv"))

df_dens_seas2 <- df_tt_full_new2 |>
  filter(species_common_name == "Canada Lynx") |>
  # Join new categories
  left_join(df_vegdetdist) |>
  mutate(secondary_category = paste0(primary_category, "_", secondary_category)) |>
  left_join(lynx, by = c("secondary_category", "season_new2")) |>
  select(-c(primary_category, dist_group)) |>
  # Calculate density
  mutate(effort = total_season2_days * (prediction ^ 2 * pi * (cam_fov_angle / 360)) / 100,
         # Catch per unit effort
         cpue = total_duration / effort,
         # Catch per unit effort in km2
         cpue_km2 = cpue / 60 / 60 / 24 * 10000) |>
  # All the NaNs are just combinations where the total_seasons_days is 0.
  select(project, location, species_common_name, season_new2, total_season2_days, total_duration, density_km2 = cpue_km2) |>
  group_by(project, location, species_common_name) |>
  summarise(density_km2 = weighted.mean(density_km2, w = total_season2_days),
            total_days = sum(total_season2_days))

# New Season 3
coyote <- read_csv(paste0(g_drive, "data/processed/detection-distance/predictions/New/Coyote.csv"))
ungulates <- read_csv(paste0(g_drive, "data/processed/detection-distance/predictions/New/LargeUngulates.csv"))

# Create value for Coyote F-C-C Winter
coyote_cc_winter <- coyote |>
  filter(secondary_category == "Forested_Closed-Closed") |>
  group_by(secondary_category, dist_group) |>
  summarise(prediction = mean(prediction)) |>
  mutate(season_new3 = "winter")

# Create value for Large Ungulates F-C-C and O-O Winter
lu_cc_oo_winter <- ungulates |>
  filter(secondary_category == "Forested_Closed-Closed" | secondary_category == "Open_Open") |>
  group_by(secondary_category, dist_group) |>
  summarise(prediction = mean(prediction)) |>
  mutate(season_new3 = "winter")

coy_ung <- bind_rows(coyote, ungulates, coyote_cc_winter, lu_cc_oo_winter)

scn_1 <- c("Coyote", "Moose", "Woodland Caribou")

df_dens_seas3 <- df_tt_full_new3 |>
  filter(species_common_name %in% scn_1) |>
  # Join new categories
  left_join(df_vegdetdist) |>
  mutate(secondary_category = paste0(primary_category, "_", secondary_category),
         dist_group = ifelse(str_detect(species_common_name, "Coyote"), "Coyote", "LargeUngulates")) |>
  left_join(coy_ung, by = c("secondary_category", "season_new3", "dist_group")) |>
  select(-c(primary_category, dist_group)) |>
  # Calculate density
  mutate(effort = total_season3_days * (prediction ^ 2 * pi * (cam_fov_angle / 360)) / 100,
         # Catch per unit effort
         cpue = total_duration / effort,
         # Catch per unit effort in km2
         cpue_km2 = cpue / 60 / 60 / 24 * 10000) |>
  # All the NaNs are just combinations where the total_seasons_days is 0.
  select(project, location, species_common_name, season_new3, total_season3_days, total_duration, density_km2 = cpue_km2) |>
  # Combine with weighted mean
  group_by(project, location, species_common_name) |>
  summarise(density_km2 = weighted.mean(density_km2, w = total_season3_days),
            total_days = sum(total_season3_days))

# Now species with no seasonal effects
# Bears, Deer, Gray Wolf, Small Mustelids
bears <- read_csv(paste0(g_drive, "data/processed/detection-distance/predictions/New/Bear.csv"))
deer <- read_csv(paste0(g_drive, "data/processed/detection-distance/predictions/New/Deer.csv"))
wolf <- read_csv(paste0(g_drive, "data/processed/detection-distance/predictions/New/Wolf.csv"))
mustelids <- read_csv(paste0(g_drive, "data/processed/detection-distance/predictions/New/SmallMustelids.csv"))

must_pred <- mean(mustelids$prediction)

mustelids_all <- data.frame(
  dist_group = "SmallMustelids",
  secondary_category = c("Open_Open", "Shrubby_Open", "Shrubby_Closed"),
  prediction = must_pred) |>
  bind_rows(mustelids)

bdwm <- bind_rows(bears, deer, wolf, mustelids_all)

scn_2 <- c("Black Bear", "White-tailed Deer", "Gray Wolf", "Fisher", "Marten")

# Snow for Bears (we don't want to count the days during the winter)
df_snow_bears <- df_tbd_new1 |>
  select(project_location, snow) |>
  separate(project_location, into = c("project", "location"), sep = "_")

df_dens_ns <- df_tt_full_ns |>
  filter(species_common_name %in% scn_2) |>
  # Join new categories
  left_join(df_vegdetdist) |>
  mutate(secondary_category = paste0(primary_category, "_", secondary_category),
         dist_group = case_when(
           species_common_name == "Black Bear" ~ "Bear",
           species_common_name == "Gray Wolf" ~ "Gray Wolf",
           species_common_name == "White-tailed Deer" ~ "Deer",
           str_detect(species_common_name, "Fisher|Marten") ~ "SmallMustelids")) |>
  left_join(bdwm, by = c("secondary_category", "dist_group")) |>
  left_join(df_snow_bears, by = c("project", "location")) |>
  mutate(total_days = ifelse(species_common_name == "Black Bear", total_days - snow, total_days)) |>
  select(-c(primary_category, dist_group, snow)) |>
  # Calculate density
  mutate(effort = total_days * (prediction ^ 2 * pi * (cam_fov_angle / 360)) / 100,
         # Catch per unit effort
         cpue = total_duration / effort,
         # Catch per unit effort in km2
         cpue_km2 = cpue / 60 / 60 / 24 * 10000) |>
  # All the NaNs are just combinations where the total_seasons_days is 0.
  select(project, location, species_common_name, total_days, density_km2 = cpue_km2)

#-----------------------------------------------------------------------------------------------------------------------

# Put all the densities together

dens_all <- bind_rows(df_dens_seas1,
                      df_dens_seas2,
                      df_dens_seas3,
                      df_dens_ns) |>
  arrange(project, location, species_common_name)

#-----------------------------------------------------------------------------------------------------------------------

# Now what do we do for the EH cameras?

# Supplemental camera deployment treatment metadata: EH & OG
eh <- read_csv(paste0(g_drive, "projects/osm-badr-site-selection/supplemental/final/supplemental-osm-treatments_EH_2022-06-16.csv")) |>
  select(location, project)

og <- read_csv(paste0(g_drive, "projects/osm-badr-site-selection/supplemental/final/supplemental-osm-treatments_OG_2022-06-16.csv")) |>
  select(location, project) |>
  mutate(location = ifelse(str_detect(project, "2018"), paste0(location, "-1"), location))

# Obtain "main report" data for these locations
# Project
proj <- "eh"
# Years
years <- "_14-15-16-17-18"

# Find appropriate tag data file
file <- list.files(path = paste0(g_drive, "data/base/clean"), full.names = TRUE) |>
  str_subset(pattern = paste0(proj, years, "_all-data_clean"))
# Import
eh_early_tags_clean <- read_csv(file) |>
  inner_join(eh, by = c("project", "location"))

# EH image report (simple)
eh_early_image <- read_csv(paste0(g_drive, "data/lookup/image-reports/", proj, years, "_image-report_simple.csv")) |>
  inner_join(eh, by = c("project", "location"))

# Reminder: Haven't done EH 2019 yet (tagged the EDD categories). So we're going to leave it out.
# Also, pretty sure there's a bit more data available (I haven't sorted into OS stressor treatments)

# Project
proj <- "og"

# Years
years <- "_all-years"

# Find appropriate tag data file
file <- list.files(path = paste0(g_drive, "data/base/clean"), full.names = TRUE) |>
  str_subset(pattern = paste0(proj, years, "_all-data_clean"))
# Import
og_tags_clean <- read_csv(file) |>
  inner_join(og, by = c("project", "location"))

# OG image report (simple)
og_image <- read_csv(paste0(g_drive, "data/lookup/image-reports/", proj, years, "_image-report_simple.csv")) |>
  inner_join(og, by = c("project", "location"))

# Join all together
sup <- bind_rows(og_tags_clean, eh_early_tags_clean)
rm(og_tags_clean, eh_early_tags_clean, eh_late_tags_clean)
sup_img <- bind_rows(og_image, eh_early_image)
rm(og_image, eh_early_image, eh_late_image)

sup_img <- sup_img |>
  select(project, location, image_date_time = date_detected, image_fov = field_of_view, image_trigger_mode = trigger) |>
  as.data.frame()

# We have to import the snow information for each deployment because that impacts the season designations
df_snow_og <- read_csv(paste0(g_drive, "data/lookup/veghf/VegForDetectionDistance/New/EDD (Re-) Modeling ABMI Off-Grid.csv")) |>
  filter(osm == "Yes") |>
  select(project, location, snow_start, snow_gone) |>
  mutate_at(c("snow_start", "snow_gone"), as.Date) |>
  inner_join(og)

df_snow_eh <- read_csv(paste0(g_drive, "data/lookup/veghf/VegForDetectionDistance/New/EDD (Re-) Modeling ABMI EH (2014-2018).csv")) |>
  filter(osm == "Yes") |>
  select(project, location, snow_start, snow_gone) |>
  mutate_at(c("snow_start", "snow_gone"), as.Date) |>
  inner_join(eh)

df_snow_sup <- bind_rows(df_snow_og, df_snow_eh)

# No snow information (these are deployments I didn't redo the EDD categories for)
df_no_snow <- df_snow_sup |>
  filter(is.na(snow_start) & is.na(snow_gone)) |>
  select(project, location) |>
  distinct()

# Something to put a pin in: missing Northern Focal 2020 (should be the same EDD categories as 2020) as well as EH 2019

#-----------------------------------------------------------------------------------------------------------------------

# Re-do TBD processing - EH first (easy)

df_tbd_eh <- get_operating_days(
  image_report = sup_img,
  # Keep project
  include_project = TRUE,
  # Summarise is now FALSE
  summarise = FALSE) |>
  mutate(operating = 1) |>
  separate(project_location, into = c("project", "location"), sep = "_") |>
  # Just EH for now
  filter(str_detect(project, "Ecosystem Health")) |>
  anti_join(df_no_snow, by = c("project", "location")) |>
  left_join(df_snow_eh, by = c("project", "location")) |>
  mutate(season_new1 = ifelse(date <= snow_gone, "snow", "nonsnow")) |>
  # Snow gone early for season 2
  mutate(snow_gone_early = snow_gone %m-% days(10)) |>
  mutate(julian_sge = as.numeric(format(snow_gone_early, "%j"))) |>
  mutate(season_new2 = ifelse(date <= snow_gone_early, "snow", "nonsnow")) |>
  mutate(julian = as.numeric(format(date, "%j"))) |>
  # I think this is done funky - something to look into another time.
  mutate(season_new3 = case_when(
    date < snow_gone_early ~ "winter",
    date >= snow_gone_early & julian < summer_green ~ "spring",
    TRUE ~ "summer")) |>
  select(project, location, date, operating, season_new1, season_new2, season_new3) |>
  # Need to consider project and location together because otherwise group_by will create phantom combinations with .drop = FALSE
  unite("project_location", c(project, location), sep = "_")

# All the Off-Grids
sup_img1 <- sup_img |>
  filter(str_detect(project, "Focal|Off-Grid"))

df_tbd_og <- get_operating_days(
  image_report = sup_img1,
  # Keep project
  include_project = TRUE,
  # Summarise is now FALSE
  summarise = FALSE) |>
  mutate(operating = 1) |>
  separate(project_location, into = c("project", "location"), sep = "_") |>
  anti_join(df_no_snow, by = c("project", "location")) |>
  left_join(df_snow_og, by = c("project", "location")) |>
  select(project, location, date, operating, snow_gone, snow_start)

# Let's just look at Focal
focal <- df_tbd_og |>
  # Make seasons -> this was hard to do when deployments span multiple years!
  filter(str_detect(project, "Focal")) |>
  mutate(snow_gone2 = snow_gone %m+% years(1),
         snow_start2 = snow_start %m+% years(1)) |>
  mutate(julian = as.numeric(format(date, "%j"))) |>
  mutate(season_new1 = ifelse(date <= snow_gone | (date >= snow_start & date <= snow_gone2), "snow", "nonsnow")) |>
  # Snow gone early for season 2
  mutate(snow_gone_early = snow_gone %m-% days(10),
         snow_gone_early2 = snow_gone2 %m-% days(10)) |>
  mutate(julian_sge = as.numeric(format(snow_gone_early, "%j")),
         julian_sge2 = as.numeric(format(snow_gone_early2, "%j"))) |>
  mutate(season_new2 = ifelse(date <= snow_gone_early | (date >= snow_start & date <= snow_gone_early2), "snow", "nonsnow")) |>
  mutate(summer_green = as.Date("2019-05-23"),
         summer_green2 = as.Date("2020-05-23")) |>
  # I think this is done funky - something to look into another time.
  mutate(season_new3 = case_when(
    date <= snow_gone_early | (date > snow_start & date <= snow_gone_early2) ~ "winter",
    (date > snow_gone_early & date <= summer_green) | (date > snow_gone_early2 & date <= summer_green2)  ~ "spring",
    TRUE ~ "summer")) |>
  select(project, location, date, operating, season_new1, season_new2, season_new3) |>
  # Need to consider project and location together because otherwise group_by will create phantom combinations with .drop = FALSE
  unite("project_location", c(project, location), sep = "_")

# Other OG
other_og <- df_tbd_og |>
  filter(!str_detect(project, "Focal")) |>
  select(project, location, date, operating, snow_start, snow_gone) |>
  mutate(season_new1 = ifelse(date <= snow_gone, "snow", "nonsnow")) |>
  # Snow gone early for season 2
  mutate(snow_gone_early = snow_gone %m-% days(10)) |>
  mutate(julian_sge = as.numeric(format(snow_gone_early, "%j"))) |>
  mutate(season_new2 = ifelse(date <= snow_gone_early, "snow", "nonsnow")) |>
  mutate(julian = as.numeric(format(date, "%j"))) |>
  # I think this is done funky - something to look into another time.
  mutate(season_new3 = case_when(
    julian >= julian_sge & julian <= summer_green ~ "spring",
    julian < julian_sge ~ "winter",
    julian > summer_green ~ "summer")) |>
  select(project, location, date, operating, season_new1, season_new2, season_new3) |>
  # Need to consider project and location together because otherwise group_by will create phantom combinations with .drop = FALSE
  unite("project_location", c(project, location), sep = "_")

# Bring all together
df_tbd <- bind_rows(df_tbd_eh, other_og, focal)

# New Season 1
df_tbd_new1 <- df_tbd |>
  select(project_location, date, operating, season_new1) |>
  mutate_at(c("project_location", "season_new1"), factor) |>
  group_by(project_location, season_new1, .drop = FALSE) |>
  summarise(operating_days = sum(operating)) |>
  ungroup() |>
  mutate_if(is.factor, as.character) |>
  group_by(project_location) |>
  mutate(total_days = sum(operating_days)) |>
  pivot_wider(id_cols = c(project_location, total_days), names_from = season_new1, values_from = operating_days) |>
  ungroup()

# New Season 2
df_tbd_new2 <- df_tbd |>
  select(project_location, date, operating, season_new2) |>
  mutate_at(c("project_location", "season_new2"), factor) |>
  group_by(project_location, season_new2, .drop = FALSE) |>
  summarise(operating_days = sum(operating)) |>
  ungroup() |>
  mutate_if(is.factor, as.character) |>
  group_by(project_location) |>
  mutate(total_days = sum(operating_days)) |>
  pivot_wider(id_cols = c(project_location, total_days), names_from = season_new2, values_from = operating_days) |>
  ungroup()

# New Season 3
df_tbd_new3 <- df_tbd |>
  select(project_location, date, operating, season_new3) |>
  mutate_at(c("project_location", "season_new3"), factor) |>
  group_by(project_location, season_new3, .drop = FALSE) |>
  summarise(operating_days = sum(operating)) |>
  ungroup() |>
  mutate_if(is.factor, as.character) |>
  group_by(project_location) |>
  mutate(total_days = sum(operating_days)) |>
  pivot_wider(id_cols = c(project_location, total_days), names_from = season_new3, values_from = operating_days) |>
  ungroup()

# No seasonal effects
df_tbd_ns <- df_tbd |>
  select(project_location, date, operating) |>
  mutate_at(c("project_location"), factor) |>
  group_by(project_location, .drop = FALSE) |>
  summarise(total_days = sum(operating)) |>
  ungroup() |>
  mutate_if(is.factor, as.character)

#-----------------------------------------------------------------------------------------------------------------------

# Calculate time in front of camera (TIFC)

soi <- c("White-tailed Deer", "Black Bear", "Moose", "Fisher", "Marten",
         "Canada Lynx", "Coyote", "Snowshoe Hare", "Woodland Caribou", "Gray Wolf")

df_tt <- sup |>
  select(project, location, image_date_time = date_detected, species_common_name = common_name,
         individual_count = number_individuals) |>
  # First calculate time by series
  calculate_time_by_series() |>
  filter(species_common_name %in% soi) |>
  # Attach snow start and snow gone information
  left_join(df_snow_sup, by = c("project", "location")) |>
  # Remove cameras with no snow information (that I didn't do new EDD categories for)
  anti_join(df_no_snow, by = c("project", "location"))

# I think we need to do this separately by EH, OG, and Focal again (until we figure out how better to do this ...)

# EH
df_tt_eh <- df_tt |>
  # Just EH for now
  filter(str_detect(project, "Ecosystem Health")) |>
  mutate(season_new1 = ifelse(series_start <= snow_gone, "snow", "nonsnow")) |>
  # Snow gone early for season 2
  mutate(snow_gone_early = snow_gone %m-% days(10)) |>
  mutate(julian_sge = as.numeric(format(snow_gone_early, "%j"))) |>
  mutate(season_new2 = ifelse(series_start <= snow_gone_early, "snow", "nonsnow")) |>
  mutate(julian = as.numeric(format(series_start, "%j"))) |>
  # I think this is done funky - something to look into another time.
  mutate(season_new3 = case_when(
    series_start < snow_gone_early ~ "winter",
    series_start >= snow_gone_early & julian < summer_green ~ "spring",
    TRUE ~ "summer")) |>
  select(project:n_images, season_new1, season_new2, season_new3) |>
  # Need to consider project and location together because otherwise group_by will create phantom combinations with .drop = FALSE
  unite("project_location", c(project, location), sep = "_")

# Focal
df_tt_focal <- df_tt |>
  # Make seasons -> this was hard to do when deployments span multiple years!
  filter(str_detect(project, "Focal")) |>
  mutate(snow_gone2 = snow_gone %m+% years(1),
         snow_start2 = snow_start %m+% years(1)) |>
  mutate(julian = as.numeric(format(series_start, "%j"))) |>
  mutate(season_new1 = ifelse(series_start <= snow_gone | (series_start >= snow_start & series_start <= snow_gone2), "snow", "nonsnow")) |>
  # Snow gone early for season 2
  mutate(snow_gone_early = snow_gone %m-% days(10),
         snow_gone_early2 = snow_gone2 %m-% days(10)) |>
  mutate(julian_sge = as.numeric(format(snow_gone_early, "%j")),
         julian_sge2 = as.numeric(format(snow_gone_early2, "%j"))) |>
  mutate(season_new2 = ifelse(series_start <= snow_gone_early | (series_start >= snow_start & series_start <= snow_gone_early2), "snow", "nonsnow")) |>
  mutate(summer_green = as.Date("2019-05-23"),
         summer_green2 = as.Date("2020-05-23")) |>
  # I think this is done funky - something to look into another time.
  mutate(season_new3 = case_when(
    series_start <= snow_gone_early | (series_start > snow_start & series_start <= snow_gone_early2) ~ "winter",
    (series_start > snow_gone_early & series_start <= summer_green) | (series_start > snow_gone_early2 & series_start <= summer_green2)  ~ "spring",
    TRUE ~ "summer")) |>
  select(project:n_images, season_new1, season_new2, season_new3) |>
  # Need to consider project and location together because otherwise group_by will create phantom combinations with .drop = FALSE
  unite("project_location", c(project, location), sep = "_")

# Off-Grid
df_tt_og <- df_tt |>
  # Just EH for now
  filter(str_detect(project, "Off-Grid")) |>
  mutate(season_new1 = ifelse(series_start <= snow_gone, "snow", "nonsnow")) |>
  # Snow gone early for season 2
  mutate(snow_gone_early = snow_gone %m-% days(10)) |>
  mutate(julian_sge = as.numeric(format(snow_gone_early, "%j"))) |>
  mutate(season_new2 = ifelse(series_start <= snow_gone_early, "snow", "nonsnow")) |>
  mutate(julian = as.numeric(format(series_start, "%j"))) |>
  # I think this is done funky - something to look into another time.
  mutate(season_new3 = case_when(
    julian >= julian_sge & julian <= summer_green ~ "spring",
    julian < julian_sge ~ "winter",
    julian > summer_green ~ "summer")) |>
  select(project:n_images, season_new1, season_new2, season_new3) |>
  # Need to consider project and location together because otherwise group_by will create phantom combinations with .drop = FALSE
  unite("project_location", c(project, location), sep = "_")

df_tt_all <- bind_rows(df_tt_eh, df_tt_focal, df_tt_og)

# Unique species seen
sp <- as.character(sort(unique(df_tt_all$species_common_name)))

# New Season 1
df_tt_new1 <- df_tt_all |>
  mutate_at(c("project_location", "species_common_name", "season_new1"), factor) |>
  group_by(project_location, species_common_name, season_new1, .drop = FALSE) |>
  summarise(total_duration = sum(series_total_time)) |>
  ungroup() |>
  mutate_if(is.factor, as.character) |>
  left_join(df_tbd_new1, by = c("project_location"))

df_tt_nn_new1 <- df_tbd_new1 |>
  # Retrieve only those that had no images of animals
  anti_join(df_tt_new1, by = "project_location") |>
  crossing(season_new1 = c("snow", "nonsnow"), species_common_name = sp) |>
  # Add total_duration column, which is zero in these cases
  mutate(total_duration = 0)

df_tt_full_new1 <- df_tt_new1 |>
  bind_rows(df_tt_nn_new1) |>
  arrange(project_location, species_common_name, season_new1) |>
  mutate(total_season1_days = ifelse(season_new1 == "snow", snow, nonsnow)) |>
  separate(project_location, into = c("project", "location"), sep = "_", remove = TRUE, extra = "merge") |>
  select(project, location, species_common_name, season_new1, total_season1_days, total_duration)

# New Season 2
df_tt_new2 <- df_tt_all |>
  mutate_at(c("project_location", "species_common_name", "season_new2"), factor) |>
  group_by(project_location, species_common_name, season_new2, .drop = FALSE) |>
  summarise(total_duration = sum(series_total_time)) |>
  ungroup() |>
  mutate_if(is.factor, as.character) |>
  left_join(df_tbd_new2, by = c("project_location"))

df_tt_nn_new2 <- df_tbd_new2 |>
  # Retrieve only those that had no images of animals
  anti_join(df_tt_new2, by = "project_location") |>
  # Same two seasons as new season 1
  crossing(season_new2 = c("snow", "nonsnow"), species_common_name = sp) |>
  # Add total_duration column, which is zero in these cases
  mutate(total_duration = 0)

df_tt_full_new2 <- df_tt_new2 |>
  bind_rows(df_tt_nn_new2) |>
  arrange(project_location, species_common_name, season_new2) |>
  mutate(total_season2_days = ifelse(season_new2 == "snow", snow, nonsnow)) |>
  separate(project_location, into = c("project", "location"), sep = "_", remove = TRUE, extra = "merge") |>
  select(project, location, species_common_name, season_new2, total_season2_days, total_duration)

# New Season 3
df_tt_new3 <- df_tt_all |>
  mutate_at(c("project_location", "species_common_name", "season_new3"), factor) |>
  group_by(project_location, species_common_name, season_new3, .drop = FALSE) |>
  summarise(total_duration = sum(series_total_time)) |>
  ungroup() |>
  mutate_if(is.factor, as.character) |>
  left_join(df_tbd_new3, by = c("project_location"))

df_tt_nn_new3 <- df_tbd_new3 |>
  # Retrieve only those that had no images of animals
  anti_join(df_tt_new3, by = "project_location") |>
  # Same two seasons as new season 1
  crossing(season_new3 = c("spring", "summer", "winter"), species_common_name = sp) |>
  # Add total_duration column, which is zero in these cases
  mutate(total_duration = 0)

df_tt_full_new3 <- df_tt_new3 |>
  bind_rows(df_tt_nn_new3) |>
  arrange(project_location, species_common_name, season_new3) |>
  mutate(total_season3_days = case_when(
    season_new3 == "spring" ~ spring,
    season_new3 == "summer" ~ summer,
    season_new3 == "winter" ~ winter
  )) |>
  separate(project_location, into = c("project", "location"), sep = "_", remove = TRUE, extra = "merge") |>
  select(project, location, species_common_name, season_new3, total_season3_days, total_duration)

# For species where there are no seasonal effects on EDD
df_tt_ns <- df_tt_all |>
  mutate_at(c("project_location", "species_common_name"), factor) |>
  group_by(project_location, species_common_name, .drop = FALSE) |>
  summarise(total_duration = sum(series_total_time)) |>
  ungroup() |>
  mutate_if(is.factor, as.character) |>
  left_join(df_tbd_ns, by = c("project_location"))

df_tt_nn_ns <- df_tbd_ns |>
  # Retrieve only those that had no images of animals
  anti_join(df_tt_ns, by = "project_location") |>
  # Same two seasons as new season 1
  crossing(species_common_name = sp) |>
  # Add total_duration column, which is zero in these cases
  mutate(total_duration = 0)

df_tt_full_ns <- df_tt_ns |>
  bind_rows(df_tt_nn_ns) |>
  arrange(project_location, species_common_name) |>
  separate(project_location, into = c("project", "location"), sep = "_", remove = TRUE, extra = "merge") |>
  select(project, location, species_common_name, total_days, total_duration)

#-----------------------------------------------------------------------------------------------------------------------

# Bears -> No season, secondary category
# Coyote -> season_new3, secondary category
# Deer -> No season, secondary category
# Gray Wolf -> No season, secondary category
# Hare -> season_new1, secondary category
# Large Ungulates -> season_new3, secondary category
# Lynx -> season_new2, secondary category
# Small Mustelids -> No season, secondary category

#-----------------------------------------------------------------------------------------------------------------------

# Calculate density at each location

df_vegdetdist_eh <- read_csv(paste0(g_drive, "data/lookup/veghf/VegForDetectionDistance/New/EDD (Re-) Modeling ABMI EH (2014-2018).csv")) |>
  select(project, location, primary_category, secondary_category) |>
  filter(!is.na(primary_category))

df_vegdetdist_og <- read_csv(paste0(g_drive, "data/lookup/veghf/VegForDetectionDistance/New/EDD (Re-) Modeling ABMI Off-Grid.csv")) |>
  select(project, location, primary_category, secondary_category) |>
  filter(!is.na(primary_category))

df_vegdetdist <- bind_rows(df_vegdetdist_eh, df_vegdetdist_og) |>
  mutate(secondary_category = ifelse(secondary_category == "Open-Shrubby", "Open-Open", secondary_category))

cam_fov_angle <- 40

# New Season 1
# Only Snowshoe Hare
hare <- read_csv(paste0(g_drive, "data/processed/detection-distance/predictions/New/Hare.csv"))

df_dens_seas1 <- df_tt_full_new1 |>
  filter(species_common_name == "Snowshoe Hare") |>
  # Join new categories
  left_join(df_vegdetdist) |>
  mutate(secondary_category = paste0(primary_category, "_", secondary_category)) |>
  left_join(hare, by = c("secondary_category", "season_new1")) |>
  select(-c(primary_category, dist_group)) |>
  # Calculate density
  mutate(effort = total_season1_days * (prediction ^ 2 * pi * (cam_fov_angle / 360)) / 100,
         # Catch per unit effort
         cpue = total_duration / effort,
         # Catch per unit effort in km2
         cpue_km2 = cpue / 60 / 60 / 24 * 10000) |>
  # All the NaNs are just combinations where the total_seasons_days is 0.
  select(project, location, species_common_name, season_new1, total_season1_days, total_duration, density_km2 = cpue_km2) |>
  group_by(project, location, species_common_name) |>
  summarise(density_km2 = weighted.mean(density_km2, w = total_season1_days),
            total_days = sum(total_season1_days))

# New Season 2
# Only Lynx
lynx <- read_csv(paste0(g_drive, "data/processed/detection-distance/predictions/New/Lynx.csv"))

df_dens_seas2 <- df_tt_full_new2 |>
  filter(species_common_name == "Canada Lynx") |>
  # Join new categories
  left_join(df_vegdetdist) |>
  mutate(secondary_category = paste0(primary_category, "_", secondary_category)) |>
  left_join(lynx, by = c("secondary_category", "season_new2")) |>
  select(-c(primary_category, dist_group)) |>
  # Calculate density
  mutate(effort = total_season2_days * (prediction ^ 2 * pi * (cam_fov_angle / 360)) / 100,
         # Catch per unit effort
         cpue = total_duration / effort,
         # Catch per unit effort in km2
         cpue_km2 = cpue / 60 / 60 / 24 * 10000) |>
  # All the NaNs are just combinations where the total_seasons_days is 0.
  select(project, location, species_common_name, season_new2, total_season2_days, total_duration, density_km2 = cpue_km2) |>
  group_by(project, location, species_common_name) |>
  summarise(density_km2 = weighted.mean(density_km2, w = total_season2_days),
            total_days = sum(total_season2_days))

# New Season 3
coyote <- read_csv(paste0(g_drive, "data/processed/detection-distance/predictions/New/Coyote.csv"))
ungulates <- read_csv(paste0(g_drive, "data/processed/detection-distance/predictions/New/LargeUngulates.csv"))

# Create value for Coyote F-C-C Winter
coyote_cc_winter <- coyote |>
  filter(secondary_category == "Forested_Closed-Closed") |>
  group_by(secondary_category, dist_group) |>
  summarise(prediction = mean(prediction)) |>
  mutate(season_new3 = "winter")

# Create value for Large Ungulates F-C-C and O-O Winter
lu_cc_oo_winter <- ungulates |>
  filter(secondary_category == "Forested_Closed-Closed" | secondary_category == "Open_Open") |>
  group_by(secondary_category, dist_group) |>
  summarise(prediction = mean(prediction)) |>
  mutate(season_new3 = "winter")

coy_ung <- bind_rows(coyote, ungulates, coyote_cc_winter, lu_cc_oo_winter)

scn_1 <- c("Coyote", "Moose", "Woodland Caribou")

df_dens_seas3 <- df_tt_full_new3 |>
  filter(species_common_name %in% scn_1) |>
  # Join new categories
  left_join(df_vegdetdist) |>
  mutate(secondary_category = paste0(primary_category, "_", secondary_category),
         dist_group = ifelse(str_detect(species_common_name, "Coyote"), "Coyote", "LargeUngulates")) |>
  left_join(coy_ung, by = c("secondary_category", "season_new3", "dist_group")) |>
  select(-c(primary_category, dist_group)) |>
  # Calculate density
  mutate(effort = total_season3_days * (prediction ^ 2 * pi * (cam_fov_angle / 360)) / 100,
         # Catch per unit effort
         cpue = total_duration / effort,
         # Catch per unit effort in km2
         cpue_km2 = cpue / 60 / 60 / 24 * 10000) |>
  # All the NaNs are just combinations where the total_seasons_days is 0.
  select(project, location, species_common_name, season_new3, total_season3_days, total_duration, density_km2 = cpue_km2) |>
  # Combine with weighted mean
  group_by(project, location, species_common_name) |>
  summarise(density_km2 = weighted.mean(density_km2, w = total_season3_days),
            total_days = sum(total_season3_days))

# Now species with no seasonal effects
# Bears, Deer, Gray Wolf, Small Mustelids
bears <- read_csv(paste0(g_drive, "data/processed/detection-distance/predictions/New/Bear.csv"))
deer <- read_csv(paste0(g_drive, "data/processed/detection-distance/predictions/New/Deer.csv"))
wolf <- read_csv(paste0(g_drive, "data/processed/detection-distance/predictions/New/Wolf.csv"))
mustelids <- read_csv(paste0(g_drive, "data/processed/detection-distance/predictions/New/SmallMustelids.csv"))

must_pred <- mean(mustelids$prediction)

mustelids_all <- data.frame(
  dist_group = "SmallMustelids",
  secondary_category = c("Open_Open", "Shrubby_Open", "Shrubby_Closed"),
  prediction = must_pred) |>
  bind_rows(mustelids)

bdwm <- bind_rows(bears, deer, wolf, mustelids_all)

scn_2 <- c("Black Bear", "White-tailed Deer", "Gray Wolf", "Fisher", "Marten")

# Snow for Bears (we don't want to count the days during the winter)
df_snow_bears <- df_tbd_new1 |>
  select(project_location, snow) |>
  separate(project_location, into = c("project", "location"), sep = "_")

df_dens_ns <- df_tt_full_ns |>
  filter(species_common_name %in% scn_2) |>
  # Join new categories
  left_join(df_vegdetdist) |>
  mutate(secondary_category = paste0(primary_category, "_", secondary_category),
         dist_group = case_when(
           species_common_name == "Black Bear" ~ "Bear",
           species_common_name == "Gray Wolf" ~ "Gray Wolf",
           species_common_name == "White-tailed Deer" ~ "Deer",
           str_detect(species_common_name, "Fisher|Marten") ~ "SmallMustelids")) |>
  left_join(bdwm, by = c("secondary_category", "dist_group")) |>
  left_join(df_snow_bears, by = c("project", "location")) |>
  mutate(total_days = ifelse(species_common_name == "Black Bear", total_days - snow, total_days)) |>
  select(-c(primary_category, dist_group, snow)) |>
  # Calculate density
  mutate(effort = total_days * (prediction ^ 2 * pi * (cam_fov_angle / 360)) / 100,
         # Catch per unit effort
         cpue = total_duration / effort,
         # Catch per unit effort in km2
         cpue_km2 = cpue / 60 / 60 / 24 * 10000) |>
  # All the NaNs are just combinations where the total_seasons_days is 0.
  select(project, location, species_common_name, total_days, density_km2 = cpue_km2)

#-----------------------------------------------------------------------------------------------------------------------

# Put all the densities together

dens_all_sup <- bind_rows(df_dens_seas1,
                          df_dens_seas2,
                          df_dens_seas3,
                          df_dens_ns) |>
  arrange(project, location, species_common_name)

#-----------------------------------------------------------------------------------------------------------------------

# Wow that was horrific. Can't wait to dig into that again ....

# Combine densities and make wide
dens_all_sup_wide <- bind_rows(dens_all_sup, dens_all) |>
  select(-total_days) |>
  pivot_wider(id_cols = c(project, location),
              names_from = c(species_common_name),
              values_from = density_km2)

# Okay this is the file for data analysis

write_csv(dens_all_sup_wide, paste0(g_drive, "Results/Density/Deployments/og-sup_all-years_density_wide_2024-03-14.csv"))














