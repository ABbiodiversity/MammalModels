#-----------------------------------------------------------------------------------------------------------------------

# Title:            Prepare VegHF
# Description:      Process the site point and buffer summaries from the Geospatial Centre
# Author:           Marcus Becker
# Date:             August 2022

# Previous scripts: None

#-----------------------------------------------------------------------------------------------------------------------

# Attach packages

library(readr)
library(dplyr)
library(stringr)
library(fs)
library(purrr)
library(lubridate)

# Set path to Shared Google Drive (G Drive)
g_drive <- "G:/Shared drives/ABMI Camera Mammals/"

#-----------------------------------------------------------------------------------------------------------------------

# Note: in the future, the veg-hf-soil summaries repository will be made into an R package.

# Read in pre-processed data (for now):

# 2019-2022 - Summarised by Eric D in November 2022
pt_2019_to_2022 <- read_csv(paste0(g_drive, "data/lookup/veghf/processed/all-sites_point_2019-2022.csv")) |>
  # Remove ARU-only points
  filter(deployment == "CAM" | deployment == "BOTH") |>
  # Adjustments to the WildtraxProject field need to be made for sites that were revisited.
  mutate(project = case_when(
    str_detect(WildtraxProject, "2014|2015|2016|2017") ~ paste0("ABMI Ecosystem Health ", survey_year),
    TRUE ~ WildtraxProject
  )) |>
  # Some locations don't have a WildtraxName (something went wrong with data collection?) - Keep for now.
  mutate(location = case_when(
    is.na(WildtraxName) ~ Site_ID,
    TRUE ~ WildtraxName
  )) |>
  select(location, project, HFclass, VEGHFAGEclass, SOILHFclass) |>
  # Duplicate locations in Southern Focal Areas
  mutate(count = if_else(str_detect(project, "Southern Focal"), 2, 1)) |>
  uncount(weights = count, .remove = TRUE, .id = "id") |>
  mutate(project = if_else(id == "2", "ABMI Southern Focal Areas 2019", project)) |>
  select(-id)
  # We're missing VegForDetectionDistance.

# Find VegForDetectionDistance from data processed in previous years.
prev_pt_2019_2020 <- read_csv(paste0(g_drive, "data/lookup/veghf/abmi-cmu_all-years_veghf-soilhf-detdistveg_2021-10-06.csv")) |>
  # Remove "-CAM" suffix from a few locations
  mutate(location = str_remove(location, "-CAM$")) |>
  # Update a couple locations with a 'B' where needed
  mutate(location = case_when(
    str_detect(location, "822|853") ~ gsub("^(.{3})(.*)$", "\\1B\\2", location),
    TRUE ~ location
  )) |>
  # Later years - notably, we are missing 2021 and 2022 information here. I thought I summarised 2021 last year.
  filter(str_detect(project, "2019|2020"),
         # Remove HF2 and CUDDE deployments - they are in new projects now, and not relevant to habitat modeling.
         !str_detect(location, "HF2|HP2X|CUDDE|BOTH")) |>
  select(location, project, VegForDetectionDistance)

# Join VegForDetectionDistance information to current VegHF summaries (for 2019 and 2020)
pt_2019_2020_all <- pt_2019_to_2022 |>
  filter(str_detect(project, "2019|2020")) |>
  full_join(prev_pt_2019_2020, by = c("location", "project")) |>
  # Let's remove the SK deployments - these won't be used for habitat modeling.
  filter(!str_detect(location, "DEW|LRN|MCC|AUB|MAL")) |>
  arrange(project) |>
  select(-HFclass)

# Notes:
# - ABMI Amphibian Monitoring 2020 is missing from VEGHF summaries

#-----------------------------------------------------------------------------------------------------------------------

# Now we need pre-2019.

# Hmm, okay, so the problem now is that the projects are messed up for OGs. Can I just take the location name?

pt_2013_to_2018 <- read_csv(paste0(g_drive, "data/lookup/veghf/abmi-cmu_all-years_veghf-soilhf-detdistveg_2021-10-06.csv")) |>
  # Only want earlier years.
  filter(str_detect(project, "13|14|15|16|17|18"))

# CMU projects - 2017 and 2018
cmu_2017_2018 <- pt_2013_to_2018 |>
  filter(str_detect(project, "CMU"))

# Sort out Off-Grid (OG) deployments
og_2013_to_2018 <- pt_2013_to_2018 |>
  filter(str_detect(location, "OG")) |>
  mutate(year = str_extract(project, "\\d+"),
         # Create variable that mimics current WildTrax project names for these deployments
         type = case_when(
           str_detect(location, "EI") ~ "Edge-Interior Surveys",
           str_detect(location, "RIVR") ~ "North Saskatchewan Monitoring",
           str_detect(location, "AAC") ~ "Adopt-a-Camera",
           str_detect(location, "CITSCI") ~ "Citizen Science Monitoring",
           TRUE ~ "Off-Grid Monitoring"
         )) |>
  mutate(project = paste0("ABMI ", type, " ", year)) |>
  select(1:5)

# Ecosystem Health
eh_2013_to_2018 <- pt_2013_to_2018 |>
  # Remove the OG first, then filter for EH deployments
  filter(!str_detect(location, "OG"),
         str_detect(project, "Ecosystem Health"))

# Put it all back together for a revised version with corrected project names
pt_2013_to_2018_all <- bind_rows(
  cmu_2017_2018,
  og_2013_to_2018,
  eh_2013_to_2018
)

#-----------------------------------------------------------------------------------------------------------------------

# Finally, let's do 2021 and 2022.

pt_2021_2022_all <- pt_2019_to_2022 |>
  filter(!str_detect(project, "2019|2020")) |>
  arrange(project) |>
  # Delineate VegForDetectionDistance automatically - to be checked manually!
  mutate(VegForDetectionDistance = case_when(
    !is.na(HFclass) ~ "HF",
    str_detect(VEGHFAGEclass, "Pine|Spruce|Mixedwood") ~ "Conif",
    str_detect(VEGHFAGEclass, "Decid") ~ "Decid",
    str_detect(VEGHFAGEclass, "Swamp|Bog|Fen") ~ "WetTreed",
    str_detect(VEGHFAGEclass, "Shrub") ~ "Shrub",
    str_detect(VEGHFAGEclass, "Grass|Bare") ~ "Grass",
    TRUE ~ "Water"
  )) |>
  select(-HFclass)

#-----------------------------------------------------------------------------------------------------------------------

# Combine 2019-2022

pt_2019_to_2022_all <- bind_rows(
  pt_2019_2020_all,
  pt_2021_2022_all
)

#-----------------------------------------------------------------------------------------------------------------------


# Image reports

# Paths to the image reports (already downloaded from WildTrax)
paths <- dir_ls(path = paste0(g_drive, "data/base/raw/from_WildTrax/ABMI/image_reports"), glob = "*.csv")

# Read into R; only keep relevant columns (this takes a while ...)
df <- map_df(.x = paths,
             .f = ~ read_csv(., col_select = c(project, location, date_detected,
                                               trigger, field_of_view, `image_url(admin only)`)))

# Pull representative images - determined to be a within range Time Lapse photo nearest to June 15.
rep_images <- df |>
  filter(trigger == "Time Lapse", # Something to investigate.
         field_of_view == "WITHIN") |>
  # Create target date
  mutate(year = year(date_detected),
         target_date = ymd_hms(paste0(year, "-06-15 13:00:00"))) |>
  group_by(project, location) |>
  # Select the image closest to the target date
  filter(abs(difftime(date_detected, target_date)) == min(abs(difftime(date_detected, target_date)))) |>
  select(project, location, date_detected, url = `image_url(admin only)`)

#-----------------------------------------------------------------------------------------------------------------------

# Join image urls to GIS data

check <- pt_2019_to_2022_all |>
  left_join(rep_images, by = c("project", "location")) |>
  arrange(project, location)














