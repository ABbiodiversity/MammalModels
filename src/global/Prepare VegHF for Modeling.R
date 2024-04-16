#-----------------------------------------------------------------------------------------------------------------------

# Title:            Prepare VegHF
# Description:      Process the site point and buffer summaries from the Geospatial Centre
# Author:           Marcus Becker
# Date:             November 2022

# Previous scripts: None

#-----------------------------------------------------------------------------------------------------------------------

# Attach packages

library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(fs)
library(purrr)
library(lubridate)
library(veghfsoil)

# Set path to Shared Google Drive (G Drive)
g_drive <- "G:/Shared drives/ABMI Camera Mammals/"

#-----------------------------------------------------------------------------------------------------------------------

# Latest summaries performed by Eric D in November 2022

# Set path to VegHF summaries in Science Centre Drive (S Drive)
s_drive <- "S:/GC_eric/FromEric/Sites_summaries/"

# Raw summary tables
d <- read_summary(
  summary_path = paste0(s_drive, "Round2022/Modelling2022/CAM&ARU/Marcus_Selection_survey_year_2019_to_2022/summaries_20221101_rev00.sqlite"),
  table = "landscape_summary_camaru_pts_2019_2022"
)

# Processing into long format
d_long <- make_veghf_long(
  d = d,
  col.label = "Site_ID",
  col.veg = "Combined_ChgByCWCS",
  col.baseyear = 2019,
  col.hfyear = "YEAR",
  col.soil = "Soil_Type_1",
  unround = FALSE,
  hf_fine = TRUE
)

# 2019-2022
pt_2019_to_2022 <- d_long |>
  # Remove ARU-only points
  filter(deployment == "CAM" | deployment == "BOTH") |>
  # Adjustments to the WildtraxProject field need to be made for sites that were revisited.
  mutate_if(is.factor, as.character) |>
  mutate(project = case_when(
    str_detect(WildtraxProject, "2014|2015|2016|2017") ~ paste0("ABMI Ecosystem Health ", survey_year),
    TRUE ~ WildtraxProject)) |>
  # Adjust 2022 locations
  mutate(project = case_when(
    project == "ABMI Ecosystem Health 2021" & str_detect(WildtraxName, "^7") ~ "ABMI Ecosystem Health 2022",
    TRUE ~ project)) |>
  # Some locations don't have a WildtraxName (something went wrong with data collection?) - Keep for now.
  mutate(location = case_when(
    is.na(WildtraxName) ~ Site_ID,
    TRUE ~ WildtraxName
  )) |>
  # Keeping HFclass for later automated classification of VegForDetectionDistance
  select(location, project, HFclass, VEGHFAGEclass, SOILHFclass) |>
  # Duplicate locations in Southern Focal Areas to reflect the two projects (2019 and 2021)
  mutate(count = if_else(str_detect(project, "Southern Focal"), 2, 1)) |>
  uncount(weights = count, .remove = TRUE, .id = "id") |>
  mutate(project = if_else(id == "2", "ABMI Southern Focal Areas 2019", project)) |>
  select(-id)
  # VegForDetectionDistance still needs to be defined.

# Find VegForDetectionDistance from data processed in previous years (2019 and 2020)
prev_pt_2019_2020 <- read_csv(paste0(g_drive, "data/lookup/veghf/abmi-cmu_all-years_veghf-soilhf-detdistveg_2021-10-06.csv")) |>
  # Later years - notably, we are missing 2021 and 2022 information here. I thought I summarised 2021 last year.
  filter(str_detect(project, "2019|2020"),
         # Remove HF2 and CUDDE deployments - they are in new projects now, and not relevant to habitat modeling.
         !str_detect(location, "HF2|HP2X|CUDDE|BOTH")) |>
  # Remove "-CAM" suffix from a few locations
  mutate(location = str_remove(location, "-CAM$")) |>
  # Update a couple locations with a 'B' where needed
  mutate(location = case_when(
    str_detect(location, "822|853") ~ gsub("^(.{3})(.*)$", "\\1B\\2", location),
    TRUE ~ location
  )) |>
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

# Issue: changes in WildTrax project naming has impacted OG cameras

pt_2013_to_2018 <- read_csv(paste0(g_drive, "data/lookup/veghf/abmi-cmu_all-years_veghf-soilhf-detdistveg_2021-10-06.csv")) |>
  # Only want earlier years.
  filter(str_detect(project, "13|14|15|16|17|18"))

# CMU projects - 2017 and 2018
cmu_2017_2018 <- pt_2013_to_2018 |>
  filter(str_detect(project, "CMU"))

# Sort out Off-Grid (OG) deployments into the appropriate project name
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
  # Create new project variable
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

# Write cleaned up version to csv
write_csv(pt_2013_to_2018_all, paste0(g_drive, "data/lookup/veghf/abmi-cmu_2013-2018_vegsoilhf-detdistveg_", Sys.Date(), ".csv"))

#-----------------------------------------------------------------------------------------------------------------------

# Finally, let's do 2021 and 2022.

pt_2021_2022_all <- pt_2019_to_2022 |>
  filter(!str_detect(project, "2019|2020")) |>
  # Fix NWSAR 2020 project name
  mutate(project = ifelse(project == "Northwest Species at Risk Program",
                           "Northwest Species at Risk Program 2020",
                           project)) |>
  # Fix OSL grid in NWSAR 2020
  mutate(location = if_else(str_detect(location, "OSL") & !str_detect(location, "0$") & str_detect(project, "2020$"),
                            str_remove(location, "0"),
                            location)) |>
  arrange(project) |>
  # Delineate VegForDetectionDistance automatically (this should be made into a function) - to be checked manually!
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

# Save as csv - these have not been manually checked.
write_csv(pt_2019_to_2022_all, paste0(g_drive, "data/lookup/veghf/abmi-cmu-nwsar_2019-2022_vegsoilhf_raw_", Sys.Date(), ".csv"))

#-----------------------------------------------------------------------------------------------------------------------

# Prepare a VegForDetectionDistance lookup

check <- pt_2019_to_2022_all |>
  bind_rows(pt_2013_to_2018_all) |>
  select(2, 1, 5) |>
  write_csv(paste(g_drive, "data/lookup/veghf/VegForDetectionDistance/"))

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
  filter(field_of_view == "WITHIN") |>
         #trigger == "Time Lapse") |> We generally want a timelapse, but they will be selected nearly all of the time
         # anyway. Include motion detection here in case a camera wasn't programmed to take TL for whatever reason.
  # Create target date
  mutate(year = year(date_detected),
         # Slightly earlier than 1pm in the afternoon - some deployments had TL programmed on even hours, so we don't
         # want two images to be selected.
         target_date = ymd_hms(paste0(year, "-06-15 12:59:59"))) |>
  group_by(project, location, trigger) |>
  # Select the image closest to the target date
  filter(abs(difftime(date_detected, target_date)) == min(abs(difftime(date_detected, target_date)))) |>
  # If a deployment spans >1 year, just choose the first record
  filter(row_number() == 1) |>
  # Drop trigger as group
  group_by(project, location) |>
  add_count() |>
  # Keep only Time Lapse image when it is available; keep Motion Detection if needed.
  filter(case_when(
    n > 1 ~ trigger == "Time Lapse",
    T ~ T
  )) |>
  select(project, location, date_detected, trigger, url = `image_url(admin only)`)

#-----------------------------------------------------------------------------------------------------------------------

# Join image URLs to GIS data

full <- pt_2019_to_2022_all |>
  full_join(rep_images, by = c("project", "location")) |>
  # Remove CMU and NWSAR deployments with no url - erroneously included in GIS summaries it seems
  filter(!(is.na(url) & str_detect(project, "CMU|Northwest"))) |>
  select(2, 1, 3:8) |>
  arrange(project, location) |>
  mutate(comments = "")

# What is missing?

missing <- full |>
  filter(is.na(VEGHFAGEclass) | is.na(url))

# Projects
missing_breakdown <- missing |>
  mutate(missing_url = ifelse(is.na(url), 1, 0),
         missing_veg = ifelse(is.na(VEGHFAGEclass), 1, 0),
         missing_both = ifelse(missing_url == 1 & missing_veg == 1, 1, 0)) |>
  group_by(project) |>
  summarise(missing_url = sum(missing_url),
            missing_veg = sum(missing_veg),
            missing_both = sum(missing_both))

proj <- unique(full$project)

for (i in proj) {

  d <- full |> filter(project == i)
  write_csv(d, paste0(g_drive, "data/lookup/veghf/manual_checking/", "VegHF Checks for ", i, ".csv"))

}

# Currently w/ Dave H for checking.

#-----------------------------------------------------------------------------------------------------------------------

