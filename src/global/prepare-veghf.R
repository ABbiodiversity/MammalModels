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

# Set path to Shared Google Drive (G Drive)
g_drive <- "G:/Shared drives/ABMI Camera Mammals/"

#-----------------------------------------------------------------------------------------------------------------------

# Note: in the future, the veg-hf-soil summaries repository will be made into an R package.

# Read in pre-processed data (for now):

# 2019-2022 - Summarised by Eric D in November 2022
pt_2019_to_2022 <- read_csv(paste0(g_drive, "data/lookup/veghf/processed/all-sites_point_2019-2022.csv")) |>
  filter(deployment == "CAM" | deployment == "BOTH") |>
  select(location = WildtraxName, project = WildtraxProject, VEGHFAGEclass, SOILHFclass) |>
  # Remove locations that do not have a WildTraxName (something went wrong with the camera?)
  filter(!is.na(location))
  # We're missing VegForDetectionDistance.

# Records without a WildTraxName
nonames <- read_csv(paste0(g_drive, "data/lookup/veghf/processed/all-sites_point_2019-2022.csv")) |>
  filter(deployment == "CAM" | deployment == "BOTH") |>
  filter(is.na(WildtraxName))

# Let's find VegForDetectionDistance from previous data.
prev_pt_2019_2020 <- read_csv(paste0(g_drive, "data/lookup/veghf/abmi-cmu_all-years_veghf-soilhf-detdistveg_2021-10-06.csv")) |>
  # Later years - notably, we are missing 2021 and 2022 information here. I thought I summarised 2021 last year.
  filter(str_detect(project, "2019|2020")) |>
  select(location, project, VegForDetectionDistance)

# Join VegForDetectionDistance information to current VegHF summaries (for 2019 and 2020)
pt_2019_2020_all <- pt_2019_to_2022 |>
  filter(str_detect(project, "2019|2020")) |>
  right_join(prev_pt_2019_2020, by = c("location", "project"))



# What's missing in terms of VegForDetectionDistance?
missing <- complete |> filter(is.na(VegForDetectionDistance))

unique(missing$project)

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

# Put it all back together
pt_2013_to_2018_rev <- bind_rows(
  cmu_2017_2018,
  og_2013_to_2018,
  eh_2013_to_2018
)

#-----------------------------------------------------------------------------------------------------------------------

# Image reports


















