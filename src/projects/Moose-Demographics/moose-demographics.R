#-----------------------------------------------------------------------------------------------------------------------

# Project:          Moose Demographics

# Title:            Tracking moose demographics (age, sex ratios) and survival rates using camera data
# Description:

# Author:           Marcus Becker

# Previous scripts: None

#-----------------------------------------------------------------------------------------------------------------------

# Import packages
library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(wildRtrax)

# Google drive
g_drive <- "G:/Shared drives/ABMI Camera Mammals/"

# Import data - we're going to focus on CMU for now
source <- "cmu"

# Find appropriate tag data file
file <- list.files(path = paste0(g_drive, "data/base/clean"), full.names = TRUE) |>
  str_subset(pattern = paste0(source, "_all-years_all-data_clean"))
# Import
tags_clean <- read_csv(file) |>
   # Why are there duplicate records? a LOT
  distinct()

# Projects included
unique(tags_clean$project) # 5 years of CMU (2017-2021)

# Grids by number of years surveyed
check <- tags_clean |>
  separate(location, into = c("grid", "station"), sep = "-") |>
  select(project, grid) |>
  distinct() |>
  group_by(grid) |>
  tally() |>
  arrange(desc(n))

#-----------------------------------------------------------------------------------------------------------------------

# Let's figure out this occupancy framework piece.

# "The calf occupies the cow (Mom)"

# This is all the deployments:
cmu_dep <- tags_clean |>
  select(location) |>
  distinct()

summary <- moose |>
  group_by(age_class, sex) |>
  tally()

moose <- tags_clean |>
  filter(common_name == "Moose") |>
  # We could restrict our analysis to only grids with the longest track record of monitoring
  #filter(str_detect(location, "^CHR|FMM|LLB|LID|LRN|MAC|MCC|WAB")) |>
  mutate(number_individuals = ifelse(str_detect(age_class, ","), 1, number_individuals)) |>
  separate_rows(c(age_class, sex), sep = ", ") |>
  filter(!str_detect(location, "T$")) |>
  mutate(field_of_view = "WITHIN",
         scientific_name = "Alces alces")

# Summary of age and sex classes
summary <- moose |>
  group_by(age_class, sex) |>
  tally() # There are a lot of `Unkn` that we should seek to clear up.

#-----------------------------------------------------------------------------------------------------------------------

# Evaluate 'independent' detections

moose_juv <- moose |>
  filter(age_class == "Juv")

# Which locations had a juvenile?
moose_juv_loc <- moose_juv |>
  select(location) |>
  distinct()

# Juvenile independent detections
juv_ind_det <- wt_ind_detect(moose_juv, threshold = 10, units = "minutes", datetime_col = date_detected)

# Number of juvenile detections by location
juv_det_loc <- juv_ind_det |>
  group_by(location) |>
  summarise(n_juv = n())

# What about detections of just female moose? (Hopefully this will grow as the Unkn's get cleared up)
moose_fem_adult <- moose |>
  filter(age_class == "Adult",
         sex == "Female")

# Female independent detections
fem_ind_det <- wt_ind_detect(moose_fem_adult, threshold = 10, units = "minutes", datetime_col = date_detected)

# Number of adult female detections by location
fem_det_loc <- fem_ind_det |>
  group_by(location) |>
  summarise(n_fem = n())

# Join all together
detections <- cmu_dep |>
  filter(!str_detect(location, "T$")) |>
  full_join(fem_det_loc, by = "location") |>
  mutate(n_fem = replace_na(n_fem, 0)) |>
  full_join(juv_det_loc, by = "location") |>
  mutate(n_juv = replace_na(n_juv, 0))


















