#-----------------------------------------------------------------------------------------------------------------------

# Project: Tracking moose demographics with camera data

# Title:
# Description:

# Author: Marcus Becker

# Previous scripts: None

#-----------------------------------------------------------------------------------------------------------------------

library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(wildRtrax)

# Google drive
g_drive <- "G:/Shared drives/ABMI Camera Mammals/"

# Import data
proj <- "cmu"

# Find appropriate tag data file
file <- list.files(path = paste0(g_drive, "data/base/clean"), full.names = TRUE) |>
  str_subset(pattern = paste0(proj, "_all-years_all-data_clean"))
# Import
tags_clean <- read_csv(file)

# Moose only!
# Tags have already been consolidated, and out of range images removed.
moose <- tags_clean |>
  filter(common_name == "Moose") |>
  # Create fov column
  mutate(field_of_view = "WITHIN",
         scientific_name = "Alces alces")

#-----------------------------------------------------------------------------------------------------------------------

# Evaluate independent detections?

# Let's look at 2018 first
tags_2018 <- tags_clean |>
  #filter(str_detect(project, "2018")) |>
  filter(!str_detect(location, "T$")) |>
  mutate(field_of_view = "WITHIN",
         scientific_name = "Doesnt matter")

ind_det_2018 <- tags_2018 |>
  filter(common_name == "Moose") |>
  wt_ind_detect(threshold = 30,
                units = "minutes",
                datetime_col = date_detected)

# Summarise
sum_2018 <- ind_det_2018 |>
  wt_summarise_cam(raw_data = tags_2018,
                   time_interval = "week",
                   variable = "detections",
                   output_format = "long")

plot <- sum_2018 |>
  separate(location, into = c("grid", "station"), sep = "-") |>
  group_by(grid, week, common_name) |>
  summarise(value = sum(value))

library(ggplot2)

ggplot(data = plot) +
  geom_col(aes(x = week, y = value)) +
  facet_wrap(~ grid)

















