#-----------------------------------------------------------------------------------------------------------------------

# Title:       Download timelapse images for Phenology analysis

# Description: Using the URL for timelapse images, download to a folder for further analysis.
# Author:      Marcus Becker
# Date:        May 2023

#-----------------------------------------------------------------------------------------------------------------------

library(dplyr)
library(stringr)
library(tidyr)
library(purrr)

# Root directory (Google)
g_drive <- "G:/Shared drives/ABMI Camera Mammals/"

# Read Timelapse url data — currently only EH, CMU, and OG projects
df_tl <- read.csv(paste0(g_drive, "data/processed/timelapse/eh-cmu-og_all-years_timelapse_12h-13h.csv"))

#-----------------------------------------------------------------------------------------------------------------------

# Locations that have already been done:
loc_done <- list.dirs(paste0(g_drive, "projects/Phenology/Timelapse Images"),
                      recursive = FALSE, full.names = FALSE)

# Available locations (CMU)
df_tl_avail <- df_tl |>
  filter(str_detect(project, "CMU"),
         # Remove trail deployments
         !str_detect(location, "T$"),
         !location %in% loc_done)

# June 27, 2023 - Starting with a sample of the CMU data
set.seed(12345)

df_tl_subset <- df_tl |>
  filter(str_detect(project, "CMU"),
         # Remove trail deployments
         !str_detect(location, "T$")) |>
  group_by(location) |>
  nest() |>
  mutate(obs = map_dbl(data, nrow)) |>
  ungroup() |>
  # Great than one year is optimal
  filter(obs > 400) |>
  separate(location, into = c("grid", "station"), sep = "-", remove = TRUE) |>
  group_by(grid) |>
  sample_n(5) |>
  select(-obs) |>
  unnest(cols = c(data)) |>
  unite("location", grid, station, sep = "-") |>
  select(project, location, date_detected, url) |>
  # Change to dataframe for tibble reasons
  data.frame()

locations <- unique(df_tl_subset$location)

# July 14, 2023
# Let's pull another random sample

set.seed(54321)

df_tl_subset <- df_tl_avail |>
  group_by(location) |>
  nest() |>
  mutate(obs = map_dbl(data, nrow)) |>
  ungroup() |>
  # Great than one year is optimal
  filter(obs > 400) |>
  separate(location, into = c("grid", "station"), sep = "-", remove = TRUE) |>
  group_by(grid) |>
  slice_sample(n = 5) |>
  select(-obs) |>
  unnest(cols = c(data)) |>
  unite("location", grid, station, sep = "-") |>
  select(project, location, date_detected, url) |>
  # Change to dataframe for tibble reasons
  data.frame()

locations <- unique(df_tl_subset$location)

#-----------------------------------------------------------------------------------------------------------------------

# Folder location for images
tl_folder <- "G:/Shared drives/ABMI Camera Mammals/projects/Phenology/Timelapse Images/"

for (i in locations) {

  print(paste0("Working on location ", i))

  # Create directory for each location
  # Does it already exist?
  already.exist <- dir.exists(paste0(tl_folder, i))
  if (already.exist) {
    print("Directory already exists, moving on to next locations")
    next
  } else {
    dir.create(paste(tl_folder, i, sep = ""))
  }

  # Root directory for location
  dir <- paste(tl_folder, i, "/", sep = "")

  # Subset the data for only images only from this location
  d <- df_tl_subset[df_tl_subset$location == i, ]

  for (u in 1:nrow(d)) {

    url <- d[u, 4]
    date_time <- d[u, 3]
    location <- d[u, 2]
    date <- strptime(date_time, format = "%Y-%m-%d")
    try(download.file(url, destfile = paste0(dir, location, "_", date, ".jpg"), mode = 'wb'))

  }

}

#-----------------------------------------------------------------------------------------------------------------------
