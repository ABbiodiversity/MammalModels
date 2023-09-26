#-----------------------------------------------------------------------------------------------------------------------

# Project:          ABMI

# Title:            Pull Winter Moose Series' Images
# Description:
# Author:           Marcus Becker

# Previous scripts: None

#-----------------------------------------------------------------------------------------------------------------------

# Set path to Shared Google Drive (G Drive)
g_drive <- "G:/Shared drives/ABMI Camera Mammals/"

library(tidyverse)

projects <- "eh"

# Load data

eh_image_report <- read_csv(paste0(g_drive, "data/lookup/image-reports/eh_19-20-21-22_image-report_simple.csv")) |>
  filter(trigger == "Motion") |>
  select(project, location, date_detected, last_col())

# Tags

eh_tags_report <- read_csv(paste0(g_drive, "data/base/clean/eh_19-20-21-22_native-sp_clean_2023-01-05.csv")) |>
  filter(common_name == "Moose") |>
  mutate(month = month(date_detected)) |>
  filter(month < 6)

# Join together
tags_images <- eh_tags_report |>
  left_join(eh_image_report, by = c("project", "location", "date_detected"))

#-----------------------------------------------------------------------------------------------------------------------

# Folder location for images
folder <- "G:/Shared drives/ABMI Camera Mammals/projects/Winter Ticks/Moose Images/"

tags_images_sample <- tags_images |> head(250)

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


