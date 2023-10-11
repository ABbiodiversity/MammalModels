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
library(wildRtrax)
library(keyring)

# Download data
# Set environment variables (username/password)
Sys.setenv(WT_USERNAME = key_get("WT_USERNAME", keyring = "wildtrax"),
           WT_PASSWORD = key_get("WT_PASSWORD", keyring = "wildtrax"))

# Authenticate into WildTrax
wt_auth()

# Load data
proj <- wt_get_download_summary(sensor_id = "CAM") |>
  filter(str_detect(project, "ABMI Ecosystem Health 2015")) |>
  pull(project_id)

image_report <- wt_download_report(project_id = proj,
                                   sensor_id = "CAM",
                                   reports = "image_report",
                                   weather_cols = FALSE)

tag_report <- wt_download_report(project_id = proj,
                                 sensor_id = "CAM",
                                 reports = "tag",
                                 weather_cols = FALSE)

image_report_subset <- image_report |>
  #filter(image_trigger_mode == "Motion Detection") |>
  mutate(month = month(image_date_time)) |>
  filter(month < 7) |>
  select(location, image_id, image_date_time, media_url)

tag_report_subset <- tag_report |>
  filter(species_common_name == "Moose") |>
  mutate(month = month(image_date_time)) |>
  filter(month < 7) |>
  select(location, image_id, image_date_time, species_common_name)

images_to_download <- tag_report_subset |>
  left_join(image_report_subset, by = c("location", "image_id", "image_date_time")) |>
  distinct()

#-----------------------------------------------------------------------------------------------------------------------

# Folder location for images
folder <- "G:/Shared drives/ABMI Camera Mammals/projects/Winter Ticks/Moose Images/"

locations <- unique(images_to_download$location)

for (i in locations) {

  print(paste0("Working on location ", i))

  # Create directory for each location
  # Does it already exist?
  already.exist <- dir.exists(paste0(folder, i))
  if (already.exist) {
    print("Directory already exists, moving on to next locations")
    next
  } else {
    dir.create(paste(folder, i, sep = ""))
  }

  # Root directory for location
  dir <- paste(folder, i, "/", sep = "")

  # Subset the data for only images only from this location
  d <- images_to_download[images_to_download$location == i, ]

  for (u in 1:nrow(d)) {

    url <- d[u, 5]
    date_time <- d[u, 3]
    location <- d[u, 1]
    id <- d[u, 2]
    date <- strptime(date_time, format = "%Y-%m-%d")
    try(download.file(url, destfile = paste0(dir, location, "_", date, "_", id, ".jpg"), mode = 'wb'))

  }

}



