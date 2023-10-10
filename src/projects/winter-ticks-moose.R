#-----------------------------------------------------------------------------------------------------------------------

# Project:          ABMI

# Title:            Pull Winter Moose Series' Images
# Description:
# Author:           Marcus Becker

# Previous scripts: None

#-----------------------------------------------------------------------------------------------------------------------

library(tidyverse)
library(wildRtrax)
library(keyring)

# Download data
# Set environment variables (username/password)
Sys.setenv(WT_USERNAME = key_get("WT_USERNAME", keyring = "wildtrax"),
           WT_PASSWORD = key_get("WT_PASSWORD", keyring = "wildtrax"))

# Authenticate into WildTrax
wt_auth()

# Set path to Shared Google Drive (G Drive)
g_drive <- "G:/Shared drives/ABMI Camera Mammals/"

# Pull ABMI EH 2015 Project ID
proj <- wt_get_download_summary(sensor_id = "CAM") |>
  filter(str_detect(project, "ABMI Ecosystem Health 2015")) |>
  pull(project_id)

# Load image report
eh15_img_rep <- wt_download_report(project_id = proj,
                                   sensor_id = "CAM",
                                   reports = "image_report",
                                   weather_cols = FALSE)

subset_eh15_img_rep <- eh15_img_rep |>
  filter(image_trigger_mode == "Motion Detection") |>
  select(location, image_id, image_date_time, media_url)

rm(eh15_img_rep)

subset_eh15_img_rep <- subset_eh15_img_rep |>
  mutate(month = month(image_date_time)) |>
  filter(month < 7) |>
  select(-month)

# Load tag report
eh15_tag_rep <- wt_download_report(project_id = proj,
                                   sensor_id = "CAM",
                                   reports = "tag",
                                   weather_cols = FALSE)

subset_eh15_tag_rep <- eh15_tag_rep |>
  select(location, image_id, image_date_time, species_common_name, individual_count, age_class, sex_class) |>
  filter(species_common_name == "Moose") |>
  mutate(month = month(image_date_time)) |>
  filter(month < 7) |>
  select(-month)

img_to_download <- subset_eh15_tag_rep |>
  left_join(subset_eh15_img_rep, by = c("image_id", "image_date_time", "location")) |>
  select(location, image_date_time, media_url) |>
  distinct()

#-----------------------------------------------------------------------------------------------------------------------

# Folder location for images
folder <- "G:/Shared drives/ABMI Camera Mammals/projects/Winter Ticks/Moose Images/"

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


