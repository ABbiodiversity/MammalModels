#-----------------------------------------------------------------------------------------------------------------------

# Title:       Download timelapse images for Phenology analysis

# Description: Using the URL for timelapse images, download to a folder for further analysis.
# Author:      Marcus Becker
# Date:        May 2023

#-----------------------------------------------------------------------------------------------------------------------

# Root directory (Google)
g_drive <- "G:/Shared drives/ABMI Camera Mammals/"

# Read timelapse url data
# ABMI EH 2021 and 2021
abmi_tl <- read.csv(paste0(g_drive, "data/processed/timelapse/abmi_2020-2021_timelapse_12h-13h.csv"))

# Let's download images for the locations randomly selected by Dave H
sites <- c("252-SW", "1000-NE", "1146-NE", "1351-NW", "1400-NE", "1416-SE")

abmi_tl_subset <- abmi_tl[abmi_tl$location %in% sites, ]

# Folder location for images
tl_folder <- "G:/Shared drives/ABMI Camera Mammals/projects/Phenology/Timelapse Images/"

# Vector of unique locations
locations <- unique(abmi_tl_subset$location)

for (i in locations) {

  # Create directory for each location
  # Does it already exist?
  already.exist <- dir.exists(paste0(tl_folder, i))
  if (!already.exist) {
    dir.create(paste(tl_folder, i, sep = ""))
  }

  # Root directory for location
  dir <- paste(tl_folder, i, "/", sep = "")

  # Subset the data for only images only from this location
  d <- abmi_tl_subset[abmi_tl_subset$location == i, ]

  for (u in 1:nrow(d)) {

    url <- d[u, 4]
    date_time <- d[u, 3]
    filename <- strptime(date_time, format = "%Y-%m-%d")
    download.file(url, destfile = paste0(dir, filename, ".jpg"), mode = 'wb')

  }

}

#-----------------------------------------------------------------------------------------------------------------------
