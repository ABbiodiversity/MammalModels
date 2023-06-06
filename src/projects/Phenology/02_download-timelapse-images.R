#-----------------------------------------------------------------------------------------------------------------------

# Title:       Download timelapse images for Phenology analysis

# Description: Using the URL for timelapse images, download to a folder for further analysis.
# Author:      Marcus Becker
# Date:        May 2023

#-----------------------------------------------------------------------------------------------------------------------

# Root directory (Google)
g_drive <- "G:/Shared drives/ABMI Camera Mammals/"

# Read Timelapse url data — currently only EH, CMU, and OG projects
df_tl <- read.csv(paste0(g_drive, "data/processed/timelapse/eh-cmu-og_all-years_timelapse_12h-13h.csv"))

# Staff/setup dates
df_ss <- read_csv(paste0(g_drive, "data/lookup/staffsetup/eh-cmu-og_all-years_staffsetup-dates.csv"))


# DJH list of suggested sites (May 2023)
sites <- read.csv(paste0(g_drive, "projects/Phenology/Cameras for phenology RGB analysis May 18 2023.csv"))
# Vector of site locations (54 in total)
locations <- sites$Row.Labels

# Now we're doing the remainder
locations <- locations[39:54]

# Subset Timelapse url data
df_tl_subset <- df_tl[df_tl$location %in% locations, ]

#-----------------------------------------------------------------------------------------------------------------------

# Folder location for images
tl_folder <- "G:/Shared drives/ABMI Camera Mammals/projects/Phenology/Timelapse Images/"

for (i in locations) {

  print("Working on location ", i)

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
