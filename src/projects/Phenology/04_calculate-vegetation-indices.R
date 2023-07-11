#-----------------------------------------------------------------------------------------------------------------------

# Title:       Calculate Vegetation Indices

# Description:
# Author:      Marcus Becker
# Date:        May 2023

#-----------------------------------------------------------------------------------------------------------------------

# Root directory (Google)
g_drive <- "G:/Shared drives/ABMI Camera Mammals/"

# Attach packages
library(phenopix)
library(stringr)
library(tictoc)

# Locations that have already been done
loc_done <- list.files(paste0(g_drive, "projects/Phenology/Outputs/VI"), pattern = "*.RData") |>
  str_extract("^[^_]+") |>
  unique()

# Locations to do
loc_todo <- list.dirs(paste0(g_drive, "projects/Phenology/Outputs/ROI"), full.names = FALSE) |>
  str_extract("^[^_]+") |>
  unique() |>
  na.omit() |>
  setdiff(loc_done)

#-----------------------------------------------------------------------------------------------------------------------

# Extract Vegetation Indices for all locations left to do

for (site in loc_todo) {

  tic()
  print(paste0("Working on location ", site))

  # Grab the appropriate folders for the location - now there are multiple
  roi_site_folders <- list.dirs(paste0(g_drive, "projects/Phenology/Outputs/ROI"),
                               full.names = FALSE) |>
    str_subset(pattern = paste0(site, "_"))

  for (folder in roi_site_folders) {

    # Extract Vegetation Indices
    try(extractVIs(# Now there are multiple folders for a location
               img.path = paste0(g_drive,
                                 "projects/Phenology/Timelapse Images/",
                                 site,
                                 "/",
                                 folder,
                                 "/"),
               # Path to folder with ROI defined for location
               roi.path = paste0(g_drive,
                                 "projects/Phenology/Outputs/ROI/",
                                 folder),
               # Path to folder where VIs will be saved
               vi.path = paste0(g_drive, "projects/Phenology/Outputs/VI/"),
               roi.name = paste0(folder, "_roi1"),
               plot = TRUE,
               begin = NULL,
               spatial = FALSE,
               date.code = 'yyyy-mm-dd',
               npixels = 1,
               file.type = "jpg",
               bind = FALSE,
               ncores = "all"))

    # Rename the Rdata from generic VI.data.RData to a site-specific name
    try(file.rename(from = paste0(g_drive, "projects/Phenology/Outputs/VI/VI.data.RData"),
                to = paste0(g_drive, "projects/Phenology/Outputs/VI/", folder, "_VI.data.RData")))

  }

  toc()

}

#-----------------------------------------------------------------------------------------------------------------------

# Combine the VI data from each location into one object

sites <- locations

for (site in sites) {

  VI <- list()

  files <- list.files(paste0(g_drive, "projects/Phenology/Outputs/VI"),
                      pattern = "*.RData",
                      full.names = TRUE) |>
    str_subset(pattern = paste0(site, "_"))

  for (file in files) {

    name <- str_extract(file, "(?<=/)[^/]*$")
    name <- str_remove(name, "_VI.data.RData$")
    load(file)
    vi <- VI.data[[1]]
    VI[[name]] <- vi

  }

  save(VI, file = paste0(g_drive, "projects/Phenology/Outputs/VI/", site, "_VI.data.RData"))

}

#-----------------------------------------------------------------------------------------------------------------------
