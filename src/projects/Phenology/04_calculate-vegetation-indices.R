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

# Move all the plots to a separate folder

plots <- list.files(paste0(g_drive, "projects/Phenology/Outputs/VI"),
                    pattern = "*.png",
                    full.names = TRUE)

for (plot in plots) {

  file.rename(from = paste0(g_drive, "projects/Phenology/Outputs/VI/", plot),
              to = paste0(g_drive, "projects/Phenology/Outputs/VI/Plots/", plot))

}

#-----------------------------------------------------------------------------------------------------------------------

# Combine the VI data from each location into one object

# Locations that have been done
loc_done <- list.files(paste0(g_drive, "projects/Phenology/Outputs/VI/Individual Objects"), pattern = "*.RData") |>
  str_extract("^[^_]+") |>
  unique()

for (site in loc_done) {

  VI <- list()

  files <- list.files(paste0(g_drive, "projects/Phenology/Outputs/VI/Individual Objects"),
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

  save(VI, file = paste0(g_drive, "projects/Phenology/Outputs/VI/Full Objects/", site, "_VI.data.RData"))

}

#-----------------------------------------------------------------------------------------------------------------------

# Create one single object (how big is this going to be ...?)

cmu_files <- list.files(paste0(g_drive, "projects/Phenology/Outputs/VI/Full Objects"),
                        full.names = FALSE) |>
  str_subset(pattern = "^[A-Z]")

VI.full <- list()

for (file in cmu_files) {

  path <- paste0(g_drive, "projects/Phenology/Outputs/VI/Full Objects/", file)
  name <- str_remove(file, "_VI.data.RData$")

  load(path)

  VI.full[[name]] <- VI

}

save(VI.full, file = paste0(g_drive, "projects/Phenology/Outputs/VI/Full Objects/CMU-all_2023-07-12_VI.data.RData"))



