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

# DJH list of suggested sites (May 2023)
sites <- read.csv(paste0(g_drive, "projects/Phenology/Cameras for phenology RGB analysis May 18 2023.csv"))
locations <- sites$Row.Labels[39:54]
locations <- locations[locations != "1393-NE"]

#-----------------------------------------------------------------------------------------------------------------------

# Locations - focus on the ones with one deployment period for now.
sites <- locations
sites <- sites[3:31]

for (site in sites) {

  print(paste0("Working on location ", site))

  # Grab the appropriate folder for the location
  roi_site_folder <- list.dirs(paste0(g_drive, "projects/Phenology/Outputs/ROI"),
                        full.names = FALSE) |>
    str_subset(pattern = site)

  # Extract Vegetation Indices
  extractVIs(img.path = paste0(g_drive, "projects/Phenology/Timelapse Images/", site, "/"),
             # Path to folder with ROI defined for location
             roi.path = paste0(g_drive,
                               "projects/Phenology/Outputs/ROI/",
                               roi_site_folder),
             # Path to folder where VIs will be saved
             vi.path = paste0(g_drive, "projects/Phenology/Outputs/VI/"),
             roi.name = paste0(roi_site_folder, "_roi1"),
             plot = TRUE,
             begin = NULL,
             spatial = FALSE,
             date.code = 'yyyy-mm-dd',
             npixels = 1,
             file.type = "jpg",
             bind = FALSE)

  # Rename the Rdata from generic VI.data.RData to a site-specific name
  file.rename(from = paste0(g_drive, "projects/Phenology/Outputs/VI/VI.data.RData"),
              to = paste0(g_drive, "projects/Phenology/Outputs/VI/", site, "_VI.data.RData"))

}

# If you want to combine the VI data together into one RData object

l <- list()

for (site in sites) {

  name <- paste0(site, "_VI.data")
  load(paste0(g_drive, "projects/Phenology/Outputs/VI/", name, ".RData"))
  d <- VI.data[[1]]
  l[[site]] <- d

}

save(l, file = paste0(g_drive, "projects/Phenology/Outputs/VI/VI.data.RData"))

#-----------------------------------------------------------------------------------------------------------------------

# Let's do those remaining locations (CMU)
sites <- locations
sites <- sites[5:16]

tic()
for (site in sites) {

  print(paste0("Working on location ", site))

  # Grab the appropriate folders for the location - now there are multiple
  roi_site_folders <- list.dirs(paste0(g_drive, "projects/Phenology/Outputs/ROI"),
                               full.names = FALSE) |>
    # Note: This effs up in cases where there are multiple matches: e.g., FMM-1 and FMM-14
    str_subset(pattern = site)

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

}
toc()

# Combine the VI data from each location into one object

sites <- c("ADE-16", "AUB-7", "AUB-10", "AUB-12", "AUB-16", "AUB-21")

for (site in sites) {

  VI <- list()

  files <- list.files(paste0(g_drive, "projects/Phenology/Outputs/VI"),
                      pattern = "*.RData",
                      full.names = TRUE) |>
    str_subset(pattern = site)

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
