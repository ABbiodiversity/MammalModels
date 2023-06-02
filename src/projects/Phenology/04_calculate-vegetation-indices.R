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
locations <- sites$Row.Labels[1:32]
locations <- locations[locations != "1393-NE"]

#-----------------------------------------------------------------------------------------------------------------------

# Move (Copy) ROI RData objects to appropriate folders
roi_rdata <- list.files(path = paste0(g_drive, "projects/Phenology/Outputs/ROI"),
                        pattern = "*Rdata",
                        full.names = TRUE)

# Just copying for now ... don't want anything bad to happen and to lose this information
for (roi in roi_rdata) {

  folder <- str_extract(roi, "(?<=/)[^/]*(?=roi.data.Rdata$)")
  file.copy(from = roi,
            to = paste0(g_drive, "projects/Phenology/Outputs/ROI/", folder, "/roi.data.Rdata"))

}

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

#-----------------------------------------------------------------------------------------------------------------------

# Let's do those remaining locations (CMU)
sites <- c("ADE-16", "AUB-7", "AUB-10", "AUB-12", "AUB-16", "AUB-21")

site <- "ADE-16"

# First, we need to separate Timelapse images in one location by deployment period


for (site in sites) {

  print(paste0("Working on location ", site))

  # Grab the appropriate folders for the location - now there are
  roi_site_folders <- list.dirs(paste0(g_drive, "projects/Phenology/Outputs/ROI"),
                               full.names = FALSE) |>
    str_subset(pattern = site)

  for (folder in roi_site_folders) {

    # Extract Vegetation Indices
    extractVIs(# Path to Timelapse images stays the same (just one folder per location)
               img.path = paste0(g_drive, "projects/Phenology/Timelapse Images/", site, "/"),
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



}



#-----------------------------------------------------------------------------------------------------------------------


# Make plots of each individual location
load(paste0(g_drive, "projects/Phenology/Outputs/VI/1400-NE_VI.data.Rdata"))

with(VI.data$`1400-NE_2020-06-01_roi1`, plot(date, ri.av, pch=20, cex = 1.5, col='red', ylim=c(0.1,0.6),
                                             ylab='Relative indexes', main = "1400-NE"))
with(VI.data$`1400-NE_2020-06-01_roi1`, points(date, gi.av, col='green', pch=20, cex = 1.5))
with(VI.data$`1400-NE_2020-06-01_roi1`, points(date, bi.av, col='blue', pch=20, cex = 1.5))




