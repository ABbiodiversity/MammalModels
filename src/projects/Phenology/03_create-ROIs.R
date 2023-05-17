#-----------------------------------------------------------------------------------------------------------------------

# Title:       Create ROIs (Regions of Interest)

# Description: Using a reference image for each location, delineate the ROI(s)
# Author:      Marcus Becker
# Date:        May 2023

#-----------------------------------------------------------------------------------------------------------------------

# Root directory (Google)
g_drive <- "G:/Shared drives/ABMI Camera Mammals/"

# Attach packages
library(phenopix)
library(stringr)

#-----------------------------------------------------------------------------------------------------------------------

# Create a folder with a reference image of each location
# Let's just do 5 for now
# Reference image ... how about June 1?

files <- list.files(paste0(g_drive, "projects/Phenology/Timelapse Images/"),
                    recursive = TRUE,
                    full.names = TRUE) |>
  # Will need this to be more nuanced in the future ...
  str_subset(pattern = "2020-06-01")

for (file in files) {

  file.copy(from = file, to = paste0(g_drive, "projects/Phenology/Timelapse Images/Reference"))

}

# New file paths for reference images
reference <- list.files(paste0(g_drive, "projects/Phenology/Timelapse Images/Reference"),
                  full.names = FALSE)

# Output folder
output_roi <- paste0(g_drive, "projects/Phenology/Outputs/ROI/")

for (ref in reference) {

  path <- paste0(g_drive, "projects/Phenology/Timelapse Images/Reference/", ref)
  name <- str_remove(ref, pattern = ".jpg")
  dir.create(paste0(output_roi, name))

  DrawMULTIROI(path_img_ref = path,
               path_ROIs = paste0(output_roi, name),
               nroi = 1,
               file.type = ".JPG",
               roi.names = paste0(name, "_roi1"))

}

#-----------------------------------------------------------------------------------------------------------------------

