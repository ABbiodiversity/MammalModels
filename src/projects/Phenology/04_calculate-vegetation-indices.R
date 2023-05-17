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

#-----------------------------------------------------------------------------------------------------------------------

# Locations
sites <- c("252-SW", "1000-NE", "1146-NE", "1351-NW", "1400-NE", "1416-SE")

for (site in sites) {

  extractVIs(img.path = paste0(g_drive, "projects/Phenology/Timelapse Images/", site, "/"),
             roi.path = paste0(g_drive, "projects/Phenology/Outputs/ROI/", site, "_2020-06-01/"),
             vi.path = paste0(g_drive, "projects/Phenology/Outputs/VI/"))


}

# Calculate veg indices
VI.data <-extractVIs(paste0(g_drive, "projects/Phenology/Timelapse Images/AUB-12/Test/"), # image path
                     paste0(g_drive, "projects/Phenology/Outputs/ROI/"), # roi path to get the stored roi
                     vi.path = paste0(g_drive, "projects/Phenology/Outputs/VI/"), # where VI will be saved
                     roi.name =  "roi", #name of the roi in the ROI.rdata
                     plot = TRUE, begin = NULL, spatial = FALSE, date.code= 'yyyy-mm-dd', npixels = 1,
                     file.type = ".jpg", bind = FALSE)

load(paste0(g_drive, "projects/Phenology/Outputs/VI/VI.data.Rdata"))


with(VI.data$roi, plot(date, ri.av, pch=20, col='red', ylim=c(0.1,0.6), ylab='Relative indexes'))
with(VI.data$roi, points(date, gi.av, col='green', pch=20))
with(VI.data$roi, points(date, bi.av, col='blue', pch=20))

library(purrr)
library(magick)

create_gif(images_folder = paste0(g_drive, "projects/Phenology/Timelapse Images/AUB-12/Test/GIF"),
           file_type = "jpg",
           fps = 2,
           gif_name = "AUB-12-Pheno",
           gif_folder = paste0(g_drive, "projects/Phenology"))


