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
sites <- c("1146-NE", "1416-SE", "252-SW", "965-SE", "1000-NE", "1351-NW", "1400-NE")

for (site in sites) {

  extractVIs(img.path = paste0(g_drive, "projects/Phenology/Timelapse Images/", site, "/"),
             # Again, will have to be more nuanced here in the future in case the image date changes
             roi.path = paste0(g_drive, "projects/Phenology/Outputs/ROI/", site, "_2020-06-01"),
             vi.path = paste0(g_drive, "projects/Phenology/Outputs/VI/"),
             roi.name = paste0(site, "_2020-06-01_roi1"),
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

l <- list()

for (site in sites) {

  name <- paste0(site, "_VI.data")
  load(paste0(g_drive, "projects/Phenology/Outputs/VI/", name, ".RData"))
  d <- VI.data[[1]]
  l[[site]] <- d

}

save(l, file = paste0(g_drive, "projects/Phenology/Outputs/VI/VI.data.RData"))

# Make plots of each individual location
load(paste0(g_drive, "projects/Phenology/Outputs/VI/1400-NE_VI.data.Rdata"))

with(VI.data$`1400-NE_2020-06-01_roi1`, plot(date, ri.av, pch=20, cex = 1.5, col='red', ylim=c(0.1,0.6),
                                             ylab='Relative indexes', main = "1400-NE"))
with(VI.data$`1400-NE_2020-06-01_roi1`, points(date, gi.av, col='green', pch=20, cex = 1.5))
with(VI.data$`1400-NE_2020-06-01_roi1`, points(date, bi.av, col='blue', pch=20, cex = 1.5))




