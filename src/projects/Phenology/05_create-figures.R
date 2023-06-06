#-----------------------------------------------------------------------------------------------------------------------

# Title:       Plots for Vegetation Indices

# Description:
# Author:      Marcus Becker
# Date:        June 2023

#-----------------------------------------------------------------------------------------------------------------------

# Make plots of each individual location
load(paste0(g_drive, "projects/Phenology/Outputs/VI/1400-NE_VI.data.Rdata"))

with(VI.data$`1400-NE_2020-06-01_roi1`, plot(date, ri.av, pch=20, cex = 1.5, col='red', ylim=c(0.1,0.6),
                                             ylab='Relative indexes', main = "1400-NE"))
with(VI.data$`1400-NE_2020-06-01_roi1`, points(date, gi.av, col='green', pch=20, cex = 1.5))
with(VI.data$`1400-NE_2020-06-01_roi1`, points(date, bi.av, col='blue', pch=20, cex = 1.5))



