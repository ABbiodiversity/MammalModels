#-----------------------------------------------------------------------------------------------------------------------

# Title:       Plots for Vegetation Indices

# Description:
# Author:      Marcus Becker
# Date:        June 2023

#-----------------------------------------------------------------------------------------------------------------------

g_drive <- "G:/Shared drives/ABMI Camera Mammals/"

library(readr)
library(dplyr)
library(phenopix)
library(magick)
library(raster)

# Let's try with AUB-12 first.

# Make plots of each individual location
load(paste0(g_drive, "projects/Phenology/Outputs/VI/Individual Objects/AUB-12_2020-02-03_VI.data.Rdata"))

with(VI.data$`AUB-12_2020-02-03_roi1`, plot(date, ri.av, pch=20, cex = 1.5, col='red', ylim=c(0.1,0.6),
                                             ylab='Relative indexes', main = "1400-NE"))
with(VI.data$`AUB-12_2020-02-03_roi1`, points(date, gi.av, col='green', pch=20, cex = 1.5))
with(VI.data$`AUB-12_2020-02-03`, points(date, bi.av, col='blue', pch=20, cex = 1.5))

#-----------------------------------------------------------------------------------------------------------------------

aub12 <- read_csv(paste0(g_drive, "projects/Phenology/Outputs/Fit/Export corrected RGB and spline AUB-12_2020-02-03.csv"))

# AUB-12 images
d <- df_files |>
  filter(str_detect(location, "AUB-12"),
         date > as.Date("2020-02-02"),
         date < as.Date("2021-01-24")) |>
  mutate(folder = str_extract(path, "(?<=/)[^/]+(?=/[^/]+$)")) |>
  filter(str_detect(folder, "_")) |>
  mutate(week = week(date)) |>
  group_by(week) |>
  filter(row_number() == 1) |>
  ungroup() |>
  dplyr::select(-week) |>
  filter(!date == "2020-02-03")

d <- data.frame(d)

for (i in 1:nrow(d)) {

  pir <- d[i, 1]
  t <- d[i, 4]
  proi <- paste0(g_drive, "projects/Phenology/Outputs/ROI/", t, "/")

  try(print_roi(path_img_ref = pir,
                path_ROIs = proi,
                col = "red"))

  date <- d[i, 3]

  if (nrow(d) > 0) {
    dev.print(jpeg, file = paste0(proi, "ROI-check_", date, ".jpg"), width = 1456, height = 1024)
    dev.off()
  } else {
    next
  }

}

# Create gif
create_gif(images_folder = paste0(g_drive, "projects/Phenology/Outputs/ROI/AUB-12_2020-02-03/gif"),
           file_type = "jpg",
           fps = 4,
           gif_name = "AUB-12_2020-02-03",
           gif_folder = paste0(g_drive, "projects/Phenology/Outputs/ROI/AUB-12_2020-02-03/gif"))

# Looks good.

#-----------------------------------------------------------------------------------------------------------------------

library(ggplot2)
library(gganimate)

check <- aub12 |>
  group_by(DOY1) |>
  tally()

aub12_gif <- aub12 |>
  mutate(date = date(Date)) |>
  left_join(d, by = "date") |>
  filter(!is.na(path)) |>
  pivot_longer(cols = c(ri.av, gi.av, bi.av, pR, pG, pB), names_to = "name", values_to = "value") |>
  mutate(colour = case_when(
    str_detect(name, "r|R") ~ "Red",
    str_detect(name, "b|B") ~ "Blue",
    str_detect(name, "g|G") ~ "Green"
  )) |>
  mutate(type = case_when(
    str_detect(name, "av") ~ "Index",
    str_detect(name, "p") ~ "Prediction"
  )) |>
  dplyr::select(-name)

aub12_lines <- aub12_gif |>
  filter(type == "Prediction",
         !colour == "Blue")

aub12_points <- aub12_gif |>
  filter(type == "Index",
         !colour == "Blue")

p <- ggplot() +
  geom_point(data = aub12_points, aes(x = date, y = value, color = colour, group = date), size = 3, alpha = 0.5) +
  #geom_line(data = aub12_lines, aes(x = date, y = value, color = colour), linewidth = 1.5) +
  labs(title = 'Date: {frame_along}',
       x = "",
       y = "") +
  scale_color_manual(values = c("green", "red")) +
  transition_reveal(date, keep_last = TRUE) +
  theme(legend.position = "none")

p

animate(p, duration = 13, fps = 4)

gganimate::anim_save(filename = "aub-12.gif")
















