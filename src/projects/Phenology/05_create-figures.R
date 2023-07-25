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

aub12_dates <- read_csv(paste0(g_drive, "projects/Phenology/Outputs/Fit/Camera phenology results subset 4 Dates July 19 2023.csv")) |>
  filter(Session == "AUB-12_2020-02-03") |>
  pivot_longer(cols = `Melt.Peak.Der`:`FirstSnow.Der`, names_to = "Metric", values_to = "Date") |>
  mutate(Date = ymd(paste0(Year, "-", Date))) |>
  select(Session, type = Metric, date = Date) |>
  mutate(colour = case_when(
    type == "Melt.Peak.Der" ~ "#00cdcd",
    type == "Melt.Peak.Seg" ~ "#66cdaa",
    type == "Melt.End.Seg" ~ "#e8e8e8",
    type == "Greenup.20" ~ "#00ee00",
    type == "Greenup.Peak" ~ "#3cb371",
    type == "Greenup.80" ~ "#006400",
    type == "Summer.Peak" ~ "#0000ff",
    type == "Sen.20" ~ "#ffa500",
    type == "Sen.Peak" ~ "#cd8500",
    type == "Sen.80" ~ "#8b4500",
    type == "FirstSnow.Der" ~ "1e90ff"
  ))

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

aub12_bi.av <- VI.data[["AUB-12_2020-02-03_roi1"]] |>
  select(date, bi.av.new = bi.av) |>
  mutate(date = date(date))

aub12_gif <- aub12 |>
  mutate(date = date(Date)) |>
  left_join(d, by = "date") |>
  left_join(aub12_bi.av, by = "date") |>
  filter(!is.na(path)) |>
  select(-bi.av) |>
  pivot_longer(cols = c(ri.av, gi.av, bi.av.new, pR, pG, pB), names_to = "name", values_to = "value") |>
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
  filter(type == "Prediction")

aub12_points_green <- aub12_gif |>
  filter(type == "Index",
         colour == "Green")

aub12_points_blue <- aub12_gif |>
  filter(type == "Index",
         colour == "Blue")

aub12_points_red <- aub12_gif |>
  filter(type == "Index",
         colour == "Red")

p_dyn <- ggplot() +
  geom_line(data = aub12_lines, aes(x = date, y = value, color = colour), linewidth = 1.5) +
  geom_point(data = aub12_points_blue, aes(x = date, y = value, group = date), color = "#0000ff", size = 4, alpha = 0.2) +
  geom_point(data = aub12_points_green, aes(x = date, y = value, group = date), color = "#00cd00", size = 4, alpha = 0.3) +
  geom_point(data = aub12_points_red, aes(x = date, y = value, group = date), color = "#ffb5c5", size = 4, alpha = 0.5) +
  labs(title = 'AUB-12: {frame_along}',
       x = "",
       y = "Relative Proportion") +
  scale_x_date(date_breaks = "2 months", date_labels = "%b %Y") +
  scale_color_manual(values = c("#0000ff", "#00cd00", "#ffb5c5")) +
  transition_reveal(date, keep_last = TRUE) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
        axis.title.y = element_text(size = 12, margin = margin(0, 0.5, 0, 0, unit = "cm")))

animate(p_dyn, duration = 13, fps = 4)

gganimate::anim_save(filename = "aub-12.gif")

# Static
p_stat <- ggplot() +
  geom_line(data = aub12_lines, aes(x = date, y = value, color = colour), linewidth = 1.5, alpha = 0.5) +
  geom_point(data = aub12_points_blue, aes(x = date, y = value, group = date), color = "#0000ff", size = 4, alpha = 0.1) +
  geom_point(data = aub12_points_green, aes(x = date, y = value, group = date), color = "#00cd00", size = 4, alpha = 0.15) +
  geom_point(data = aub12_points_red, aes(x = date, y = value, group = date), color = "#ffb5c5", size = 4, alpha = 0.2) +
  geom_vline(data = aub12_dates, aes(xintercept = date), color = aub12_dates$colour, linewidth = 1) +
  labs(title = 'AUB-12',
       x = "",
       y = "Relative Proportion") +
  scale_x_date(date_breaks = "2 months", date_labels = "%b %Y") +
  scale_color_manual(values = c("#0000ff", "#00cd00", "#ffb5c5")) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
        axis.title.y = element_text(size = 12, margin = margin(0, 0.5, 0, 0, unit = "cm")))

p_stat

ggsave(filename = "aub-12.png")














