#-----------------------------------------------------------------------------------------------------------------------

# Project:          ABMI

# Title:            Evaluating Spatial and Temporal Trends in Winter Tick Infestation Rates on Moose in Alberta
# Description:
# Author:           Marcus Becker

# Previous scripts:

#-----------------------------------------------------------------------------------------------------------------------

# Attach packages
library(tidyverse)
library(wildrtrax)
library(keyring)

# Download data
# Set environment variables (username/password)
Sys.setenv(WT_USERNAME = key_get("WT_USERNAME", keyring = "wildtrax"),
           WT_PASSWORD = key_get("WT_PASSWORD", keyring = "wildtrax"))

# Authenticate into WildTrax
wt_auth()

# Load data

# Set path to Shared Google Drive (G Drive)
g_drive <- "G:/Shared drives/ABMI Camera Mammals/"

# Source functions that may be needed (?)
# source("./src/Functions/estimate-density-tifc.R")

consolidate_tags <- function(main_report) {

  # Species tags w/ full information.
  y <- main_report |>
    mutate(age_class = trimws(strrep(str_c(age_class, ", "), individual_count), whitespace = ", "),
           sex_class = trimws(strrep(str_c(sex_class, ", "), individual_count), whitespace = ", "),
           tag_comments = trimws(strrep(str_c(sex_class, ", "), individual_count), whitespace = ", ")) |>
    # Now grouping my project_id, location_id, and image_id
    group_by(project_id, location_id, image_id, species_common_name) |>
    mutate(individual_count = sum(individual_count),
           age_class = paste0(age_class, collapse = ", "),
           sex_class = paste0(sex_class, collapse = ", ")) |>
    distinct(project_id, location_id, image_id, species_common_name, .keep_all = TRUE) |>
    ungroup()

  return(y)

}

#-----------------------------------------------------------------------------------------------------------------------

# TAGGING KEY

# 'Ticks' (under health_diseases)
# - 1 -> No ticks
# - 2 -> Slight (0-15%)
# - 3 -> Moderate (15-40%)
# - 4 -> Extreme (40-80%)
# - 5 -> Ghost (80%+)

# - UNKN (Not visible due to image quality)

#-----------------------------------------------------------------------------------------------------------------------

# Download data
# Project: "Prevalence of Winter Ticks on Moose
proj <- 2154

# Main report
data <- wt_download_report(project_id = proj,
                           sensor_id = "CAM",
                           reports = "main") |>
  mutate(tag_comments = ifelse(str_detect(tag_comments, ", "), str_replace(tag_comments, ",.*", ""), tag_comments)) |>
  filter(!is.na(tag_comments)) |>
  # Select relevant columns for analysis
  select(location, image_id, latitude, longitude, image_date_time, species_common_name,
         individual_count, age_class, sex_class,
         health_diseases, tag_comments, tag_needs_review)

images <- wt_download_report(project_id = proj,
                             sensor_id = "CAM",
                             reports = "image_report")

threshold <- 120

# Adult Females
adult_female <- data |>
  filter(age_class == "Adult",
         sex_class == "Female") |>
  select(-c(latitude, longitude, image_id)) |>
  # Order the dataframe
  arrange(location, image_date_time, species_common_name) |>
  # Calculate the time difference between subsequent images
  mutate(interval = int_length(image_date_time %--% lag(image_date_time))) |>
  # Is this considered a new detection?
  mutate(new_detection = ifelse(is.na(interval) | abs(interval) >= threshold, TRUE, FALSE)) |>
  # Number independent detections
  mutate(detection = c(1, cumsum(new_detection[-1]) + 1))

adult_female_unkns <- adult_female |>
  filter(tag_comments == "UNKN") |>
  group_by(location, detection) |>
  summarise(n_individuals = max(individual_count),
            start = min(image_date_time)) |>
  mutate(hli = "UNKN")

adult_female_kns <- adult_female |>
  filter(!tag_comments == "UNKN") |>
  mutate(tag_comments = as.numeric(tag_comments)) |>
  group_by(location, detection) |>
  summarise(n_individuals = max(individual_count),
            start = min(image_date_time),
            hli = mean(tag_comments, na.rm = TRUE)) |>
  # Sigh
  mutate(hli = round(hli, digits = 0)) |>
  mutate(hli = as.character(hli))

adult_female_all <- bind_rows(adult_female_kns, adult_female_unkns) |>
  mutate(date = date(start)) |>
  ungroup() |>
  distinct(location, n_individuals, hli, date) |>
  mutate(class = "Adult Female")

# Adult Males
adult_male <- data |>
  filter(age_class == "Adult",
         sex_class == "Male") |>
  select(-c(latitude, longitude, image_id)) |>
  # Order the dataframe
  arrange(location, image_date_time, species_common_name) |>
  # Calculate the time difference between subsequent images
  mutate(interval = int_length(image_date_time %--% lag(image_date_time))) |>
  # Is this considered a new detection?
  mutate(new_detection = ifelse(is.na(interval) | abs(interval) >= threshold, TRUE, FALSE)) |>
  # Number independent detections
  mutate(detection = c(1, cumsum(new_detection[-1]) + 1))

adult_male_unkns <- adult_male |>
  filter(tag_comments == "UNKN") |>
  group_by(location, detection) |>
  summarise(n_individuals = max(individual_count),
            start = min(image_date_time)) |>
  mutate(hli = "UNKN")

adult_male_kns <- adult_male |>
  filter(!tag_comments == "UNKN") |>
  mutate(tag_comments = as.numeric(tag_comments)) |>
  group_by(location, detection) |>
  summarise(n_individuals = max(individual_count),
            start = min(image_date_time),
            hli = mean(tag_comments, na.rm = TRUE)) |>
  # Sigh
  mutate(hli = round(hli, digits = 0)) |>
  mutate(hli = as.character(hli))

adult_male_all <- bind_rows(adult_male_kns, adult_male_unkns) |>
  mutate(date = date(start)) |>
  ungroup() |>
  distinct(location, n_individuals, hli, date) |>
  mutate(class = "Adult Male")

# Adult Unkns
adult_unkn <- data |>
  filter(age_class == "Adult",
         sex_class == "Unkn") |>
  select(-c(latitude, longitude, image_id)) |>
  # Order the dataframe
  arrange(location, image_date_time, species_common_name) |>
  # Calculate the time difference between subsequent images
  mutate(interval = int_length(image_date_time %--% lag(image_date_time))) |>
  # Is this considered a new detection?
  mutate(new_detection = ifelse(is.na(interval) | abs(interval) >= threshold, TRUE, FALSE)) |>
  # Number independent detections
  mutate(detection = c(1, cumsum(new_detection[-1]) + 1))

adult_unkn_unkns <- adult_unkn |>
  filter(tag_comments == "UNKN") |>
  group_by(location, detection) |>
  summarise(n_individuals = max(individual_count),
            start = min(image_date_time)) |>
  mutate(hli = "UNKN")

adult_unkn_kns <- adult_unkn |>
  filter(!tag_comments == "UNKN") |>
  mutate(tag_comments = as.numeric(tag_comments)) |>
  group_by(location, detection) |>
  summarise(n_individuals = max(individual_count),
            start = min(image_date_time),
            hli = mean(tag_comments, na.rm = TRUE)) |>
  # Sigh
  mutate(hli = round(hli, digits = 0)) |>
  mutate(hli = as.character(hli))

adult_unkn_all <- bind_rows(adult_unkn_kns, adult_unkn_unkns) |>
  mutate(date = date(start)) |>
  ungroup() |>
  distinct(location, n_individuals, hli, date) |>
  mutate(class = "Adult Unkns")

# Juveniles
juv <- data |>
  filter(age_class == "Juv") |>
  select(-c(latitude, longitude, image_id)) |>
  # Order the dataframe
  arrange(location, image_date_time, species_common_name) |>
  # Calculate the time difference between subsequent images
  mutate(interval = int_length(image_date_time %--% lag(image_date_time))) |>
  # Is this considered a new detection?
  mutate(new_detection = ifelse(is.na(interval) | abs(interval) >= threshold, TRUE, FALSE)) |>
  # Number independent detections
  mutate(detection = c(1, cumsum(new_detection[-1]) + 1))

juv_unkns <- juv |>
  filter(tag_comments == "UNKN") |>
  group_by(detection, location) |>
  summarise(n_individuals = max(individual_count),
            start = min(image_date_time)) |>
  mutate(hli = "UNKN")

juv_kns <- juv |>
  filter(!tag_comments == "UNKN") |>
  mutate(tag_comments = as.numeric(tag_comments)) |>
  group_by(detection, location) |>
  summarise(n_individuals = max(individual_count),
            start = min(image_date_time),
            hli = mean(tag_comments, na.rm = TRUE)) |>
  # Sigh
  mutate(hli = round(hli, digits = 0)) |>
  mutate(hli = as.character(hli))

juv_all <- bind_rows(juv_kns, juv_unkns) |>
  mutate(date = date(start)) |>
  ungroup() |>
  distinct(location, n_individuals, hli, date) |>
  mutate(class = "Juv")

#-----------------------------------------------------------------------------------------------------------------------

all <- bind_rows(juv_all, adult_male_all, adult_female_all, adult_unkn_all)

class_status <- all |>
  mutate(classified = ifelse(hli == "UNKN", "no", "yes")) |>
  uncount(n_individuals) |>
  group_by(classified) |>
  tally()

class_demo_status <- all |>
  uncount(n_individuals) |>
  filter(!hli == "UNKN") |>
  group_by(class) |>
  tally()

class_demo_tick <- all |>
  uncount(n_individuals) |>
  filter(!hli == "UNKN") |>
  mutate(tick = ifelse(hli > 1, "yes", "no")) |>
  group_by(class, tick) |>
  tally()

# Sankey
# Sankey
#// Enter Flows between Nodes, like this:
#  //         Source [AMOUNT] Target

#Total \nIndividuals [1458] Classified \nTick Status #52A211
#Total \nIndividuals [419] Unverified \nTick Status #708090

#Classified \nTick Status [593] Adult Female #9467BD
#Classified \nTick Status [389] Adult Male #9467BD
#Classified \nTick Status [289] Adult Unkn #9467BD
#Classified \nTick Status [187] Juvenile #fb8072

#Adult Female [368] No Ticks\n(HLI 1) #9467BD
#Adult Female [225] Ticks\n(HLI 2-5) #9467BD

#Adult Male [260] No Ticks\n(HLI 1) #9467BD
#Adult Male [129] Ticks\n(HLI 2-5) #9467BD

#Adult Unkn [189] No Ticks\n(HLI 1) #9467BD
#Adult Unkn [100] Ticks\n(HLI 2-5) #9467BD

#Juvenile [149] No Ticks\n(HLI 1) #fb8072
#Juvenile [38] Ticks\n(HLI 2-5) #fb8072

#-----------------------------------------------------------------------------------------------------------------------

monthly_trend <- all |>
  filter(!hli == "UNKN") |>
  mutate(hli = as.numeric(hli)) |>
  mutate(month = month(date, label = TRUE)) |>
  uncount(n_individuals) |>
  group_by(month) |>
  summarise(mean = mean(hli),
            n = n())

#-----------------------------------------------------------------------------------------------------------------------

biweekly_values <- all |>
  filter(!hli == "UNKN") |>
  mutate(hli = as.numeric(hli)) |>
  mutate(yday = yday(date)) |>
  uncount(n_individuals) |>
  mutate(period = case_when(
    yday <= 15 ~ "Jan 1-15",
    yday > 15 & yday <= 31 ~ "Jan 16-31",
    yday > 31 & yday <= 46 ~ "Feb 1-15",
    yday > 46 & yday <= 59 ~ "Feb 16-28",
    yday > 59 & yday <= 74 ~ "Mar 1-15",
    yday > 74 & yday <= 90 ~ "Mar 16-31",
    yday > 90 & yday <= 105 ~ "Apr 1-15",
    yday > 105 & yday <= 120 ~ "Apr 16-30",
    yday > 120 & yday <= 135 ~ "May 1-15",
    yday > 135 & yday <= 151 ~ "May 16-31",
    yday > 151 & yday <= 166 ~ "June 1-15",
    yday > 166 ~ "June 16-30"
  )) |>
  mutate(period = factor(period, levels = c("Jan 1-15", "Jan 16-31",
                                            "Feb 1-15", "Feb 16-28",
                                            "Mar 1-15", "Mar 16-31",
                                            "Apr 1-15", "Apr 16-30",
                                            "May 1-15", "May 16-31",
                                            "June 1-15", "June 16-30"))) |>
  mutate(hli = ifelse(location == "1024-NW" & date == "2015-05-30" & hli == "4", 5, hli))

biweekly_trend <- biweekly_values |>
  group_by(period) |>
  summarise(mean = mean(hli))

hline <- data.frame(
  period = c(1:12),
  mean = biweekly_trend$mean
)


# Okay, let's plot this ... somehow.

library(ggplot2)
library(abmi.themes)
add_abmi_fonts()

biweekly_values |>
  mutate(year = year(date)) |>
  ggplot(aes(x = period, y = hli)) +
  geom_jitter(aes(color = hli),
    alpha = 0.25, width = 0.15, height = 0.25, size = 2) +
  geom_segment(data = hline,
               aes(x = period - 0.4, xend = period + 0.4,
                   y = mean, yend = mean),
               color = "red", linewidth = 1.75) +
  labs(y = "",
       x = "") +
  scale_color_gradient(low = "grey10", high = "grey80") +
  scale_y_continuous(breaks = c(1, 2, 3, 4, 5), labels = c("None (1)", "Slight (2)", "Moderate (3)", "Severe (4)", "Ghost (5)")) +
  theme_abmi() +
  theme(axis.text.x = element_text(hjust = 1, angle = 45, size = 15),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 17, face = "bold"),
        axis.title.y = element_blank(),
        legend.position = "none")

# Facet wrap by year

biweekly_trend_year <- biweekly_values |>
  mutate(year = year(date)) |>
  group_by(year, period, .drop = FALSE) |>
  summarise(mean = mean(hli))

hline <- data.frame(
  year = c(2015:2018)
) |>
  crossing(period = 1:12) |>
  bind_cols(mean = biweekly_trend_year$mean) |>
  mutate(year = as.factor(year))


biweekly_values |>
  mutate(year = year(date)) |>
  ggplot(aes(x = period, y = hli)) +
  geom_jitter(aes(color = hli),
              alpha = 0.25, width = 0.15, height = 0.25, size = 1.5) +
  scale_color_gradient(low = "grey10", high = "grey80", guide = "none") +
  new_scale_color() +
  geom_point(data = hline, aes(x = period, y = mean, color = year), size = 2) +
  geom_line(data = hline, aes(x = period, y = mean, color = year), linewidth = 1) +
  #geom_segment(data = hline,
  #             aes(x = period - 0.4, xend = period + 0.4,
  #                 y = mean, yend = mean, color = year), linewidth = 1.75) +
  labs(y = "",
       x = "") +
  scale_y_continuous(breaks = c(1, 2, 3, 4), labels = c("None (1)", "Slight (2)", "Moderate (3)", "Severe (4)"), limits = c(1, 4)) +
  theme_abmi() +
  theme(axis.text.x = element_text(hjust = 1, angle = 45, size = 12),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 14, face = "bold"),
        axis.title.y = element_blank(),
        strip.text = element_text(size = 14),
        legend.position = "top")

#-----------------------------------------------------------------------------------------------------------------------

# Values by region (NR? LUF?)

library(sf)
library(ggnewscale)
library(MetBrewer)

load(paste0(g_drive, "data/spatial/provincial-boundary.Rdata"))

province.shapefile <- province.shapefile |> st_transform(4326)

ab_wmus <- st_read(paste0(g_drive, "data/spatial/ab_wmus_all.shp")) |>
  st_transform(4326) |>
  select(WMUNIT_NAM)

# Spatial subset of the HLI data
locations <- data |>
  select(location, latitude, longitude) |>
  distinct() |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 3400)

all_locations <- all |>
  uncount(n_individuals) |>
  left_join(locations) |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
  st_join(ab_wmus, left = TRUE) |>
  st_set_geometry(NULL) |>
  #select(-Color) |>
  filter(!hli == "UNKN") |>
  mutate(hli = as.numeric(hli),
         month = month(date, label = TRUE, abbr = FALSE)) |>
  filter(month == "April" | month == "May") |>
  group_by(WMUNIT_NAM) |>
  summarise(n_moose = n(),
            avg_hli = mean(hli)) |>
  filter(n_moose > 3)

ab_wmus_gg <- ab_wmus |>
  left_join(all_locations) |>
  #filter(!is.na(avg_hli)) |>
  mutate(avg_hli = ifelse(WMUNIT_NAM == "Saddle Hills", 3, avg_hli)) |>
  mutate(avg_hli = ifelse(WMUNIT_NAM == "Beaverlodge", 2.5, avg_hli)) |>
  st_transform(3400)


city.locations <- data.frame(City = c("Calgary", "Edmonton", "Grande Prarie", "Fort McMurrary"),
                             Easting = c(567338, 599585, 256736, 723381),
                             Northing = c(5654236, 5930763, 6121296, 6290032))

ggplot() +
  #geom_sf(data = province.shapefile, aes(color = NRNAME, fill = NRNAME), show.legend = FALSE) +
  geom_sf(data = ab_wmus_gg, aes(color = avg_hli, fill = avg_hli), show.legend = TRUE) +
  coord_sf(datum = NA) +
  scale_fill_viridis_c(na.value = "grey90", name = "Mean HLI") +
  scale_color_viridis_c(na.value = "grey60", name = "Mean HLI") +
  new_scale_color() +
  new_scale_fill() +
  geom_point(data = city.locations, aes(x = Easting, y = Northing), size = 3, shape = 25, fill = "#FFFFFF") +
  geom_text(data = city.locations, aes(x = Easting, y = Northing, label = City),
            size = 3, hjust = 0, nudge_x = 12000, nudge_y = 2000) +
  theme_light() +
  theme(axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        title = element_blank(),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12),
        axis.line = element_blank(),
        panel.border = element_blank(),
        legend.position = "right")

province.shapefile <- st_transform(province.shapefile, 3400)

# Spatial subset of the HLI data
locations <- data |>
  select(location, latitude, longitude) |>
  distinct() |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

loc <- read_csv(paste0(g_drive, "data/lookup/locations/abmi-cmu_public-locations_2021-10-20.csv")) |>
  filter(str_detect(project, "Health 2015|Health 2016|Health 2017|Health 2018")) |>select(location, latitude, longitude) |>
  distinct() |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)


# Sampling sites
ggplot() +
  geom_sf(data = province.shapefile,
          #aes(color = NRNAME),
          color = "grey80",
          fill = "grey80",
          show.legend = FALSE) +
  geom_sf(data = loc, color = "#122451") +
  #geom_sf(data = ab_wmus_gg, aes(color = avg_hli, fill = avg_hli), show.legend = TRUE) +
  coord_sf(datum = NA) +
  new_scale_color() +
  new_scale_fill() +
  geom_point(data = city.locations, aes(x = Easting, y = Northing), size = 4, shape = 25, fill = "#FFFFFF") +
  geom_label(data = city.locations, aes(x = Easting, y = Northing, label = City),
            size = 3.5, hjust = 0, nudge_x = 12000, nudge_y = 2000) +
  theme_light() +
  theme(axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        title = element_blank(),
        legend.text = element_blank(),
        legend.title = element_blank(),
        axis.line = element_blank(),
        panel.border = element_blank(),
        legend.position = "none")

#-----------------------------------------------------------------------------------------------------------------------

# Images

images_comment <- images |>
  filter(!is.na(image_comments)) |>
  select(location, image_id, image_date_time, image_comments, media_url) |>
  # Anything that says "example"
  filter(str_detect(image_comments, "Normal|normal"))

ext <- data |>
  filter(tag_comments == "4", age_class == "Juv") |>
  select(location, image_date_time, individual_count, tag_comments)

#-----------------------------------------------------------------------------------------------------------------------

# gifs

create_gif(images_folder = paste0(g_drive, "data/base/sample-images/Moose Ticks/June"),
           file_type = "jpg",
           fps = 1,
           gif_folder = paste0(g_drive, "data/base/sample-images/Moose Ticks"),
           gif_name = "june.gif")




