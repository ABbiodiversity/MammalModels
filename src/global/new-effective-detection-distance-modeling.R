#-----------------------------------------------------------------------------------------------------------------------

# Title:       Effective detection distance (EDD) modeling
# Description:
# Author(s):   Dave Huggard, Marcus Becker

# Previous scripts:

#-----------------------------------------------------------------------------------------------------------------------

# Attach packages
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(mgcv)

# Set path to Google Drive
g_drive <- "G:/Shared drives/ABMI Camera Mammals/"

# Lure lookup
df_lure <- read_csv(paste0(g_drive, "data/lookup/lure/abmi_all-years_lure_2023-11-27.csv"))

# Veg/HF lookup
new_edd_cat <- read_csv(paste0(g_drive, "data/lookup/veghf/VegForDetectionDistance/New EDD Modeling.csv")) |>
  mutate(use = case_when(
    str_detect(location, "^W") ~ "No",
    is.na(use) ~ "Yes",
    TRUE ~ use)) |>
  filter(!use == "No",
         !is.na(primary_category)) |>
  select(-c(deadfall, use))

sp <- c("Black Bear", "Grizzly Bear",
        "Gray Wolf",
        "Moose", "Woodland Caribou", "Elk (wapiti)",
        "Canada Lynx",
        "Coyote",
        "Snowshoe Hare",
        "Marten", "Fisher",
        "Mule Deer", "White-tailed Deer", "Deer")

# EDD modeling species groups
df_edd_groups <- read_csv(paste0(g_drive, "data/lookup/species-distance-groups.csv")) |>
  # Let's change these up
  # Focus on the important ones for now
  filter(species_common_name %in% sp) |>
  mutate(dist_group = case_when(
    species_common_name == "Gray Wolf" ~ "Gray Wolf",
    str_detect(species_common_name, "Moose|Caribou|Elk") ~ "LargeUngulates",
    species_common_name == "Coyote" ~ "Coyote",
    str_detect(species_common_name, "Deer") ~ "Deer",
    str_detect(species_common_name, "Marten|Fisher") ~ "SmallMustelids",
    species_common_name == "Snowshoe Hare" ~ "Hare",
    TRUE ~ dist_group
  ))

# Old season date cutoffs (julian day)
summer.start.j <- 106
summer.end.j <- 288


#-----------------------------------------------------------------------------------------------------------------------

# Pole position data
df_pole <- read_csv(
  paste0(g_drive,"data/base/raw/previous/ALL_native-mammals_2019-09-01.csv"),
  col_types = cols(distance = col_character(),
                   number_during_gap = col_number(),
                   number_individuals = col_character()),
  na = "") |>
  # Only observations with pole information
  # Note: There's a problem with the 'B' named sites
  filter(!is.na(distance),
         !str_detect(deployment, "RIVR|OGC|EI|CITSCI|Cudde|AAC")) |>
  mutate(location = str_remove(deployment, "^ABMI-"),
         project = ifelse(str_detect(location, "OG"),
                          paste0("ABMI Off-Grid Monitoring ", Year),
                          paste0("ABMI Ecosystem Health ", Year)),
         species_common_name = ifelse(common_name == "Mule deer", "Mule Deer", common_name)) |>
  # Standardize variable names
  select(location, project, image_date_time = date_time_taken, species_common_name, distance) |>
  mutate(dashes = str_count(location, "-")) |>
  mutate(location = ifelse(dashes == "3", paste0(location, "-1"), location)) |>
  # Join extra tagging
  # bind_rows(df_pole_extra) |>
  # Make columns of number of individuals at each pole position
  mutate(at_pole = str_count(distance, "A"),
         behind_pole = str_count(distance, "B"),
         front_pole = str_count(distance, "F"),
         ic_pole = str_count(distance, "IC"),
         ip_pole = str_count(distance, "IP"),
         na_pole = str_count(distance, "NA")) |>
  # Join lure information
  left_join(df_lure, by = c("location", "project")) |>
  # Filter out lured deployments; only unlured are used in the EDD modeling
  filter(lure == "No") |>
  # Create variable for julian date
  mutate(julian = as.numeric(format(ymd_hms(image_date_time), "%j"))) |>
  # Remove wetland sites
  filter(!str_detect(location, "^W"))

# Join new Veg category information
df_pole_veg <- df_pole |>
  left_join(new_edd_cat, by = c("location", "project")) |>
  left_join(df_edd_groups, by = "species_common_name") |>
  filter(!is.na(dist_group)) |>
  # Sum total individuals in each of at_pole through ip_pole
  mutate(n = rowSums(across(at_pole:ip_pole))) |>
  # Only use records where number_individuals = behind_pole or front_pole
  filter(n == behind_pole | n == front_pole,
         !is.na(primary_category)) |>
  mutate(image_date_time = ymd_hms(image_date_time)) |>
  # Create season variables based on julian day, and calculate the proportion of individuals behind pole
  mutate(season_old = as.factor(ifelse(julian  >= summer.start.j & julian <= summer.end.j, "summer", "winter")),
         interval = interval(snow_start, snow_gone),
         intersect = image_date_time %within% interval,
         intersect = ifelse(is.na(intersect), "Not applicable", intersect),
         season_new = as.factor(ifelse(image_date_time <= snow_gone, "snow", "nonsnow")),
         season_new = as.factor(if_else(season_new == "snow" & intersect == FALSE, "nonsnow", season_new)),
         prop_behind = behind_pole / n) |>
  select(location, project, species_common_name, dist_group, n, prop_behind, VegHF, season_old,
         primary_category, secondary_category, season_new)

# What do I have of each dist_group and new veg category?
check <- df_pole_veg |>
  group_by(dist_group, primary_category) |>
  tally()











