#-----------------------------------------------------------------------------------------------------------------------

# Title:            Images for pole position tagging
# Description:      Pull more images for pole position tagging to feed into EDD (re-)modeling.
# Author:           Marcus Becker
# Date:             January 2024

# Previous scripts: None

#-----------------------------------------------------------------------------------------------------------------------

library(tidyverse)
library(wildRtrax) # To download data
library(keyring)   # For storing credentials safely

g_drive <- "G:/Shared drives/ABMI Camera Mammals/"

# Species character strings
load(paste0(g_drive, "data/lookup/wt_cam_sp_str.RData"))

# Lure lookup
df_lure <- read_csv(paste0(g_drive, "data/lookup/lure/abmi_all-years_lure_2023-11-27.csv"))

# Source functions for TIFC workflow
source("./src/functions/estimate-density-tifc.R")

# We're going to focus on Ecosystem Health 2019-2023
# Project
proj <- "eh"
# Years
years <- "_19-20-21-22-23"

# Download data
# Set environment variables (username/password)
Sys.setenv(WT_USERNAME = key_get("WT_USERNAME", keyring = "wildtrax"),
           WT_PASSWORD = key_get("WT_PASSWORD", keyring = "wildtrax"))

# Authenticate into WildTrax
wt_auth()

# Pull EH project IDs - 2019, 2020, 2021, and 2022.
eh_proj <- wt_get_download_summary(sensor_id = "CAM") |>
  filter(str_detect(project, "Health 2019|Health 2020|Health 2021|Health 2022|Health 2023")) |>
  select(project, project_id)

eh_proj_ids <- eh_proj$project_id

tag_reports <- map_df(.x = eh_proj_ids,
                      .f = ~ wt_download_report(
                        project_id = .x,
                        sensor_id = "CAM",
                        report = "tag",
                        weather_cols = FALSE))

image_reports <- map_df(.x = eh_proj_ids,
                        .f = ~ wt_download_report(
                          project_id = .x,
                          sensor_id = "CAM",
                          report = "image_report",
                          weather_cols = FALSE) |>
                          mutate_if(is.numeric, as.character))

#-----------------------------------------------------------------------------------------------------------------------

tags <- tag_reports |>
  filter(species_class == "Mammalia",
         !str_detect(species_common_name, "Domestic|Human")) |>
  consolidate_tags() |>
  left_join(eh_proj, by = "project_id") |>
  left_join(df_lure, by = c("project", "location")) |>
  filter(lure == "No") |>
  mutate(image_date_time = ymd_hms(image_date_time),
         month = month(image_date_time),
         season = case_when(
           month < 3 | month > 10 ~ "winter",
           month == "3" | month == "4" | month == "5" ~ "spring",
           TRUE ~ "summer"
         )) |>
  mutate(individual_count = as.numeric(individual_count)) |>
  filter(individual_count < 2) |>
  select(project, location, image_id, image_date_time, season, species_common_name, individual_count, age_class, sex_class)

tags_select <- tags |>
  group_by(species_common_name) |>
  add_count() |>
  filter(n > 13,
         !str_detect(species_common_name, "^Deer|^Bear|Voles|^Foxes|Allies")) |>
  ungroup()

check <- tags_select |> group_by(species_common_name) |> tally()

do_all_tags <- tags_select |>
  filter(n < 503) |>
  # But not squirrels ...
  filter(!species_common_name == "Red Squirrel") |>
  select(-n)

sample_from_tags <- tags_select |>
  filter(n > 503) |>
  select(-n) |>
  group_by(project, location, season, species_common_name) |>
  add_count() |>
  ungroup() |>
  mutate(sample_size = ifelse(n < 5, n, 5)) |>
  mutate(sample_size = ifelse(
    (species_common_name == "White-tailed Deer"
      | species_common_name == "Mule Deer"
      | species_common_name == "Snowshoe Hare"), 1, sample_size)) |>
  select(-n) |>
  group_by(project, location, season, species_common_name, sample_size) |>
  nest() |>
  mutate(sample = map2(data, sample_size, sample_n)) |>
  ungroup() |>
  select(-c(data, sample_size)) |>
  unnest(cols = c(sample))

images <- image_reports |>
  filter(image_fov == "WITHIN",
         image_trigger_mode == "Motion Detection") |>
  mutate(project_id = as.numeric(project_id),
         image_id = as.numeric(image_id),
         image_date_time = ymd_hms(image_date_time)) |>
  left_join(eh_proj, by = "project_id") |>
  select(project, location, image_id, image_date_time, media_url)

final <- bind_rows(do_all_tags, sample_from_tags) |>
  left_join(images, by = c("project", "location", "image_id", "image_date_time")) |>
  filter(!is.na(media_url)) |>
  select(1:4, species_common_name, media_url)

#-----------------------------------------------------------------------------------------------------------------------

# Tags from lower cameras

# Pull EH project IDs - 2019, 2020, 2021, and 2022.
low_proj <- wt_get_download_summary(sensor_id = "CAM") |>
  filter(str_detect(project, "Height|Trajectories")) |>
  select(project, project_id)

low_proj_ids <- low_proj$project_id

low_tag_reports <- map_df(.x = low_proj_ids,
                      .f = ~ wt_download_report(
                        project_id = .x,
                        sensor_id = "CAM",
                        report = "tag",
                        weather_cols = FALSE)) |>
  mutate(low = ifelse(project_id == "1096" | str_detect(location, "-M$"), TRUE, FALSE)) |>
  filter(low == TRUE,
         !str_detect(location, "^1-"))

low_image_reports <- map_df(.x = low_proj_ids,
                        .f = ~ wt_download_report(
                          project_id = .x,
                          sensor_id = "CAM",
                          report = "image_report",
                          weather_cols = FALSE) |>
                          mutate_if(is.numeric, as.character))

low_tags <- low_tag_reports |>
  filter(species_class == "Mammalia",
         !str_detect(species_common_name, "Domestic|Human")) |>
  consolidate_tags() |>
  left_join(low_proj, by = "project_id") |>
  #left_join(df_lure, by = c("project", "location")) |>
  #filter(lure == "No") |>
  mutate(image_date_time = ymd_hms(image_date_time),
         month = month(image_date_time),
         season = case_when(
           month < 3 | month > 10 ~ "winter",
           month == "3" | month == "4" | month == "5" ~ "spring",
           TRUE ~ "summer"
         )) |>
  mutate(individual_count = as.numeric(individual_count)) |>
  filter(individual_count < 2) |>
  select(project, location, image_id, image_date_time, season, species_common_name, individual_count, age_class, sex_class)

low_tags_select <- low_tags |>
  group_by(species_common_name) |>
  add_count() |>
  filter(n > 13,
         !str_detect(species_common_name, "^Deer|^Bear|Voles|^Foxes|Allies|Rodents")) |>
  ungroup()

check <- low_tags_select |> group_by(species_common_name) |> tally()

low_do_all_tags <- low_tags_select |>
  filter(n < 250) |>
  # But not squirrels ...
  filter(!str_detect(species_common_name, "Squirrel")) |>
  select(-n)

low_sample_from_tags <- low_tags_select |>
  filter(n > 250) |>
  filter(!str_detect(species_common_name, "Squirrel")) |>
  select(-n) |>
  group_by(project, location, season, species_common_name) |>
  add_count() |>
  ungroup() |>
  mutate(sample_size = ifelse(n < 15, n, 15)) |>
  mutate(sample_size = ifelse(
    (str_detect(species_common_name, "Deer|Hare|Bear") & n > 5), 5, sample_size)) |>
  select(-n) |>
  group_by(project, location, season, species_common_name, sample_size) |>
  nest() |>
  mutate(sample = map2(data, sample_size, sample_n)) |>
  ungroup() |>
  select(-c(data, sample_size)) |>
  unnest(cols = c(sample))

low_images <- low_image_reports |>
  filter(image_fov == "WITHIN",
         image_trigger_mode == "Motion Detection") |>
  mutate(project_id = as.numeric(project_id),
         image_id = as.numeric(image_id),
         image_date_time = ymd_hms(image_date_time)) |>
  left_join(low_proj, by = "project_id") |>
  select(project, location, image_id, image_date_time, media_url)

low_final <- bind_rows(low_do_all_tags, low_sample_from_tags) |>
  left_join(low_images, by = c("project", "location", "image_id", "image_date_time")) |>
  filter(!is.na(media_url)) |>
  select(1:4, species_common_name, media_url)

#-----------------------------------------------------------------------------------------------------------------------

# Put together

all_together <- bind_rows(final, low_final)

write_csv(all_together, "file.csv")



