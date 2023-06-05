#-----------------------------------------------------------------------------------------------------------------------

# Title:       Process Image Reports to create tidy data of timelapse URLs

# Description: Take image reports for various projects from WildTrax and create tidy data of timelapse image urls for
#              download in the next script.
# Author:      Marcus Becker
# Date:        May 2023

#-----------------------------------------------------------------------------------------------------------------------

# Attach packages
library(readr)
library(dplyr)
library(stringr)
library(hms)
library(tidyr)
library(lubridate)
library(purrr)

# Root directory (Shared Google Drive)
g_drive <- "G:/Shared drives/ABMI Camera Mammals/data/"

# Path to files of interest — only EH, CMU, and OG for now
files <- list.files(paste0(g_drive, "lookup/image-reports"),
                    pattern = "*.csv",
                    full.names = TRUE) |>
  str_subset(pattern = "og|cmu|eh")

# Load image reports, only saving columns of interest
image_reports <- map_df(
  .x = files, .f = ~ read_csv(
    .x, col_select = c(project, location, date_detected, trigger,
                       field_of_view, url = `image_url.admin.only.`)))

# Tag reports, in order to obtain STAFF/SETUP images
files <- list.files(paste0(g_drive, "base/clean"),
                    pattern = "*.csv",
                    full.names = TRUE) |>
  str_subset(pattern = "all-data") |>
  str_subset(pattern = "og|cmu|eh")

staff_setup <- map_df(
  .x = files, .f = ~ read_csv(
    .x, col_select = c(project, location, date_detected, common_name))) |>
  filter(common_name == "STAFF/SETUP")

#-----------------------------------------------------------------------------------------------------------------------

# Obtain dataframe of relevant Timelapse images
df_tl <- image_reports |>
  filter(trigger == "Timed") |>
  mutate(hour = as.character(as_hms(date_detected))) |>
  # Depending on the location, sometimes Timelapse images are 12h or 13h (or both)
  filter(hour == "13:00:00" | hour == "12:00:00") |>
  # Just take one per day if multiple are present (i.e., both a 12h and a 13h image)
  mutate(date = date(date_detected)) |>
  arrange(location, desc(date_detected)) |>
  group_by(location, date) |>
  filter(row_number() == 1) |>
  ungroup() |>
  # Remove unnecessary columns
  select(-c(trigger, hour, date, field_of_view))

# Dates when field of view was out of range
df_fov <- image_reports |>
  filter(field_of_view != "WITHIN",
         trigger == "Timed") |>
  mutate(date = date(date_detected)) |>
  select(location, field_of_view, date) |>
  distinct()

# STAFF/SETUP dates
staff_setup_days <- staff_setup |>
  mutate(date = date(date_detected)) |>
  select(-c(date_detected, project)) |>
  distinct() |>
  arrange(location)

#-----------------------------------------------------------------------------------------------------------------------

# Write to csv(s)

write_csv(df_tl, paste0(g_drive, "processed/timelapse/eh-cmu-og_all-years_timelapse_12h-13h.csv"))
write_csv(df_fov, paste0(g_drive, "processed/timelapse/eh-cmu-og_all-years_out-of-range.csv"))

write_csv(staff_setup_days, paste0(g_drive, "lookup/staffsetup/eh-cmu-og_all-years_staffsetup-dates.csv"))

#-----------------------------------------------------------------------------------------------------------------------
