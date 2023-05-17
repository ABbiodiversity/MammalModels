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

# Root directory (Shared Google Drive)
g_drive <- "G:/Shared drives/ABMI Camera Mammals/data/base/raw/from_WildTrax/ABMI/"

# Download image report(s) for projects of interest:

# CMU 2019 & 2020
cmu_2019 <- read_csv(paste0(g_drive, "image_reports/CMU_CMU_Ecosystem_Monitoring_Camera_Program_2019_image_report.csv"))
cmu_2020 <- read_csv(paste0(g_drive, "image_reports/CMU_CMU_Ecosystem_Monitoring_Camera_Program_2020_image_report.csv"))

# ABMI EH 2020 and 2021
abmi_2020 <- read_csv(paste0(g_drive, "image_reports/ABMI_ABMI_Ecosystem_Health_2020_image_report.csv"))
abmi_2021 <- read_csv(paste0(g_drive, "image_reports/ABMI_ABMI_Ecosystem_Health_2021_image_report.csv"))


#-----------------------------------------------------------------------------------------------------------------------

# Clean data - this could probably be a function of some sort?

cmu_2019_test <- cmu_2019 |>
  select(project, location, date_detected, trigger, url = `image_url(admin only)`) |>
  # Only interested in Time Lapse images
  filter(trigger == "Time Lapse") |>
  mutate(hour = as.character(as_hms(date_detected))) |>
  # Depending on the location, sometimes TL images are 12h or 13h (or both)
  filter(hour == "13:00:00" | hour == "12:00:00") |>
  mutate(date = date(date_detected)) |>
  arrange(location, desc(date_detected)) |>
  group_by(location, date) |>
  # Just take one per day if multiple are present (i.e., a 12h and a 13h image)
  filter(row_number() == 1) |>
  ungroup() |>
  select(-c(trigger, hour, date)) |>
  # Quick rename of project to make file size smaller
  mutate(project = "CMU 2019")

# Both CMU 2019 and CMU 2020
cmu <- bind_rows(cmu_2019_test, cmu_2020_test)

# Write csv
write_csv(cmu, paste0(g_drive, "image_reports/CMU_2019-2020_timelapse_12h-13h.csv"))

# Clean data - Ecosystem Health
# * To do for the future - just do this for all of the ABMI data (EH and OG)
abmi <- bind_rows(abmi_2020, abmi_2021) |>
  select(project, location, date_detected, trigger, url = `image_url(admin only)`) |>
  filter(trigger == "Time Lapse") |>
  mutate(hour = as.character(as_hms(date_detected))) |>
  filter(hour == "13:00:00" | hour == "12:00:00") |>
  mutate(date = date(date_detected)) |>
  arrange(location, desc(date_detected)) |>
  group_by(location, date) |>
  # Just take one per day if multiple are present (i.e., a 12h and a 13h image)
  filter(row_number() == 1) |>
  ungroup() |>
  select(-c(trigger, hour, date))

g_drive <- "G:/Shared drives/ABMI Camera Mammals/"

# Write csv
write_csv(abmi, paste0(g_drive, "data/processed/timelapse/abmi_2020-2021_timelapse_12h-13h.csv")

#-----------------------------------------------------------------------------------------------------------------------
