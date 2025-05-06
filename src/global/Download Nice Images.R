#-----------------------------------------------------------------------------------------------------------------------

# Title:       Obtain images tagged as 'NICE'

# Description: Take image reports for various projects from WildTrax and create tidy data of nice image urls for
#              download into folder provided by the IC.
# Author:      Marcus Becker
# Date:        May 2023

#-----------------------------------------------------------------------------------------------------------------------

library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)

# Shared Google Drive (ABMI Camera Mammals) - locations where image lookup data is stored.
g_drive <- "G:/Shared drives/ABMI Camera Mammals/data/"

# ABMI and CMU data only — currently 4 files.
image_files <- list.files(paste0(g_drive, "lookup/image-reports/"),
                    full.names = TRUE) |>
  str_subset("nwsar")

check <-

# Read in image data, filtering for just the 'nice' images
df_nice <- map_df(image_files, ~ read_csv(.x) |>
              select(project, location, common_name, last_col()))

# Now load tag data
tag_files <- list.files(paste0(g_drive, "base/clean/"), full.names = TRUE) |>
  str_subset("eh|og|cmu|nwsar|bg|bdt") |>
  # Just need the files that have already removed the non-native sp tags
  str_subset("native-sp")

df_tags <- map_df(tag_files, ~ read_csv(.x) |>
                 select(project, location, date_detected, common_name))

# Join together
df_nice_tags <- df_nice |>
  left_join(df_tags, by = c("project", "location", "date_detected")) |>
  filter(!is.na(common_name)) |>
  select(project, location, date_detected, common_name, is_nice,
         url = `image_url.admin.only.`) |>
  arrange(project, location, common_name) |>
  mutate(date = date(date_detected))

#-----------------------------------------------------------------------------------------------------------------------

# Download each image and place in appropriate folder

# Root folder (from IC)
folder <- "G:/.shortcut-targets-by-id/1HXFzwGbeoY_oXop1Eg_LcMPTScUMmk8J/Camera Trap Tuesday/Nice/"

# First, create a folder for each species

sp <- unique(df_nice_tags$common_name)

for (i in sp) {

  # Create directory for each species
  # Does it already exist?
  already.exist <- dir.exists(paste0(folder, i))
  if (!already.exist) {
    dir.create(paste(folder, i, sep = ""))
  }

  # Root directory for species
  dir <- paste(folder, i, "/", sep = "")

  # Subset the data for only images only from this species
  d <- df_nice_tags[df_nice_tags$common_name == i, ]

  for (u in 1:nrow(d)) {

    url <- d[u, 6]
    date <- d[u, 7]
    location <- d[u, 2]
    filename <- paste0(location, "_", date)
    download.file(url, destfile = paste0(dir, filename, ".jpg"), mode = "wb")

  }

}

#-----------------------------------------------------------------------------------------------------------------------
