#-----------------------------------------------------------------------------------------------------------------------

# Title:       Create ROIs (Regions of Interest)

# Description: Using a reference image for each location, delineate the ROI(s)
# Author:      Marcus Becker
# Date:        May 2023

#-----------------------------------------------------------------------------------------------------------------------

# Root directory (Google)
g_drive <- "G:/Shared drives/ABMI Camera Mammals/"

# Attach packages
library(phenopix)
library(stringr)
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)

# All Timelapse image files
files <- list.files(paste0(g_drive, "projects/Phenology/Timelapse Images/"),
                    recursive = TRUE,
                    full.names = TRUE)

df_files <- data.frame("path" = files) |>
  mutate(file = str_extract(path, "(?<=/)[^/]*(?=.jpg$)")) |>
  separate(file, into = c("location", "date"), sep = "_") |>
  mutate(date = ymd(date))

# Staff/setup dates
df_ss <- read_csv(paste0(g_drive, "data/lookup/staffsetup/eh-cmu-og_all-years_staffsetup-dates.csv"))

# DJH list of suggested sites (May 2023)
sites <- read.csv(paste0(g_drive, "projects/Phenology/Cameras for phenology RGB analysis May 18 2023.csv"))

# Have only looked at a subset so far
locations <- sites$Row.Labels[39:54]

#-----------------------------------------------------------------------------------------------------------------------

# We want to choose the day after a staff/setup event occurred as the Reference image
# This is due to the field of view potentially changing
target_ref <- df_ss |>
  filter(location %in% locations) |>
  # Note: for the retrieval STAFF/SETUP, there is no timelapse after that day (except CMU ...)
  mutate(date = date + 1,
         reference = TRUE) |>
  select(location, date, reference) |>
  # Based on manual inspection from WildTrax (ugh), a few of these staff/setups are bogus.
  filter(!(location == "CHR-102" & date == "2018-09-23"),
         !(location == "MCC-28" & date == "2018-10-07"),
         !(location == "MCC-43" & date > "2021-07-16"))

# Pull the target reference image file paths
ref_img_files <- df_files |>
  left_join(target_ref, by = c("location", "date")) |>
  filter(reference == TRUE) |>
  # Add a count so we know if there was more than one monitoring period
  group_by(location) |>
  add_count()

# Copy images to Reference folder
for (file in 1:nrow(ref_img_files)) {

  # Note that this is the second 'batch' - will have to figure out a better way to do this in the future.
  path <- pull(ref_img_files[file, 1])
  loc <- pull(ref_img_files[file, 2])
  date <- pull(ref_img_files[file, 3])

  file.copy(from = path,
            to = paste0(g_drive, "projects/Phenology/Reference Images/Batch2/", loc, "_", date, ".jpg"))

}

#-----------------------------------------------------------------------------------------------------------------------

# New file paths for reference images (Batch 2)
ref_img_paths <- list.files(paste0(g_drive, "projects/Phenology/Reference Images/Batch2"),
                            full.names = FALSE)

# Output folder
output_roi <- paste0(g_drive, "projects/Phenology/Outputs/ROI/")

# Draw ROIs for each camera deployment
for (ref in ref_img_paths) {

  path <- paste0(g_drive, "projects/Phenology/Reference Images/Batch2/", ref)
  name <- str_remove(ref, pattern = ".jpg")
  dir.create(paste0(output_roi, name))

  DrawMULTIROI(path_img_ref = path,
               path_ROIs = paste0(output_roi, name, "/"),
               nroi = 1,
               file.type = ".JPG",
               roi.names = paste0(name, "_roi1"))
}

#-----------------------------------------------------------------------------------------------------------------------

# Now, we need to set up folders for different monitoring periods, where applicable

loc_mp <- ref_img_files |>
  select(location, n) |>
  distinct() |>
  filter(n > 1)

for (loc in 1:nrow(loc_mp)) {

  path <- paste0(g_drive, "projects/Phenology/Timelapse Images/", loc_mp[loc, 1], "/")

  periods <- seq(1:pull(loc_mp[loc, 2]))

  for (i in periods) {
    # Note: We want to name the folders something more descriptive: the date of the ROI.
    dir.create(paste0(path, i))

  }

}

# Locations
loc <- loc_mp$location

# Timelapse images to move to different folders
tl_img_to_move <- df_files |>
  left_join(target_ref, by = c("location", "date")) |>
  mutate(reference = if_else(is.na(reference), 0, 1)) |>
  group_by(location) |>
  mutate(folder = cumsum(reference)) |>
  # Note: Want to name the folder something more descriptive (see above)
  filter(!folder < 1) |>
  filter(location %in% loc) |>
  mutate(date = as.character(date))

# Move images to appropriate folders
for (l in loc) {

  print(paste0("Working on ", l))

  d <- tl_img_to_move |>
    filter(location == l)

  for (i in 1:nrow(d)) {

    file_name <- paste0(d[i, 2], "_", d[i, 3])
    curr_path <- pull(d[i, 1])
    dest_path <- paste0(g_drive, "projects/Phenology/Timelapse Images/", l, "/",
                        d[i, 5], "/", file_name, ".jpg")

    file.copy(from = curr_path, to = dest_path)

  }

}

# So, this worked but we still have a lot of work to do to clean up the workflow process.

#-----------------------------------------------------------------------------------------------------------------------
