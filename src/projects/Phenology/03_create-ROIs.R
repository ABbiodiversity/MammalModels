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

# List all Timelapse image files that have been downloaded so far
files <- list.files(paste0(g_drive, "projects/Phenology/Timelapse Images/"),
                    recursive = TRUE,
                    full.names = TRUE)

# Turn into dataframe
df_files <- data.frame("path" = files) |>
  mutate(file = str_extract(path, "(?<=/)[^/]*(?=.jpg$)")) |>
  separate(file, into = c("location", "date"), sep = "_") |>
  mutate(date = ymd(date))

# All locations that have had timelapse images downloaded
locations <- unique(df_files$location)

# Staff/setup dates
df_ss <- read_csv(paste0(g_drive, "data/lookup/staffsetup/eh-cmu-og_all-years_staffsetup-dates.csv"))

# Reference images / locations that have already had an ROI drawn
ref_done <- list.files(paste0(g_drive, "projects/Phenology/Reference Images/"),
                       recursive = TRUE, full.names = FALSE) |>
  str_remove_all(".jpg$")

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
         !(location == "MCC-43" & date > "2021-07-16")) |>
  mutate(file = paste0(location, "_", date)) |>
  filter(!file %in% ref_done)

# Pull the target reference image file paths
ref_img_files <- df_files |>
  left_join(target_ref, by = c("location", "date")) |>
  filter(reference == TRUE) |>
  # Add a count so we know if there was more than one monitoring period
  group_by(location) |>
  add_count()

# Copy images to Reference folder
for (file in 1:nrow(ref_img_files)) {

  path <- pull(ref_img_files[file, 1])
  loc <- pull(ref_img_files[file, 2])
  date <- pull(ref_img_files[file, 3])

  file.copy(from = path,
            to = paste0(g_drive, "projects/Phenology/Reference Images/", loc, "_", date, ".jpg"))

}

#-----------------------------------------------------------------------------------------------------------------------

# List all the images that already have ROIs done
roi_dirs <- list.dirs(paste0(g_drive, "projects/Phenology/Outputs/ROI"), full.names = FALSE)

# File paths for reference images
ref_img_paths <- list.files(paste0(g_drive, "projects/Phenology/Reference Images"), full.names = FALSE) |>
  str_remove(pattern = ".jpg") |>
  # Reference images without an ROI drawn
  setdiff(roi_dirs)

# Output folder
output_roi <- paste0(g_drive, "projects/Phenology/Outputs/ROI/")

# Draw ROIs for each camera deployment
for (ref in ref_img_paths) {

  path <- paste0(g_drive, "projects/Phenology/Reference Images/", ref, ".jpg")
  dir.create(paste0(output_roi, ref))

  DrawMULTIROI(path_img_ref = path,
               path_ROIs = paste0(output_roi, ref, "/"),
               nroi = 1,
               file.type = ".JPG",
               roi.names = paste0(ref, "_roi1"))
}

#-----------------------------------------------------------------------------------------------------------------------

# Now, we need to set up folders for different monitoring periods, where applicable

upd_roi_dirs <- list.dirs(paste0(g_drive, "projects/Phenology/Outputs/ROI"), full.names = FALSE)

loc_mp <- data.frame("folder" = upd_roi_dirs) |>
  filter(!folder == "") |>
  separate(folder, into = c("location", "date"), sep = "_", remove = FALSE) |>
  group_by(location) |>
  add_count()

for (loc in 1:nrow(loc_mp)) {

  path <- paste0(g_drive, "projects/Phenology/Timelapse Images/", loc_mp[loc, 2], "/")

  # Create new directories
  # Does it already exist?
  already.exist <- dir.exists(paste0(path, loc_mp[loc, 1]))
  if(already.exist) {
    print(paste0("The required directories already exist for location ", loc, ". Moving on."))
  } else {
    dir.create(paste0(path, loc_mp[loc, 1]))
    print(paste0("Create a directory for location ", loc, " and period ", loc_mp[loc, 1]))
  }

}

loc <- unique(df_files$location)

#
new_target_ref <- df_ss |>
  filter(location %in% loc) |>
  # Note: for the retrieval STAFF/SETUP, there is no timelapse after that day (except CMU ...)
  mutate(date = date + 1,
         reference = TRUE) |>
  select(location, date, reference) |>
  # Based on manual inspection from WildTrax (ugh), a few of these staff/setups are bogus.
  filter(!(location == "CHR-102" & date == "2018-09-23"),
         !(location == "MCC-28" & date == "2018-10-07"),
         !(location == "MCC-43" & date > "2021-07-16")) |>
  mutate(folder_name = paste0(location, "_", date))

# Timelapse images to move to different folders
tl_img_to_move <- df_files |>
  left_join(new_target_ref, by = c("location", "date")) |>
  mutate(reference = if_else(is.na(reference), 0, 1)) |>
  group_by(location) |>
  mutate(folder = cumsum(reference)) |>
  ungroup() |>
  filter(!folder < 1) |>
  # Fill folder_name down
  fill(folder_name) |>
  mutate(date = as.character(date)) |>
  select(path, location, date, folder_name) |>
  # Remove images (rows) that are already in a sub-folder
  mutate(done = ifelse(str_detect(path, paste0(folder_name, "/")), "yes", "no")) |>
  filter(done == "no")

# Locations to move into subfolders
loc <- unique(tl_img_to_move$location)

# Move images to appropriate folders
tic()
for (l in loc) {

  print(paste0("Working on ", l))

  d <- tl_img_to_move |>
    filter(location == l)

  for (i in 1:nrow(d)) {

    file_name <- paste0(d[i, 2], "_", d[i, 3])
    curr_path <- pull(d[i, 1])
    dest_path <- paste0(g_drive, "projects/Phenology/Timelapse Images/", l, "/",
                        d[i, 4], "/", file_name, ".jpg")

    file.rename(from = curr_path, to = dest_path)

  }

}
toc()

#-----------------------------------------------------------------------------------------------------------------------

# Retrieve a mid-July timelapse image and write ROI on it for reference

print_roi <- function (path_img_ref, path_ROIs, which = "all", col, file.type = ".jpg") {

  file <- list.files(path = path_img_ref, pattern = file.type)
  img <- brick(paste(path_img_ref, file, sep = ""))
  rois <- paste(path_ROIs, "roi.data.Rdata", sep = "")
  roi.data <- NULL
  load(rois)
  nrois <- length(roi.data)
  roi.names <- names(roi.data)
  plotRGB(img)
  if (which == "all") {
    if (missing(col))
      col <- palette()[1:nrois]
    for (a in 1:nrois) {
      act.polygons <- roi.data[[a]]$polygon
      raster::lines(act.polygons, col = col[a], lwd = 2)
    }
  }
  else {
    pos.roi <- which(roi.names %in% which == TRUE)
    raster::lines(roi.data[[pos.roi]]$polygon, lwd = 2)
  }
}

# Mid-month images
df_midmonth <- df_files |>
  filter(str_detect(date, "-15$"),
         str_detect(location, "^[A-Z]")) |>
  mutate(folder = str_extract(path, "(?<=/)[^/]+(?=/[^/]+$)")) |>
  filter(str_detect(folder, "_"))

upd_roi_dirs <- list.dirs(paste0(g_drive, "projects/Phenology/Outputs/ROI"), full.names = FALSE) |>
  str_subset("^[A-Z]")

for (session in upd_roi_dirs) {

  print(paste0("Working on session ", session))

  # Subset the data for only images only from this location
  d <- df_midmonth[df_midmonth$folder == session, ]

  for (i in 1:nrow(d)) {

    pir <- d[i, 1]
    t <- d[i, 4]
    proi <- paste0(g_drive, "projects/Phenology/Outputs/ROI/", t, "/")

    try(print_roi(path_img_ref = pir,
              path_ROIs = proi,
              col = "red"))

    date <- d[i, 3]

    dev.print(jpeg, file = paste0(proi, "ROI-check_", date, ".jpg"), width = 1024, height = 1024)
    dev.off()

  }

}





