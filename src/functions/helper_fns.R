# Helper functions for the TIFC workflow ... to be included in wildRtrax??

#-----------------------------------------------------------------------------------------------------------------------

# Title:       Consolidate tags
# Description: Run this function of the tag report to consolidate tags from the same image into the same row.

# Note:        This is an issue with respect to multiple images with the same timestamp.

wt_consolidate_tags <- function(x) {

  g_drive <- "G:/Shared drives/ABMI Camera Mammals/"

  # Load native_sp tags
  load(paste0(g_drive, "data/lookup/wt_cam_sp_str.RData"))

  # The normies
  y <- x |>
    filter(common_name %in% native_sp,
           !number_individuals == "VNA") |>
    mutate(number_individuals = as.numeric(number_individuals)) |>
    # Run distinct to get rid of multiple images of the same thing at the exact same time
    distinct() |>
    # Include number_individuals in case there are back-to-back images with different number of individuals
    group_by(project, location, date_detected, common_name, number_individuals) |>
    mutate(number_individuals = sum(number_individuals),
           age_class = paste0(age_class, collapse = ", "),
           sex = paste0(sex, collapse = ", ")) |>
    distinct(project, location, date_detected, common_name, number_individuals, .keep_all = TRUE) |>
    ungroup() |>
    mutate(number_individuals = as.character(number_individuals))

  # Tags of species that have VNA - usually we don't care about these, but don't want to lose info.
  z <- x |>
    filter(common_name %in% native_sp,
           number_individuals == "VNA")

  # All the STAFF/SETUP, etc, then bound back together.
  x <- x |>
    filter(!common_name %in% native_sp) |>
    bind_rows(y, z)

  return(x)

}

#-----------------------------------------------------------------------------------------------------------------------

# Title:       Make VegForDetDist
# Description: Make VegForDetectionDistance column out of VEGHFAGEclass information

make_vegfordetdist <- function(x) {

  x <- x |>
    mutate(VegForDetectionDistance = case_when(
      str_detect(VEGHFAGEclass, "Fen|Bog|Marsh|dSwamp") ~ "WetTreed",
      str_detect(VEGHFAGEclass, "Decid") ~ "Decid",
      str_detect(VEGHFAGEclass, "Pine|Mixed") ~ "Conif",
      str_detect(VEGHFAGEclass, "Water") ~ "Water",
      str_detect(VEGHFAGEclass, "Grass") ~ "Grass",
      str_detect(VEGHFAGEclass, "Shrubby") ~ "WetGrass",
      TRUE ~ "HF"
    ))

  return(x)

}

#-----------------------------------------------------------------------------------------------------------------------

# Title:       Add 'N' gap class
# Description: a 'NONE' image is used to demarcate when a series should be truncated because an animal left the field of view.

add_gap_class_n <- function(x) {

  # Load native_sp tags
  g_drive <- "G:/Shared drives/ABMI Camera Mammals/"
  load(paste0(g_drive, "data/lookup/wt_cam_sp_str.RData"))

  y <- x |>
    select(project, location, date_detected, common_name) |>
    arrange(project, location, date_detected) |>
    # Create gap class column
    mutate(common_name_next = lead(common_name),
           gap_class = ifelse(common_name != "NONE" & common_name_next == "NONE", "N", NA)) |>
    # Include only N gap class for native mammals
    filter(gap_class == "N",
           common_name %in% native_sp) |>
    select(-c(common_name_next))

  return(y)

}

#-----------------------------------------------------------------------------------------------------------------------

# Title:       Days in a year
# Description: Check whether a year is a leap year

library(assertive)

days_in_year <- function(year) {
  # If year is div. by 400 return TRUE
  if(is_divisible_by(year, 400)) {
    return(366)
  }
  # If year is div. by 100 return FALSE
  if(is_divisible_by(year, 100)) {
    return(365)
  }
  # If year is div. by 4 return TRUE
  if(is_divisible_by(year, 4)) {
    return(366)
  }
  # Otherwise return FALSE
  365
}

#-----------------------------------------------------------------------------------------------------------------------

#' @param x image report
#'

summarise_time_by_day <- function(x) {

  x <- x |>
    unite("project_location", project, location, sep = "_", remove = TRUE)

  # Locations which started operation again after an intermediate pause
  inter <- x |>
    filter(field_of_view == "START - First Good Image in FOV" | field_of_view == "END - Last Good Image in FOV") |>
    group_by(project_location) |>
    tally() |>
    filter(n > 1) |>
    select(project_location)

  inter_pairs <- x |>
    filter(field_of_view == "START - First Good Image in FOV" | field_of_view == "END - Last Good Image in FOV") |>
    # Return all rows with a match in inter
    semi_join(inter, by = c("project_location")) |>
    arrange(project_location, date_detected) |>
    select(project_location, date_detected, field_of_view) |>
    group_by(project_location) |>
    # This code is gross. Issue is that cameras <2019 are formatted slightly differently wrt START / END tagging
    mutate(starts_again = ifelse(lead(field_of_view) == "START - First Good Image in FOV" & field_of_view == "END - Last Good Image in FOV", 1, NA),
           restart = ifelse(lag(starts_again) == "1" & lag(field_of_view) == "END - Last Good Image in FOV", 1, NA)) |>
    filter(starts_again == "1" | restart == "1") |>
    select(-c(starts_again, restart)) |>
    ungroup() |>
    group_split(field_of_view) |>
    bind_cols() |>
    mutate(time_diff = difftime(`date_detected...5`, `date_detected...2`, units = "hours")) |>
    filter(time_diff > 12) |>
    select(1:3, 5, 6)

  to_remove <- inter_pairs |>
    mutate(end_date = as.Date(`date_detected...2`),
           start_date = as.Date(`date_detected...5`)) |>
    select(project_location = 1, end_date, start_date) |>
    rowwise() |>
    mutate(date = list(seq(from = end_date, to = start_date, by = "day"))) |>
    unnest(date) |>
    select(project_location, date)

  range <- x |>
    # Remove Out of Range images
    filter(field_of_view == "WITHIN") |>
    group_by(project_location) |>
    summarise(start_date = as.Date(min(date_detected)),
              end_date = as.Date(max(date_detected))) |>
    filter(!is.na(end_date)) |>
    group_by(project_location) |>
    mutate(date = list(seq(from = start_date, to = end_date, by = "day"))) |>
    unnest(date) |>
    ungroup() |>
    select(project_location, date) |>
    mutate(operating = 1) |>
    mutate(julian = yday(date),
           season = ifelse(julian < 106 | julian > 288, "total_winter_days", "total_summer_days")) |>
    anti_join(to_remove, by = c("project_location", "date")) |>
    group_by(project_location, season) |>
    summarise(operating_days = sum(operating)) |>
    ungroup() |>
    group_by(project_location) |>
    mutate(total_days = sum(operating_days)) |>
    pivot_wider(id_cols = c(project_location, total_days), names_from = season, values_from = operating_days)

  return(range)

}

# This ^ is great for my narrow ABMI purposes.
# What would a more generic user function look like?

# wt_get_operating_days -> generate long data frame of all days of operation.
# wt_summarise_operating_days -> feed in custom monitoring periods (can loop), which will be summarised.
#                                There could be an 'ABMI' pre-format option. Maybe?
# Have to think about project vs location, etc.

#----------------------















