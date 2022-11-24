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
