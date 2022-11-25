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

  x <- image_fov_trigger

  x <- x |>
    unite("project_location", project, location, sep = "_", remove = TRUE)

  range <- x |>
    # Remove Out of Range images
    filter(field_of_view == "WITHIN") |>
    group_by(project_location) |>
    summarise(start_date_time = min(date_detected),
              end_date_time = max(date_detected)) |>
    ungroup() |>
    filter(!is.na(end_date_time))

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

  ends <- inter_pairs |>
    select(1, date_detected = `date_detected...2`, field_of_view = `field_of_view...3`)

  inter_pairs <- inter_pairs |>
    select(1, date_detected = `date_detected...5`, field_of_view = `field_of_view...6`) |>
    bind_rows(ends) |>
    arrange(project_location, date_detected) |>
    select(project_location, date_detected, field_of_view)

  start <- as.Date("2019-01-01")
  end <- max(range$end_date_time)
  interval <- start %--% end
  days <- ceiling(as.duration(interval) / ddays(1))

  dep <- range |>
    pull(project_location)

  time.by.day <- array(0, c(length(dep), 366))
  time.since.start <- array(0, c(length(dep), days))

  for (i in 1:length(dep)) {
    df <- range[range$project_location == dep[i],]
    for (k in 1:nrow(df)) {
      # For days since 2009. floor() is used so that first & last day of operation is included.
      j1 <- floor(julian(df$start_date_time[k], origin = start))
      j2 <- floor(julian(df$end_date_time[k], origin = start))
      if (!is.na(j1) & !is.na(j2)) {
        time.since.start[i, (j1:j2)] <- 1
      }
    }
    # To take off time(s) when camera wasn't working.
    if (dep[i] %in% inter_pairs$project_location) {
      df1<- inter_pairs[as.character(inter_pairs$project_location) == as.character(dep[i]),]
      for (j in seq(1, (nrow(df1) - 1), 2)) { # Assumes all extra times are formatted as END/START pairs
        # Use ceiling() so that day of failure is excluded from operating days
        j1 <- ceiling(julian(df1$date_detected[j], origin = start))
        j2 <- floor(julian(df1$date_detected[j + 1], origin = start))
        if (j2 > j1) time.since.start[i, j1:(j2-1)] <- 0
      }
    }
  }

  years <- seq(as.numeric(year(start)), as.numeric(year(end)))
  days.per.year1 <- map_dbl(years, .f = days_in_year)
  #days.per.year2 <- c(365, 365, 365, 365)
  Jan1 <- cumsum(c(1, days.per.year1))

  yrnum <- julday <- NULL

  for (i in 1:ncol(time.since.start)) {
    yrnum[i] <- sum(i >= Jan1)
    julday[i] <- i - Jan1[yrnum[i]] + 1
  }
  for (i in 1:ncol(time.by.day)) {
    time.by.day[,i] <- rowSums(time.since.start[,which(julday == i)])
  }

  rownames(time.by.day) <- rownames(time.since.start) <- dep
  columns <- as.character(1:366)

  # Summarise time-by-day for each camera deployment
  tbd_summary <- time.by.day %>%
    as_tibble(rownames = "project_location", .name_repair = ~ columns) %>%
    mutate(total_days = rowSums(select(., -project_location)),
           total_summer_days = rowSums(select(., -c(project_location:106, 289:366))),
           total_winter_days = rowSums(select(., -c(project_location, 107:288)))) %>%
    select(project_location, total_days, total_summer_days, total_winter_days) %>%
    separate(project_location, into = c("project", "location"), sep = "_")

  return(tbd_summary)

}




