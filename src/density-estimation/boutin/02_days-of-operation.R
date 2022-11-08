
#-----------------------------------------------------------------------------------------------------------------------

# Title:            Days of Operation
# Description:      Assess the days of operation for each BOUTIN camera deployment, accounting for all possible failures.
# Author:           Marcus Becker
# Date:             November 2022

# Previous scripts: 01_clean-raw

#-----------------------------------------------------------------------------------------------------------------------

# Attach packages
library(readr)
library(dplyr)
library(lubridate)
library(stringr)
library(tidyr)
library(tibble)

# Set path to Google Drive
g_drive <- "G:/Shared drives/ABMI Camera Mammals/"

# Load tag data
df_all <- read_csv(paste0(g_drive, "data/base/clean/boutin_all-years_all-data_clean_2022-11-07.csv"))

# Timelapse images
df_tl <- read_csv(paste0(g_drive, "data/lookup/timelapse/boutin_all-timelapse_all-years.csv"))

#-----------------------------------------------------------------------------------------------------------------------

# Naive date ranges - Probably needs to be checked.

df_ranges_tl <- df_tl |>
  group_by(project, location) |>
  summarise(start_tl = min(date_detected),
            end_tl = max(date_detected))

df_ranges_naive <- df_all |>
  group_by(project, location) |>
  summarise(start_date_time = min(date_detected),
            end_date_time = max(date_detected)) |>
  left_join(df_ranges_tl, by = c("project", "location")) |>
  mutate(end_date_time = if_else(end_tl < end_date_time | is.na(end_tl), end_date_time, end_tl)) |>
  select(project, location, start_date_time, end_date_time) |>
  ungroup() |>
  unite(col = "project_location", project, location, sep = "_", remove = TRUE)

#-----------------------------------------------------------------------------------------------------------------------

# Summarise time-by-day for each camera deployment

start <- as.Date("2009-01-01")
end <- df_ranges_naive |> filter(!is.na(end_date_time)) |> pull(end_date_time) |> max()
interval <- start %--% end
days <- ceiling(as.duration(interval) / ddays(1))

df_inter_pairs <- data.frame(
  project_location = "Boutin 17-18_A10_4",
  date_detected = ymd_hms(c("2017-09-28 00:00:00", "2017-09-29 00:00:00"))
)

# Create vector of deployments
dep <- df_ranges_naive |> filter(!is.na(end_date_time)) |> pull(project_location)

# Date ranges, no NAs
ranges <- df_ranges_naive |> filter(!is.na(end_date_time))

# Build arrays
time.by.day <- array(0, c(length(dep), 366))
time.since.2009 <- array(0, c(length(dep), days))

# Populate arrays
for (i in 1:length(dep)) {
  df <- ranges[ranges$project_location == dep[i],]
  for (k in 1:nrow(df)) {
    # For days since 2009. floor() is used so that first & last day of operation is included.
    j1 <- floor(julian(df$start_date_time[k], origin = strptime("2009-01-01 00:00:00", format = "%Y-%m-%d %H:%M:%S")))
    j2 <- floor(julian(df$end_date_time[k], origin = strptime("2009-01-01 00:00:00", format = "%Y-%m-%d %H:%M:%S")))
    if (!is.na(j1) & !is.na(j2)) {
      time.since.2009[i, (j1:j2)] <- 1
    }
  }
  # To take off time(s) when camera wasn't working.
  if (dep[i] %in% df_inter_pairs$project_location) {
    df1<- df_inter_pairs[as.character(df_inter_pairs$project_location) == as.character(dep[i]),]
    for (j in seq(1, (nrow(df1) - 1), 2)) { # Assumes all extra times are formatted as END/START pairs
      # Use ceiling() so that day of failure is excluded from operating days
      j1 <- ceiling(julian(df1$date_detected[j], origin = strptime("2009-01-01 00:00:00", format = "%Y-%m-%d %H:%M:%S")))
      j2 <- floor(julian(df1$date_detected[j + 1], origin = strptime("2009-01-01 00:00:00", format = "%Y-%m-%d %H:%M:%S")))
      if (j2 > j1) time.since.2009[i, j1:(j2-1)] <- 0
    }
  }
}

# Add to each year (note whether it was a leap year). Currently up to 2021 (from 2009).
days.per.year <- c(365, 365, 365, 366, 365, 365, 365, 366, 365, 365, 365, 366, 365)
Jan1 <- cumsum(c(1, days.per.year))

yrnum <- julday <- NULL

# Row sums
for (i in 1:ncol(time.since.2009)) {
  yrnum[i] <- sum(i >= Jan1)
  julday[i] <- i - Jan1[yrnum[i]] + 1
}
for (i in 1:ncol(time.by.day)) {
  time.by.day[,i] <- rowSums(time.since.2009[,which(julday == i)])
}

rownames(time.by.day) <- rownames(time.since.2009) <- dep
columns <- as.character(1:366)

# Summarise time-by-day for each camera deployment
df_tbd_summary <- time.by.day %>%
  as_tibble(rownames = "project_location", .name_repair = ~ columns) %>%
  mutate(total_days = rowSums(select(., -project_location)),
         total_summer_days = rowSums(select(., -c(project_location:106, 289:366))),
         total_winter_days = rowSums(select(., -c(project_location, 107:288)))) %>%
  select(project_location, total_days, total_summer_days, total_winter_days) %>%
  separate(project_location, into = c("project", "location"), sep = "_")





