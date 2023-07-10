#-----------------------------------------------------------------------------------------------------------------------

# Title:       Tidy data
# Date:        June 2023
# Author:      Marcus Becker

# Description:


#-----------------------------------------------------------------------------------------------------------------------

# Attach packages
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(janitor)
library(purrr)
library(lubridate)
library(readxl)

# Root directory
g_drive <- "G:/Shared drives/ABMI Camera Mammals/"

# Probabilistic gaps
prob_gaps <- read_csv(paste0(g_drive, "data/processed/probabilistic-gaps/gap-leave-prob_predictions_2021-10-05.csv")) |>
  rename(diff_time_previous = diff_time)

# Gap groups
gap_groups <- read_csv(paste0(g_drive, "data/lookup/species-gap-groups.csv"))

# VegHF
df_veghf <- read_csv(paste0(g_drive, "data/lookup/Combined vegHF soilHF and detection distance veg for cameras May 2020.csv")) |>
  select(deployment, VegHF, VegForDetectionDistance)

vg <- read.csv(paste0(g_drive, "data/supplemental/assumptions-tests/investigation-behaviour-by-veghf/lookup/Veg groupings for test of investigation assumptions.csv")) |>
  rename(VEGHFAGEclass = VegHF)

# Average time between photos
df_tbp <- read_csv(paste0(g_drive, "data/processed/time-btwn-images/abmi-cmu_all-years_tbp_2021-06-25.csv")) |>
  select(common_name, tbp)

# Load tags previously done in 2020
df_tags <- list.files(path = paste0(g_drive, "data/supplemental/assumptions-tests/investigation-behaviour-by-veghf/tagged-data/"),
                      pattern = "*.csv", recursive = TRUE, full.names = TRUE) |>
  map_df(~ read_csv(., col_types = cols(`Number of Individuals` = col_character(),
                                        `Comments` = col_character(),
                                        `Observation Period` = col_character(),
                                        `Date Taken` = col_character()))) |>
  clean_names() |>
  select(deployment, year, date_taken, common_name, behaviour = comments) |>
  filter(str_detect(behaviour, "O|IP|IC|DP|DC|CL|L"))

#-----------------------------------------------------------------------------------------------------------------------

# Clean tag data from 2020

df_data_clean <- df_data_all |>
  # Weirdly lose a few obs from one deployment (all coyote)
  mutate(date_taken = ymd_hms(date_taken, quiet = TRUE)) |>
  filter(!is.na(date_taken)) |>
  arrange(deployment, year, date_taken) |>
  # Drop duplicates (problem of reading in same deployments multiple times)
  distinct(deployment, year, date_taken, common_name, .keep_all = TRUE) |>
  # Recreate series
  mutate(series_num = 0,
         dt_previous = lag(date_taken),
         dt_next = lead(date_taken),
         diff_time_previous = as.numeric(date_taken - dt_previous),
         diff_time_next = as.numeric(abs(date_taken - dt_next)),
         dep_lag = lag(deployment),
         diff_dep = ifelse(deployment != dep_lag, TRUE, FALSE),
         diff_series = ifelse(diff_dep == TRUE | abs(diff_time_previous) > 120, 1, 0),
         series_num = c(0, cumsum(diff_series[-1]))) |>
  select(deployment, year, common_name, date_taken, diff_time_previous, diff_time_next, behaviour, series_num) |>
  group_by(series_num) |>
  mutate(diff_time_previous = ifelse(is.na(diff_time_previous) | abs(diff_time_previous) > 120, 0, diff_time_previous),
         diff_time_next = ifelse(is.na(diff_time_next) | abs(diff_time_next) > 120, 0, diff_time_next)) |>
  left_join(gap_groups, by = c("common_name")) |>
  left_join(prob_gaps, by = c("gap_group", "diff_time_previous")) |>
  left_join(df_tbp, by = "common_name") |>
  mutate(pred = ifelse(is.na(pred), 1, pred),
         diff_time_previous_adj = diff_time_previous * pred,
         bookend = ifelse(row_number() == 1 | row_number() == n(), 1, 0),
         image_time = round(ifelse(bookend == 1,
                                   ((diff_time_previous_adj + diff_time_next) / 2) + (tbp / 2),
                                   (diff_time_previous_adj + diff_time_next) / 2), digits = 2)) |>
  select(deployment, year, common_name, date_taken, behaviour, series_num, image_time) |>
  # Create new rows for records with multiple individuals
  separate_rows(behaviour) |>
  # Remove extraneous records
  filter(str_detect(behaviour, "O$|IP$|IC$|DP$|DC$|CL$|L$")) |>
  ungroup() |>
  # Join veghf information
  left_join(df_veghf, by = "deployment") |>
  # Change 'CL' to 'L' = "Loitering"
  mutate(behaviour = ifelse(behaviour == "CL", "L", behaviour),
         series_num = series_num + 1)

# Wide data for modeling
df_wide <- df_data_clean |>
  group_by(deployment, year, common_name, VegHF, VegForDetectionDistance, series_num) |>
  summarise(total_time = sum(image_time),
            total_time_IP = sum(image_time[behaviour == "IP"]),
            total_time_IC = sum(image_time[behaviour == "IC"]),
            total_time_DP = sum(image_time[behaviour == "DP"]),
            total_time_DC = sum(image_time[behaviour == "DC"]),
            total_time_L = sum(image_time[behaviour == "L"]),
            total_time_O = sum(image_time[behaviour == "O"])) |>
  ungroup() |>
  left_join(vg, by = "VegHF") |>
  # Count amount of data by common_name, veghf1 group
  group_by(common_name, VegHF1) |>
  add_count() |>
  filter(n >= 5)

#-----------------------------------------------------------------------------------------------------------------------

# Load data
d_tags_old <- read.csv(paste0(g_drive, "data/supplemental/assumptions-tests/investigation-behaviour-by-veghf/processed/all-species-behaviour_wide_2020-10-12.csv")) |>
  select(common_name, VEGHFAGEclass = VegHF, VegHF1, total_time:total_time_O)

#-----------------------------------------------------------------------------------------------------------------------

# VegHF info
df_veghf <- read_csv(paste0(g_drive, "data/lookup/veghf/all-projects_all-years_veghfageclass-for-habitat-modeling.csv"))

df_sample_data <- read_csv(paste0(g_drive, "data/supplemental/assumptions-tests/investigation-behaviour-by-veghf/data/sample-for-tagging-2022.csv")) |>
  mutate(file = paste0(common_name, "_", series_num, "_", image_number, ".jpg")) |>
  select(file, common_name, project, location, date_detected, age_class, sex, number_individuals)

# Load data tagged in 2022 by Cassie S

df_tags_2022 <- read_excel(paste0(g_drive, "data/supplemental/assumptions-tests/investigation-behaviour-by-veghf/tagged-data/investigating-behaviour_export2_12Sep2022.xlsx"),
                           sheet = "investigating-behaviour_export2") |>
  select(common_name = species, file, number_individuals = count, behav1:behav5) |>
  arrange(common_name, file) |>
  # Join together
  left_join(df_sample_data, by = c("file", "common_name", "number_individuals")) |>
  filter(!is.na(project)) |>
  mutate(across(behav1:behav5, ~ replace_na(., "NA"))) |>
  unite(behaviour, behav1:behav5, sep = ", ", remove = TRUE) |>
  mutate(behaviour = str_remove_all(behaviour, ", NA")) |>
  separate(file, into = c("sp", "series_num", "img"), sep = "_") |>
  select(project, location, common_name, date_detected, age_class, sex, number_individuals, behaviour, series_num) |>
  arrange(location, project, date_detected) |>
  mutate(dt_previous = lag(date_detected),
         dt_next = lead(date_detected),
         diff_time_previous = as.numeric(date_detected - dt_previous),
         diff_time_next = as.numeric(abs(date_detected - dt_next))) |>
  mutate(diff_time_previous = ifelse(is.na(diff_time_previous) | abs(diff_time_previous) > 120, 0, diff_time_previous),
         diff_time_next = ifelse(is.na(diff_time_next) | abs(diff_time_next) > 120, 0, diff_time_next)) |>
  left_join(gap_groups, by = c("common_name")) |>
  left_join(prob_gaps, by = c("gap_group", "diff_time_previous")) |>
  left_join(df_tbp, by = "common_name") |>
  mutate(pred = ifelse(is.na(pred), 1, pred),
         diff_time_previous_adj = diff_time_previous * pred,
         bookend = ifelse(row_number() == 1 | row_number() == n(), 1, 0),
         image_time = round(ifelse(bookend == 1,
                                   ((diff_time_previous_adj + diff_time_next) / 2) + (tbp / 2),
                                   (diff_time_previous_adj + diff_time_next) / 2), digits = 2)) |>
  select(project, location, common_name, date_detected, behaviour, series_num, image_time) |>
  separate_rows(behaviour) |>
  # Join veghf information
  left_join(df_veghf, by = c("project", "location")) |>
  filter(!is.na(VEGHFAGEclass)) |>
  group_by(project, location, common_name, VEGHFAGEclass, series_num) |>
  summarise(total_time = sum(image_time),
            total_time_IP = sum(image_time[behaviour == "IP"]),
            total_time_IC = sum(image_time[behaviour == "IC"]),
            total_time_DP = sum(image_time[behaviour == "DP"]),
            total_time_DC = sum(image_time[behaviour == "DC"]),
            total_time_L = sum(image_time[behaviour == "L"]),
            total_time_O = sum(image_time[behaviour == "O"])) |>
  ungroup() |>
  left_join(vg, by = "VEGHFAGEclass") |>
  select(common_name, VEGHFAGEclass, VegHF1, total_time:total_time_O)

# Join all series information together
df_behaviour <- bind_rows(d_tags_old, df_tags_2022)


