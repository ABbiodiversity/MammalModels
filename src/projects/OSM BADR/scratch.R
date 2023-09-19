#-----------------------------------------------------------------------------------------------------------------------

library(wildRtrax)
library(keyring)
library(dplyr)
library(stringr)
library(tidyr)

Sys.setenv(WT_USERNAME = key_get("WT_USERNAME", keyring = "wildtrax"),
           WT_PASSWORD = key_get("WT_PASSWORD", keyring = "wildtrax"))

wt_auth()

proj <- wt_get_download_summary(
  sensor_id = "CAM"
)

proj <- 947

report <- wt_download_report(
  project_id = proj,
  sensor_id = "CAM",
  report = "image",
  weather_cols = FALSE
)

#-----------------------------------------------------------------------------------------------------------------------

osm_2021_mms <- report |>
  select(location, serial_number) |>
  distinct()

osm_2021 <- report |>
  select(location, date_detected, trigger)

range <- osm_2021 |>
  group_by(location) |>
  summarise(start_date_time = min(date_detected),
            end_date_time = max(date_detected)) |>
  ungroup() |>
  mutate(days_operating = round(difftime(end_date_time, start_date_time, units = "days"), digits = 0))

osm_2021 <- osm_2021 |>
  group_by(location, trigger) |>
  tally() |>
  arrange(location) |>
  pivot_wider(id_cols = location, names_from = trigger, values_from = n, values_fill = 0) |>
  mutate(`CodeLoc Not Entered` = 0,
         Total = `Motion Detection` + `Time Lapse` + `CodeLoc Not Entered`) |>
  left_join(osm_2021_mms, by = "location") |>
  mutate(Year = 2021) |>
  #mutate(landscape_unit = str_extract(location, "^.{1}")) |>
  mutate(Issue = ifelse(`Time Lapse` > 0, "No", "Yes")) |>
  select(Year, location:Total, Issue, serial_number)

#-----------------------------------------------------------------------------------------------------------------------

osm_2022_mms <- read_csv("osm-2022-camera-serial_numbers.csv") |>
  select(location, 4) |>
  mutate(Year = 2022)

osm_2022 <- read_csv("osm-2022-camera-stats.csv") |>
  mutate(Year = 2022) |>
  select(Year, location, `Motion Detection`:Total, Issue) |>
  left_join(osm_2022_mms, by = c("location", "Year")) |>
  filter(`Time Lapse` < 1)

write_csv(osm_2022, "osm-2022-no-timelapse.csv")

osm_2021 |>
  filter(`Time Lapse` < 1) |>
  write_csv("osm-2021-no-timelapse.csv")

#-----------------------------------------------------------------------------------------------------------------------

all <- bind_rows(osm_2021, osm_2022)

check <- all |>
  group_by(serial_number) |>
  tally() |>
  arrange(desc(n))

#-----------------------------------------------------------------------------------------------------------------------

g_drive <- "G:/Shared drives/ABMI Camera Mammals/data/base/"

eh_2020 <- read_csv(paste0(g_drive, "raw/from_WildTrax/ABMI/image_reports/ABMI_ABMI_Ecosystem_Health_2020_image_report.csv"))

check <- eh_2020 |>
  group_by(location, trigger) |>
  tally() |>
  arrange(location) |>
  pivot_wider(id_cols = location, names_from = trigger, values_from = n, values_fill = 0) |>
  mutate(Total = `Motion Detection` + `Time Lapse` + `CodeLoc Not Entered`)





