#-----------------------------------------------------------------------------------------------------------------------

# Title:        Investigating Species Diel Activity Patterns
# Description:

# Author:
# Last Updated: June 2024

#-----------------------------------------------------------------------------------------------------------------------

# Install required packages if needed
if (!requireNamespace("overlap"))
  install.packages("overlap")
if (!requireNamespace("activity"))
  install.packages("activity")
if (!requireNamespace("tidyverse"))
  install.packages("tidyverse")

# Attach packages
library(overlap)
library(activity)
library(tidyverse)

# Set path to Shared Google Drive (G Drive)
g_drive <- "G:/Shared drives/ABMI Camera Mammals/"

## This function turns POSIX formatted date-times into radian time
posix2radian <- function(x) {

  if (is.POSIXt(x) == F) {

    stop(simpleError("Time must be in POSIXct or POSIXt format"))

  } else {

    s <- second(x)
    m <- minute(x)
    h <- hour(x)

    rad_time <- ((h + (m + (s / 60)) / 60) / 24) * 2 * pi

    return(rad_time)
  }
}

# Seasonal cutoffs
summer.start.j <- 106
summer.end.j <-  288

# Deployment locations
dep_loc <- read_csv(paste0(g_drive, "data/lookup/locations/all-projects_all-years_locations-for-habitat-modeling.csv")) |>
  filter(str_detect(project, "CMU")) |>
  select(location, latitude, longitude) |>
  distinct() |>
  separate(location, into = c("grid", "station"), sep = "-")

# Monitoring periods
df_mp <- read_csv(paste0(g_drive, "data/lookup/monitoring-periods/cmu/monitoring-periods_2024-05-14.csv")) |>
  mutate(date = map2(.x = date_start, .y = date_end, .f = ~ seq.Date(from = .x, to = .y, by = "day"))) |>
  unnest(cols = c(date)) |>
  select(monitoring_period, date)

#-----------------------------------------------------------------------------------------------------------------------

# Load and prepare data

df <- read_csv(paste0(g_drive, "data/base/clean/cmu_all-years_native-sp_clean_2024-06-26.csv")) |>
  filter(!number_individuals == "VNA") |>
  mutate(number_individuals = as.numeric(number_individuals)) |>
  # Species and grids of interest
  filter(str_detect(common_name, "White-tailed Deer|Moose|Woodland Caribou|Gray Wolf"),
         str_detect(location, "MAC|FMM|WAB|CHR|LID|LLB|PEL|CAL")) |>
  separate(location, into = c("grid", "station"), sep = "-") |>
  mutate(treatment = ifelse(str_detect(grid, "MAC|FMM|WAB|PEL|CAL"), "Reference", "Wolf-Reduction"),
         rad_time = posix2radian(date_detected),
         julian = as.numeric(format(date_detected, "%j")),
         date = as.Date(date_detected),
         season = ifelse(julian >= summer.start.j & julian <= summer.end.j, "Summer", "Winter")) |>
  select(-julian) |>
  # "Spread" rows out if number_individuals > 1 (TBD if this is the right approach)
  uncount(weights = number_individuals) |>
  # Join locations
  left_join(dep_loc, by = c("grid", "station")) |>
  # Join monitoring periods
  left_join(df_mp, by = "date") |>
  # Remove observation before the start of monitoring period 2017
  filter(!is.na(monitoring_period))

# Calculate "solar time" using the average anchoring method (Vazquez et al)

solar_time <- solartime(dat = df$date_detected,
                        lat = df$latitude,
                        lon = df$longitude,
                        # An offset in numeric hours to UTC (Alberta is 6 hours behind)
                        tz = -6,
                        format = "%Y-%m-%d %H:%M:%S")

df <- df |>
  mutate(solar_time = solar_time$solar) |>
  select(monitoring_period, grid, station, date_detected, season,
         common_name, age_class, sex, treatment, rad_time, solar_time)

write_csv(df, "Image data for species diel patterns analysis.csv")

#-----------------------------------------------------------------------------------------------------------------------

# Step 1. Let's look at the Treatment (Wolf Reduction) vs Baseline

# Caribou:
caribou_ref <- df |>
  filter(common_name == "Woodland Caribou",
         treatment == "Reference") |>
  pull(rad_time)

caribou_wr <- df |>
  filter(common_name == "Woodland Caribou",
         treatment == "Wolf-Reduction") |>
  pull(rad_time)

# Plots
densityPlot(caribou_ref, col = "black", lty = 1, main = "Diel Activity Patterns of Caribou (Reference)")
densityPlot(caribou_wr, col = "black", lty = 1, main = "Diel Activity Patterns of Caribou (Wolf Reduction)")

# Compare together
overlapPlot(caribou_ref, caribou_wr, adjust = 1, main = "Caribou: Reference vs Wolf Reduction")
legend("topright", lty = c(2,1), col = c("blue","black"), legend = c("Wolf Reduction","Reference"), cex = 1.2)

# Wolf:
wolf_ref <- df |>
  filter(common_name == "Gray Wolf",
         treatment == "Reference") |>
  pull(rad_time)

wolf_wr <- df |>
  filter(common_name == "Gray Wolf",
         treatment == "Wolf-Reduction") |>
  pull(rad_time)

# Plots
densityPlot(wolf_ref, col = "black", lty = 1, main = "Diel Activity Patterns of Wolf (Reference)")
densityPlot(wolf_wr, col = "black", lty = 1, main = "Diel Activity Patterns of Wolf (Wolf Reduction)")

# Plots
overlapPlot(wolf_ref, wolf_wr, adjust=1, main="Wolf: Reference vs Wolf Reduction")
legend("topleft", lty = c(2,1), col = c("blue","black"), legend = c("Wolf Reduction","Reference"), cex = 1.2)

# Deerses

deer_ref <- df |>
  filter(common_name == "White-tailed Deer",
         treatment == "Reference") |>
  pull(rad_time)

deer_wr <- df |>
  filter(common_name == "White-tailed Deer",
         treatment == "Wolf-Reduction") |>
  pull(rad_time)

densityPlot(deer_ref, col = "black", lty = 1, main = "Diel Activity Patterns of Deer (Reference)")
densityPlot(deer_wr, col = "black", lty = 1, main = "Diel Activity Patterns of Deer (Wolf Reduction)")

# Plots
overlapPlot(deer_ref, deer_wr, adjust=1, main="Deer: Reference vs Wolf Reduction")
legend("topright", lty = c(2,1), col = c("blue","black"), legend = c("Wolf Reduction","Reference"), cex = 1.2)

# Mooses

moose_ref <- df |>
  filter(common_name == "Moose",
         treatment == "Reference") |>
  pull(rad_time)

moose_wr <- df |>
  filter(common_name == "Moose",
         treatment == "Wolf-Reduction") |>
  pull(rad_time)

densityPlot(moose_ref, col = "black", lty = 1, main = "Diel Activity Patterns of Moose (Reference)")
densityPlot(moose_wr, col = "black", lty = 1, main = "Diel Activity Patterns of Moose (Reduction)")

# Plots
overlapPlot(moose_ref, moose_wr, adjust=1, main="Moose: Reference vs Wolf Reduction")
legend("topright", lty = c(2,1), col = c("blue","black"), legend = c("Wolf Reduction","Reference"), cex = 1.2)

# Let's plot the overlap between species

# Wolf and Caribou in Reference
overlapPlot(caribou_ref, wolf_ref, adjust = 1, main = "Caribou v Wolves in Reference")
legend("topleft", lty = c(2, 1), col = c("blue", "black"), legend = c("Wolf", "Caribou"), cex = 1.2)

# Wolf and Caribou in Wolf Reduction
overlapPlot(caribou_wr, wolf_wr, adjust = 1, main = "Caribou v Wolves in Wolf Reduction")
legend("topleft", lty = c(2, 1), col = c("blue", "black"), legend = c("Wolf", "Caribou"), cex = 1.2)

#-----------------------------------------------------------------------------------------------------------------------

# Can fit models using the activity package

# First caribou in reference
caribou_ref <- df |>
  filter(common_name == "Woodland Caribou",
         treatment == "Reference")

caribou_ref_mod <- activity::fitact(caribou_ref$solar_time,
                                   sample = "model",
                                   # Suggest more reps, but takes a while
                                   reps = 100)

# Plot output
plot(caribou_ref_mod)

# Caribou in wolf reduction
caribou_wr <- df |>
  filter(common_name == "Woodland Caribou",
         treatment == "Wolf-Reduction")

caribou_wr_mod <- activity::fitact(caribou_wr$solar_time,
                                    sample = "model",
                                    # Suggest more reps, but takes a while
                                    reps = 100)

plot(caribou_wr_mod)

# Plot both on the same axis

plot(caribou_ref_mod, yunit = "density", data = "none", las = 1, lwd = 2,
     tline = list(lwd=2), # Thick line
     cline = list(lty=0)) # Supress confidence intervals

plot(caribou_wr_mod, yunit = "density", data = "none", add = TRUE,
     tline = list(col="red", lwd=2),
     cline = list(lty=0))

legend("topright", c("Reference", "Wolf Reduction"), col=1:2, lty=1, lwd=2)

# Calculate coefficient of overlap

activity::compareCkern(caribou_ref_mod, caribou_wr_mod, reps = 100)

# 0 = no overlap, 1 = high overlap

#-----------------------------------------------------------------------------------------------------------------------

















