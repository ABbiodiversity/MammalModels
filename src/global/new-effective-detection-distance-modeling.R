#-----------------------------------------------------------------------------------------------------------------------

# Title:       Effective detection distance (EDD) modeling
# Description:
# Author(s):   Dave Huggard, Marcus Becker

# Previous scripts:

#-----------------------------------------------------------------------------------------------------------------------

# Attach packages
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(mgcv)

# Set path to Google Drive
g_drive <- "G:/Shared drives/ABMI Camera Mammals/"

# Lure lookup
df_lure <- read_csv(paste0(g_drive, "data/lookup/lure/abmi_all-years_lure_2023-11-27.csv"))

# Veg/HF lookup
new_edd_cat <- read_csv(paste0(g_drive, "data/lookup/veghf/VegForDetectionDistance/New EDD Modeling.csv")) |>
  mutate(use = case_when(
    str_detect(location, "^W") ~ "No",
    is.na(use) ~ "Yes",
    TRUE ~ use)) |>
  filter(!use == "No",
         !is.na(primary_category)) |>
  select(-c(deadfall, use))

sp <- c("Black Bear", "Grizzly Bear",
        "Gray Wolf",
        "Moose", "Woodland Caribou", "Elk (wapiti)",
        "Canada Lynx",
        "Coyote",
        "Snowshoe Hare",
        "Marten", "Fisher",
        "Mule Deer", "White-tailed Deer", "Deer")

# EDD modeling species groups
df_edd_groups <- read_csv(paste0(g_drive, "data/lookup/species-distance-groups.csv")) |>
  # Let's change these up
  # Focus on the important ones for now
  filter(species_common_name %in% sp) |>
  # New species groups (I think these make more sense, but we can experiment)
  mutate(dist_group = case_when(
    species_common_name == "Gray Wolf" ~ "Gray Wolf",
    str_detect(species_common_name, "Moose|Caribou|Elk") ~ "LargeUngulates",
    species_common_name == "Coyote" ~ "Coyote",
    str_detect(species_common_name, "Deer") ~ "Deer",
    str_detect(species_common_name, "Marten|Fisher") ~ "SmallMustelids",
    species_common_name == "Snowshoe Hare" ~ "Hare",
    TRUE ~ dist_group
  ))

# Old arbitrary season date cutoffs (julian day)
summer.start.j <- 106
summer.end.j <- 288

summer_green <- 143

#-----------------------------------------------------------------------------------------------------------------------

# Extra pole tagging done in September 2020
df_pole_extra <- read_csv(paste0(g_drive, "data/processed/detection-distance/clean-data/extra-pole-position_2021-06-21.csv")) |>
  rename(species_common_name = common_name, image_date_time = date_detected)

# Pole position data
df_pole <- read_csv(
  paste0(g_drive,"data/base/raw/previous/ALL_native-mammals_2019-09-01.csv"),
  col_types = cols(distance = col_character(),
                   number_during_gap = col_number(),
                   number_individuals = col_character()),
  na = "") |>
  # Only observations with pole information
  # Note: There's a problem with the 'B' named sites
  filter(!is.na(distance),
         !str_detect(deployment, "RIVR|OGC|EI|CITSCI|Cudde|AAC")) |>
  mutate(location = str_remove(deployment, "^ABMI-"),
         project = ifelse(str_detect(location, "OG"),
                          paste0("ABMI Off-Grid Monitoring ", Year),
                          paste0("ABMI Ecosystem Health ", Year)),
         species_common_name = ifelse(common_name == "Mule deer", "Mule Deer", common_name)) |>
  # Standardize variable names
  select(location, project, image_date_time = date_time_taken, species_common_name, distance) |>
  mutate(dashes = str_count(location, "-")) |>
  mutate(location = ifelse(dashes == "3", paste0(location, "-1"), location)) |>
  select(-dashes) |>
  mutate(image_date_time = ymd_hms(image_date_time)) |>
  # Join extra tagging
  bind_rows(df_pole_extra) |>
  # Make columns of number of individuals at each pole position
  mutate(at_pole = str_count(distance, "A"),
         behind_pole = str_count(distance, "B"),
         front_pole = str_count(distance, "F"),
         ic_pole = str_count(distance, "IC"),
         ip_pole = str_count(distance, "IP"),
         na_pole = str_count(distance, "NA")) |>
  # Join lure information
  left_join(df_lure, by = c("location", "project")) |>
  # Filter out lured deployments; only unlured are used in the EDD modeling
  filter(lure == "No") |>
  # Create variable for julian date
  mutate(julian = as.numeric(format(ymd_hms(image_date_time), "%j"))) |>
  # Remove wetland sites
  filter(!str_detect(location, "^W"))

# Join new Veg category information
df_pole_veg <- df_pole |>
  left_join(new_edd_cat, by = c("location", "project")) |>
  left_join(df_edd_groups, by = "species_common_name") |>
  filter(!is.na(dist_group)) |>
  # Sum total individuals in each of at_pole through ip_pole
  mutate(n = rowSums(across(at_pole:ip_pole))) |>
  # Only use records where number_individuals = behind_pole or front_pole
  filter(n == behind_pole | n == front_pole,
         !is.na(primary_category)) |>
  # Create season variables based on julian day, and calculate the proportion of individuals behind pole
  mutate(season_old = as.factor(ifelse(julian  >= summer.start.j & julian <= summer.end.j, "summer", "winter")),
         interval = interval(snow_start, snow_gone),
         intersect = image_date_time %within% interval,
         intersect = ifelse(is.na(intersect), "Not applicable", intersect),
         season_new1 = ifelse(image_date_time <= snow_gone, "snow", "nonsnow"),
         season_new1 = if_else(season_new1 == "snow" & intersect == FALSE, "nonsnow", season_new1),
         season_new1 = as.factor(ifelse(is.na(season_new1), "nonsnow", season_new1)),
         snow_gone_early = snow_gone %m-% days(10),
         season_new2 = ifelse(image_date_time <= snow_gone_early, "snow", "nonsnow"),
         season_new2 = if_else(season_new2 == "snow" & intersect == FALSE, "nonsnow", season_new2),
         season_new2 = as.factor(ifelse(is.na(season_new2), "nonsnow", season_new2)),
         julian_sge = as.numeric(format(ymd(snow_gone_early), "%j")),
         season_new3 = case_when(
           julian >= julian_sge & julian <= summer_green ~ "spring",
           julian < julian_sge ~ "winter",
           julian > summer_green ~ "summer"),
         season_new3 = as.factor(ifelse(is.na(season_new3), "spring", season_new3)),
         prop_behind = behind_pole / n) |>
  select(dist_group, n, prop_behind, VegHF, season_old,
         primary_category, secondary_category, season_new1, season_new2, season_new3)

# What do I have of each dist_group and new veg category?
check <- df_pole_veg |>
  group_by(dist_group, primary_category) |>
  tally()

#-----------------------------------------------------------------------------------------------------------------------

# Step 2. EDD Modeling.

# Start with only Large Ungulates
d.sp <- df_pole_veg |>
  filter(dist_group == "SmallMustelids") |>
  # Turn Forested Open-Shrubby to Open-Open (for now)
  mutate(secondary_category = ifelse(secondary_category == "Open-Shrubby", "Open-Open", secondary_category)) |>
  mutate(secondary_category = paste0(primary_category, "_", secondary_category)) |>
  mutate_if(is.character, as.factor) |>
  group_by(secondary_category, season_new3) |>
  add_count() |>
  #filter(nn >= 20) |>
  filter(primary_category == "Forested",
         season_new1 == "snow") |>
  as.data.frame()

unique(d.sp$secondary_category)

d.sp |>
  group_by(secondary_category, season_new1) |>
  tally()

m <- list(NULL)

  # Null model
  #m[[1]]<-try(gam(prop_behind~1,weights=d.sp$n,data=d.sp,family="binomial"))
  # Old - VegHF
  m[[2]]<-try(gam(prop_behind ~ VegHF, weights = d.sp$n, data=d.sp, family="binomial"))
  # Old - Season as originally defined
  m[[3]]<-try(gam(prop_behind ~ season_old, weights = d.sp$n, data=d.sp, family="binomial"))
  # Old - VegHF plus old season
  m[[4]]<-try(gam(prop_behind ~ VegHF + season_old, weights = d.sp$n, data=d.sp, family="binomial"))
  # New - Primary Category
  m[[5]]<-try(gam(prop_behind ~ primary_category, weights = d.sp$n, data=d.sp, family="binomial"))
  # New - Secondary Category
  m[[1]]<-try(gam(prop_behind ~ secondary_category, weights = d.sp$n, data=d.sp, family="binomial"))
  # New - Season 1 (Snow / Non-snow period)
  m[[7]]<-try(gam(prop_behind ~ season_new1, weights = d.sp$n, data=d.sp, family="binomial"))
  # New - Season 2 (Snow / Non-snow period minus 10 days)
  m[[8]]<-try(gam(prop_behind ~ season_new2, weights = d.sp$n, data=d.sp, family="binomial"))
  # New - Season 3 (Winter / Spring / Summer)
  m[[9]]<-try(gam(prop_behind ~ season_new3, weights = d.sp$n, data=d.sp, family="binomial"))
  # New - Primary Category plus New Season 1
  m[[10]]<-try(gam(prop_behind ~ primary_category + season_new1, weights = d.sp$n, data=d.sp, family="binomial"))
  # New - Primary Category plus New Season 2
  m[[11]]<-try(gam(prop_behind ~ primary_category + season_new2, weights = d.sp$n, data=d.sp, family="binomial"))
  # New - Primary Category plus New Season 3
  m[[12]]<-try(gam(prop_behind ~ primary_category + season_new3, weights = d.sp$n, data=d.sp, family="binomial"))
  # New - Secondary Category plus New Season 1
  m[[13]]<-try(gam(prop_behind ~ secondary_category + season_new1, weights = d.sp$n, data=d.sp, family="binomial"))
  # New - Secondary Category plus New Season 2
  m[[14]]<-try(gam(prop_behind ~ secondary_category + season_new2, weights = d.sp$n, data=d.sp, family="binomial"))
  # New - Secondary Category plus New Season 3
  m[[15]]<-try(gam(prop_behind ~ secondary_category + season_new3, weights = d.sp$n, data=d.sp, family="binomial"))

  nModels<-length(m)

  bic.ta<-rep(999999999,(nModels))

  for (i in 1:nModels) {

    bic.ta[i] <- BIC(m[[i]])

  }

  bic.delta<-bic.ta-min(bic.ta)
  bic.exp<-exp(-1/2*bic.delta)
  bic.wt<-bic.exp/sum(bic.exp)
  best.model<-which.max(bic.wt)

  # Okay, so for LargeUngulates it looks like Model 15 is by far the best.

  # Let's predict this out.
  newdata <- d.sp |>
    select(secondary_category) |>
    distinct() |>
    arrange(secondary_category)

  predictions <- data.frame("prediction" = predict(m[[1]], newdata = newdata)) |>
    mutate(prediction = 5 / sqrt(1 - plogis(prediction)))

  results <- newdata |> bind_cols(predictions)

#-----------------------------------------------------------------------------------------------------------------------

# Let's visualize

library(ggplot2)

results |>
  #mutate(season_new3 = factor(season_new3, levels = c("winter", "spring", "summer"))) |>
  mutate(season_new2 = factor(season_new2, levels = c("snow", "nonsnow"))) |>
  mutate(prediction = round(prediction, digits = 1)) |>
  #separate(secondary_category, into = c("primary", "secondary"), sep = "_") |>
  #mutate(secondary = case_when(
  #  secondary == "Closed-Closed" ~ "C-C",
  #  secondary == "Open-Closed" ~ "O-C",
  #  secondary == "Open-Open" ~ "O-O",
  #  TRUE ~ secondary
  #)) |>
  #mutate(secondary_category = paste0(primary, " (", secondary, ")")) |>
  ggplot(aes(x = primary_category, y = prediction, fill = season_new2, label = prediction)) +
  geom_col(position = position_dodge2(width = 0.7, preserve = "single")) +
  geom_text(position = position_dodge2(width = 0.9, preserve = "single"), size = 2, vjust = -0.5) +
  scale_fill_manual(values = c("#829EBC", "darkgreen")) +
  #scale_fill_manual(values = c("#829EBC", "#CD7F32", "darkgreen")) +
  scale_y_continuous(breaks = seq(0, 18, by = 2), limits = c(0, 18)) +
  labs(y = "EDD (metres)",
       title = "Coyote") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 72.5, hjust = 1, size = 8))





