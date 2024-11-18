#-----------------------------------------------------------------------------------------------------------------------

# Title:       Effective detection distance (EDD) modeling
# Description:
# Author(s):   Dave Huggard, Marcus Becker

# Previous scripts:

#-----------------------------------------------------------------------------------------------------------------------

# Attach packages
library(tidyverse)
library(googlesheets4)
library(googledrive)
library(mgcv)

# Set path to Google Drive
g_drive_old <- "G:/Shared drives/ABMI Camera Mammals/"

g_drive <- "G:/Shared drives/ABMI Mammals/"

# Lure metadata
df_lure <- read_csv(paste0(g_drive, "Data/Lure/ABMI Lure All Projects 2013-2023.csv"))

# Sheet ID for the original sites with the new EDD categories
sheet_id <- drive_find(type = "spreadsheet",
                        shared_drive = "ABMI Mammals") |>
  filter(str_detect(name, "Sites for Modeling")) |>
  select(id) |>
  pull()

new_edd_cat <- read_sheet(ss = sheet_id) |>
  mutate(use = case_when(
    str_detect(location, "^W") ~ "No",
    is.na(use) ~ "Yes",
    TRUE ~ use)) |>
  filter(!use == "No",
         !is.na(primary_category)) |>
  select(project, location, primary_category, secondary_category)

# Sheet ID for for new sites with the new EDD categories (ABMI EH 2019-2023)
sheet_id <- drive_find(type = "spreadsheet",
                       shared_drive = "ABMI Mammals") |>
  filter(str_detect(name, "2019-2023")) |>
  select(id) |>
  pull()

new <- read_sheet(ss = sheet_id) |>
  select(project, location, primary_category, secondary_category) |>
  filter(!is.na(primary_category))

# Bind together
new_edd_cat <- bind_rows(new_edd_cat, new)

# Species to model for detection distance
sp <- c("Black Bear", "Grizzly Bear",
        "Gray Wolf",
        "Moose", "Woodland Caribou", "Elk (wapiti)",
        "Canada Lynx",
        "Coyote",
        "Snowshoe Hare",
        "Marten", "Fisher",
        "Mule Deer", "White-tailed Deer", "Deer")

# EDD modeling species groups
df_edd_groups <- read_csv(paste0(g_drive, "Data/Detection Distance/Species Detection Distance Groups.csv"))

# Read in animal pole position data for modeling:
df_pole <- read_csv(paste0(g_drive, "Data/Detection Distance/All Tagged Pole Position Data September 2024.csv"))

#-----------------------------------------------------------------------------------------------------------------------

# Old season cutoff Julian days
summer.start.old <- 106 # April 16
winter.start.old <- 288 # October 15

# New season cutoff Julian days
spring.start <- 99 # A little earler - April 9
summer.start <- 143 # Summer green-up has (mostly) occurred
winter.start <- 288 # Same as before, Oct 15

# Format data for modeling

df_pole_veg <- df_pole |>
  # Make columns of number of individuals at each pole position
  mutate(at = str_count(distance, "A"),
         behind = str_count(distance, "B"),
         front = str_count(distance, "F"),
         ic = str_count(distance, "IC"),
         ip = str_count(distance, "IP")) |>
  # Join lure information and filter out lured deployments (no supposed to be used for EDD modeling)
  left_join(df_lure) |>
  filter(lure == "No") |>
  # Create variable for Julian date
  mutate(julian = as.numeric(format(ymd_hms(image_date_time), "%j"))) |>
  # Remove wetland sites
  filter(!str_detect(location, "^W")) |>
  # Sum total individuals in each distance category
  mutate(n = rowSums(across(at:ip))) |>
  # Only use records where n = behind or front
  filter(n == behind | n == front) |>
  # Join new veg categories
  left_join(new_edd_cat) |>
  # Remove data points without new veg categories
  # Note: Mostly (all?) OG sites ... could come back and redo those once I tag those locations.
  filter(!is.na(primary_category)) |>
  filter(!primary_category == "NA") |>
  mutate(prop_behind = behind / n) |>
  filter(species_common_name %in% sp) |>
  left_join(df_edd_groups) |>
  # Create season variables based on Julian day
  mutate(season_old = as.factor(ifelse(julian >= summer.start.old & julian <= winter.start.old, "Summer", "Winter")),
         season_new = as.factor(case_when(
           julian >= spring.start & julian <= summer.start ~ "Spring",
           julian > summer.start & julian <= winter.start ~ "Summer",
           TRUE ~ "Winter"
         ))) |>
  # Just a few small fixes - not going to use "Open-Shrubby" category
  mutate(secondary_category = ifelse((primary_category == "Forested" & secondary_category == "Open"),
                                     "Open-Open", secondary_category),
         secondary_category = ifelse(secondary_category == "Open-Shrubby", "Open-Open", secondary_category),
         overall_category = paste0(primary_category, "_", secondary_category)) |>
  mutate_if(is.character, as.factor) |>
  select(location, image_date_time, species_common_name, dist_group, prop_behind, n,
         season_old, season_new, primary_category, secondary_category, overall_category)

# 13,181 total observations for the species of interest.

# What do I have of each dist_group and new veg category?
check <- df_pole_veg |>
  group_by(dist_group, overall_category, season_new, .drop = FALSE) |>
  tally()

# Very few observations for a few groups in 'Open' (which makes sense biologically), especially Small Mustelids

# Note: Focus future tagging on this effort - find species detections at cameras in categories that we need pole position tags for.

#-----------------------------------------------------------------------------------------------------------------------

# EDD Modeling, by Dist Group. There are 8 groups.

dist_groups <- unique(df_pole_veg$dist_group)

# New data to predict with
newdata <- df_pole_veg |>
  select(overall_category, season_new) |>
  distinct() |>
  arrange(overall_category, season_new)

# Note: There will be unreliable values for certain combinations (e.g., Bears in Winter, SmallMustelids in Open)
#       due to this modeling. Won't have significant downstream effects, however.

# Dataframe to store results
results <- newdata

for (i in 1:length(dist_groups)) {

  d.sp <- df_pole_veg[df_pole_veg$dist_group == dist_groups[[i]], ]

  # Model EDD as a function of overall_category and season.
  # Note: Not trying any other model structures for now (for simplicity)
  m <- try(gam(prop_behind ~ overall_category + season_new, weights = d.sp$n, data = d.sp, family = "binomial"))

  p <- predict(m, newdata = newdata)

  edd <- map_dbl(p, ~ 5 / sqrt(1 - plogis(.)))

  results[[dist_groups[i]]] <- edd

}

# Turn results into long format
results_long <- results |>
  pivot_longer(cols = Deer:SmallMustelids, values_to = "edd", names_to = "dist_group") |>
  arrange(dist_group, overall_category, season_new)

# Save results
write_csv(results_long, paste0(g_drive, "Data/Detection Distance/EDD Predictions by Species Group Season Veg Category.csv"))


# In the future, can you use the models and BIC weighting below to figure out best model.
# However, this complicates things with very little benefit (mostly).

m <- list(NULL)

# Null model
m[[1]] <- try(gam(prop_behind ~ 1, weights = d.sp$n, data = d.sp, family = "binomial"))
# Primary Category
m[[2]] <- try(gam(prop_behind ~ primary_category, weights = d.sp$n, data = d.sp, family = "binomial"))
# Overall Category
m[[3]] <- try(gam(prop_behind ~ overall_category, weights = d.sp$n, data = d.sp, family = "binomial"))
# Season
m[[4]] <- try(gam(prop_behind ~ season_new, weights = d.sp$n, data = d.sp, family = "binomial"))
# Primary Category plus Season
m[[5]] <- try(gam(prop_behind ~ primary_category + season_new, weights = d.sp$n, data = d.sp, family = "binomial"))
# Overall Category plus Season
m[[6]] <- try(gam(prop_behind ~ overall_category + season_new, weights = d.sp$n, data = d.sp, family = "binomial"))

n_models <- length(m)

bic.ta<-rep(999999999,(n_models))

for (i in 1:n_models) {

  bic.ta[i] <- BIC(m[[i]])

}

bic.delta <- bic.ta - min(bic.ta)
bic.exp <- exp(-1/2 * bic.delta)
bic.wt <- bic.exp / sum(bic.exp)
best.model <- which.max(bic.wt)


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

#-----------------------------------------------------------------------------------------------------------------------

