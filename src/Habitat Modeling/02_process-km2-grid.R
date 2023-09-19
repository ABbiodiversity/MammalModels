#-----------------------------------------------------------------------------------------------------------------------

# Project(s):       ABMI EH, ABMI OG, CMU, BG, NWSAR

# Title:            Process km2 grid information file
# Description:
# Author:           Marcus Becker, David J. Huggard

# Previous scripts: None

#-----------------------------------------------------------------------------------------------------------------------

# Load packages
library(readr)
library(dplyr, warn.conflicts = FALSE)
library(tidyr)
library(tibble)
library(stringr)
library(Matrix)

# Root directory (Google Drive)
g_drive <- "G:/Shared drives/ABMI Camera Mammals/"

#-----------------------------------------------------------------------------------------------------------------------

# Import data:
load(paste0(g_drive, "data/lookup/locations/veg-hf_grid_v61hf2016v3WildFireUpTo2016.Rdata"))
veg_current   <- dd_kgrid[["veg_current"]]
veg_reference <- dd_kgrid[["veg_reference"]]
rm(dd_kgrid)
gc()

# Province-wide km2 grid info
load(paste0(g_drive, "data/lookup/locations/kgrid_table_km.Rdata"))

#-----------------------------------------------------------------------------------------------------------------------

# Current veghf
veg_current <- data.frame(as.matrix(veg_current)) |>
  # Calculate area of each cell area in the grid (vast majority are 1,000,000 m2)
  mutate(cell_area = rowSums(across(everything()))) |>
  # Convert each veghf area to proportion of the cell area
  mutate(across(-last_col(), ~ .x / cell_area)) |>
  rownames_to_column(var = "LinkID") |>
  select(LinkID, cell_area, everything())

# Add in info about km2 cells - lat, long, natural region and subregion, landuse framework
kgrid_veg_current_north <- kgrid |>
  mutate(Lat = ifelse(POINT_Y < 51.5, 51.5, POINT_Y)) |>
  select(Long = POINT_X, TrueLat = POINT_Y, Lat, NR = NRNAME, NSR = NSRNAME, LUF = LUF_NAME, AHM:pAspen) |>
  rownames_to_column(var = "LinkID") |>
  # Join veghf information
  left_join(veg_current, by = "LinkID") |>
  # Include only the north cells (by definition, anything that isn't in the grassland natural region)
  filter(!NR == "Grassland") |>
  # Combine veg categories in the km2 grid to match available coefficients
  mutate(TreedSwamp = rowSums(across(TreedSwamp1:TreedSwamp9)),
         TreedFen = rowSums(across(TreedFen1:TreedFen9)),
         TreedShrubSwamp = TreedSwamp + ShrubbySwamp,
         NonTreedFenMarsh = ShrubbyFen + GraminoidFen + Marsh,
         # Collapse oldest age classes (9 isn't modeled)
         Spruce8 = Spruce8 + Spruce9,
         Pine8 = Pine8 + Pine9,
         Decid8 = Decid8 + Decid9,
         Mixedwood8 = Mixedwood8 + Mixedwood9,
         TreedBog8 = TreedBog8 + TreedBog9,
         TreedFen8 = TreedFen8 + TreedFen9,
         WetlandMargin = 0) |>
  # Remove the oldest age class columns
  select(-ends_with("9")) |>
  # Add alternative NSR groupings for modeling; note that the Boreal NR is broken down further
  mutate(NSR_ALT = case_when(
    NSR == "Central Parkland" | NSR == "Foothills Parkland" | NSR == "Peace River Parkland" ~ "NSR1Parkland",
    NSR == "Dry Mixedwood" ~ "NSR1DryMixedwood",
    NSR == "Central Mixedwood" ~ "NSR1CentralMixedwood",
    NSR == "Lower Foothills" | NSR == "Upper Foothills" ~ "NSR1Foothills",
    NSR == "Lower Boreal Highlands" | NSR == "Upper Boreal Highlands" | NSR == "Boreal Subarctic" | NSR == "Northern Mixedwood" ~ "NSR1North",
    NSR == "Kazan Uplands" | NSR == "Peace-Athabasca Delta" | NSR == "Athabasca Plain" ~ "NSR1Shield",
    NSR == "Montane" | NSR == "Subalpine" | NSR == "Alpine" ~ "NSR1Mountain"
  )) |>
  # Split `nr_alt` into wide form
  mutate(value = 1) |>
  pivot_wider(names_from = NSR_ALT, values_from = value, values_fill = list(value = 0))

#rm(veg_current)
gc()

# Group HF types
df_hf <- read_csv(paste0(g_drive, "data/lookup/locations/lookup-hf-class.csv")) |>
  # Only interested in the HF types present in the kgrid data
  filter(HF_GROUP %in% colnames(kgrid_veg_current_north))

# 15 groups - make sure to add HFor, which got dropped in the line above.
groups <- c(unique(df_hf$UseInAnalysis), "HFor")

# Loop
for (i in groups) {

  # For each group, pull out component HF types:
  hf_types_in_group <- df_hf |>
    filter(UseInAnalysis == i) |>
    select(HF_GROUP) |>
    pull()

  # Add the individual cutblock ('CC') types to the HF group
  if (i == "HFor") {
    hf_types_in_group <- str_subset(colnames(kgrid_veg_current_north), pattern = "^CC")
  }

  # Define new variable name, which is the HF group
  varname <- i

  # Add a column for each HF group, summing the component HF types
  kgrid_veg_current_north <- kgrid_veg_current_north |>
    # Sum rows using tidyeval
    mutate({{varname}} := rowSums(across(!!hf_types_in_group)))

  # Print out the hf types that were finished
  print(hf_types_in_group)

}

#-----------------------------------------------------------------------------------------------------------------------

# Reference veghf
veg_reference <- data.frame(as.matrix(veg_reference)) |>
  # Calculate area of each cell area in the grid (vast majority are 1,000,000 m2)
  mutate(cell_area = rowSums(across(everything()))) |>
  # Convert each veghf area to proportion of the cell area
  mutate(across(-last_col(), ~ .x / cell_area)) |>
  rownames_to_column(var = "LinkID") |>
  select(LinkID, cell_area, everything())

# Add in info about km2 cells - lat, long, natural region and subregion, landuse framework
kgrid_veg_reference_north <- kgrid |>
  mutate(Lat = ifelse(POINT_Y < 51.5, 51.5, POINT_Y)) |>
  select(Long = POINT_X, TrueLat = POINT_Y, Lat, NR = NRNAME, NSR = NSRNAME, LUF = LUF_NAME, AHM:pAspen) |>
  rownames_to_column(var = "LinkID") |>
  # Join veghf information
  left_join(veg_reference, by = "LinkID") |>
  # Include only the north cells (by definition, anything that isn't in the grassland natural region)
  filter(!NR == "Grassland") |>
  # Combine veg categories in the km2 grid to match available coefficients
  mutate(TreedSwamp = rowSums(across(TreedSwamp1:TreedSwamp9)),
         TreedFen = rowSums(across(TreedFen1:TreedFen9)),
         TreedShrubSwamp = TreedSwamp + ShrubbySwamp,
         NonTreedFenMarsh = ShrubbyFen + GraminoidFen + Marsh,
         # Collapse oldest age classes (9 isn't modeled)
         Spruce8 = Spruce8 + Spruce9,
         Pine8 = Pine8 + Pine9,
         Decid8 = Decid8 + Decid9,
         Mixedwood8 = Mixedwood8 + Mixedwood9,
         TreedBog8 = TreedBog8 + TreedBog9,
         TreedFen8 = TreedFen8 + TreedFen9,
         WetlandMargin = 0) |>
  # Remove the oldest age class columns
  select(-ends_with("9")) |>
  # Add alternative NSR groupings for modeling; note that the Boreal NR is broken down further
  mutate(NSR_ALT = case_when(
    NSR == "Central Parkland" | NSR == "Foothills Parkland" | NSR == "Peace River Parkland" ~ "NSR1Parkland",
    NSR == "Dry Mixedwood" ~ "NSR1DryMixedwood",
    NSR == "Central Mixedwood" ~ "NSR1CentralMixedwood",
    NSR == "Lower Foothills" | NSR == "Upper Foothills" ~ "NSR1Foothills",
    NSR == "Lower Boreal Highlands" | NSR == "Upper Boreal Highlands" | NSR == "Boreal Subarctic" | NSR == "Northern Mixedwood" ~ "NSR1North",
    NSR == "Kazan Uplands" | NSR == "Peace-Athabasca Delta" | NSR == "Athabasca Plain" ~ "NSR1Shield",
    NSR == "Montane" | NSR == "Subalpine" | NSR == "Alpine" ~ "NSR1Mountain"
  )) |>
  # Split `nr_alt` into wide form
  mutate(value = 1) |>
  pivot_wider(names_from = NSR_ALT, values_from = value, values_fill = list(value = 0))

#-----------------------------------------------------------------------------------------------------------------------

# Save processed km2 grid data
save(file = paste0(g_drive, "data/processed/km2-grid/km2-grid-north_current-backfilled_processed.RData"),
     kgrid_veg_current_north,
     kgrid_veg_reference_north)

#-----------------------------------------------------------------------------------------------------------------------
