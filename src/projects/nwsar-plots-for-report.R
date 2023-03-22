#-----------------------------------------------------------------------------------------------------------------------

# Title:  Plots for NWSAR report
# Author: Melanie Dickie, Marcus Becker
# Date:   March 7, 2023

#-----------------------------------------------------------------------------------------------------------------------

# Attach packages
library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)

# Root directory
g_drive <- "G:/Shared drives/ABMI Camera Mammals/"

df_nwsar <- read_csv(paste0(g_drive, "results/density/areas/nwsar_grid-density-by-project_2023-03-03.csv"))

#-----------------------------------------------------------------------------------------------------------------------

# Add latitude and range
df_nwsar <- df_nwsar |>
  mutate(latitude = factor(case_when(
    str_detect(grid, "RUN|SOR") ~ "South",
    str_detect(grid, "OSL|CMP") ~ "Mid",
    str_detect(grid, "RBW|NOR") ~ "North"),
    levels = c("South", "Mid", "North"))) |>
  mutate(range = factor(case_when(
    str_detect(grid, "RUN|OSL|RBW") ~ "Chinchaga",
    str_detect(grid, "SOR|CMP|NOR") ~ "Caribou Mountains"),
    levels = c("Chinchaga", "Caribou Mountains"))) |>
  mutate(project = str_replace_all(project, "[^0-9]", ""))

predators <- c("Black Bear", "Canada Lynx", "Gray Wolf")
prey <- c("White-tailed Deer", "Moose", "Woodland Caribou", "Snowshoe Hare")

# Predators plot
df_nwsar |>
  filter(common_name %in% predators) |>
  ggplot(mapping = aes(x = latitude, y = density_avg, color = project)) +
  geom_point(size = 3, position = position_dodge(width = 0.3)) +
  geom_errorbar(aes(ymin = density_lci_0.9, ymax = density_uci_0.9),
                width = 0.3, position = "dodge", linewidth = 0.5) +
  xlab("") +
  ylab("Density")+
  labs(color = "Sample Year:") +
  theme_bw()+
  theme(axis.text.x = element_text(size=12)) +
  theme(axis.text.y = element_text(size=10), axis.title.y = element_text(size=16)) +
  theme(axis.title.x = element_blank()) +
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) +
  theme(legend.title = element_text(size=14), axis.title = element_text(size=22))+
  theme(legend.text = element_text(size=14))+
  theme(legend.position = "bottom")+
  theme(strip.text.x = element_text(size = 16), strip.text.y = element_text(size = 12))+
  facet_grid(common_name ~ range, scales = "free_y")

# Prey plot
df_nwsar |>
  filter(common_name %in% prey) |>
  ggplot(mapping = aes(x = latitude, y = density_avg, color = project)) +
  geom_point(size = 3, position = position_dodge(width = 0.3)) +
  geom_errorbar(aes(ymin = density_lci_0.9, ymax = density_uci_0.9),
                width = 0.3, position = "dodge", linewidth = 0.5) +
  xlab("") +
  ylab("Density")+
  labs(color = "Sample Year:") +
  theme_bw()+
  theme(axis.text.x = element_text(size=12)) +
  theme(axis.text.y = element_text(size=10), axis.title.y = element_text(size=16)) +
  theme(axis.title.x = element_blank()) +
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) +
  theme(legend.title = element_text(size=14), axis.title = element_text(size=22))+
  theme(legend.text = element_text(size=14))+
  theme(legend.position = "bottom")+
  theme(strip.text.x = element_text(size = 16), strip.text.y = element_text(size = 9))+
  facet_grid(common_name ~ range, scales = "free_y")

#-----------------------------------------------------------------------------------------------------------------------

# Time by day summary
df_tbd <- read_csv(paste0(g_drive, "data/processed/time-by-day/nwsar_20-21_tbd-summary_2023-01-19.csv")) |>
  separate(project_location, into = c("project", "location"), sep = "_", remove = TRUE) |>
  separate(location, into = c("grid", "station"), sep = "-", remove = TRUE) |>
  mutate(range = factor(case_when(
    str_detect(grid, "RUN|OSL|RBW") ~ "Chinchaga",
    str_detect(grid, "SOR|CMP|NOR") ~ "Caribou Mountains"),
    levels = c("Chinchaga", "Caribou Mountains"))) |>
  mutate(project = str_replace_all(project, "[^0-9]", "")) |>
  group_by(project, grid) |>
  summarise(mean_effort = mean(total_days, na.rm = TRUE))

# Write data out for Jillian
check <- df_nwsar |>
  select(Range, Location = Loc, Grid = grid, Year = project,
         Species = common_name, Occupied = occupied, N_Deployments = n_deployments,
         Prop_Occupied = prop_occupied, Density, UCI, LCI)

write_csv(check, paste0(g_drive, "results/density/areas/nwsar_grid_results.csv"))

#-----------------------------------------------------------------------------------------------------------------------
