#-----------------------------------------------------------------------------------------------------------------------

# Project:          ABMI (OSM)

# Title:            Create figures for OSM BADR results
# Description:      Uses both OSM BADR camera data from 2021 as well as relevant EH data
# Author:           Marcus Becker

# Previous scripts: 01_estimate-treatment-effects

#-----------------------------------------------------------------------------------------------------------------------

# Attach packages
library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
#library(abmi.themes)

# Set path to Shared Google Drive (G Drive)
g_drive <- "G:/Shared drives/ABMI Camera Mammals/"

# Import data
buffer <- read_csv(paste0(g_drive, "results/osm/2021_buffer_treatment_results.csv")) |>
  mutate(fine_scale = str_extract(treatment, "\\d+"),
         treatment = str_remove(treatment, "\\d+"),
         treatment = str_to_title(str_remove(treatment, " buffer $"))) |>
  mutate(treatment = factor(treatment, levels = c("Road", "Plant/Mine")),
         fine_scale = factor(fine_scale, levels = c("10", "30", "100", "300"))) |>
  mutate(Habitat = case_when(
    vegetation == "decidmix40" ~ "Deciduous Mixedwood",
    vegetation == "treedlow20" ~ "Treed Lowland"
  ))

on_off <- read_csv(paste0(g_drive, "results/osm/2021_on-off_treatment_results.csv")) |>
  mutate(fine_scale = case_when(
    str_detect(treatment, "On") ~ "On",
    str_detect(treatment, "Off") ~ "Off",
    TRUE ~ "")) |>
  mutate(treatment = str_to_title(str_remove(treatment, " Off$| On$"))) |>
  mutate(treatment = factor(treatment,
                            levels = c("Reference", "Dense Linear Features", "High Activity In Situ", "Low Activity Well Pads"))) |>
  mutate(Habitat = case_when(
    vegetation == "decidmix40" ~ "Deciduous Mixedwood",
    vegetation == "treedlow20" ~ "Treed Lowland"
  ))

# JEM means

jem_means <- read_csv(paste0(g_drive, "data/processed/osm/2021_mean_jem_density_values.csv")) |>
  filter(!vegetation == "wetland") |>
  mutate(Habitat = case_when(
    vegetation == "decidmix40" ~ "Deciduous Mixedwood",
    vegetation == "treedlow20" ~ "Treed Lowland"
  ))

on_off_jem <- jem_means |>
  filter(type == "HF",
         !treatment == "pre-insitu") |>
  select(Habitat, treatment, fine_scale, common_name, mean_density = density_adj) |>
  mutate(fine_scale = replace_na(fine_scale, ""),
         treatment = str_to_title(treatment)) |>
  mutate(treatment = factor(treatment,
                            levels = c("Reference", "Dense Linear Features", "High Activity In Situ", "Low Activity Well Pads")))

buffer_jem <- jem_means |>
  filter(!type == "HF",
         !treatment == "pre-insitu") |>
  select(Habitat, treatment, fine_scale, common_name, mean_density = density_adj) |>
  mutate(fine_scale = replace_na(fine_scale, ""),
         treatment = str_to_title(str_remove(treatment, " buffer$")),
         fine_scale = factor(fine_scale, levels = c("10", "30", "100", "300")))

#-----------------------------------------------------------------------------------------------------------------------

# Make plots

unique(jem_means$common_name)

# Let's start with the on_off plus reference

veg.col<-c("#FF8800","#33AA33")

# White-tailed Deer

on_off |>
  filter(common_name == "White-tailed Deer") |>
  ggplot(aes(x = fine_scale, y = mean_density)) +
  geom_point(aes(color = Habitat),
             size = 3,
             position = position_dodge(width = 0.75)) +
  geom_linerange(aes(ymin = lci_density, ymax = uci_density, color = Habitat),
                  linewidth = 0.5, position = position_dodge(width = 0.75)) +
  geom_point(data = (on_off_jem |> filter(common_name == "WhitetailedDeer")),
             aes(x = fine_scale, y = mean_density, color = Habitat),
             size = 2,
             alpha = 0.15,
             position = position_jitterdodge(jitter.width = 0.2)) +
  scale_color_manual(values = veg.col) +
  scale_y_sqrt() +
  theme_minimal() +
  coord_cartesian(ylim = c(0, 6)) +
  labs(x = "Placement",
       y = expression(Density~(individuals~per~km^2))) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
        axis.title.x = element_text(size = 14, margin = margin(0.4, 0, 0, 0, unit = "cm")),
        axis.title.y = element_text(size = 10, margin = margin(0, 0.4, 0, 0, unit = "cm")),
        strip.text = element_text(size = 11),
        legend.position = "top",
        legend.title = element_blank(),
        legend.margin = margin(0, 0, 0, 0),
        legend.spacing.x = unit(0.05, "cm"),
        axis.ticks = element_blank(),
        panel.grid.major.y = element_line(linewidth = 0.5, color = "grey80"),
        panel.grid.major.x = element_blank()) +
  facet_grid(. ~ treatment, scales = "free_x", space = "free")

ggsave(paste0(g_drive, "results/osm/figures/wtd_on-off_sqrt.png"), height = 4, width = 8, dpi = 500, bg = "white")

# Now the 2 buffer treatments:

buffer |>
  filter(common_name == "White-tailed Deer") |>
  ggplot(aes(x = fine_scale, y = mean_density)) +
  geom_point(aes(color = vegetation),
             size = 3,
             position = position_dodge(width = 0.5)) +
  geom_linerange(aes(ymin = lci_density, ymax = uci_density, color = vegetation),
                 linewidth = 0.5, position = position_dodge(width = 0.5)) +
  geom_point(data = (buffer_jem |> filter(common_name == "WhitetailedDeer")),
             aes(x = fine_scale, y = mean_density, color = vegetation),
             size = 2,
             alpha = 0.2,
             position = position_jitterdodge(jitter.width = 0.1)) +
  scale_color_manual(values = veg.col) +
  scale_y_sqrt(breaks = c(0, 2, 4, 6, 8, 10, 12), limits = c(0, 14)) +
  theme_classic() +
  labs(x = "Placement (metres away)",
       y = expression(Density~(individuals~per~km^2))) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
        axis.title.x = element_text(size = 14, margin = margin(0.4, 0, 0, 0, unit = "cm")),
        axis.title.y = element_text(size = 12, margin = margin(0, 0.4, 0, 0, unit = "cm")),
        strip.text = element_text(size = 11),
        legend.position = "none",
        axis.ticks = element_blank(),
        panel.grid.major.y = element_line(linewidth = 0.5, color = "grey80")) +
  facet_grid(. ~ treatment, scales = "free_x", space = "free")

ggsave(paste0(g_drive, "results/osm/figures/wtd_buffer_sqrt.png"), height = 4, width = 8, dpi = 500)

#-----------------------------------------------------------------------------------------------------------------------

# Moose

on_off |>
  filter(common_name == "Moose") |>
  mutate(lci_density = ifelse(mean_density > 0, lci_density, 0),
         uci_density = ifelse(mean_density > 0, uci_density, 0)) |>
  ggplot(aes(x = fine_scale, y = mean_density)) +
  geom_point(aes(color = Habitat),
             size = 3,
             position = position_dodge(width = 0.75)) +
  geom_linerange(aes(ymin = lci_density, ymax = uci_density, color = Habitat),
                 linewidth = 0.5, position = position_dodge(width = 0.75)) +
  geom_point(data = (on_off_jem |> filter(common_name == "Moose")),
             aes(x = fine_scale, y = mean_density, color = Habitat),
             size = 2,
             alpha = 0.15,
             position = position_jitterdodge(jitter.width = 0.2)) +
  scale_color_manual(values = veg.col) +
  scale_y_sqrt() +
  theme_minimal() +
  coord_cartesian(ylim = c(0, 3)) +
  labs(x = "Placement",
       y = expression(Density~(individuals~per~km^2))) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
        axis.title.x = element_text(size = 14, margin = margin(0.4, 0, 0, 0, unit = "cm")),
        axis.title.y = element_text(size = 10, margin = margin(0, 0.4, 0, 0, unit = "cm")),
        strip.text = element_text(size = 11),
        legend.position = "top",
        legend.title = element_blank(),
        legend.margin = margin(0, 0, 0, 0),
        legend.spacing.x = unit(0.05, "cm"),
        axis.ticks = element_blank(),
        panel.grid.major.y = element_line(linewidth = 0.5, color = "grey80"),
        panel.grid.major.x = element_blank()) +
  facet_grid(. ~ treatment, scales = "free_x", space = "free")

ggsave(paste0(g_drive, "results/osm/figures/moose_on-off_sqrt.png"), height = 4, width = 8, dpi = 500)

# Now the 2 buffer treatments:

buffer |>
  filter(common_name == "Moose") |>
  mutate(lci_density = ifelse(mean_density > 0, lci_density, 0),
         uci_density = ifelse(mean_density > 0, uci_density, 0)) |>
  ggplot(aes(x = fine_scale, y = mean_density)) +
  geom_point(aes(color = Habitat),
             size = 3,
             position = position_dodge(width = 0.5)) +
  geom_linerange(aes(ymin = lci_density, ymax = uci_density, color = Habitat),
                 linewidth = 0.5, position = position_dodge(width = 0.5)) +
  geom_point(data = (buffer_jem |> filter(common_name == "Moose")),
             aes(x = fine_scale, y = mean_density, color = Habitat),
             size = 2,
             alpha = 0.2,
             position = position_jitterdodge(jitter.width = 0.1)) +
  scale_color_manual(values = veg.col) +
  scale_y_sqrt(breaks = c(0, 1, 2, 3)) +
  theme_minimal() +
  labs(x = "Placement (metres away)",
       y = expression(Density~(individuals~per~km^2))) +
  coord_cartesian(ylim = c(0, 3)) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
        axis.title.x = element_text(size = 14, margin = margin(0.4, 0, 0, 0, unit = "cm")),
        axis.title.y = element_text(size = 10, margin = margin(0, 0.4, 0, 0, unit = "cm")),
        strip.text = element_text(size = 11),
        legend.position = "none",
        legend.title = element_blank(),
        legend.margin = margin(0, 0, 0, 0),
        legend.spacing.x = unit(0.05, "cm"),
        axis.ticks = element_blank(),
        panel.grid.major.y = element_line(linewidth = 0.5, color = "grey80"),
        panel.grid.major.x = element_blank()) +
  facet_grid(. ~ treatment, scales = "free_x", space = "free")

ggsave(paste0(g_drive, "results/osm/figures/moose_buffer_sqrt.png"), height = 4, width = 8, dpi = 500)

#-----------------------------------------------------------------------------------------------------------------------

# Canada Lynx

on_off |>
  filter(common_name == "Canada Lynx") |>
  ggplot(aes(x = fine_scale, y = mean_density)) +
  geom_point(aes(color = vegetation),
             size = 3,
             position = position_dodge(width = 0.75)) +
  geom_linerange(aes(ymin = lci_density, ymax = uci_density, color = vegetation),
                 linewidth = 0.5, position = position_dodge(width = 0.75)) +
  geom_point(data = (on_off_jem |> filter(common_name == "CanadaLynx")),
             aes(x = fine_scale, y = mean_density, color = vegetation),
             size = 2,
             alpha = 0.15,
             position = position_jitterdodge(jitter.width = 0.2)) +
  scale_color_manual(values = veg.col) +
  scale_y_sqrt() +
  theme_classic() +
  labs(x = "Placement",
       y = expression(Density~(individuals~per~km^2))) +
  coord_cartesian(ylim = c(0, 0.6)) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
        axis.title.x = element_text(size = 14, margin = margin(0.4, 0, 0, 0, unit = "cm")),
        axis.title.y = element_text(size = 12, margin = margin(0, 0.4, 0, 0, unit = "cm")),
        strip.text = element_text(size = 11),
        legend.position = "none",
        axis.ticks = element_blank(),
        panel.grid.major.y = element_line(linewidth = 0.5, color = "grey80")) +
  facet_grid(. ~ treatment, scales = "free_x", space = "free")

ggsave(paste0(g_drive, "results/osm/figures/lynx_on-off_sqrt.png"), height = 4, width = 8, dpi = 500)

# Now the 2 buffer treatments:

buffer |>
  filter(common_name == "Canada Lynx") |>
  ggplot(aes(x = fine_scale, y = mean_density)) +
  geom_point(aes(color = vegetation),
             size = 3,
             position = position_dodge(width = 0.5)) +
  geom_linerange(aes(ymin = lci_density, ymax = uci_density, color = vegetation),
                 linewidth = 0.5, position = position_dodge(width = 0.5)) +
  geom_point(data = (buffer_jem |> filter(common_name == "CanadaLynx")),
             aes(x = fine_scale, y = mean_density, color = vegetation),
             size = 2,
             alpha = 0.2,
             position = position_jitterdodge(jitter.width = 0.1)) +
  scale_color_manual(values = veg.col) +
  scale_y_sqrt() +
  theme_classic() +
  labs(x = "Placement (metres away)",
       y = expression(Density~(individuals~per~km^2))) +
  coord_cartesian(ylim = c(0, 0.4)) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
        axis.title.x = element_text(size = 14, margin = margin(0.4, 0, 0, 0, unit = "cm")),
        axis.title.y = element_text(size = 12, margin = margin(0, 0.4, 0, 0, unit = "cm")),
        strip.text = element_text(size = 11),
        legend.position = "none",
        axis.ticks = element_blank(),
        panel.grid.major.y = element_line(linewidth = 0.5, color = "grey80")) +
  facet_grid(. ~ treatment, scales = "free_x", space = "free")

ggsave(paste0(g_drive, "results/osm/figures/lynx_buffer_sqrt.png"), height = 4, width = 8, dpi = 500)

#-----------------------------------------------------------------------------------------------------------------------

# Canada Lynx

on_off |>
  filter(common_name == "Black Bear") |>
  ggplot(aes(x = fine_scale, y = mean_density)) +
  geom_point(aes(color = vegetation),
             size = 3,
             position = position_dodge(width = 0.75)) +
  geom_linerange(aes(ymin = lci_density, ymax = uci_density, color = vegetation),
                 linewidth = 0.5, position = position_dodge(width = 0.75)) +
  geom_point(data = (on_off_jem |> filter(common_name == "BlackBear")),
             aes(x = fine_scale, y = mean_density, color = vegetation),
             size = 2,
             alpha = 0.15,
             position = position_jitterdodge(jitter.width = 0.2)) +
  scale_color_manual(values = veg.col) +
  scale_y_sqrt() +
  theme_classic() +
  labs(x = "Placement",
       y = expression(Density~(individuals~per~km^2))) +
  coord_cartesian(ylim = c(0, 2)) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
        axis.title.x = element_text(size = 14, margin = margin(0.4, 0, 0, 0, unit = "cm")),
        axis.title.y = element_text(size = 12, margin = margin(0, 0.4, 0, 0, unit = "cm")),
        strip.text = element_text(size = 11),
        legend.position = "none",
        axis.ticks = element_blank(),
        panel.grid.major.y = element_line(linewidth = 0.5, color = "grey80")) +
  facet_grid(. ~ treatment, scales = "free_x", space = "free")

ggsave(paste0(g_drive, "results/osm/figures/blackbear_on-off_sqrt.png"), height = 4, width = 8, dpi = 500)

# Now the 2 buffer treatments:

buffer |>
  filter(common_name == "Black Bear") |>
  ggplot(aes(x = fine_scale, y = mean_density)) +
  geom_point(aes(color = vegetation),
             size = 3,
             position = position_dodge(width = 0.5)) +
  geom_linerange(aes(ymin = lci_density, ymax = uci_density, color = vegetation),
                 linewidth = 0.5, position = position_dodge(width = 0.5)) +
  geom_point(data = (buffer_jem |> filter(common_name == "BlackBear")),
             aes(x = fine_scale, y = mean_density, color = vegetation),
             size = 2,
             alpha = 0.2,
             position = position_jitterdodge(jitter.width = 0.1)) +
  scale_color_manual(values = veg.col) +
  scale_y_sqrt() +
  theme_classic() +
  labs(x = "Placement (metres away)",
       y = expression(Density~(individuals~per~km^2))) +
  coord_cartesian(ylim = c(0, 1.5)) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
        axis.title.x = element_text(size = 14, margin = margin(0.4, 0, 0, 0, unit = "cm")),
        axis.title.y = element_text(size = 12, margin = margin(0, 0.4, 0, 0, unit = "cm")),
        strip.text = element_text(size = 11),
        legend.position = "none",
        axis.ticks = element_blank(),
        panel.grid.major.y = element_line(linewidth = 0.5, color = "grey80")) +
  facet_grid(. ~ treatment, scales = "free_x", space = "free")

ggsave(paste0(g_drive, "results/osm/figures/blackbear_buffer_sqrt.png"), height = 4, width = 8, dpi = 500)

#-----------------------------------------------------------------------------------------------------------------------

# Coyote

on_off |>
  filter(common_name == "Coyote") |>
  ggplot(aes(x = fine_scale, y = mean_density)) +
  geom_point(aes(color = vegetation),
             size = 3,
             position = position_dodge(width = 0.75)) +
  geom_linerange(aes(ymin = lci_density, ymax = uci_density, color = vegetation),
                 linewidth = 0.5, position = position_dodge(width = 0.75)) +
  geom_point(data = (on_off_jem |> filter(common_name == "Coyote")),
             aes(x = fine_scale, y = mean_density, color = vegetation),
             size = 2,
             alpha = 0.15,
             position = position_jitterdodge(jitter.width = 0.2)) +
  scale_color_manual(values = veg.col) +
  scale_y_sqrt() +
  theme_classic() +
  labs(x = "Placement",
       y = expression(Density~(individuals~per~km^2))) +
  coord_cartesian(ylim = c(0, 0.2)) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
        axis.title.x = element_text(size = 14, margin = margin(0.4, 0, 0, 0, unit = "cm")),
        axis.title.y = element_text(size = 12, margin = margin(0, 0.4, 0, 0, unit = "cm")),
        strip.text = element_text(size = 11),
        legend.position = "none",
        axis.ticks = element_blank(),
        panel.grid.major.y = element_line(linewidth = 0.5, color = "grey80")) +
  facet_grid(. ~ treatment, scales = "free_x", space = "free")

ggsave(paste0(g_drive, "results/osm/figures/coyote_on-off_sqrt.png"), height = 4, width = 8, dpi = 500)

# Now the 2 buffer treatments:

buffer |>
  filter(common_name == "Coyote") |>
  ggplot(aes(x = fine_scale, y = mean_density)) +
  geom_point(aes(color = vegetation),
             size = 3,
             position = position_dodge(width = 0.5)) +
  geom_linerange(aes(ymin = lci_density, ymax = uci_density, color = vegetation),
                 linewidth = 0.5, position = position_dodge(width = 0.5)) +
  geom_point(data = (buffer_jem |> filter(common_name == "Coyote")),
             aes(x = fine_scale, y = mean_density, color = vegetation),
             size = 2,
             alpha = 0.2,
             position = position_jitterdodge(jitter.width = 0.1)) +
  scale_color_manual(values = veg.col) +
  scale_y_sqrt() +
  theme_classic() +
  labs(x = "Placement (metres away)",
       y = expression(Density~(individuals~per~km^2))) +
  coord_cartesian(ylim = c(0, 0.5)) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
        axis.title.x = element_text(size = 14, margin = margin(0.4, 0, 0, 0, unit = "cm")),
        axis.title.y = element_text(size = 12, margin = margin(0, 0.4, 0, 0, unit = "cm")),
        strip.text = element_text(size = 11),
        legend.position = "none",
        axis.ticks = element_blank(),
        panel.grid.major.y = element_line(linewidth = 0.5, color = "grey80")) +
  facet_grid(. ~ treatment, scales = "free_x", space = "free")

ggsave(paste0(g_drive, "results/osm/figures/coyote_buffer_sqrt.png"), height = 4, width = 8, dpi = 500)

#-----------------------------------------------------------------------------------------------------------------------

# Fisher

on_off |>
  filter(common_name == "Fisher") |>
  ggplot(aes(x = fine_scale, y = mean_density)) +
  geom_point(aes(color = vegetation),
             size = 3,
             position = position_dodge(width = 0.75)) +
  geom_linerange(aes(ymin = lci_density, ymax = uci_density, color = vegetation),
                 linewidth = 0.5, position = position_dodge(width = 0.75)) +
  geom_point(data = (on_off_jem |> filter(common_name == "Fisher")),
             aes(x = fine_scale, y = mean_density, color = vegetation),
             size = 2,
             alpha = 0.15,
             position = position_jitterdodge(jitter.width = 0.2)) +
  scale_color_manual(values = veg.col) +
  scale_y_sqrt() +
  theme_classic() +
  labs(x = "Placement",
       y = expression(Density~(individuals~per~km^2))) +
  coord_cartesian(ylim = c(0, 0.05)) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
        axis.title.x = element_text(size = 14, margin = margin(0.4, 0, 0, 0, unit = "cm")),
        axis.title.y = element_text(size = 12, margin = margin(0, 0.4, 0, 0, unit = "cm")),
        strip.text = element_text(size = 11),
        legend.position = "none",
        axis.ticks = element_blank(),
        panel.grid.major.y = element_line(linewidth = 0.5, color = "grey80")) +
  facet_grid(. ~ treatment, scales = "free_x", space = "free")

ggsave(paste0(g_drive, "results/osm/figures/fisher_on-off_sqrt.png"), height = 4, width = 8, dpi = 500)

# Now the 2 buffer treatments:

buffer |>
  filter(common_name == "Fisher") |>
  ggplot(aes(x = fine_scale, y = mean_density)) +
  geom_point(aes(color = vegetation),
             size = 3,
             position = position_dodge(width = 0.5)) +
  geom_linerange(aes(ymin = lci_density, ymax = uci_density, color = vegetation),
                 linewidth = 0.5, position = position_dodge(width = 0.5)) +
  geom_point(data = (buffer_jem |> filter(common_name == "Fisher")),
             aes(x = fine_scale, y = mean_density, color = vegetation),
             size = 2,
             alpha = 0.2,
             position = position_jitterdodge(jitter.width = 0.1)) +
  scale_color_manual(values = veg.col) +
  scale_y_sqrt() +
  theme_classic() +
  labs(x = "Placement (metres away)",
       y = expression(Density~(individuals~per~km^2))) +
  coord_cartesian(ylim = c(0, 0.05)) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
        axis.title.x = element_text(size = 14, margin = margin(0.4, 0, 0, 0, unit = "cm")),
        axis.title.y = element_text(size = 12, margin = margin(0, 0.4, 0, 0, unit = "cm")),
        strip.text = element_text(size = 11),
        legend.position = "none",
        axis.ticks = element_blank(),
        panel.grid.major.y = element_line(linewidth = 0.5, color = "grey80")) +
  facet_grid(. ~ treatment, scales = "free_x", space = "free")

ggsave(paste0(g_drive, "results/osm/figures/fisher_buffer_sqrt.png"), height = 4, width = 8, dpi = 500)

#-----------------------------------------------------------------------------------------------------------------------

# Marten

on_off |>
  filter(common_name == "Marten") |>
  ggplot(aes(x = fine_scale, y = mean_density)) +
  geom_point(aes(color = vegetation),
             size = 3,
             position = position_dodge(width = 0.75)) +
  geom_linerange(aes(ymin = lci_density, ymax = uci_density, color = vegetation),
                 linewidth = 0.5, position = position_dodge(width = 0.75)) +
  geom_point(data = (on_off_jem |> filter(common_name == "Marten")),
             aes(x = fine_scale, y = mean_density, color = vegetation),
             size = 2,
             alpha = 0.15,
             position = position_jitterdodge(jitter.width = 0.2)) +
  scale_color_manual(values = veg.col) +
  scale_y_sqrt() +
  theme_classic() +
  labs(x = "Placement",
       y = expression(Density~(individuals~per~km^2))) +
  coord_cartesian(ylim = c(0, 0.2)) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
        axis.title.x = element_text(size = 14, margin = margin(0.4, 0, 0, 0, unit = "cm")),
        axis.title.y = element_text(size = 12, margin = margin(0, 0.4, 0, 0, unit = "cm")),
        strip.text = element_text(size = 11),
        legend.position = "none",
        axis.ticks = element_blank(),
        panel.grid.major.y = element_line(linewidth = 0.5, color = "grey80")) +
  facet_grid(. ~ treatment, scales = "free_x", space = "free")

ggsave(paste0(g_drive, "results/osm/figures/fisher_on-off_sqrt.png"), height = 4, width = 8, dpi = 500)

# Now the 2 buffer treatments:

buffer |>
  filter(common_name == "Fisher") |>
  ggplot(aes(x = fine_scale, y = mean_density)) +
  geom_point(aes(color = vegetation),
             size = 3,
             position = position_dodge(width = 0.5)) +
  geom_linerange(aes(ymin = lci_density, ymax = uci_density, color = vegetation),
                 linewidth = 0.5, position = position_dodge(width = 0.5)) +
  geom_point(data = (buffer_jem |> filter(common_name == "Fisher")),
             aes(x = fine_scale, y = mean_density, color = vegetation),
             size = 2,
             alpha = 0.2,
             position = position_jitterdodge(jitter.width = 0.1)) +
  scale_color_manual(values = veg.col) +
  scale_y_sqrt() +
  theme_classic() +
  labs(x = "Placement (metres away)",
       y = expression(Density~(individuals~per~km^2))) +
  coord_cartesian(ylim = c(0, 0.1)) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
        axis.title.x = element_text(size = 14, margin = margin(0.4, 0, 0, 0, unit = "cm")),
        axis.title.y = element_text(size = 12, margin = margin(0, 0.4, 0, 0, unit = "cm")),
        strip.text = element_text(size = 11),
        legend.position = "none",
        axis.ticks = element_blank(),
        panel.grid.major.y = element_line(linewidth = 0.5, color = "grey80")) +
  facet_grid(. ~ treatment, scales = "free_x", space = "free")

ggsave(paste0(g_drive, "results/osm/figures/fisher_buffer_sqrt.png"), height = 4, width = 8, dpi = 500)

#-----------------------------------------------------------------------------------------------------------------------

# Gray Wolf

on_off |>
  filter(common_name == "Gray Wolf") |>
  ggplot(aes(x = fine_scale, y = mean_density)) +
  geom_point(aes(color = vegetation),
             size = 3,
             position = position_dodge(width = 0.75)) +
  geom_linerange(aes(ymin = lci_density, ymax = uci_density, color = vegetation),
                 linewidth = 0.5, position = position_dodge(width = 0.75)) +
  geom_point(data = (on_off_jem |> filter(common_name == "GrayWolf")),
             aes(x = fine_scale, y = mean_density, color = vegetation),
             size = 2,
             alpha = 0.15,
             position = position_jitterdodge(jitter.width = 0.2)) +
  scale_color_manual(values = veg.col) +
  scale_y_sqrt() +
  theme_classic() +
  labs(x = "Placement",
       y = expression(Density~(individuals~per~km^2))) +
  coord_cartesian(ylim = c(0, 0.4)) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
        axis.title.x = element_text(size = 14, margin = margin(0.4, 0, 0, 0, unit = "cm")),
        axis.title.y = element_text(size = 12, margin = margin(0, 0.4, 0, 0, unit = "cm")),
        strip.text = element_text(size = 11),
        legend.position = "none",
        axis.ticks = element_blank(),
        panel.grid.major.y = element_line(linewidth = 0.5, color = "grey80")) +
  facet_grid(. ~ treatment, scales = "free_x", space = "free")

ggsave(paste0(g_drive, "results/osm/figures/wolf_on-off_sqrt.png"), height = 4, width = 8, dpi = 500)

# Now the 2 buffer treatments:

buffer |>
  filter(common_name == "Gray Wolf") |>
  ggplot(aes(x = fine_scale, y = mean_density)) +
  geom_point(aes(color = vegetation),
             size = 3,
             position = position_dodge(width = 0.5)) +
  geom_linerange(aes(ymin = lci_density, ymax = uci_density, color = vegetation),
                 linewidth = 0.5, position = position_dodge(width = 0.5)) +
  geom_point(data = (buffer_jem |> filter(common_name == "GrayWolf")),
             aes(x = fine_scale, y = mean_density, color = vegetation),
             size = 2,
             alpha = 0.2,
             position = position_jitterdodge(jitter.width = 0.1)) +
  scale_color_manual(values = veg.col) +
  scale_y_sqrt() +
  theme_classic() +
  labs(x = "Placement (metres away)",
       y = expression(Density~(individuals~per~km^2))) +
  coord_cartesian(ylim = c(0, 0.75)) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
        axis.title.x = element_text(size = 14, margin = margin(0.4, 0, 0, 0, unit = "cm")),
        axis.title.y = element_text(size = 12, margin = margin(0, 0.4, 0, 0, unit = "cm")),
        strip.text = element_text(size = 11),
        legend.position = "none",
        axis.ticks = element_blank(),
        panel.grid.major.y = element_line(linewidth = 0.5, color = "grey80")) +
  facet_grid(. ~ treatment, scales = "free_x", space = "free")

ggsave(paste0(g_drive, "results/osm/figures/wolf_buffer_sqrt.png"), height = 4, width = 8, dpi = 500)

#-----------------------------------------------------------------------------------------------------------------------

# Woodland Caribou

on_off |>
  filter(common_name == "Woodland Caribou") |>
  ggplot(aes(x = fine_scale, y = mean_density)) +
  geom_point(aes(color = vegetation),
             size = 3,
             position = position_dodge(width = 0.75)) +
  geom_linerange(aes(ymin = lci_density, ymax = uci_density, color = vegetation),
                 linewidth = 0.5, position = position_dodge(width = 0.75)) +
  geom_point(data = (on_off_jem |> filter(common_name == "WoodlandCaribou")),
             aes(x = fine_scale, y = mean_density, color = vegetation),
             size = 2,
             alpha = 0.15,
             position = position_jitterdodge(jitter.width = 0.2)) +
  scale_color_manual(values = veg.col) +
  scale_y_sqrt() +
  theme_classic() +
  labs(x = "Placement",
       y = expression(Density~(individuals~per~km^2))) +
  coord_cartesian(ylim = c(0, 2)) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
        axis.title.x = element_text(size = 14, margin = margin(0.4, 0, 0, 0, unit = "cm")),
        axis.title.y = element_text(size = 12, margin = margin(0, 0.4, 0, 0, unit = "cm")),
        strip.text = element_text(size = 11),
        legend.position = "none",
        axis.ticks = element_blank(),
        panel.grid.major.y = element_line(linewidth = 0.5, color = "grey80")) +
  facet_grid(. ~ treatment, scales = "free_x", space = "free")

ggsave(paste0(g_drive, "results/osm/figures/caribou_on-off_sqrt.png"), height = 4, width = 8, dpi = 500)

# Now the 2 buffer treatments:

buffer |>
  filter(common_name == "Woodland Caribou") |>
  ggplot(aes(x = fine_scale, y = mean_density)) +
  geom_point(aes(color = vegetation),
             size = 3,
             position = position_dodge(width = 0.5)) +
  geom_linerange(aes(ymin = lci_density, ymax = uci_density, color = vegetation),
                 linewidth = 0.5, position = position_dodge(width = 0.5)) +
  geom_point(data = (buffer_jem |> filter(common_name == "WoodlandCaribou")),
             aes(x = fine_scale, y = mean_density, color = vegetation),
             size = 2,
             alpha = 0.2,
             position = position_jitterdodge(jitter.width = 0.1)) +
  scale_color_manual(values = veg.col) +
  scale_y_sqrt() +
  theme_classic() +
  labs(x = "Placement (metres away)",
       y = expression(Density~(individuals~per~km^2))) +
  coord_cartesian(ylim = c(0, 0.75)) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
        axis.title.x = element_text(size = 14, margin = margin(0.4, 0, 0, 0, unit = "cm")),
        axis.title.y = element_text(size = 12, margin = margin(0, 0.4, 0, 0, unit = "cm")),
        strip.text = element_text(size = 11),
        legend.position = "none",
        axis.ticks = element_blank(),
        panel.grid.major.y = element_line(linewidth = 0.5, color = "grey80")) +
  facet_grid(. ~ treatment, scales = "free_x", space = "free")

ggsave(paste0(g_drive, "results/osm/figures/caribou_buffer_sqrt.png"), height = 4, width = 8, dpi = 500)

#-----------------------------------------------------------------------------------------------------------------------

# Snowshoe Hare

on_off |>
  filter(common_name == "Snowshoe Hare") |>
  ggplot(aes(x = fine_scale, y = mean_density)) +
  geom_point(aes(color = vegetation),
             size = 3,
             position = position_dodge(width = 0.75)) +
  geom_linerange(aes(ymin = lci_density, ymax = uci_density, color = vegetation),
                 linewidth = 0.5, position = position_dodge(width = 0.75)) +
  geom_point(data = (on_off_jem |> filter(common_name == "SnowshoeHare")),
             aes(x = fine_scale, y = mean_density, color = vegetation),
             size = 2,
             alpha = 0.15,
             position = position_jitterdodge(jitter.width = 0.2)) +
  scale_color_manual(values = veg.col) +
  scale_y_sqrt() +
  theme_classic() +
  labs(x = "Placement",
       y = expression(Density~(individuals~per~km^2))) +
  coord_cartesian(ylim = c(0, 2)) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
        axis.title.x = element_text(size = 14, margin = margin(0.4, 0, 0, 0, unit = "cm")),
        axis.title.y = element_text(size = 12, margin = margin(0, 0.4, 0, 0, unit = "cm")),
        strip.text = element_text(size = 11),
        legend.position = "none",
        axis.ticks = element_blank(),
        panel.grid.major.y = element_line(linewidth = 0.5, color = "grey80")) +
  facet_grid(. ~ treatment, scales = "free_x", space = "free")

ggsave(paste0(g_drive, "results/osm/figures/hare_on-off_sqrt.png"), height = 4, width = 8, dpi = 500)

# Now the 2 buffer treatments:

buffer |>
  filter(common_name == "Snowshoe Hare") |>
  ggplot(aes(x = fine_scale, y = mean_density)) +
  geom_point(aes(color = vegetation),
             size = 3,
             position = position_dodge(width = 0.5)) +
  geom_linerange(aes(ymin = lci_density, ymax = uci_density, color = vegetation),
                 linewidth = 0.5, position = position_dodge(width = 0.5)) +
  geom_point(data = (buffer_jem |> filter(common_name == "SnowshoeHare")),
             aes(x = fine_scale, y = mean_density, color = vegetation),
             size = 2,
             alpha = 0.2,
             position = position_jitterdodge(jitter.width = 0.1)) +
  scale_color_manual(values = veg.col) +
  scale_y_sqrt() +
  theme_classic() +
  labs(x = "Placement (metres away)",
       y = expression(Density~(individuals~per~km^2))) +
  coord_cartesian(ylim = c(0, 2)) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
        axis.title.x = element_text(size = 14, margin = margin(0.4, 0, 0, 0, unit = "cm")),
        axis.title.y = element_text(size = 12, margin = margin(0, 0.4, 0, 0, unit = "cm")),
        strip.text = element_text(size = 11),
        legend.position = "none",
        axis.ticks = element_blank(),
        panel.grid.major.y = element_line(linewidth = 0.5, color = "grey80")) +
  facet_grid(. ~ treatment, scales = "free_x", space = "free")

ggsave(paste0(g_drive, "results/osm/figures/hare_buffer_sqrt.png"), height = 4, width = 8, dpi = 500)



