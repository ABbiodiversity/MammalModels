#-----------------------------------------------------------------------------------------------------------------------

# Project:          ABMI (OSM)

# Title:            Create figures for OSM BADR results
# Description:      Uses both OSM BADR camera data from 2021 as well as relevant EH data
# Author:           Marcus Becker

# Previous scripts: 01_estimate-treatment-effects

#-----------------------------------------------------------------------------------------------------------------------

# Attach packages
library(tidyverse)
library(ggplot2)
library(grid)
library(gridExtra)
#library(abmi.themes)

# Set path to Shared Google Drive (G Drive)
g_drive <- "G:/Shared drives/ABMI Camera Mammals/"

# Import data
buffer <- read_csv(paste0(g_drive, "Results/OSM BADR/2021-2022_osm_buffer_treatment_results_new.csv")) |>
  mutate(fine_scale = str_extract(treatment, "\\d+"),
         treatment = str_remove(treatment, "\\d+"),
         treatment = str_to_title(str_remove(treatment, " buffer $")),
         fine_scale = paste0(treatment, " ", fine_scale),
         fine_scale = factor(fine_scale, levels = c("Road 10", "Road 30", "Road 100", "Road 300",
                                                    "Plant/Mine 10", "Plant/Mine 30", "Plant/Mine 100", "Plant/Mine 300"))) |>
  mutate(Habitat = case_when(
    vegetation == "decidmix40" ~ "Deciduous Mixedwood",
    vegetation == "treedlow20" ~ "Treed Lowland"
  ))

on_off <- read_csv(paste0(g_drive, "Results/OSM BADR/2021-2022_osm-on-off_treatment_results_new.csv")) |>
  mutate(fine_scale = case_when(
    str_detect(treatment, "On") ~ "On",
    str_detect(treatment, "Off") ~ "Off",
    TRUE ~ "")) |>
  mutate(treatment = str_to_title(str_remove(treatment, " Off$| On$"))) |>
  mutate(treatment = factor(treatment,
                            levels = c("Reference", "Dense Linear Features", "Low Activity Well Pads",  "High Activity In Situ"))) |>
  mutate(Habitat = case_when(
    vegetation == "decidmix40" ~ "Deciduous Mixedwood",
    vegetation == "treedlow20" ~ "Treed Lowland"
  ))

on_off_2021 <- read_csv(paste0(g_drive, "Results/OSM BADR/2021_on-off_treatment_results.csv")) |>
  mutate(fine_scale = case_when(
    str_detect(treatment, "On") ~ "On",
    str_detect(treatment, "Off") ~ "Off",
    TRUE ~ "")) |>
  mutate(treatment = str_to_title(str_remove(treatment, " Off$| On$"))) |>
  mutate(treatment = factor(treatment,
                            levels = c("Reference", "Dense Linear Features", "Low Activity Well Pads",  "High Activity In Situ"))) |>
  mutate(Habitat = case_when(
    vegetation == "decidmix40" ~ "Deciduous Mixedwood",
    vegetation == "treedlow20" ~ "Treed Lowland"
  ))

# JEM means

jem_means <- read_csv(paste0(g_drive, "data/processed/osm/2021-2022_osm_mean_jem_density_values_new.csv")) |>
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
                            levels = c("Reference", "Dense Linear Features", "Low Activity Well Pads", "High Activity In Situ")))

buffer_jem <- jem_means |>
  filter(!type == "HF",
         !treatment == "pre-insitu") |>
  select(Habitat, treatment, fine_scale, common_name, mean_density = density_adj) |>
  mutate(fine_scale = replace_na(fine_scale, ""),
         treatment = str_to_title(str_remove(treatment, " buffer$")),
         fine_scale = paste0(treatment, " ", fine_scale),
         fine_scale = factor(fine_scale, levels = c("Road 10", "Road 30", "Road 100", "Road 300",
                                                   "Plant/Mine 10", "Plant/Mine 30", "Plant/Mine 100", "Plant/Mine 300")))

# Smoothed Results

on_off_smooth <- read_csv(paste0(g_drive, "Results/OSM BADR/OSM mammals 2021 2022 Smoothed On Off HF.csv")) |>
  select(common_name = Sp, treatment = Treat, mean_density = Mean, lci_density = q5, uci_density = q95) |>
  mutate(fine_scale = case_when(
    str_detect(treatment, "On") ~ "On",
    str_detect(treatment, "Off") ~ "Off",
    TRUE ~ "")) |>
  mutate(treatment = str_to_title(str_remove(treatment, " Off$| On$"))) |>
  mutate(treatment = factor(treatment,
                            levels = c("Reference", "Dense Linear Features", "Low Activity Well Pads",  "High Activity In Situ")))


#-----------------------------------------------------------------------------------------------------------------------

# Make plots

# White-tailed Deer

plot_on_off <- on_off |>
  filter(common_name == "White-tailed Deer") |>
         #Habitat == "Deciduous Mixedwood") |>
  ggplot(aes(x = fine_scale, y = mean_density)) +
  geom_point(aes(color = treatment),
             size = 3.5,
             position = position_dodge(width = 0.75)) +
  geom_linerange(aes(ymin = lci_density, ymax = uci_density, color = treatment),
                  linewidth = 0.5, position = position_dodge(width = 0.75)) +
  geom_point(data = (on_off_jem |>
                       filter(common_name == "White.tailed.Deer")),
                              #Habitat == "Deciduous Mixedwood")),
             aes(x = fine_scale, y = mean_density, color = treatment),
             size = 2,
             alpha = 0.15,
             position = position_jitterdodge(jitter.width = 0.2)) +
  #geom_point(data = (on_off_2021 |>
  #                     filter(common_name == "White-tailed Deer")),
  #           aes(color = treatment),
  #           #color = "blue",
  #           size = 4,
  #           alpha = 0.4,
  #           shape = 17,
  #           position = position_dodge(width = 0.75)) +
  scale_color_manual(values = c("Dark Green", "#FFC300", "#FF5733", "#C70039")) +
  #scale_color_brewer(palette = "Set1") +
  scale_y_sqrt() +
  theme_minimal() +
  coord_cartesian(ylim = c(0, 20)) +
  labs(x = "Placement",
       y = expression(Density~(individuals~per~km^2))) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
        axis.title.x = element_text(size = 14, margin = margin(0.4, 0, 0, 0, unit = "cm")),
        #axis.title.x = element_blank(),
        axis.title.y = element_text(size = 10, margin = margin(0, 0.4, 0, 0, unit = "cm")),
        strip.text = element_text(size = 13),
        legend.position = "top",
        legend.title = element_blank(),
        legend.margin = margin(0, 0, 0, 0),
        legend.spacing.x = unit(0.05, "cm"),
        legend.text = element_text(size = 12),
        axis.ticks = element_blank(),
        panel.grid.major.y = element_line(linewidth = 0.5, color = "grey80"),
        panel.grid.major.x = element_blank()) +
  facet_grid(. ~ Habitat, scales = "free_x", space = "free")

plot_on_off

ggsave(paste0(g_drive, "results/osm/figures/2022/Presentation/wtd_on-ff_sqrt.png"),
       plot_on_off, height = 5, width = 8, dpi = 500, bg = "white")

# Now the 2 buffer treatments:

plot_buffer <- buffer |>
  filter(common_name == "White-tailed Deer") |>
  mutate(lci_density = ifelse(mean_density > 0, lci_density, 0),
         uci_density = ifelse(mean_density > 0, uci_density, 0)) |>
  #mutate(fine_scale = paste0(treatment, " ", fine_scale)) |>
  ggplot(aes(x = fine_scale, y = mean_density)) +
  geom_point(aes(color = treatment, group = treatment),
             size = 3.5,
             position = position_dodge(width = 0.2)) +
  geom_linerange(aes(ymin = lci_density, ymax = uci_density, color = treatment, group = treatment),
                 linewidth = 0.5, position = position_dodge(width = 0.2)) +
  geom_point(data = (buffer_jem |> filter(common_name == "WhitetailedDeer")),
             aes(x = fine_scale, y = mean_density, color = treatment, group = treatment),
             size = 2,
             alpha = 0.2,
             position = position_jitterdodge(jitter.width = 0.1)) +
  scale_color_manual(values = c("#80461B", "#CD7F32")) +
  scale_y_sqrt(breaks = c(0, 2, 4, 6, 8)) +
  scale_x_discrete(labels = c("10m", "30m", "100m", "300m", "10m", "30m", "100m", "300m")) +
  coord_cartesian(ylim = c(0, 8)) +
  theme_minimal() +
  labs(x = "Placement",
       y = expression(Density~(individuals~per~km^2))) +
  guides(color = guide_legend(reverse=TRUE)) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 8),
        axis.title.x = element_text(size = 15, margin = margin(0.6, 0, 0, 0, unit = "cm")),
        axis.title.y = element_text(size = 10, margin = margin(0, 0.4, 0, 0, unit = "cm")),
        strip.text = element_text(size = 13),
        legend.position = "top",
        legend.title = element_blank(),
        legend.margin = margin(0, 0, 0, 0),
        legend.spacing.x = unit(0.05, "cm"),
        legend.text = element_text(size = 12),
        axis.ticks = element_blank(),
        panel.grid.major.y = element_line(linewidth = 0.5, color = "grey80"),
        panel.grid.major.x = element_blank()) +
  facet_grid(. ~ Habitat, scales = "free_x", space = "free")

ggsave(paste0(g_drive, "results/osm/figures/wtd_buffer_sqrt_new.png"), plot_buffer,
       height = 5, width = 8, dpi = 500, bg = "white")

# Join together
full_plot <- grid.arrange(plot_on_off, plot_buffer)

ggsave(paste0(g_drive, "results/osm/figures/wtd_full_sqrt.png"), full_plot, height = 8, width = 8, dpi = 500, bg = "white")


ref <- on_off_smooth |>
  filter(treatment == "Reference") |>
  select(-fine_scale) |>
  mutate(distance = 51.5)

pm_dist_smooth <- read_csv(paste0(g_drive, "Results/OSM BADR/OSM mammals 2021 2022 Smoothed Plant mine distance.csv")) |>
  mutate(treatment = "Plant/Mine") |>
  select(common_name = Sp, treatment, distance = Dist, mean_density = Mean, lci_density = q5, uci_density = q95)

dist_smooth <- read_csv(paste0(g_drive, "Results/OSM BADR/OSM mammals 2021 2022 Smoothed Road distance.csv")) |>
  mutate(treatment = "Roads") |>
  select(common_name = Sp, treatment, distance = Dist, mean_density = Mean, lci_density = q5, uci_density = q95) |>
  bind_rows(pm_dist_smooth) |>
  mutate(distance = case_when(
    distance == "0-20m" ~ 300,
    distance == "20-50m" ~ 270,
    distance == "50-200m" ~ 220,
    distance == ">200m" ~ 130
  ))

# Plots of Smoothed Results

plot_on_off_smooth <- on_off_smooth |>
  filter(common_name == "White-tailed Deer") |>
  ggplot(aes(x = fine_scale, y = mean_density)) +
  geom_point(aes(color = treatment),
             size = 3.5,
             position = position_dodge(width = 0.6)) +
  geom_linerange(aes(ymin = lci_density, ymax = uci_density, color = treatment),
                 linewidth = 0.5, position = position_dodge(width = 0.6)) +
  scale_color_manual(values = c("Dark Green", "#FFC300", "#FF5733", "#C70039")) +
  #scale_color_brewer(palette = "Set1") +
  theme_minimal() +
  #scale_y_sqrt() +
  coord_cartesian(ylim = c(0, 2)) +
  labs(x = "",
       y = expression(Relative~Density~(Individuals~per~km^2))) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 12),
        #axis.title.x = element_text(size = 13, margin = margin(0.4, 0, 0, 0, unit = "cm"), hjust = 0.72),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 10, margin = margin(0, 0.4, 0, 0, unit = "cm")),
        #strip.text = element_text(size = 13),
        legend.position = "top",
        legend.title = element_blank(),
        legend.margin = margin(0, 0, 0, 0),
        legend.spacing.x = unit(0.05, "cm"),
        legend.text = element_text(size = 12),
        axis.ticks = element_blank(),
        panel.grid.major.y = element_line(linewidth = 0.5, color = "grey80"),
        panel.grid.major.x = element_blank())

plot_on_off_smooth

ggsave(paste0(g_drive, "Results/OSM BADR/Figures/2022/White-tailed Deer On Off Smoothed Sqrt.png"),
       plot_on_off_smooth, height = 5, width = 8, dpi = 500, bg = "white")

# Smoothed Dist Treatment

plot_dist_smooth <- dist_smooth |>
  filter(common_name == "White-tailed Deer") |>
  ggplot(aes(x = distance, y = mean_density)) +
  geom_point(aes(color = treatment),
             size = 3.5) +
  geom_linerange(aes(ymin = lci_density, ymax = uci_density, color = treatment),
                 linewidth = 0.5) +
  scale_color_manual(values = c("#80461B", "#CD7F32")) +
  scale_x_continuous(breaks = c(130, 220, 270, 300),
                     labels = c(">200m", "50-200m", "30m", "10m")) +
  geom_point(data = ref |> filter(common_name == "White-tailed Deer"),
             aes(x = distance, y = mean_density),
             color = "darkgreen",
             size = 3.5) +
  geom_linerange(data = ref |> filter(common_name == "White-tailed Deer"),
                 aes(ymin = lci_density, ymax = uci_density),
                 color = "darkgreen",
                 linewidth = 0.5) +
  coord_cartesian(ylim = c(0, 4),
                  xlim = c(0, 310)) +
  theme_minimal() +
  labs(x = "",
       y = expression(Relative~Density~(Individuals~per~km^2))) +
  guides(color = guide_legend(reverse=TRUE)) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 12),
        #axis.title.x = element_text(size = 15, margin = margin(0.6, 0, 0, 0, unit = "cm"), hjust = 0.72),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 10, margin = margin(0, 0.4, 0, 0, unit = "cm")),
        strip.text = element_text(size = 13),
        legend.position = "top",
        legend.title = element_blank(),
        legend.margin = margin(0, 0, 0, 0),
        legend.spacing.x = unit(0.05, "cm"),
        legend.text = element_text(size = 12),
        axis.ticks = element_blank(),
        panel.grid.major.y = element_line(linewidth = 0.5, color = "grey80"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

plot_dist_smooth

# Join together
full_plot <- grid.arrange(plot_on_off_smooth, plot_dist_smooth)

ggsave(paste0(g_drive, "Results/OSM BADR/Figures/2022/WTD Full Smooth.png"), full_plot, height = 8, width = 8, dpi = 500, bg = "white")

#-----------------------------------------------------------------------------------------------------------------------

# Moose

plot_on_off <- on_off |>
  filter(common_name == "Moose") |>
  mutate(lci_density = ifelse(mean_density > 0, lci_density, 0),
         uci_density = ifelse(mean_density > 0, uci_density, 0)) |>
  ggplot(aes(x = fine_scale, y = mean_density)) +
  geom_point(aes(color = treatment),
             size = 3.5,
             position = position_dodge(width = 0.75)) +
  geom_linerange(aes(ymin = lci_density, ymax = uci_density, color = treatment),
                 linewidth = 0.5, position = position_dodge(width = 0.75)) +
  geom_point(data = (on_off_jem |> filter(common_name == "Moose")),
             aes(x = fine_scale, y = mean_density, color = treatment),
             size = 2,
             alpha = 0.15,
             position = position_jitterdodge(jitter.width = 0.2)) +
  scale_color_manual(values = c("Dark Green", "#FFC300", "#FF5733", "#C70039")) +
  #scale_color_brewer(palette = "Set1") +
  scale_y_sqrt() +
  theme_minimal() +
  coord_cartesian(ylim = c(0, 3)) +
  labs(x = "Placement",
       y = expression(Density~(individuals~per~km^2))) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
        #axis.title.x = element_text(size = 14, margin = margin(0.4, 0, 0, 0, unit = "cm")),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 10, margin = margin(0, 0.4, 0, 0, unit = "cm")),
        strip.text = element_text(size = 13),
        legend.position = "top",
        legend.title = element_blank(),
        legend.margin = margin(0, 0, 0, 0),
        legend.spacing.x = unit(0.05, "cm"),
        legend.text = element_text(size = 12),
        axis.ticks = element_blank(),
        panel.grid.major.y = element_line(linewidth = 0.5, color = "grey80"),
        panel.grid.major.x = element_blank()) +
  facet_grid(. ~ Habitat, scales = "free_x", space = "free")

# Now the 2 buffer treatments:

plot_buffer <- buffer |>
  filter(common_name == "Moose") |>
  #mutate(lci_density = ifelse(mean_density > 0, lci_density, 0),
  #       uci_density = ifelse(mean_density > 0, uci_density, 0)) |>
  #mutate(fine_scale = paste0(treatment, " ", fine_scale)) |>
  ggplot(aes(x = fine_scale, y = mean_density)) +
  geom_point(aes(color = treatment, group = treatment),
             size = 3.5,
             position = position_dodge(width = 0.2)) +
  geom_linerange(aes(ymin = lci_density, ymax = uci_density, color = treatment, group = treatment),
                 linewidth = 0.5, position = position_dodge(width = 0.2)) +
  geom_point(data = (buffer_jem |> filter(common_name == "Moose")),
             aes(x = fine_scale, y = mean_density, color = treatment, group = treatment),
             size = 2,
             alpha = 0.2,
             position = position_jitterdodge(jitter.width = 0.1)) +
  scale_color_manual(values = c("#80461B", "#CD7F32")) +
  scale_y_sqrt() +
  scale_x_discrete(labels = c("10m", "30m", "100m", "300m", "10m", "30m", "100m", "300m")) +
  coord_cartesian(ylim = c(0, 3)) +
  theme_minimal() +
  labs(x = "Placement",
       y = expression(Density~(individuals~per~km^2))) +
  guides(color = guide_legend(reverse=TRUE)) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 8),
        axis.title.x = element_text(size = 15, margin = margin(0.6, 0, 0, 0, unit = "cm")),
        axis.title.y = element_text(size = 10, margin = margin(0, 0.4, 0, 0, unit = "cm")),
        strip.text = element_text(size = 13),
        legend.position = "top",
        legend.title = element_blank(),
        legend.margin = margin(0, 0, 0, 0),
        legend.spacing.x = unit(0.05, "cm"),
        legend.text = element_text(size = 12),
        axis.ticks = element_blank(),
        panel.grid.major.y = element_line(linewidth = 0.5, color = "grey80"),
        panel.grid.major.x = element_blank()) +
  facet_grid(. ~ Habitat, scales = "free_x", space = "free")

ggsave(paste0(g_drive, "results/osm/figures/moose_buffer_sqrt_new.png"), plot_buffer,
       height = 5, width = 8, dpi = 500, bg = "white")

# Join together
full_plot <- grid.arrange(plot_on_off, plot_buffer)

ggsave(paste0(g_drive, "results/osm/figures/moose_full_sqrt.png"), full_plot, height = 8, width = 8, dpi = 500, bg = "white")

#-----------------------------------------------------------------------------------------------------------------------

# Canada Lynx

plot_on_off <- on_off |>
  filter(common_name == "Canada Lynx") |>
         #Habitat == "Treed Lowland") |>
  mutate(lci_density = ifelse(mean_density > 0, lci_density, 0),
         uci_density = ifelse(mean_density > 0, uci_density, 0)) |>
  filter(common_name == "Canada Lynx") |>
  #mutate(lci_density = ifelse(mean_density > 0, lci_density, 0),
  #       uci_density = ifelse(mean_density > 0, uci_density, 0)) |>
  ggplot(aes(x = fine_scale, y = mean_density)) +
  geom_point(aes(color = treatment),
             size = 3.5,
             position = position_dodge(width = 0.75)) +
  geom_linerange(aes(ymin = lci_density, ymax = uci_density, color = treatment),
                 linewidth = 0.5, position = position_dodge(width = 0.75)) +
  geom_point(data = (on_off_jem |>
                       filter(common_name == "Canada.Lynx")),
                              #Habitat == "Treed Lowland")),
             aes(x = fine_scale, y = mean_density, color = treatment),
             size = 2,
             alpha = 0.15,
             position = position_jitterdodge(jitter.width = 0.2)) +
  scale_color_manual(values = c("Dark Green", "#FFC300", "#FF5733", "#C70039")) +
  #scale_color_brewer(palette = "Set1") +
  scale_y_sqrt() +
  theme_minimal() +
  coord_cartesian(ylim = c(0, 0.3)) +
  labs(x = "Placement",
       y = expression(Density~(individuals~per~km^2))) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
        axis.title.x = element_text(size = 14, margin = margin(0.4, 0, 0, 0, unit = "cm")),
        #axis.title.x = element_blank(),
        axis.title.y = element_text(size = 10, margin = margin(0, 0.4, 0, 0, unit = "cm")),
        strip.text = element_text(size = 13),
        legend.position = "top",
        legend.title = element_blank(),
        legend.margin = margin(0, 0, 0, 0),
        legend.spacing.x = unit(0.05, "cm"),
        legend.text = element_text(size = 12),
        axis.ticks = element_blank(),
        panel.grid.major.y = element_line(linewidth = 0.5, color = "grey80"),
        panel.grid.major.x = element_blank()) +
  facet_grid(. ~ Habitat, scales = "free_x", space = "free")

ggsave(paste0(g_drive, "results/osm/figures/Presentation/lynx_tl_02.png"),
       plot_on_off, height = 5, width = 7.5, dpi = 500, bg = "white")

ggsave(paste0(g_drive, "results/osm/figures/lynx_on_off_sqrt_new.png"), plot_on_off,
       height = 5, width = 8, dpi = 500, bg = "white")

# Now the 2 buffer treatments:

plot_buffer <- buffer |>
  filter(common_name == "Canada Lynx") |>
  #mutate(lci_density = ifelse(mean_density > 0, lci_density, 0),
  #       uci_density = ifelse(mean_density > 0, uci_density, 0)) |>
  #mutate(fine_scale = paste0(treatment, " ", fine_scale)) |>
  ggplot(aes(x = fine_scale, y = mean_density)) +
  geom_point(aes(color = treatment, group = treatment),
             size = 3.5,
             position = position_dodge(width = 0.2)) +
  geom_linerange(aes(ymin = lci_density, ymax = uci_density, color = treatment, group = treatment),
                 linewidth = 0.5, position = position_dodge(width = 0.2)) +
  geom_point(data = (buffer_jem |> filter(common_name == "CanadaLynx")),
             aes(x = fine_scale, y = mean_density, color = treatment, group = treatment),
             size = 2,
             alpha = 0.2,
             position = position_jitterdodge(jitter.width = 0.1)) +
  scale_color_manual(values = c("#80461B", "#CD7F32")) +
  scale_y_sqrt() +
  scale_x_discrete(labels = c("10m", "30m", "100m", "300m", "10m", "30m", "100m", "300m")) +
  coord_cartesian(ylim = c(0, 0.4)) +
  theme_minimal() +
  labs(x = "Placement",
       y = expression(Density~(individuals~per~km^2))) +
  guides(color = guide_legend(reverse=TRUE)) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 8),
        axis.title.x = element_text(size = 15, margin = margin(0.6, 0, 0, 0, unit = "cm")),
        axis.title.y = element_text(size = 10, margin = margin(0, 0.4, 0, 0, unit = "cm")),
        strip.text = element_text(size = 13),
        legend.position = "top",
        legend.title = element_blank(),
        legend.margin = margin(0, 0, 0, 0),
        legend.spacing.x = unit(0.05, "cm"),
        legend.text = element_text(size = 12),
        axis.ticks = element_blank(),
        panel.grid.major.y = element_line(linewidth = 0.5, color = "grey80"),
        panel.grid.major.x = element_blank()) +
  facet_grid(. ~ Habitat, scales = "free_x", space = "free")

ggsave(paste0(g_drive, "results/osm/figures/lynx_buffer_sqrt_new.png"), plot_buffer,
       height = 5, width = 8, dpi = 500, bg = "white")

# Join together
full_plot <- grid.arrange(plot_on_off, plot_buffer)

ggsave(paste0(g_drive, "results/osm/figures/lynx_full_sqrt.png"), full_plot, height = 8, width = 8, dpi = 500, bg = "white")

#-----------------------------------------------------------------------------------------------------------------------

# Black Bear

plot_on_off <- on_off |>
  filter(common_name == "Black Bear") |>
  #mutate(lci_density = ifelse(mean_density > 0, lci_density, 0),
  #       uci_density = ifelse(mean_density > 0, uci_density, 0)) |>
  ggplot(aes(x = fine_scale, y = mean_density)) +
  geom_point(aes(color = treatment),
             size = 3.5,
             position = position_dodge(width = 0.75)) +
  geom_linerange(aes(ymin = lci_density, ymax = uci_density, color = treatment),
                 linewidth = 0.5, position = position_dodge(width = 0.75)) +
  geom_point(data = (on_off_jem |> filter(common_name == "BlackBear")),
             aes(x = fine_scale, y = mean_density, color = treatment),
             size = 2,
             alpha = 0.15,
             position = position_jitterdodge(jitter.width = 0.2)) +
  scale_color_manual(values = c("Dark Green", "#FFC300", "#FF5733", "#C70039")) +
  #scale_color_brewer(palette = "Set1") +
  scale_y_sqrt() +
  theme_minimal() +
  coord_cartesian(ylim = c(0, 2)) +
  labs(x = "Placement",
       y = expression(Density~(individuals~per~km^2))) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
        axis.title.x = element_text(size = 14, margin = margin(0.4, 0, 0, 0, unit = "cm")),
        #axis.title.x = element_blank(),
        axis.title.y = element_text(size = 10, margin = margin(0, 0.4, 0, 0, unit = "cm")),
        strip.text = element_text(size = 13),
        legend.position = "top",
        legend.title = element_blank(),
        legend.margin = margin(0, 0, 0, 0),
        legend.spacing.x = unit(0.05, "cm"),
        legend.text = element_text(size = 12),
        axis.ticks = element_blank(),
        panel.grid.major.y = element_line(linewidth = 0.5, color = "grey80"),
        panel.grid.major.x = element_blank()) +
  facet_grid(. ~ Habitat, scales = "free_x", space = "free")

ggsave(paste0(g_drive, "results/osm/figures/blackbear_on_off_sqrt_new.png"), plot_on_off,
       height = 5, width = 8, dpi = 500, bg = "white")

# Now the 2 buffer treatments:

plot_buffer <- buffer |>
  filter(common_name == "Black Bear") |>
  #mutate(lci_density = ifelse(mean_density > 0, lci_density, 0),
  #       uci_density = ifelse(mean_density > 0, uci_density, 0)) |>
  #mutate(fine_scale = paste0(treatment, " ", fine_scale)) |>
  ggplot(aes(x = fine_scale, y = mean_density)) +
  geom_point(aes(color = treatment, group = treatment),
             size = 3.5,
             position = position_dodge(width = 0.2)) +
  geom_linerange(aes(ymin = lci_density, ymax = uci_density, color = treatment, group = treatment),
                 linewidth = 0.5, position = position_dodge(width = 0.2)) +
  geom_point(data = (buffer_jem |> filter(common_name == "BlackBear")),
             aes(x = fine_scale, y = mean_density, color = treatment, group = treatment),
             size = 2,
             alpha = 0.2,
             position = position_jitterdodge(jitter.width = 0.1)) +
  scale_color_manual(values = c("#80461B", "#CD7F32")) +
  scale_y_sqrt() +
  scale_x_discrete(labels = c("10m", "30m", "100m", "300m", "10m", "30m", "100m", "300m")) +
  coord_cartesian(ylim = c(0, 2)) +
  theme_minimal() +
  labs(x = "Placement",
       y = expression(Density~(individuals~per~km^2))) +
  guides(color = guide_legend(reverse=TRUE)) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 8),
        axis.title.x = element_text(size = 15, margin = margin(0.6, 0, 0, 0, unit = "cm")),
        axis.title.y = element_text(size = 10, margin = margin(0, 0.4, 0, 0, unit = "cm")),
        strip.text = element_text(size = 13),
        legend.position = "top",
        legend.title = element_blank(),
        legend.margin = margin(0, 0, 0, 0),
        legend.spacing.x = unit(0.05, "cm"),
        legend.text = element_text(size = 12),
        axis.ticks = element_blank(),
        panel.grid.major.y = element_line(linewidth = 0.5, color = "grey80"),
        panel.grid.major.x = element_blank()) +
  facet_grid(. ~ Habitat, scales = "free_x", space = "free")

ggsave(paste0(g_drive, "results/osm/figures/blackbear_buffer_sqrt_new.png"), plot_buffer,
       height = 5, width = 8, dpi = 500, bg = "white")

# Join together
full_plot <- grid.arrange(plot_on_off, plot_buffer)

ggsave(paste0(g_drive, "results/osm/figures/blackbear_full_sqrt.png"), full_plot, height = 8, width = 8, dpi = 500, bg = "white")

#-----------------------------------------------------------------------------------------------------------------------

# Coyote

plot_on_off <- on_off |>
  filter(common_name == "Coyote") |>
  mutate(lci_density = ifelse(mean_density > 0, lci_density, 0),
         uci_density = ifelse(mean_density > 0, uci_density, 0)) |>
  ggplot(aes(x = fine_scale, y = mean_density)) +
  geom_point(aes(color = treatment),
             size = 3.5,
             position = position_dodge(width = 0.75)) +
  geom_linerange(aes(ymin = lci_density, ymax = uci_density, color = treatment),
                 linewidth = 0.5, position = position_dodge(width = 0.75)) +
  geom_point(data = (on_off_jem |> filter(common_name == "Coyote")),
             aes(x = fine_scale, y = mean_density, color = treatment),
             size = 2,
             alpha = 0.15,
             position = position_jitterdodge(jitter.width = 0.2)) +
  scale_color_manual(values = c("Dark Green", "#FFC300", "#FF5733", "#C70039")) +
  #scale_color_brewer(palette = "Set1") +
  scale_y_sqrt() +
  theme_minimal() +
  coord_cartesian(ylim = c(0, 0.5)) +
  labs(x = "Placement",
       y = expression(Density~(individuals~per~km^2))) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
        #axis.title.x = element_text(size = 14, margin = margin(0.4, 0, 0, 0, unit = "cm")),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 10, margin = margin(0, 0.4, 0, 0, unit = "cm")),
        strip.text = element_text(size = 13),
        legend.position = "top",
        legend.title = element_blank(),
        legend.margin = margin(0, 0, 0, 0),
        legend.spacing.x = unit(0.05, "cm"),
        legend.text = element_text(size = 12),
        axis.ticks = element_blank(),
        panel.grid.major.y = element_line(linewidth = 0.5, color = "grey80"),
        panel.grid.major.x = element_blank()) +
  facet_grid(. ~ Habitat, scales = "free_x", space = "free")

# Now the 2 buffer treatments:

plot_buffer <- buffer |>
  filter(common_name == "Coyote") |>
  mutate(lci_density = ifelse(mean_density > 0, lci_density, 0),
         uci_density = ifelse(mean_density > 0, uci_density, 0)) |>
  #mutate(fine_scale = paste0(treatment, " ", fine_scale)) |>
  ggplot(aes(x = fine_scale, y = mean_density)) +
  geom_point(aes(color = treatment, group = treatment),
             size = 3.5,
             position = position_dodge(width = 0.2)) +
  geom_linerange(aes(ymin = lci_density, ymax = uci_density, color = treatment, group = treatment),
                 linewidth = 0.5, position = position_dodge(width = 0.2)) +
  geom_point(data = (buffer_jem |> filter(common_name == "Coyote")),
             aes(x = fine_scale, y = mean_density, color = treatment, group = treatment),
             size = 2,
             alpha = 0.2,
             position = position_jitterdodge(jitter.width = 0.1)) +
  scale_color_manual(values = c("#80461B", "#CD7F32")) +
  scale_y_sqrt() +
  scale_x_discrete(labels = c("10m", "30m", "100m", "300m", "10m", "30m", "100m", "300m")) +
  coord_cartesian(ylim = c(0, 0.5)) +
  theme_minimal() +
  labs(x = "Placement",
       y = expression(Density~(individuals~per~km^2))) +
  guides(color = guide_legend(reverse=TRUE)) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 8),
        axis.title.x = element_text(size = 15, margin = margin(0.6, 0, 0, 0, unit = "cm")),
        axis.title.y = element_text(size = 10, margin = margin(0, 0.4, 0, 0, unit = "cm")),
        strip.text = element_blank(),
        legend.position = "top",
        legend.title = element_blank(),
        legend.margin = margin(0, 0, 0, 0),
        legend.spacing.x = unit(0.05, "cm"),
        legend.text = element_text(size = 12),
        axis.ticks = element_blank(),
        panel.grid.major.y = element_line(linewidth = 0.5, color = "grey80"),
        panel.grid.major.x = element_blank()) +
  facet_grid(. ~ Habitat, scales = "free_x", space = "free")

# Join together
full_plot <- grid.arrange(plot_on_off, plot_buffer)

ggsave(paste0(g_drive, "results/osm/figures/coyote_full_sqrt.png"), full_plot, height = 8, width = 8, dpi = 500, bg = "white")


#-----------------------------------------------------------------------------------------------------------------------

# Fisher

on_off |>
  filter(common_name == "Fisher") |>
  mutate(lci_density = ifelse(mean_density > 0, lci_density, 0),
         uci_density = ifelse(mean_density > 0, uci_density, 0)) |>
  ggplot(aes(x = fine_scale, y = mean_density)) +
  geom_point(aes(color = Habitat),
             size = 3,
             position = position_dodge(width = 0.75)) +
  geom_linerange(aes(ymin = lci_density, ymax = uci_density, color = Habitat),
                 linewidth = 0.5, position = position_dodge(width = 0.75)) +
  geom_point(data = (on_off_jem |> filter(common_name == "Fisher")),
             aes(x = fine_scale, y = mean_density, color = Habitat),
             size = 2,
             alpha = 0.15,
             position = position_jitterdodge(jitter.width = 0.2)) +
  scale_color_manual(values = veg.col) +
  scale_y_sqrt() +
  theme_minimal() +
  labs(x = "Placement",
       y = expression(Density~(individuals~per~km^2))) +
  coord_cartesian(ylim = c(0, 0.05)) +
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

ggsave(paste0(g_drive, "results/osm/figures/fisher_on-off_sqrt.png"), height = 3.65, width = 8, dpi = 500)

# Now the 2 buffer treatments:

buffer |>
  filter(common_name == "Fisher") |>
  mutate(lci_density = ifelse(mean_density > 0, lci_density, 0),
         uci_density = ifelse(mean_density > 0, uci_density, 0)) |>
  ggplot(aes(x = fine_scale, y = mean_density)) +
  geom_point(aes(color = Habitat),
             size = 3,
             position = position_dodge(width = 0.5)) +
  geom_linerange(aes(ymin = lci_density, ymax = uci_density, color = Habitat),
                 linewidth = 0.5, position = position_dodge(width = 0.5)) +
  geom_point(data = (buffer_jem |> filter(common_name == "Fisher")),
             aes(x = fine_scale, y = mean_density, color = Habitat),
             size = 2,
             alpha = 0.2,
             position = position_jitterdodge(jitter.width = 0.1)) +
  scale_color_manual(values = veg.col) +
  scale_y_sqrt() +
  theme_minimal() +
  labs(x = "Placement (metres away)",
       y = expression(Density~(individuals~per~km^2))) +
  coord_cartesian(ylim = c(0, 0.05)) +
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

ggsave(paste0(g_drive, "results/osm/figures/fisher_buffer_sqrt.png"), height = 3.65, width = 8, dpi = 500)

#-----------------------------------------------------------------------------------------------------------------------

# Marten

on_off |>
  filter(common_name == "Marten") |>
  mutate(lci_density = ifelse(mean_density > 0, lci_density, 0),
         uci_density = ifelse(mean_density > 0, uci_density, 0)) |>
  ggplot(aes(x = fine_scale, y = mean_density)) +
  geom_point(aes(color = Habitat),
             size = 3,
             position = position_dodge(width = 0.75)) +
  geom_linerange(aes(ymin = lci_density, ymax = uci_density, color = Habitat),
                 linewidth = 0.5, position = position_dodge(width = 0.75)) +
  geom_point(data = (on_off_jem |> filter(common_name == "Marten")),
             aes(x = fine_scale, y = mean_density, color = Habitat),
             size = 2,
             alpha = 0.15,
             position = position_jitterdodge(jitter.width = 0.2)) +
  scale_color_manual(values = veg.col) +
  scale_y_sqrt() +
  theme_minimal() +
  labs(x = "Placement",
       y = expression(Density~(individuals~per~km^2))) +
  coord_cartesian(ylim = c(0, 0.15)) +
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

ggsave(paste0(g_drive, "results/osm/figures/marten_on-off_sqrt.png"), height = 4, width = 8, dpi = 500)

# Now the 2 buffer treatments:

buffer |>
  filter(common_name == "Marten") |>
  mutate(lci_density = ifelse(mean_density > 0, lci_density, 0),
         uci_density = ifelse(mean_density > 0, uci_density, 0)) |>
  ggplot(aes(x = fine_scale, y = mean_density)) +
  geom_point(aes(color = Habitat),
             size = 3,
             position = position_dodge(width = 0.5)) +
  geom_linerange(aes(ymin = lci_density, ymax = uci_density, color = Habitat),
                 linewidth = 0.5, position = position_dodge(width = 0.5)) +
  geom_point(data = (buffer_jem |> filter(common_name == "Marten")),
             aes(x = fine_scale, y = mean_density, color = Habitat),
             size = 2,
             alpha = 0.2,
             position = position_jitterdodge(jitter.width = 0.1)) +
  scale_color_manual(values = veg.col) +
  scale_y_sqrt() +
  theme_minimal() +
  labs(x = "Placement (metres away)",
       y = expression(Density~(individuals~per~km^2))) +
  coord_cartesian(ylim = c(0, 0.15)) +
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

ggsave(paste0(g_drive, "results/osm/figures/marten_buffer_sqrt.png"), height = 3.65, width = 8, dpi = 500)

#-----------------------------------------------------------------------------------------------------------------------

# Gray Wolf

plot_on_off <- on_off |>
  filter(common_name == "Gray Wolf") |>
  mutate(lci_density = ifelse(mean_density > 0, lci_density, 0),
         uci_density = ifelse(mean_density > 0, uci_density, 0)) |>
  ggplot(aes(x = fine_scale, y = mean_density)) +
  geom_point(aes(color = treatment),
             size = 3.5,
             position = position_dodge(width = 0.75)) +
  geom_linerange(aes(ymin = lci_density, ymax = uci_density, color = treatment),
                 linewidth = 0.5, position = position_dodge(width = 0.75)) +
  geom_point(data = (on_off_jem |> filter(common_name == "GrayWolf")),
             aes(x = fine_scale, y = mean_density, color = treatment),
             size = 2,
             alpha = 0.15,
             position = position_jitterdodge(jitter.width = 0.2)) +
  scale_color_manual(values = c("Dark Green", "#FFC300", "#FF5733", "#C70039")) +
  #scale_color_brewer(palette = "Set1") +
  scale_y_sqrt() +
  theme_minimal() +
  coord_cartesian(ylim = c(0, 0.75)) +
  labs(x = "Placement",
       y = expression(Density~(individuals~per~km^2))) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
        #axis.title.x = element_text(size = 14, margin = margin(0.4, 0, 0, 0, unit = "cm")),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 10, margin = margin(0, 0.4, 0, 0, unit = "cm")),
        strip.text = element_text(size = 13),
        legend.position = "top",
        legend.title = element_blank(),
        legend.margin = margin(0, 0, 0, 0),
        legend.spacing.x = unit(0.05, "cm"),
        legend.text = element_text(size = 12),
        axis.ticks = element_blank(),
        panel.grid.major.y = element_line(linewidth = 0.5, color = "grey80"),
        panel.grid.major.x = element_blank()) +
  facet_grid(. ~ Habitat, scales = "free_x", space = "free")

# Now the 2 buffer treatments:

plot_buffer <- buffer |>
  filter(common_name == "Gray Wolf") |>
  mutate(lci_density = ifelse(mean_density > 0, lci_density, 0),
         uci_density = ifelse(mean_density > 0, uci_density, 0)) |>
  #mutate(fine_scale = paste0(treatment, " ", fine_scale)) |>
  ggplot(aes(x = fine_scale, y = mean_density)) +
  geom_point(aes(color = treatment, group = treatment),
             size = 3.5,
             position = position_dodge(width = 0.2)) +
  geom_linerange(aes(ymin = lci_density, ymax = uci_density, color = treatment, group = treatment),
                 linewidth = 0.5, position = position_dodge(width = 0.2)) +
  geom_point(data = (buffer_jem |> filter(common_name == "GrayWolf")),
             aes(x = fine_scale, y = mean_density, color = treatment, group = treatment),
             size = 2,
             alpha = 0.2,
             position = position_jitterdodge(jitter.width = 0.1)) +
  scale_color_manual(values = c("#80461B", "#CD7F32")) +
  scale_y_sqrt() +
  scale_x_discrete(labels = c("10m", "30m", "100m", "300m", "10m", "30m", "100m", "300m")) +
  coord_cartesian(ylim = c(0, 0.75)) +
  theme_minimal() +
  labs(x = "Placement",
       y = expression(Density~(individuals~per~km^2))) +
  guides(color = guide_legend(reverse=TRUE)) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 8),
        axis.title.x = element_text(size = 15, margin = margin(0.6, 0, 0, 0, unit = "cm")),
        axis.title.y = element_text(size = 10, margin = margin(0, 0.4, 0, 0, unit = "cm")),
        strip.text = element_blank(),
        legend.position = "top",
        legend.title = element_blank(),
        legend.margin = margin(0, 0, 0, 0),
        legend.spacing.x = unit(0.05, "cm"),
        legend.text = element_text(size = 12),
        axis.ticks = element_blank(),
        panel.grid.major.y = element_line(linewidth = 0.5, color = "grey80"),
        panel.grid.major.x = element_blank()) +
  facet_grid(. ~ Habitat, scales = "free_x", space = "free")

# Join together
full_plot <- grid.arrange(plot_on_off, plot_buffer)

ggsave(paste0(g_drive, "results/osm/figures/graywolf_full_sqrt.png"), full_plot, height = 8, width = 8, dpi = 500, bg = "white")

#-----------------------------------------------------------------------------------------------------------------------

# Woodland Caribou

on_off |>
  filter(common_name == "Woodland Caribou") |>
  mutate(lci_density = ifelse(mean_density > 0, lci_density, 0),
         uci_density = ifelse(mean_density > 0, uci_density, 0)) |>
  ggplot(aes(x = fine_scale, y = mean_density)) +
  geom_point(aes(color = Habitat),
             size = 3,
             position = position_dodge(width = 0.75)) +
  geom_linerange(aes(ymin = lci_density, ymax = uci_density, color = Habitat),
                 linewidth = 0.5, position = position_dodge(width = 0.75)) +
  geom_point(data = (on_off_jem |> filter(common_name == "WoodlandCaribou")),
             aes(x = fine_scale, y = mean_density, color = Habitat),
             size = 2,
             alpha = 0.15,
             position = position_jitterdodge(jitter.width = 0.2)) +
  scale_color_manual(values = veg.col) +
  scale_y_sqrt() +
  theme_minimal() +
  labs(x = "Placement",
       y = expression(Density~(individuals~per~km^2))) +
  coord_cartesian(ylim = c(0, 1)) +
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

ggsave(paste0(g_drive, "results/osm/figures/caribou_on-off_sqrt.png"), height = 4, width = 8, dpi = 500)

# Now the 2 buffer treatments:

buffer |>
  filter(common_name == "Woodland Caribou") |>
  mutate(lci_density = ifelse(mean_density > 0, lci_density, 0),
         uci_density = ifelse(mean_density > 0, uci_density, 0)) |>
  ggplot(aes(x = fine_scale, y = mean_density)) +
  geom_point(aes(color = Habitat),
             size = 3,
             position = position_dodge(width = 0.5)) +
  geom_linerange(aes(ymin = lci_density, ymax = uci_density, color = Habitat),
                 linewidth = 0.5, position = position_dodge(width = 0.5)) +
  geom_point(data = (buffer_jem |> filter(common_name == "WoodlandCaribou")),
             aes(x = fine_scale, y = mean_density, color = Habitat),
             size = 2,
             alpha = 0.2,
             position = position_jitterdodge(jitter.width = 0.1)) +
  scale_color_manual(values = veg.col) +
  scale_y_sqrt() +
  theme_minimal() +
  labs(x = "Placement (metres away)",
       y = expression(Density~(individuals~per~km^2))) +
  coord_cartesian(ylim = c(0, 1)) +
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

ggsave(paste0(g_drive, "results/osm/figures/caribou_buffer_sqrt.png"), height = 3.65, width = 8, dpi = 500)

#-----------------------------------------------------------------------------------------------------------------------

# Snowshoe Hare

plot_on_off <- on_off |>
  filter(common_name == "Snowshoe Hare") |>
  mutate(lci_density = ifelse(mean_density > 0, lci_density, 0),
         uci_density = ifelse(mean_density > 0, uci_density, 0)) |>
  ggplot(aes(x = fine_scale, y = mean_density)) +
  geom_point(aes(color = treatment),
             size = 3.5,
             position = position_dodge(width = 0.75)) +
  geom_linerange(aes(ymin = lci_density, ymax = uci_density, color = treatment),
                 linewidth = 0.5, position = position_dodge(width = 0.75)) +
  geom_point(data = (on_off_jem |> filter(common_name == "SnowshoeHare")),
             aes(x = fine_scale, y = mean_density, color = treatment),
             size = 2,
             alpha = 0.15,
             position = position_jitterdodge(jitter.width = 0.2)) +
  scale_color_manual(values = c("Dark Green", "#FFC300", "#FF5733", "#C70039")) +
  #scale_color_brewer(palette = "Set1") +
  scale_y_sqrt() +
  theme_minimal() +
  coord_cartesian(ylim = c(0, 2)) +
  labs(x = "Placement",
       y = expression(Density~(individuals~per~km^2))) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
        #axis.title.x = element_text(size = 14, margin = margin(0.4, 0, 0, 0, unit = "cm")),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 10, margin = margin(0, 0.4, 0, 0, unit = "cm")),
        strip.text = element_text(size = 13),
        legend.position = "top",
        legend.title = element_blank(),
        legend.margin = margin(0, 0, 0, 0),
        legend.spacing.x = unit(0.05, "cm"),
        legend.text = element_text(size = 12),
        axis.ticks = element_blank(),
        panel.grid.major.y = element_line(linewidth = 0.5, color = "grey80"),
        panel.grid.major.x = element_blank()) +
  facet_grid(. ~ Habitat, scales = "free_x", space = "free")

# Now the 2 buffer treatments:

plot_buffer <- buffer |>
  filter(common_name == "Snowshoe Hare") |>
  mutate(lci_density = ifelse(mean_density > 0, lci_density, 0),
         uci_density = ifelse(mean_density > 0, uci_density, 0)) |>
  #mutate(fine_scale = paste0(treatment, " ", fine_scale)) |>
  ggplot(aes(x = fine_scale, y = mean_density)) +
  geom_point(aes(color = treatment, group = treatment),
             size = 3.5,
             position = position_dodge(width = 0.2)) +
  geom_linerange(aes(ymin = lci_density, ymax = uci_density, color = treatment, group = treatment),
                 linewidth = 0.5, position = position_dodge(width = 0.2)) +
  geom_point(data = (buffer_jem |> filter(common_name == "SnowshoeHare")),
             aes(x = fine_scale, y = mean_density, color = treatment, group = treatment),
             size = 2,
             alpha = 0.2,
             position = position_jitterdodge(jitter.width = 0.1)) +
  scale_color_manual(values = c("#80461B", "#CD7F32")) +
  scale_y_sqrt() +
  scale_x_discrete(labels = c("10m", "30m", "100m", "300m", "10m", "30m", "100m", "300m")) +
  coord_cartesian(ylim = c(0, 2)) +
  theme_minimal() +
  labs(x = "Placement",
       y = expression(Density~(individuals~per~km^2))) +
  guides(color = guide_legend(reverse=TRUE)) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 8),
        axis.title.x = element_text(size = 15, margin = margin(0.6, 0, 0, 0, unit = "cm")),
        axis.title.y = element_text(size = 10, margin = margin(0, 0.4, 0, 0, unit = "cm")),
        strip.text = element_blank(),
        legend.position = "top",
        legend.title = element_blank(),
        legend.margin = margin(0, 0, 0, 0),
        legend.spacing.x = unit(0.05, "cm"),
        legend.text = element_text(size = 12),
        axis.ticks = element_blank(),
        panel.grid.major.y = element_line(linewidth = 0.5, color = "grey80"),
        panel.grid.major.x = element_blank()) +
  facet_grid(. ~ Habitat, scales = "free_x", space = "free")

# Join together
full_plot <- grid.arrange(plot_on_off, plot_buffer)

ggsave(paste0(g_drive, "results/osm/figures/hare_full_sqrt.png"), full_plot, height = 8, width = 8, dpi = 500, bg = "white")



