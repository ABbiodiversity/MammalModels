#-----------------------------------------------------------------------------------------------------------------------

# Project:          ABMI (OSM)

# Title:            Create series of plots for presentations
# Description:
# Author:           Marcus Becker

# Previous scripts: 01_estimate-treatment-effects

#-----------------------------------------------------------------------------------------------------------------------

# Attach packages
library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(ggtext)

# Set path to Shared Google Drive (G Drive)
g_drive <- "G:/Shared drives/ABMI Camera Mammals/"

# Import data
buffer <- read_csv(paste0(g_drive, "results/osm/2021_buffer_treatment_results.csv")) |>
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

on_off <- read_csv(paste0(g_drive, "results/osm/2021_on-off_treatment_results.csv")) |>
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
  )) |>
  filter(Habitat == "Deciduous Mixedwood")

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

theme_set(theme_minimal(base_family = ""))

# Change the settings so that geom_text uses the same font
update_geom_defaults("text", list(colour = "grey20", family = theme_get()$text$family))

# To make sure the arrows only appear in one facet
arrow <- tibble(Habitat = "Deciduous Mixedwood")

#-----------------------------------------------------------------------------------------------------------------------

# We'll use the example of Moose to walk through the results.

# Plot 1 - Blank - the 'canvas'

plot1 <- on_off |>
  filter(common_name == "Moose") |>
  # Remove values
  mutate(lci_density = NA,
         uci_density = NA,
         mean_density = NA) |>
  ggplot(aes(x = fine_scale, y = mean_density)) +
  geom_point(aes(color = treatment),
             size = 3.5,
             position = position_dodge(width = 0.75)) +
  geom_linerange(aes(ymin = lci_density, ymax = uci_density, color = treatment),
                 linewidth = 0.5, position = position_dodge(width = 0.75)) +
  scale_color_manual(values = c("Dark Green", "#FFC300", "#FF5733", "#C70039")) +
  scale_y_sqrt() +
  theme_minimal() +
  coord_cartesian(ylim = c(0, 3)) +
  labs(x = "Placement",
       y = expression(Density~(individuals~per~km^2))) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
        axis.title.x = element_text(size = 14, margin = margin(0.4, 0, 0, 0, unit = "cm")),
        axis.title.y = element_text(size = 10, margin = margin(0, 0.4, 0, 0, unit = "cm")),
        strip.text = element_text(size = 13),
        legend.position = "top",
        legend.title = element_blank(),
        legend.margin = margin(0, 0, 0, 0),
        legend.spacing.x = unit(0.05, "cm"),
        legend.text = element_text(size = 12),
        axis.ticks = element_blank(),
        panel.grid.major.y = element_line(linewidth = 0.5, color = "grey80"),
        panel.grid.major.x = element_blank())
  #facet_grid(. ~ Habitat, scales = "free_x", space = "free")

# Save
ggsave(paste0(g_drive, "results/osm/figures/Presentation/moose_01.png"), plot1,
       height = 5, width = 8, dpi = 500, bg = "white")

ggsave(paste0(g_drive, "results/osm/figures/Presentation/moose_01_v2.png"), plot1,
       height = 5, width = 7.5, dpi = 500, bg = "white")

#-----------------------------------------------------------------------------------------------------------------------

# Plot 2  - Add individual values from Reference JEMs

# Text for plot 2
text2 <- tibble(
  tibble(
    x_range = c(2.45),
    y_range = c(0.62),
    Habitat = "Deciduous Mixedwood",
    label = c("Each point represents an\naverage JEM value")
  )
)

# Create basis of plot 2 which will be repeated - important because we want consistent jittering.
plot2 <- plot1 +
  # Add JEM values for reference
  geom_point(data = (on_off_jem |>
                       filter(common_name == "Moose",
                              treatment == "Reference",
                              Habitat == "Deciduous Mixedwood")),
             aes(x = fine_scale, y = mean_density, color = treatment),
             size = 2,
             alpha = 0.1,
             position = position_jitterdodge(jitter.width = 0.3))

plot2 +
  # Add text and arrow
  geom_text(data = text2, aes(x_range, y_range, label = label),
            size = 4, colour = "Dark Green") +
  geom_curve(data = arrow, aes(x = 2.25, y = 0.36, xend = 1.2, yend = 0.1),
             colour = "Dark Green",
             linewidth = 0.4,
             curvature = -0.15,
             arrow = arrow(length = unit(2.5, "mm")))

# Save
ggsave(paste0(g_drive, "results/osm/figures/Presentation/moose_02_v2.png"),
       height = 5, width = 7.5, dpi = 500, bg = "white")

#-----------------------------------------------------------------------------------------------------------------------

# Plot 3 - Add mean reference JEM value

# Text for plot 3
text3 <- tibble(
  tibble(
    x_range = c(2.45),
    y_range = c(0.62),
    Habitat = "Deciduous Mixedwood",
    label = c("This is the mean density value\n(with a 90% confidence interval)")
  )
)

# Mean density data for reference
d3 <- on_off |>
  filter(common_name == "Moose",
         Habitat == "Deciduous Mixedwood") |>
  mutate(lci_density = ifelse(treatment == "Reference", lci_density, NA),
         uci_density = ifelse(treatment == "Reference", uci_density, NA),
         mean_density = ifelse(treatment == "Reference", mean_density, NA))

# Basis of plot 3
plot3 <- plot2 +
  # Add mean JEM values for both vegetation types, plus error bars
  geom_point(data = d3,
             aes(color = treatment),
             size = 3.5,
             position = position_dodge(width = 0.75)) +
  geom_linerange(data = d3,
                 aes(ymin = lci_density, ymax = uci_density, color = treatment),
                 linewidth = 0.5, position = position_dodge(width = 0.75))

plot3 +
  # Add text and arrow
  geom_text(data = text3, aes(x_range, y_range, label = label),
            size = 4, colour = "Dark Green") +
  geom_curve(data = arrow, aes(x = 2.25, y = 0.36, xend = 1.2, yend = 0.37),
             colour = "Dark Green",
             size = 0.4,
             curvature = -0.15,
             arrow = arrow(length = unit(2.5, "mm")))

ggsave(paste0(g_drive, "results/osm/figures/Presentation/moose_03_v2.png"),
       height = 5, width = 7.5, dpi = 500, bg = "white")

#-----------------------------------------------------------------------------------------------------------------------

# Plot 4 - Add dense linear features

# Text for plot 4
text4 <- tibble(
  tibble(
    x_range = c(2.45),
    y_range = c(2.5),
    Habitat = "Deciduous Mixedwood",
    label = c("Here are the JEM values for\nthe dense linear features stressor")
  )
)

plot4 <- plot3 +
  geom_point(data = (on_off_jem |>
                       filter(common_name == "Moose",
                              Habitat == "Deciduous Mixedwood") |>
                       mutate(mean_density = ifelse(treatment == "Dense Linear Features", mean_density, NA))),
             aes(x = fine_scale, y = mean_density, color = treatment),
             size = 2,
             # Slightly less transparency to help see the yellow points.
             alpha = 0.25,
             position = position_jitterdodge(jitter.width = 0.3))

# Add text and arrow
plot4_1 <- plot4 +
  geom_text(data = text4, aes(x_range, y_range, label = label),
            size = 4, colour = "#FFC300") +
  geom_curve(data = arrow, aes(x = 1.8, y = 2.05, xend = 1.6, yend = 0.9),
             colour = "#FFC300",
             size = 0.4,
             curvature = 0.35,
             arrow = arrow(length = unit(2.5, "mm")))

# Save
ggsave(paste0(g_drive, "results/osm/figures/Presentation/moose_04_v2.png"),
       plot4_1, height = 5, width = 7.5, dpi = 500, bg = "white")

#-----------------------------------------------------------------------------------------------------------------------

# Plot 5 - Add the distinction between 'Off' and 'On' footprint

# Text for plot 5
text5 <- tibble(
  tibble(
    x_range = c(2.9),
    y_range = c(0.7),
    Habitat = "Deciduous Mixedwood",
    label = c("Separated by\nplacement\n'Off'/'On' footprint")
  )
)

plot4_1 +
  # Add text and 2 arrows
  geom_text(data = text5, aes(x_range, y_range, label = label),
            size = 4, colour = "#FFC300") +
  geom_curve(data = arrow, aes(x = 2.6, y = 0.4, xend = 1.9, yend = 0.32),
             colour = "#FFC300",
             size = 0.4,
             curvature = -0.1,
             arrow = arrow(length = unit(2.5, "mm"))) +
  geom_curve(data = arrow, aes(x = 2.8, y = 0.4, xend = 2.7, yend = 0.08),
             colour = "#FFC300",
             size = 0.4,
             curvature = 0.1,
             arrow = arrow(length = unit(2.5, "mm")))

# Save
ggsave(paste0(g_drive, "results/osm/figures/Presentation/moose_05_v2.png"),
       height = 5, width = 7.5, dpi = 500, bg = "white")

#-----------------------------------------------------------------------------------------------------------------------

# Plot 6 - Mean values of each fine scale strata (dense linear features)

text6 <- tibble(
  tibble(
    x_range = c(2.1),
    y_range = c(1.3),
    Habitat = "Deciduous Mixedwood",
    label = c("Mean values\n(90% CI)")
  )
)

# Mean density data for dense linear features
d6 <- on_off |>
  filter(common_name == "Moose",
         Habitat == "Deciduous Mixedwood") |>
  mutate(lci_density = ifelse(treatment == "Dense Linear Features", lci_density, NA),
         uci_density = ifelse(treatment == "Dense Linear Features", uci_density, NA),
         mean_density = ifelse(treatment == "Dense Linear Features", mean_density, NA))

plot6 <- plot4 +
  # Add mean values for dense linear features treatment
  geom_point(data = d6,
             aes(color = treatment),
             size = 3.5,
             position = position_dodge(width = 0.75)) +
  geom_linerange(data = d6,
                 aes(ymin = lci_density, ymax = uci_density, color = treatment),
                 linewidth = 0.5, position = position_dodge(width = 0.75))

# Add text (no arrow this time)
plot6 +
  geom_text(data = text6, aes(x_range, y_range, label = label),
            size = 4, colour = "#FFC300")

# Save
ggsave(paste0(g_drive, "results/osm/figures/Presentation/moose_06_v2.png"),
       height = 5, width = 7.5, dpi = 500, bg = "white")

#-----------------------------------------------------------------------------------------------------------------------

# Plot 7 - Low Activity Well Pads

text7 <- tibble(
  tibble(
    x_range = c(1.9),
    y_range = c(2.35),
    Habitat = "Deciduous Mixedwood",
    label = c("JEM and mean values for\nlow activity well pad ...")
  )
)

# Mean density data for low activity well pads
d7 <- on_off |>
  filter(common_name == "Moose",
         Habitat == "Deciduous Mixedwood") |>
  mutate(lci_density = ifelse(treatment == "Low Activity Well Pads", lci_density, NA),
         uci_density = ifelse(treatment == "Low Activity Well Pads", uci_density, NA),
         mean_density = ifelse(treatment == "Low Activity Well Pads", mean_density, NA))

plot7 <- plot6 +
  geom_point(data = (on_off_jem |>
                       filter(common_name == "Moose",
                              Habitat == "Deciduous Mixedwood") |>
                       mutate(mean_density = ifelse(treatment == "Low Activity Well Pads", mean_density, NA))),
             aes(x = fine_scale, y = mean_density, color = treatment),
             size = 2,
             # Back to the usual transparency
             alpha = 0.15,
             position = position_jitterdodge(jitter.width = 0.3)) +
  # Add mean values for high activity in situ treatment
  geom_point(data = d7,
             aes(color = treatment),
             size = 3.5,
             position = position_dodge(width = 0.75)) +
  geom_linerange(data = d7,
                 aes(ymin = lci_density, ymax = uci_density, color = treatment),
                 linewidth = 0.5, position = position_dodge(width = 0.75))

plot7 +
  geom_text(data = text7, aes(x_range, y_range, label = label),
            size = 4, colour = "#FF5733") +
  geom_curve(data = arrow, aes(x = 1.8, y = 1.9, xend = 1.9, yend = 1.1),
             colour = "#FF5733",
             size = 0.4,
             curvature = 0.12,
             arrow = arrow(length = unit(2.5, "mm")))

# Save
ggsave(paste0(g_drive, "results/osm/figures/Presentation/moose_07_v2.png"),
       height = 5, width = 7.5, dpi = 500, bg = "white")

#-----------------------------------------------------------------------------------------------------------------------

# Plot 8 - High Activity In Situ

# Text for plot 8
text8 <- tibble(
  tibble(
    x_range = c(1.9),
    y_range = c(2.35),
    Habitat = "Deciduous Mixedwood",
    label = c("... and high activity\nin situ treatments")
  )
)

# Mean density data for high activity in situ
d8 <- on_off |>
  filter(common_name == "Moose",
         Habitat == "Deciduous Mixedwood") |>
  mutate(lci_density = ifelse(treatment == "High Activity In Situ", lci_density, NA),
         uci_density = ifelse(treatment == "High Activity In Situ", uci_density, NA),
         mean_density = ifelse(treatment == "High Activity In Situ", mean_density, NA))

plot8 <- plot7 +
  geom_point(data = (on_off_jem |>
                       filter(common_name == "Moose",
                              Habitat == "Deciduous Mixedwood") |>
                       mutate(mean_density = ifelse(treatment == "High Activity In Situ", mean_density, NA))),
             aes(x = fine_scale, y = mean_density, color = treatment),
             size = 2,
             # Back to the usual transparency
             alpha = 0.15,
             position = position_jitterdodge(jitter.width = 0.3)) +
  # Add mean values for high activity in situ treatment
  geom_point(data = d8,
             aes(color = treatment),
             size = 3.5,
             position = position_dodge(width = 0.75)) +
  geom_linerange(data = d8,
                 aes(ymin = lci_density, ymax = uci_density, color = treatment),
                 linewidth = 0.5, position = position_dodge(width = 0.75))

plot8 +
  geom_text(data = text8, aes(x_range, y_range, label = label),
            size = 4, colour = "#C70039") +
  geom_curve(data = arrow, aes(x = 2.2, y = 1.85, xend = 2.3, yend = 0.25),
             colour = "#C70039",
             size = 0.4,
             curvature = -0.15,
             arrow = arrow(length = unit(2.5, "mm")))

# Save
ggsave(paste0(g_drive, "results/osm/figures/Presentation/moose_08_v2.png"),
       height = 5, width = 7.5, dpi = 500, bg = "white")

# Version without error bar on 0 values
d8_1 <- on_off |>
  filter(common_name == "Moose") |>
  mutate(lci_density = ifelse(mean_density > 0, lci_density, 0),
         uci_density = ifelse(mean_density > 0, uci_density, 0)) |>
  mutate(lci_density = ifelse(treatment == "High Activity In Situ", lci_density, NA),
         uci_density = ifelse(treatment == "High Activity In Situ", uci_density, NA),
         mean_density = ifelse(treatment == "High Activity In Situ", mean_density, NA))

# Alternative
plot8_1 <- plot7 +
  geom_point(data = (on_off_jem |>
                       filter(common_name == "Moose") |>
                       mutate(mean_density = ifelse(treatment == "High Activity In Situ", mean_density, NA))),
             aes(x = fine_scale, y = mean_density, color = treatment),
             size = 2,
             # Back to the usual transparency
             alpha = 0.15,
             position = position_jitterdodge(jitter.width = 0.3)) +
  # Add mean values for high activity in situ treatment
  geom_point(data = d8_1,
             aes(color = treatment),
             size = 3.5,
             position = position_dodge(width = 0.75)) +
  geom_linerange(data = d8_1,
                 aes(ymin = lci_density, ymax = uci_density, color = treatment),
                 linewidth = 0.5, position = position_dodge(width = 0.75))

# Alternative
plot8_1 +
  geom_text(data = text8, aes(x_range, y_range, label = label),
            size = 4, colour = "#C70039") +
  geom_curve(data = arrow, aes(x = 2.2, y = 1.85, xend = 2.3, yend = 0.25),
             colour = "#C70039",
             size = 0.4,
             curvature = -0.15,
             arrow = arrow(length = unit(2.5, "mm")))

# Save alternative
ggsave(paste0(g_drive, "results/osm/figures/Presentation/moose_08-1.png"),
       height = 5, width = 8, dpi = 500, bg = "white")

#-----------------------------------------------------------------------------------------------------------------------

# Plot 9 - the final product.

# (Which was actually plot 8 minus the text and curve.)
ggsave(paste0(g_drive, "results/osm/figures/Presentation/moose_09_v2.png"),
       plot8, height = 5, width = 7.5, dpi = 500, bg = "white")

# Now save the alternative
ggsave(paste0(g_drive, "results/osm/figures/Presentation/moose_09-1.png"),
       plot8_1, height = 5, width = 8, dpi = 500, bg = "white")

#-----------------------------------------------------------------------------------------------------------------------

# Other species





