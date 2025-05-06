#-----------------------------------------------------------------------------------------------------------------------

# Title:       Create figures
# Description: Create figures to visualize the results for reports and discussion.

# Author:      Marcus A Becker
# Date:        August 20, 2022

#-----------------------------------------------------------------------------------------------------------------------

# Attach packages
library(readr)
library(ggplot2)
#library(abmi.themes) # For ABMI-themed plots.
library(ggtext)
library(dplyr)
library(tidyr)
library(stringr)
library(forcats)

# Google Drive
g_drive <- "G:/Shared drives/ABMI Camera Mammals/projects/Camera-Heights/"
g_drive <- "G:/Shared drives/ABMI Camera Mammals/"

# Load previously processed data:

df_tifc <-       read_csv(paste0(g_drive, "data/processed/heights-experiment_tifc-bootstrap-comp.csv"))
df_boot <-       read_csv(paste0(g_drive, "data/processed/heights-experiment_tfic-bootstrap-raw-values.csv"))
df_detections <- df_detections
df_images <-     read_csv(paste0(g_drive, "Results/Comparisons/Heights/heights-experiment_summary-of-images-BDT.csv"))

# Add ABMI fonts (i.e., Montserrat from Google)
add_abmi_fonts()

sp_of_interest <- c("Black Bear", "White-tailed Deer", "Moose", "Coyote", "Cougar", "Marten",
                    "Snowshoe Hare", "Fisher", "Canada Lynx", "Gray Wolf", "Woodland Caribou")

#-----------------------------------------------------------------------------------------------------------------------

# Figure 1 - Comparison of independent detections

# Prepare data
df_detections_plot <- df_detections |>
  # Only BDT
  # filter(str_detect(location, "OG-ALPAC")) |>
  filter(species_common_name %in% sp_of_interest) |>
  mutate(captured = case_when(
    # Just switching around here
    is.na(start_full_height) ~ "High (1m) Only",
    is.na(start_half_height) ~ "Low (0.5m) Only",
    TRUE ~ "Both Cameras"
  )) |>
  mutate(captured = factor(captured, levels = c("High (1m) Only",
                                                "Both Cameras",
                                                "Low (0.5m) Only"))) |>
  select(species_common_name, captured) |>
  mutate(both = if_else(captured == "Low (0.5m) Only", 1, 0)) |>
  group_by(species_common_name) |>
  mutate(prop = round(sum(both) / n() * 100, digits = 2)) |>
  add_count() |>
  ungroup()

# Create Figure 1
plot <- df_detections_plot |>
  mutate(species_common_name = as.factor(paste0(species_common_name, " (n = ", n, ")"))) |>
  mutate(species_common_name = fct_reorder(species_common_name, prop)) |>
  # Plot
  ggplot(mapping = aes(fill = captured, x = species_common_name)) +
  geom_bar(position = "fill", width = 0.6, color = "grey50") +
  scale_fill_manual(values = c("#bf8374", "grey90", "#af9ad3"), guide = guide_legend(reverse = TRUE)) +
  #scale_fill_brewer(palette = "Dark2", direction = -1, guide = guide_legend(reverse = TRUE)) +
  scale_y_continuous(labels = scales::percent) +
  coord_flip() +
  labs(title = "Proportion of total detections captured by each camera",
       caption = "Where n is the number of detections of a species across the 68 sites") +
  theme_minimal() +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.spacing.x = unit(2, "pt"),
        #plot.subtitle = element_text(face = "italic", margin = margin(0, 0, 10, 0)),
        #legend.text = element_text(margin = margin(r = 1, unit = "cm")),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(size = 16, margin = margin(0, 0, 0.5, 1.75, unit = "cm")))

plot

# Save figure
ggsave(filename = paste0(g_drive, "Projects/Camera Heights/Figures/Proportion of Detections Captured.png"), height = 5, width = 9, bg = "white", dpi = 300)

# Table of detections for Appendix of report
det <- df_detections_plot |>
  group_by(species_species_common_name, captured) |>
  tally() |>
  pivot_wider(species_species_common_name, names_from = captured, values_from = n) |>
  mutate(across(everything(), ~ replace_na(., 0))) |>
  mutate(`Total Detections` = `High (1m) Only` + `Both Cameras` + `Low (0.5m) Only`) |>
  mutate(pct_high = round(`High (1m) Only` / `Total Detections` * 100, digits = 1),
         pct_low = round(`Low (0.5m) Only` / `Total Detections` * 100, digits = 1),
         pct_both = round(`Both Cameras` / `Total Detections` * 100, digits = 1)) |>
  mutate(`High (1m) Only` = paste0(`High (1m) Only`, " (", pct_high, "%)"),
         `Low (0.5m) Only` = paste0(`Low (0.5m) Only`, " (", pct_low, "%)"),
         `Both Cameras` = paste0(`Both Cameras`, " (", pct_both, "%)")) |>
  select(Species = species_species_common_name, 5, 2, 3, 4)

write_csv(det, paste0(g_drive, "Projects/Camera Heights/Tables/Summary of Detections (ALPAC BDT).csv"))

#-----------------------------------------------------------------------------------------------------------------------

# Figure 2 - Images per detection

df_images |>
  filter(!species_common_name == "Red Squirrel") |>
  #mutate(species_common_name = paste0(species_common_name, "\n(n detections = ", npairs, ")")) |>
  mutate(species_common_name = fct_reorder(species_common_name, factor_median, .desc = TRUE)) |>
  ggplot(aes(x = species_common_name,
             y = factor_median,
             ymin = factor_lci,
             ymax = factor_uci,
             color = factor_median)) +
  geom_hline(yintercept = 1, size = 0.5, linetype = 2) +
  geom_errorbar(width = 0.3, size = 1) +
  geom_point(size = 5) +
  geom_text(aes(x = species_common_name,
                y = factor_median,
                label = paste0(round(factor_median, digits = 1), "x")),
            size = 3,
            nudge_y = 0.3, nudge_x = 0.3,
            color = "grey20") +
  scale_color_viridis_c(direction = -1) +
  scale_y_continuous(breaks = seq(0, 5, 1), limits = c(0, 5), oob = scales::rescale_none) +
  coord_flip() +
  labs(y = "Factor",
       title = "Number of images per detection event",
       subtitle = "Value at the lower camera represented as a factor of the corresponding higher camera value.",
       caption = "90% confidence intervals in the factor value represented by error bars and obtained via bootstrapping. ALPAC BDT Camera Only.") +
  theme_minimal() +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_text(margin = margin(7, 0, 0, 0), size = 14),
        axis.text.y = element_text(size = 10),
        legend.position = "none",
        plot.subtitle = element_text(size = 10),
        plot.caption = element_text(size = 8))

# Save figure
ggsave(filename = paste0(g_drive, "Projects/Camera Heights/Figures/Number of Images per Detection (ALPAC BDT).png"), height = 5, width = 9, bg = "white", dpi = 300)

#-----------------------------------------------------------------------------------------------------------------------

# Figure 3 - Comparison of time in front of camera.

# Prepare TIFC data for plotting:
df_tifc_plot <- df_tifc %>%
  select(species_common_name = sp, npairs, 5:7) %>%
  filter(npairs > 3) %>% # Only cougar removed.
  # For labeling in plot
  mutate(n_pairs = paste0(species_common_name, "\n(n cam pairs = ", npairs, ")")) %>%
  # Species as an ordered factor
  mutate(n_pairs = fct_reorder(n_pairs, half_dur_as_pct_of_full_median, .desc = TRUE)) |>
  # Change Inf to 1000 for viz (will be truncated in the plot range anyway)
  mutate(half_dur_as_pct_of_full_uci = ifelse(is.infinite(half_dur_as_pct_of_full_uci), 1000, half_dur_as_pct_of_full_uci))

# Create Figure 3 (for report):
df_tifc_plot %>%
  #filter(str_detect(species_common_name, "White-tailed Deer")) |>
  ggplot(aes(x = species_common_name,
             y = half_dur_as_pct_of_full_median,
             ymin = half_dur_as_pct_of_full_lci,
             ymax = half_dur_as_pct_of_full_uci,
             color = half_dur_as_pct_of_full_median)) +
  geom_hline(yintercept = 100, size = 0.5, linetype = 2) +
  geom_errorbar(width = 0.3, size = 1) +
  geom_point(size = 4) +
  geom_text(aes(x = species_common_name,
                y = half_dur_as_pct_of_full_median,
                label = paste0(round(half_dur_as_pct_of_full_median, digits = 1), "%")),
            size = 3,
            nudge_y = 30, nudge_x = 0.3,
            color = "grey20") +
  #geom_dots(data = check) +
  scale_y_continuous(breaks = seq(0, 450, 100),
                     limits = c(0, 450),
                     oob = scales::rescale_none,
                     labels = scales::percent_format(scale = 1)) +
  scale_color_viridis_c(direction = -1) +
  coord_flip() +
  labs(y = "Percentage",
       title = "Time in camera field of view",
       subtitle = "Value at the lower camera expressed as a percentage (%) of the corresponding higher camera value.",
       caption = "90% confidence intervals in the percentage value represented by error bars and obtained via bootstrapping.") +
  #theme_abmi() +
  theme_minimal() +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_text(margin = margin(7, 0, 0, 0), size = 14),
        legend.position = "none",
        plot.caption = element_text(size = 8),
        plot.subtitle = element_text(size = 10),
        axis.text.y = element_text(size = 11))

# Save plot - something definitely a little wonky with ggsave. Also saved directly via RStudio.
ggsave(filename = "figures/figure3.png", dpi = 300, height = 5, width = 9, bg = "white")

# Let's try constructing a new plot for the boostraps

library(ggdist)
library(ggridges)

df_boot1 <- df_boot |>
  pivot_longer(everything(), names_to = "species_common_name", values_to = "rep") |>
  arrange(species_common_name) |>
  left_join(df_tifc_plot) |>
  mutate(n_pairs = fct_rev(n_pairs)) |>
  filter(!(species_common_name == "Cougar" | species_common_name == "Woodland Caribou")) |>
  filter(rep < 451, rep > 30)

limit <- max(abs(df_boot1$rep)) * c(-1, 1)

plot <- df_boot1 |>
  ggplot(aes(x = rep, y = n_pairs, fill = stat(x))) +
             #fill = factor(stat(quantile)))) +
             #fill = 0.5 - abs(0.5 - stat(ecdf))
             #)) +
  stat_density_ridges(scale = 2.5,
                      rel_min_height = 0.01,
                      quantile_lines = TRUE,
                      quantiles = c(0.05, 0.5, 0.95),
                      alpha = 0.7,
                      vline_color = c("black"),
                      vline_width = 0.2,
                      geom = "density_ridges_gradient",
                      calc_ecdf = TRUE) +
  geom_vline(xintercept = 100, size = 1, linetype = 2) +
  #scale_fill_viridis_c(direction = -1, option = "D",
  #                     rescaler = ~ scales::rescale_mid(.x, mid = 100)) +
                       #oob = scales::squish(rep, range = c(50, 350))) +
  scale_fill_gradientn(colours = c("darkred", "white", "darkblue"),
                       rescaler = ~ scales::rescale_mid(
                         .x, to = c(0, 1), mid = 100)) +
  scale_x_continuous(breaks = seq(0, 450, 100),
                     limits = c(0, 450),
                     oob = scales::rescale_none,
                     labels = scales::percent_format(scale = 1)) +
  geom_text(aes(y = n_pairs,
                x = half_dur_as_pct_of_full_median,
                label = paste0(round(half_dur_as_pct_of_full_median, digits = 0), "%")),
            size = 3,
            nudge_y = -0.15, nudge_x = 6,
            color = "grey20") +
  theme_ridges() +
  #theme_minimal() +
  labs(x = "",
       title = "Time in Field of View: Low Camera Value Expressed as % of High Camera",
       #subtitle = "Value at the lower camera expressed as a percentage (%) of the corresponding higher camera value",
       caption = "90% confidence intervals and median percentage value represented by\n black quantile lines (0.05, 0.5, 0.95) and obtained via bootstrapping.\nBased on data from 68 camera pairs.") +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_text(margin = margin(7, 0, 0, 0), size = 14),
        legend.position = "none",
        plot.caption = element_text(size = 10),
        plot.title = element_text(size = 13, margin = margin(0, 0, 1, 0, unit = "cm")),
        plot.subtitle = element_text(size = 10),
        axis.text.y = element_text(size = 12))

plot

ggsave(filename = "Camera Heights Results 2024-07-08.png", dpi = 300, height = 6.5, width = 8.5, bg = "white")

#-----------------------------------------------------------------------------------------------------------------------
