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

df_tifc <-       read_csv(paste0(g_drive, "Data/Model Comparisons/Model Effects and Bootstrap By Species.csv"))
df_boot <-       read_csv(paste0(g_drive, "Data/Model Comparisons/Bootstrap Raw Values.csv"))


sp_of_interest <- c("Black Bear", "White-tailed Deer", "Moose", "Coyote", "Cougar", "Marten",
                    "Snowshoe Hare", "Fisher", "Canada Lynx", "Gray Wolf", "Woodland Caribou")

# Let's try constructing a new plot for the boostraps

library(ggdist)
library(ggridges)

# Prepare TIFC data for plotting:
df_tifc_plot <- df_tifc |>
  select(species_common_name = sp_season, npairs, 10:12) |>
  filter(npairs > 8) %>%
  # For labeling in plot
  mutate(n_pairs = paste0(species_common_name, "\n(n cam pairs = ", npairs, ")")) %>%
  # Species as an ordered factor
  mutate(n_pairs = fct_reorder(n_pairs, hf2_dur_as_pct_of_pc900_median, .desc = TRUE)) |>
  # Take out Elk
  filter(!species_common_name == "Elk (wapiti)")

sp <- unique(df_tifc_plot$species_common_name)

df_boot1 <- df_boot |>
  pivot_longer(everything(), names_to = "species_common_name", values_to = "rep") |>
  arrange(species_common_name) |>
  filter(species_common_name %in% sp) |>
  left_join(df_tifc_plot) |>
  mutate(n_pairs = fct_rev(n_pairs)) |>
  #filter(!(species_common_name == "Cougar" | species_common_name == "Woodland Caribou")) |>
  filter(rep < 451, rep > 30)

plot <- df_boot1 |>
  ggplot(aes(x = rep, y = n_pairs, fill = stat(x))) +
  #fill = factor(stat(quantile)))) +
  #fill = 0.5 - abs(0.5 - stat(ecdf))
  #)) +
  stat_density_ridges(scale = 1.25,
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
  scale_x_continuous(breaks = seq(50, 200, 100),
                     limits = c(50, 200),
                     oob = scales::rescale_none,
                     labels = scales::percent_format(scale = 1)) +
  geom_text(aes(y = n_pairs,
                x = hf2_dur_as_pct_of_pc900_median,
                label = paste0(round(hf2_dur_as_pct_of_pc900_median, digits = 0), "%")),
            size = 3,
            nudge_y = -0.15, nudge_x = 6,
            color = "grey20") +
  theme_ridges() +
  #theme_minimal() +
  labs(x = "",
       title = "Time in Field of View: HF2 Camera Value Expressed as % of PC900 Camera",
       #subtitle = "Value at the lower camera expressed as a percentage (%) of the corresponding higher camera value",
       caption = "90% confidence intervals and median percentage value represented by\n black quantile lines (0.05, 0.5, 0.95) and obtained via bootstrapping.\nBased on data from 45 camera pairs.") +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_text(margin = margin(7, 0, 0, 0), size = 14),
        legend.position = "none",
        plot.caption = element_text(size = 10),
        plot.title = element_text(size = 13, margin = margin(0, 0, 1, 0, unit = "cm")),
        plot.subtitle = element_text(size = 10),
        axis.text.y = element_text(size = 12))

plot

ggsave(filename = "Camera Models Results 2024-09-12.png", dpi = 300, height = 6.5, width = 8.5, bg = "white")


#-----------------------------------------------------------------------------------------------------------------------
