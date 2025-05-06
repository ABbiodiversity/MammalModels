#-----------------------------------------------------------------------------------------------------------------------

# Project:          Game Trail Versus Random

# Title:            Create Figures
# Description:      The purpose of this script is to summarise the data from the game trail and random paired camera

# Author:           Marcus Becker

# Previous scripts: None

#-----------------------------------------------------------------------------------------------------------------------

# Let's try constructing a new plot for the boostraps

library(ggdist)
library(ggridges)

df_boot <- df
df_tifc <- table

# Prepare TIFC data for plotting:
df_tifc_plot <- df_tifc %>%
  select(species_common_name = sp, npairs, 5:7) %>%
  filter(npairs > 3) %>% # Only cougar removed.
  # For labeling in plot
  mutate(n_pairs = paste0(species_common_name, "\n(n cam pairs = ", npairs, ")")) %>%
  # Species as an ordered factor
  mutate(n_pairs = fct_reorder(n_pairs, R_dur_as_pct_of_GT_median, .desc = TRUE)) |>
  # Change Inf to 1000 for viz (will be truncated in the plot range anyway)
  mutate(R_dur_as_pct_of_GT_uci = ifelse(is.infinite(R_dur_as_pct_of_GT_uci), 1000, R_dur_as_pct_of_GT_uci))

df_boot1 <- df_boot |>
  pivot_longer(everything(), names_to = "species_common_name", values_to = "rep") |>
  arrange(species_common_name) |>
  left_join(df_tifc_plot) |>
  mutate(n_pairs = fct_rev(n_pairs)) |>
  filter(rep < 600, rep > 20)

plot <- df_boot1 |>
  ggplot(aes(x = rep, y = n_pairs, fill = stat(x))) +
  #fill = factor(stat(quantile)))) +
  #fill = 0.5 - abs(0.5 - stat(ecdf))
  #)) +
  stat_density_ridges(scale = 1.65,
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
  scale_x_continuous(breaks = seq(0, 500, 100),
                     limits = c(0, 500),
                     oob = scales::rescale_none,
                     labels = scales::percent_format(scale = 1)) +
  geom_text(aes(y = n_pairs,
                x = R_dur_as_pct_of_GT_median,
                label = paste0(round(R_dur_as_pct_of_GT_median, digits = 0), "%")),
            size = 3,
            nudge_y = -0.15, nudge_x = 6,
            color = "grey20") +
  theme_ridges() +
  #theme_minimal() +
  labs(x = "",
       title = "Time in Field of View: Random Camera Value Expressed as % of Game Trail Camera",
       #subtitle = "Value at the lower camera expressed as a percentage (%) of the corresponding higher camera value",
       caption = "90% confidence intervals and median percentage value represented by\n black quantile lines (0.05, 0.5, 0.95) and obtained via bootstrapping.\nBased on data from 22 camera pairs.") +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_text(margin = margin(7, 0, 0, 0), size = 14),
        legend.position = "none",
        plot.caption = element_text(size = 10),
        plot.title = element_text(size = 12, margin = margin(0, 0, 1, 0, unit = "cm")),
        plot.subtitle = element_text(size = 10),
        axis.text.y = element_text(size = 12))

plot

ggsave(filename = "Game Trail vs Random Results 2024-07-17.png", dpi = 300, height = 6.5, width = 8.5, bg = "white")

#-----------------------------------------------------------------------------------------------------------------------

# Let's try constructing a new plot for the boostraps

library(ggdist)
library(ggridges)

df_boot <- df
df_tifc <- table

# Prepare TIFC data for plotting:
df_tifc_plot <- df_tifc |>
  select(species_common_name = sp, npairs, 5:7) |>
  filter(npairs > 10) |>
  # For labeling in plot
  mutate(n_pairs = paste0(species_common_name, "\n(n cam pairs = ", npairs, ")")) %>%
  # Species as an ordered factor
  mutate(n_pairs = fct_reorder(n_pairs, R_dur_as_pct_of_GT_median, .desc = TRUE)) |>
  # Change Inf to 1000 for viz (will be truncated in the plot range anyway)
  mutate(R_dur_as_pct_of_GT_uci = ifelse(is.infinite(R_dur_as_pct_of_GT_uci), 1000, R_dur_as_pct_of_GT_uci))

df_boot1 <- df_boot |>
  select(`Black Bear`, `Canada Lynx`, Coyote, Moose, `Snowshoe Hare`, `White-tailed Deer`) |>
  pivot_longer(everything(), names_to = "species_common_name", values_to = "rep") |>
  arrange(species_common_name) |>
  left_join(df_tifc_plot) |>
  mutate(n_pairs = fct_rev(n_pairs)) |>
  filter(rep < 400, rep > 10)

plot <- df_boot1 |>
  ggplot(aes(x = rep, y = n_pairs, fill = after_stat(x))) +
  #fill = factor(stat(quantile)))) +
  #fill = 0.5 - abs(0.5 - stat(ecdf))
  #)) +
  stat_density_ridges(scale = 1.5,
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
  scale_x_continuous(breaks = seq(0, 300, 100),
                     limits = c(0, 300),
                     oob = scales::rescale_none,
                     labels = scales::percent_format(scale = 1)) +
  geom_text(aes(y = n_pairs,
                x = R_dur_as_pct_of_GT_median,
                label = paste0(round(R_dur_as_pct_of_GT_median, digits = 0), "%")),
            size = 3,
            nudge_y = -0.15, nudge_x = 6,
            color = "grey20") +
  theme_ridges() +
  #theme_minimal() +
  labs(x = "",
       title = "Density: Random Camera Value Expressed as % of Game Trail Camera",
       #subtitle = "Value at the lower camera expressed as a percentage (%) of the corresponding higher camera value",
       caption = "90% confidence intervals and median percentage value represented by\n black quantile lines (0.05, 0.5, 0.95) and obtained via bootstrapping.\nBased on data from 42 camera pairs.") +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_text(margin = margin(7, 0, 0, 0), size = 14),
        legend.position = "none",
        plot.caption = element_text(size = 10),
        plot.title = element_text(size = 12, margin = margin(0, 0, 1, 0, unit = "cm")),
        plot.subtitle = element_text(size = 10),
        axis.text.y = element_text(size = 12))

plot

ggsave(filename = "Game Trail vs Random Results Density 2024-08-01.png", dpi = 300, height = 6.5, width = 8.5, bg = "white")


