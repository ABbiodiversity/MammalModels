#-----------------------------------------------------------------------------------------------------------------------

# Project:          BOUTIN

# Title:            Visualize results
# Description:

# Author:           Marcus Becker
# Date:             December 2022

# Previous scripts: 01_boutin-calculate-density-by-location, 02-boutin-summarise-density

#-----------------------------------------------------------------------------------------------------------------------

library(ggplot2)
library(readr)
library(dplyr)

g_drive <- "G:/Shared drives/ABMI Camera Mammals/"

# Load data
df_mp_density <- read_csv(paste0(g_drive, "results/density/areas/boutin_density-by-monitoring-period_2022-12-16.csv"))

# Create plot
df_mp_density |>
  filter(!str_detect(monitoring_period, "2014")) |>
  mutate(monitoring_period = factor(monitoring_period,
                                    levels = c("spring_2015", "fall_2015",
                                               "spring_2016", "fall_2016",
                                               "spring_2017", "fall_2017",
                                               "spring_2018", "fall_2018",
                                               "spring_2019", "fall_2019",
                                               "spring_2020", "fall_2020",
                                               "spring_2021", "fall_2021"))) |>
  ggplot(aes(x = monitoring_period, y = density_avg, color = common_name)) +
  geom_point(size = 3) +
  geom_linerange(aes(ymin = density_lci_0.9, ymax = density_uci_0.9), size = 0.7) +
  geom_line(aes(group = common_name)) +
  labs(y = "Density (per km2)",
       x = "") +
  theme_classic() +
  facet_wrap(~ common_name, nrow = 2, scales = "free_y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

# Save plot
ggsave(paste0(g_drive, "results/figures/boutin_yukon_2022-12-16.png"), width = 5, height = 4.25)

#-----------------------------------------------------------------------------------------------------------------------
