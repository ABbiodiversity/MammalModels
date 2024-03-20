# Full Smooth Figures

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

#-----------------------------------------------------------------------------------------------------------------------

# Species to do:
# White-tailed Deer (2,4), Moose (2,1), Woodland Caribou (0.04,0.03),
# Fisher (0.01, 0.02), Marten (0.03, 0.02), Gray Wolf (0.05, 0.05),
# Canada Lynx (0.04, 0.03), Black Bear (1, 1), Snowshoe Hare (0.4, 0.4), Coyote (0.05, 0.15)

sp <- "Coyote"

# Plots of Smoothed Results

plot_on_off_smooth <- on_off_smooth |>
  filter(common_name == sp) |>
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
  coord_cartesian(ylim = c(0, 0.05)) +
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

# Smoothed Dist Treatment

plot_dist_smooth <- dist_smooth |>
  filter(common_name == sp) |>
  ggplot(aes(x = distance, y = mean_density)) +
  geom_point(aes(color = treatment),
             size = 3.5) +
  geom_linerange(aes(ymin = lci_density, ymax = uci_density, color = treatment),
                 linewidth = 0.5) +
  scale_color_manual(values = c("#80461B", "#CD7F32")) +
  scale_x_continuous(breaks = c(130, 220, 270, 300),
                     labels = c(">200m", "100m", "30m", "10m")) +
  geom_point(data = ref |> filter(common_name == sp),
             aes(x = distance, y = mean_density),
             color = "darkgreen",
             size = 3.5) +
  geom_linerange(data = ref |> filter(common_name == sp),
                 aes(ymin = lci_density, ymax = uci_density),
                 color = "darkgreen",
                 linewidth = 0.5) +
  coord_cartesian(ylim = c(0, 0.15),
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

ggsave(paste0(g_drive, "Results/OSM BADR/Figures/2022/", sp, " Full Smooth.png"), full_plot, height = 8, width = 8, dpi = 500, bg = "white")


