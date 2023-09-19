# Use of Industry Camera Data in the BADR design

load("C:/Users/mabec/Downloads/summaries_20221102_RICC_rev00_2022-11-14 (1).RData")

ricc_veg <- d_long |>
  filter(str_detect(project, "AOC|MEG")) |>
  select(project, survey_year, location, VEGAGEclass, VEGHFAGEclass) |>
  mutate(Habitat = case_when(
    str_detect(VEGAGEclass, "Bog|Fen|Marsh|Swamp") ~ "Treed Lowland",
    str_detect(VEGAGEclass, "Decid|Mixedwood") ~ "Deciduous Mixedwood",
    TRUE ~ NA
  )) |>
  filter(!is.na(Habitat)) |>
  mutate(treatment = "Dense Linear Features",
         fine_scale = ifelse(VEGHFAGEclass == "SeismicLineWide", "On", "Off")) |>
  select(project, location, Habitat, treatment, fine_scale)


species <- c("Black Bear", "Moose", "White-tailed Deer", "Snowshoe Hare", "Canada Lynx", "Gray Wolf", "Coyote")

# Density data
ricc_dens <- read_csv(paste0(g_drive, "results/density/deployments/ricc_all-years_density_long_2022-11-24.csv")) |>
  filter(common_name %in% species)

# Join to Veg data
ricc <- ricc_veg |>
  left_join(ricc_dens, by = c("project", "location")) |>
  filter(!is.na(density_km2))

# Summarise density function
source("src/functions/summarise-density.R")

ricc_mean_density <- ricc |>
  mutate(group = paste0(Habitat, "_", fine_scale)) |>
  select(-project) |>
  summarise_density(
    group_id = group,
    agg_samp_per = TRUE,
    #samp_per_col = project,
    species_col = common_name,
    dens_col = density_km2,
    conflevel = 0.9) |>
  ungroup() |>
  separate(group, into = c("Habitat", "fine_scale"), sep = "_") |>
  mutate(treatment = "Dense Linear Features\n(Industry)",
         source = treatment) |>
  select(Habitat, treatment, fine_scale, common_name, mean_density = density_avg,
         lci_density = density_lci_0.9, uci_density = density_uci_0.9, source)

ricc_moose_mean <- ricc_mean_density |>
  filter(common_name == "Moose")

ricc_wtd_mean <- ricc_mean_density |>
  filter(common_name == "White-tailed Deer")

# Now do the 'JEM' level summaries ...
ricc_data <- ricc |>
  #filter(common_name == "Moose") |>
  mutate(group = paste0(Habitat, "_", fine_scale)) |>
  summarise_density(
    group_id = group,
    agg_samp_per = FALSE,
    samp_per_col = project,
    species_col = common_name,
    dens_col = density_km2,
    conflevel = 0.9) |>
  ungroup() |>
  separate(group, into = c("Habitat", "fine_scale"), sep = "_") |>
  mutate(treatment = "Dense Linear Features\n(Industry)") |>
  select(Habitat, treatment, fine_scale, common_name, mean_density = density_avg)

ricc_moose_data <- ricc_data |> filter(common_name == "Moose")
ricc_wtd_data <- ricc_data |> filter(common_name == "White-tailed Deer")

# Now let's try plotting with the other stuff.

badr_mean <- on_off |>
  filter(#common_name == "Moose",
         treatment == "Dense Linear Features" | treatment == "Reference") |>
  mutate(treatment = ifelse(str_detect(treatment, "Dense"), "Dense Linear Features\n(BADR + Historic)", "Reference\n(BADR + Historic)")) |>
  mutate(lci_density = ifelse(mean_density > 0, lci_density, 0),
         uci_density = ifelse(mean_density > 0, uci_density, 0),
         source = "BADR + ABMI") |>
  select(-c(vegetation)) |>
  bind_rows(ricc_mean_density) |>
  mutate(treatment = factor(treatment, levels = c("Reference\n(BADR + Historic)",
                                              "Dense Linear Features\n(BADR + Historic)",
                                              "Dense Linear Features\n(Industry)")))

jem <- on_off_jem |>
  filter(#common_name == "Moose",
         treatment == "Dense Linear Features" | treatment == "Reference") |>
  mutate(treatment = ifelse(str_detect(treatment, "Dense"), "Dense Linear Features\n(BADR + Historic)", "Reference\n(BADR + Historic)")) |>
  bind_rows(ricc_data) |>
  mutate(treatment = factor(treatment, levels = c("Reference\n(BADR + Historic)",
                                              "Dense Linear Features\n(BADR + Historic)",
                                              "Dense Linear Features\n(Industry)"))) |>
  mutate(source = ifelse(str_detect(treatment, "Industry"), "Industry", "BADR + ABMI"))

jem_moose_data <- jem |>
  filter(common_name == "Moose") |>
  #bind_rows(ricc_moose_data) |>
  filter(!str_detect(treatment, "Reference")) |>
  mutate(group = paste0(Habitat, "_", fine_scale)) |>
  summarise_density(
    group_id = group,
    agg_samp_per = TRUE,
    #samp_per_col = project,
    species_col = common_name,
    dens_col = mean_density,
    conflevel = 0.9)

badr_moose_mean <- on_off |>
  filter(common_name == "Moose",
    treatment == "Dense Linear Features" | treatment == "Reference") |>
  mutate(treatment = ifelse(str_detect(treatment, "Dense"), "Dense Linear Features\n(BADR + Historic)", "Reference\n(BADR + Historic)")) |>
  mutate(lci_density = ifelse(mean_density > 0, lci_density, 0),
         uci_density = ifelse(mean_density > 0, uci_density, 0),
         source = "BADR + ABMI") |>
  select(-c(vegetation))

# Black Bear

bb <- badr_mean |>
  filter(common_name == "Black Bear") |>
  ggplot(aes(x = fine_scale, y = mean_density)) +
  geom_point(aes(color = treatment, shape = source),
             size = 3.5,
             position = position_dodge(width = 0.5)) +
  geom_linerange(aes(ymin = lci_density, ymax = uci_density, color = treatment),
                 linewidth = 0.5, position = position_dodge(width = 0.5)) +
  geom_point(data = (jem |> filter(common_name == "BlackBear" | common_name == "Black Bear")),
             aes(x = fine_scale, y = mean_density, color = treatment, shape = source),
             size = 2,
             alpha = 0.15,
             position = position_jitterdodge(jitter.width = 0.2)) +
  scale_color_manual(values = c("Dark Green", "#FFC300", "#FFC300")) +
  scale_y_sqrt() +
  theme_minimal() +
  coord_cartesian(ylim = c(0, 2)) +
  labs(x = "Placement",
       y = expression(Density~(individuals~per~km^2)),
       title = "Black Bear") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 10, margin = margin(0, 0.4, 0, 0, unit = "cm")),
        strip.text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, size = 14),
        legend.position = "none",
        legend.title = element_blank(),
        legend.margin = margin(0, 0, 0, 0),
        legend.spacing.x = unit(0.05, "cm"),
        axis.ticks = element_blank(),
        panel.grid.major.y = element_line(linewidth = 0.5, color = "grey80"),
        panel.grid.major.x = element_blank()) +
  facet_grid(. ~ Habitat, scales = "free_x", space = "free")

# Moose

moose <- badr_mean |>
  filter(common_name == "Moose") |>
  ggplot(aes(x = fine_scale, y = mean_density)) +
  geom_point(aes(color = treatment, shape = source),
             size = 3.5,
             position = position_dodge(width = 0.5)) +
  geom_linerange(aes(ymin = lci_density, ymax = uci_density, color = treatment),
                 linewidth = 0.5, position = position_dodge(width = 0.5)) +
  geom_point(data = (jem |> filter(common_name == "Moose")),
             aes(x = fine_scale, y = mean_density, color = treatment, shape = source),
             size = 2,
             alpha = 0.15,
             position = position_jitterdodge(jitter.width = 0.2)) +
  scale_color_manual(values = c("Dark Green", "#FFC300", "#FFC300")) +
  scale_y_sqrt() +
  theme_minimal() +
  coord_cartesian(ylim = c(0, 3)) +
  labs(x = "Placement",
       y = expression(Density~(dividuals~per~km^2)),
       title = "Moose") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 10, margin = margin(0, 0.4, 0, 0, unit = "cm")),
        strip.text = element_blank(),
        #strip.text = element_text(size = 9),
        plot.title = element_text(hjust = 0.5, size = 14),
        legend.position = "none",
        legend.title = element_blank(),
        legend.margin = margin(0, 0, 0, 0),
        legend.spacing.x = unit(0.05, "cm"),
        axis.ticks = element_blank(),
        panel.grid.major.y = element_line(linewidth = 0.5, color = "grey80"),
        panel.grid.major.x = element_blank()) +
  facet_grid(. ~ Habitat, scales = "free_x", space = "free")

moose

# White-tailed Deer

wtd <- badr_mean |>
  filter(common_name == "White-tailed Deer") |>
  ggplot(aes(x = fine_scale, y = mean_density)) +
  geom_point(aes(color = treatment, shape = treatment),
             size = 3.5,
             position = position_dodge(width = 0.5)) +
  geom_linerange(aes(ymin = lci_density, ymax = uci_density, color = treatment),
                 linewidth = 0.5, position = position_dodge(width = 0.5)) +
  geom_point(data = (jem |> filter(common_name == "WhitetailedDeer" | common_name == "White-tailed Deer")),
             aes(x = fine_scale, y = mean_density, color = treatment, shape = treatment),
             size = 2,
             alpha = 0.15,
             position = position_jitterdodge(jitter.width = 0.2)) +
  scale_color_manual(values = c("Dark Green", "#FFC300", "#FFC300")) +
  scale_shape_manual(values = c(16, 16, 17)) +
  scale_y_sqrt() +
  theme_minimal() +
  coord_cartesian(ylim = c(0, 10)) +
  labs(x = "Placement",
       y = expression(Density~(individuals~per~km^2)),
       title = "White-tailed Deer") +
  guides(color = "legend") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
        axis.title.x = element_text(size = 11, margin = margin(0.2, 0, 0, 0, unit = "cm")),
        axis.title.y = element_text(size = 10, margin = margin(0, 0.4, 0, 0, unit = "cm")),
        strip.text = element_blank(),
        #strip.text = element_text(size = 9),
        plot.title = element_text(hjust = 0.5, size = 14),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.margin = margin(0, 0, 0, 0),
        legend.spacing.x = unit(0.05, "cm"),
        axis.ticks = element_blank(),
        panel.grid.major.y = element_line(linewidth = 0.5, color = "grey80"),
        panel.grid.major.x = element_blank()) +
  facet_grid(. ~ Habitat, scales = "free_x", space = "free")

wtd

# Join together
full_plot <- grid.arrange(bb, moose, wtd, heights = c(3.5, 3.2, 3.9))

ggsave(paste0(g_drive, "results/osm/figures/industry_full_sqrt.png"), full_plot, height = 8, width = 8, dpi = 500, bg = "white")



