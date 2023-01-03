check <- df_pveghf_all %>%
  filter(is.na(VEGHFAGEclass))

# BG locations

bg <- read_csv(paste0(root, "data/base/raw/from_WildTrax/BU/BU_Big_Grids_2016_report.csv"))

bg1 <- bg %>%
  select(location, project, latitude, longitude) %>%
  distinct()

write_csv(bg1, paste0(root, "data/lookup/locations/bg_public-locations_2022-01-10.csv"))


check <- colnames(kgrid_veg_current_north)


check <- str_subset(colnames(kgrid_veg_current_north), pattern = "^CC")


check <- kgrid_veg_current_north %>%
  select(LinkID, nr_alt) %>%
  mutate(n = 1)

check1 <- check %>%
  pivot_wider(names_from = nr_alt, values_from = n, values_fill = list(n = 0))
