# Obtain VegHF

library(tidyverse)

load("S:/samba/abmisc/AB_data_v2023/sites/processed/landcover/abmi-cam-aru_2009_2023.Rdata")

t <- site.lookup |>
  filter(deployment == "CAM" | deployment == "BOTH") |>
  select(Site_ID, survey_year, WildtraxName, WildtraxProject) |>
  mutate(survey_year = as.character(survey_year)) |>
  distinct()

d <- as.data.frame(as.matrix(d.wide.pts$veg.current)) |>
  rownames_to_column(var = "Site") |>
  separate(Site, into = c("Site_ID", "survey_year"), sep = "_") |>
  filter(str_detect(Site_ID, "CMU")) |>
  pivot_longer(cols = SpruceR:last_col(), values_to = "ColumnA", names_to = "ColumnB") |>
  filter(ColumnA == "1") |>
  rename(VegHF = ColumnB) |>
  select(Site_ID, survey_year, VegHF) |>
  left_join(t, by = c("Site_ID", "survey_year")) |>
  select(location = WildtraxName, project = WildtraxProject, VegHF)

write_csv(d, "CMU VegHF Categories.csv")
