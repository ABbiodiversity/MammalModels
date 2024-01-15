#-----------------------------------------------------------------------------------------------------------------------

# Title:            Lure Effects
# Description:      Calculating the impact of lure using ABMI EH data, and generating calibrations for each species.
# Author:           Marcus Becker, David J Huggard
# Date:             January 2024

# Previous scripts:

#-----------------------------------------------------------------------------------------------------------------------

# Root directory (Google Drive)
g_drive <- "G:/Shared drives/ABMI Camera Mammals/"

# Attach packages
library(tidyverse)

# Load data:

# Time by day
df_tbd_early <- read_csv(paste0(g_drive, "data/processed/time-by-day/eh_14-15-16-17-18_tbd-summary_2023-05-23.csv"))
df_tbd_later <- read_csv(paste0(g_drive, "data/processed/time-by-day/eh_19-20-21-22_tbd-summary_2023-01-19.csv"))

# Lure lookup
df_lure <- read_csv(paste0(g_drive, "data/lookup/lure/abmi_all-years_lure_2023-11-27.csv"))

eh_early <- read_csv(paste0(g_drive, "Results/Density/Deployments/eh_14-15-16-17-18_density_long_2023-05-23.csv"))
eh_later <- read_csv(paste0(g_drive, "Results/Density/Deployments/eh_19-20-21-22_density_long_2023-01-19.csv"))

# Species of interest
sp <- c("White-tailed Deer", "Mule Deer", "Moose", "Woodland Caribou",
        "Black Bear", "Grizzly Bear", "Canada Lynx", "Fisher", "Marten",
        "Elk (wapiti)", "Gray Wolf", "Coyote", "Red Fox", "Cougar")

df_density <- bind_rows(eh_early, eh_later) |>
  filter(common_name %in% sp)

#-----------------------------------------------------------------------------------------------------------------------

df_tbd <- bind_rows(df_tbd_early, df_tbd_later) |>
  select(1, 2) |>
  filter(!str_detect(project_location, "B$")) |>
  separate(project_location, into = c("project", "site", "station"), sep = "_|-", remove = TRUE) |>
  filter(!str_detect(site, "^W")) |>
  unite("project_site", project, site, sep = "_")

df_lure1 <- df_lure |>
  filter(str_detect(project, "Ecosystem Health"),
         !str_detect(location, "B$|b$|^W|^ST|VNA|DNC")) |>
  separate(location, into = c("site", "station"), sep = "-", remove = TRUE) |>
  unite("project_site", project, site, sep = "_") |>
  group_by(project_site) |>
  summarise(lured = sum(lure == "Yes"),
            unlured = sum(lure == "No")) |>
  mutate(check = lured - unlured) |>
  # Exclude sites missing one of lured or unlured
  filter(lured > 0, unlured > 0)

df_lure_include <- df_lure1 |>
  filter(check == "0") |>
  select(project_site)

df_lure_include_2 <- df_lure1 |>
  filter(!check == "0")

df_tbd_subset <- df_tbd |>
  inner_join(df_lure_include_2, by = c("project_site")) |>
  separate(project_site, into = c("project", "site"), sep = "_") |>
  unite("location", site, station, sep = "-") |>
  left_join(df_lure, by = c("project", "location")) |>
  separate(location, into = c("site", "station"), sep = "-", remove = FALSE)

# First do those that have more lured
d_1 <- df_tbd_subset |>
  filter(check > 0) |>
  group_by(site) |>
  mutate(days_check = abs(total_days - total_days[lure == "No"]))

d_2 <- d_1 |>
  filter(lure == "Yes") |>
  slice(which.min(days_check))

d <- d_1 |>
  filter(lure == "No") |>
  bind_rows(d_2) |>
  arrange(site) |>
  select(project, location, site, station, total_days, lure)

# Next do those that have more unlured
t_1 <- df_tbd_subset |>
  filter(check < 0) |>
  group_by(site) |>
  mutate(days_check = abs(total_days - total_days[lure == "Yes"]))

t_2 <- t_1 |>
  filter(lure == "No") |>
  slice(which.min(days_check))

t <- t_1 |>
  filter(lure == "Yes") |>
  bind_rows(t_2) |>
  arrange(site) |>
  select(project, location, site, station, total_days, lure)

together <- bind_rows(t, d)

# Site information
north_sites <- read_csv(paste0(g_drive, "data/lookup/climate/site-climate-summary_v2020.csv")) |>
  filter(!NATURAL_REGIONS == "Grassland") |>
  select(site = SITE_ID) |>
  pull()

# Density data
df_density_1 <- df_density %>%
  separate(location, into = c("site", "station"), sep = "-", remove = FALSE) |>
  unite("project_site", project, site, sep = "_") |>
  semi_join(df_lure_include, by = c("project_site")) |>
  separate(project_site, into = c("project", "site"), sep = "_")

df_density_2 <- df_density |>
  semi_join(together, by = c("project", "location")) |>
  separate(location, into = c("site", "station"), sep = "-", remove = FALSE)

df_density_all <- bind_rows(df_density_1, df_density_2) |>
  filter(site %in% north_sites) |>
  # Remove Bears in Winter
  filter(!((common_name == "Black Bear" | common_name == "Grizzly Bear") & season == "winter")) |>
  filter(!is.na(density_km2),
         total_season_days >= 20) |>
  group_by(project, location, common_name) |>
  # Compute weighted mean using total season days as weight
  summarise(density_km2 = weighted.mean(density_km2, total_season_days)) |>
  ungroup() |>
  mutate(common_name = str_remove(common_name, " ")) |>
  pivot_wider(id_cols = c(project, location), names_from = common_name, values_from = density_km2) |>
  separate(location, into = c("site", "station"), sep = "-", remove = FALSE) |>
  left_join(df_lure, by = c("project", "location")) |>
  mutate(lure = factor(lure),
         site = factor(site)) |>
  mutate(across(BlackBear:WoodlandCaribou, ~ ifelse(is.na(.), 0, .))) |>
  as.data.frame()

SpTable <- str_remove(sp, " ")

d <- df_density_all

sitelist<-sort(unique(d$site))  # Sites as unit for resampling
niter<-1e4
pa.bs<-agp.bs<-ta.bs<-array(NA,c(length(SpTable),2,niter))
dimnames(pa.bs)[[1]]<-dimnames(agp.bs)[[1]]<-dimnames(ta.bs)[[1]]<-SpTable
dimnames(pa.bs)[[2]]<-dimnames(agp.bs)[[2]]<-dimnames(ta.bs)[[2]]<-c("Unlured","Lured")

for (iter in 1:niter) {
  if (iter/100==round(iter/100)) print(paste(iter,niter,date()))
  s <- sample(1:length(sitelist),length(sitelist),replace=TRUE)
  if (iter==1) s<-1:length(sitelist)  # Use actual data in first iteration
  i<-unlist(lapply(sitelist[s],function(x) which(d$site %in% x == TRUE)))  # Records to use - each deployment in the resampled sites
  d1<-d[i,]
  for (sp in 1:length(SpTable)) {
    pa.bs[sp,,iter]<-as.numeric(by(sign(d1[,SpTable[sp]]),d1$lure, mean))
    ta.bs[sp,,iter]<-as.numeric(by(d1[,SpTable[sp]],d1$lure,mean))
    agp.bs[sp,,iter]<-ta.bs[sp,,iter]/pa.bs[sp,,iter]
  }  # Next sp
}  # Next iter
pa.bs.sum<-agp.bs.sum<-ta.bs.sum<-array(NA,c(length(SpTable),3))  # Lure effect ratios for each species, {direct data, 5%'ile, 95%'ile}
for (sp in 1:length(SpTable)) {
  pa.bs.sum[sp,]<-c(pa.bs[sp,2,1]/pa.bs[sp,1,1],quantile(pa.bs[sp,2,]/pa.bs[sp,1,],c(0.05,0.95)))
  agp.bs.sum[sp,]<-c(agp.bs[sp,2,1]/agp.bs[sp,1,1],quantile(agp.bs[sp,2,]/agp.bs[sp,1,],c(0.05,0.95)))
  ta.bs.sum[sp,]<-c(ta.bs[sp,2,1]/ta.bs[sp,1,1],quantile(ta.bs[sp,2,]/ta.bs[sp,1,],c(0.05,0.95)))
}

dimnames(pa.bs.sum)[[1]] <- SpTable
dimnames(pa.bs.sum)[[2]] <- c("PA", "PA.lci", "PA.uci")
pa <- as.data.frame(pa.bs.sum)

dimnames(agp.bs.sum)[[1]] <- SpTable
dimnames(agp.bs.sum)[[2]] <- c("AGP", "AGP.lci", "AGP.uci")
agp <- as.data.frame(agp.bs.sum)

dimnames(ta.bs.sum)[[1]] <- SpTable
dimnames(ta.bs.sum)[[2]] <- c("TA", "TA.lci", "TA.uci")
ta <- as.data.frame(ta.bs.sum)

check <- bind_cols(pa, agp, ta) %>% tibble::rownames_to_column(var = "common_name")

# Write results
readr::write_csv(check, paste0(root, "data/processed/lure/lure-effect-summary_", Sys.Date(), ".csv"))



