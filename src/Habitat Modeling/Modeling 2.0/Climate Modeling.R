# ---
# title: ABMI models - Run Climate Models
# author: Marcus Becker
# created: September, 2024
# ---

# 1. Load packages
library(tidyverse) # Basic data wrangling
library(sf)
library(AICcmodavg) # Model selection
library(MuMIn) # Model averaging
library(parallel) # Parallel computing

g_drive <- "G:/Shared drives/ABMI Camera Mammals/"

# 2. Load models script
source("src/Habitat Modeling/Modeling 2.0/00_ClimateModels.R")

n <- length(modelsclimate)

sp <- c("White-tailed Deer", "Moose", "Black Bear")

# 3. Load data

# First, the density data with the old climate variables:
dataset.out <- "G:/Shared drives/ABMI Camera Mammals/data/lookup/R Dataset SpTable for ABMI North and South mammal coefficients 2022.RData"
load(dataset.out)

d_new <- d |>
  select(location_project, project, location, NearestSite) |>
  mutate(year = as.character(str_extract_all(project, "\\d+")))

# Second, the new climate variables:
load("S:/samba/abmisc/AB_data_v2023/sites/processed/climate/abmi-camera-climate_2023.Rdata")

new_climate <- rownames_to_column(camera.climate) |>
  separate(rowname, into = c("location", "year"), sep = "_", remove = TRUE) |>
  select(-year) |>
  # Remove 'CMU-' at the beginning of the location name
  mutate(location = str_remove(location, "^CMU-|^NWSAR-")) |>
  left_join(d_new, by = c("location"), relationship = "many-to-many") |>
  select(location_project, project, location, NearestSite, year, everything()) |>
  # Lot of NAs - mostly from 2022 and 2023 projects, which aren't in the density data (yet). To look into!
  filter(!is.na(project))

# Join these new climate variables to d
d_new <- d |>
  select(-c(AHM, FFP, MAP, MAT, MCMT, MWMT)) |>
  left_join(new_climate, by = c("location_project", "project", "location", "NearestSite")) |>
  filter(!is.na(bio9))

# Let's just try White-tailed Deer
d_moose <- d_new |>
  select(location_project, SummerDays, WinterDays, moose_summer = `MooseSummer`, moose_winter = `MooseWinter`, Lured,
         Easting, Northing, MAT:HTV) |>
  rowwise() |>
  mutate(Count = weighted.mean(x = c(moose_summer, moose_winter), w = c(SummerDays, WinterDays))) |>
  ungroup() |>
  select(location_project, Count, Lured, Easting:last_col()) |>
  filter(!is.na(Count),
         !is.na(HTV)) |>
  select(Count, FFP, MAP, TD, CMD, bio15, bio9) |>
  mutate(Present = ifelse(Count > 0, 1, 0)) |>
  filter(Count < 10)

# Let's get a tidier dataframe going here, that one is gross.
d_species <- d_new |>
  select(location_project, SummerDays, WinterDays, Lured, Easting, Northing, MAT:HTV, BeaverSummer:`White-tailed Jack RabbitWinter`) |>
  pivot_longer(cols = BeaverSummer:`White-tailed Jack RabbitWinter`, names_to = "species_common_name", values_to = "density_km2") |>
  mutate(season = ifelse(str_detect(species_common_name, "Summer"), "summer", "winter")) |>
  mutate(species_common_name = str_remove(species_common_name, "Summer$|Winter$")) |>
  mutate(days = ifelse(season == "summer", SummerDays, WinterDays)) |>
  group_by(location_project, species_common_name, pick(Lured:HTV)) |>
  summarise(full_density_km2 = weighted.mean(density_km2, w = days)) |>
  ungroup() |>
  filter(!is.na(full_density_km2),
         !is.na(HTV)) |>
  mutate(present = ifelse(full_density_km2 > 0, 1, 0)) |>
  select(location_project, species_common_name, full_density_km2, present, FFP, MAP, TD, CMD, bio15, bio9) |>
  group_by(location_project, species_common_name) |>
  filter(row_number() == 1) |>
  ungroup()

# 4. Model using Climate Variables

library(glmmTMB)

sp <- unique(d_species$species_common_name)

# Latest kgrid
load("S:/samba/abmisc/AB_data_v2023/kgrid/kgrid_2.2.Rdata")

kgrid_new <- kgrid
kgrid_new$Intercept <- 1

kgrid_preds <- kgrid_new |> select(LinkID, X, Y)

# Get our friend the provincial boundary----
ab <- read_sf(paste0(g_drive, "Results/Habitat Models/lpr_000b21a_e.shp")) |>
  dplyr::filter(PRNAME=="Alberta") |>
  st_transform(crs=3400)

d_preds <- d_species |>
  select(location_project) |>
  distinct()

for (i in sp) {

  print(paste0("Working on ", i))

  d <- d_species |>
    filter(species_common_name == i)

  climate.list <- list()

  climate.list[[1]] <- try(glm(present ~ 1,
                               data = d,
                               family = "binomial"))

  # Run other models
  for(j in 1:n){

    climate.list[[j + 1]] <- try(update(climate.list[[1]], formula=modelsclimate[[j]]))

  }

  climate.list <- climate.list[sapply(climate.list, function(x) !inherits(x, "try-error"))]

  # Model averaging----
  averagemodel <- model.avg(climate.list , rank = "AICc")

  # Get predictions----
  d_preds[[i]] <- predict(averagemodel, type = "response", data = d)

  # Get coefficients----
  averagecoefficients <- data.frame(coef = averagemodel$coefficients["full",])

  kgrid_preds[[i]] <- predict(averagemodel, type = "response", newdata = kgrid_new)

  climate <- ggplot(kgrid_preds) +
    geom_sf(data=ab, fill="white") +
    geom_raster(aes(x=X, y=Y, fill=.data[[as.character(i)]]), show.legend = FALSE) +
    scale_fill_viridis_c() +
    theme_bw() +
    labs(title = i, x = "", y = "")
    theme(axis.title = element_blank())

  ggsave(paste0("G:/Shared drives/ABMI Mammals/Habitat Modeling/2024/Climate/Prediction Maps/", i, " Climate Predictions.png"),
         dpi = 500, height = 7, width = 5)

  save(file = paste0("G:/Shared drives/ABMI Mammals/Habitat Modeling/2024/Climate/Coefficients/", i, " Climate Coefficients.RData"),
       averagecoefficients)

}



climate.list <- list()



  # Run null model
  #climate.list[[1]] <- try(glmmTMB(Count ~ 1,
  #                                 data = d_moose,
  #                                 family = ziGamma(link = "log"),
  #                                 ziformula = ~ 1))

  climate.list[[1]] <- try(glm(present ~ 1,
                               data = d_moose,
                               family = "binomial"))

  # Run other models
  for(j in 1:n){

    climate.list[[j + 1]] <- try(update(climate.list[[1]], formula=modelsclimate[[j]]))

    }

  climate.list <- climate.list[sapply(climate.list, function(x) !inherits(x, "try-error"))]

  # Model averaging----
  averagemodel <- model.avg(climate.list , rank = "AICc")

  # Get predictions----
  averageprediction <- predict(averagemodel, type = "response", data = d_deer)

  # Get coefficients----
  averagecoefficients <- data.frame(coef = averagemodel$coefficients["full",])

# 5. Make Predictions

  # Latest kgrid
  load("S:/samba/abmisc/AB_data_v2023/kgrid/kgrid_2.2.Rdata")
  kgrid_new <- kgrid
  kgrid_new$Intercept <- 1

  kgrid_new$ClimatePred <- predict(averagemodel, type = "response", newdata = kgrid_new)

  # Get our friend the provincial boundary----
  ab <- read_sf(paste0(g_drive, "Results/Habitat Models/lpr_000b21a_e.shp")) |>
    dplyr::filter(PRNAME=="Alberta") |>
    st_transform(crs=3400)

  climate <- ggplot(kgrid_new) +
    geom_sf(data=ab, fill="white") +
    geom_raster(aes(x=X, y=Y, fill=ClimatePred), show.legend = TRUE) +
    scale_fill_viridis_c() +
    theme_bw() +
    theme(axis.title = element_blank())

  climate

# 3. Write Function

model_climate <- function(i){

  #1. Loop settings----
  boot.i <- loop$bootstrap[i]
  species.i <- as.character(loop$species[i])

  #2. Get the data----
  covs.i <- covs[covs$surveyid %in% boot[,boot.i],]
  bird.i <- bird[bird$surveyid %in% boot[,boot.i], species.i]
  off.i <- off[off$surveyid %in% boot[,boot.i], species.i]

  dat.i <- data.frame(count = bird.i, offset = off.i) |>
    cbind(covs.i)

  #3. Make model list---
  climate.list <- list()

  #4. Run null model----
  climate.list[[1]] <- try(glm(count ~ 1 + offset(offset),
                               data = dat.i,
                               family = "poisson"))

  if(!inherits(climate.list[[1]], "try-error")){
    #5. Run the other climate models----
    for(j in 1:n){climate.list[[j + 1]] <- try(update(climate.list[[1]], formula=modelsclimate[[j]]))}

    climate.list <- climate.list[sapply(climate.list, function(x) !inherits(x, "try-error"))]

    #7. Model averaging----
    averagemodel <- model.avg(climate.list , rank = "AICc")

    #8. Get predictions----
    averageprediction <- predict(averagemodel, type="link", data=dat.i)

    #9. Get coefficients----
    averagecoefficients <- data.frame(coef = averagemodel$coefficients["full",])

    #10. Save some things----
    save(averagemodel, file = file.path(root, "Results", "ClimateModels", "Models", paste0("ClimateModel_", species.i, "_", boot.i, ".Rdata")))

    write.csv(averageprediction, file = file.path(root, "Results", "ClimateModels", "Predictions", paste0("ClimateModelPrediction_", species.i, "_", boot.i, ".csv")), row.names = FALSE)

    write.csv(averagecoefficients, file = file.path(root, "Results", "ClimateModels", "Coefficients", paste0("ClimateModelCoefficients_", species.i, "_", boot.i, ".csv")), row.names = TRUE)
  }

  if(inherits(climate.list[[1]], "try-error")){

    error <- data.frame(class = class(climate.list[[1]]),
                        error = climate.list[[1]][[1]],
                        species = species.i,
                        boot = boot.i)

    write.csv(error, file = file.path(root, "Results", "ClimateModels", "Try-errors", paste0("ClimateModelErrors_", species.i, "_", boot.i, ".csv")), row.names = FALSE)

  }

}












