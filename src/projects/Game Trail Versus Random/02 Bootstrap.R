#-----------------------------------------------------------------------------------------------------------------------

# Project:          Game Trail Versus Random

# Title:            Bootstrap
# Description:      The purpose of this script is to summarise the data from the game trail and random paired camera

# Author:           Marcus Becker

# Previous scripts: None

#-----------------------------------------------------------------------------------------------------------------------

# Prepare data for bootstrapping

boot <- df_tt |>
  mutate(location = str_remove(location, "-A$")) |>
  separate(location, into = c("location", "treatment"), sep = "-", extra = "merge") |>
  mutate(location = ifelse(str_detect(treatment, "-2$"), paste0(location, "-2"), location),
         treatment = str_remove(treatment, "-2$")) |>
  pivot_wider(id_cols = c(location, species_common_name),
              names_from = "treatment",
              values_from = c("total_duration")) |>
  mutate(loc_sp = paste0(location, "_", species_common_name),
         sp = paste0(species_common_name)) |>
  #rename(full_height = `1m`,
  #       half_height = `0.5m`) |>
  select(loc_sp, 3:6, location, sp) |>
  # Remove deployment-species-season combos without any duration/images at either camera
  filter(GT > 0 | R > 0)

# Bootstrap

# New sp list with enough pairs

sp.list <- c("White-tailed Deer", "Mule Deer", "Moose", "Horse", "Coyote", "Red Fox")

boot <- boot |> filter(sp %in% sp.list)

sp.list <- sort(unique(boot$sp))

niter <- 1000
bs1 <- array(0, c(length(sp.list), 2, niter))

dimnames(bs1)[[1]] <- sp.list

bs.sum<-array(0, c(length(sp.list), 1, 3))
dimnames(bs.sum) <- list(sp.list, c("duration_GT_as_pct_of_R"), c("median","q5","q95"))

# Initialize an empty dataframe to store the bootstrapped values
df <- data.frame(matrix(ncol = length(sp.list), nrow = niter))
colnames(df) <- sp.list

# Set seed for reproducibility:
set.seed(12345)

for (sp in 1:length(sp.list)) {

  print(paste(sp, length(sp.list), date()))
  boot.sp <- boot[boot$sp == sp.list[sp], ]
  loc.list <- sort(unique(boot.sp$location))

  for (iter in 1:niter) {
    # Sample from the locations, with replacement.
    s <- sample(1:length(loc.list), length(loc.list), replace = TRUE)

    boot1 <- NULL

    for (j in 1:length(s))

      boot1 <- rbind(boot1, boot.sp[boot.sp$location == loc.list[s[j]], ])

    x <- mean(boot1$GT)
    bs1[sp.list[sp], 1, iter] <- as.numeric(x)

    x <- mean(boot1$R)
    bs1[sp.list[sp], 2, iter] <- as.numeric(x)

  }

  # Calculate ratio between the values of both cameras
  ratio1 <- bs1[sp, 2, ] / bs1[sp, 1, ] * 100
  ratio1 <- ratio1[!is.na(ratio1)]
  # Save to dataframe to store bootstrapped values
  df[[sp.list[sp]]] <- ratio1
  # Obtain median, 5% and 95% quantiles for CIs
  bs.sum[sp, 1, ] <- quantile(ratio1, c(0.5, 0.05, 0.95))

}

# Collect values from matrices into table
table <- data.frame(
  sp = sort(unique(boot$sp)),
  npairs = as.numeric(by(boot$location, boot$sp, length)),
  total_duration_GT = as.numeric(by(boot$GT, boot$sp, sum)),
  total_duration_R = as.numeric(by(boot$R, boot$sp, sum)),
  R_dur_as_pct_of_GT_median = bs.sum[,1,1],
  R_dur_as_pct_of_GT_lci = bs.sum[,1,2],
  R_dur_as_pct_of_GT_uci = bs.sum[,1,3]) |>
  # Round
  mutate(across(.cols = total_duration_GT:last_col(), .f = ~ round(., digits = 2)))

#-----------------------------------------------------------------------------------------------------------------------

# Bootstrapping the OSM data first

# Issues: 13-0-MAAA10, 13-0-MAZ15, 13-0-MAAA16

boot <- df_tt |>
  separate(location, into = c("location", "treatment"), sep = "(?=.$)") |>
  pivot_wider(id_cols = c(location, species_common_name),
              names_from = "treatment",
              values_from = c("total_duration")) |>
  mutate(loc_sp = paste0(location, "_", species_common_name),
         sp = paste0(species_common_name)) |>
  rename(random = A,
         trail = B) |>
  select(loc_sp, 3:6, location, sp) |>
  # Remove deployment-species-season combos without any duration/images at either camera
  filter(trail > 0 | random > 0) |>
  # I think I need to make the NAs in full 0 - those full cameras didn't capture images of any species
  mutate(random = ifelse(is.na(random), 0, random))

# NEW with densities

boot_new <- dens_all |>
  separate(location, into = c("location", "treatment"), sep = "(?=.$)") |>
  pivot_wider(id_cols = c(location, species_common_name),
              names_from = "treatment",
              values_from = c("density_km2")) |>
  mutate(loc_sp = paste0(location, "_", species_common_name),
         sp = paste0(species_common_name)) |>
  rename(random = A,
         trail = B) |>
  select(loc_sp, 3:6, location, sp) |>
  # Remove deployment-species-season combos without any duration/images at either camera
  filter(trail > 0 | random > 0)

# Bootstrap

# New sp list with enough pairs

sp.list <- c("White-tailed Deer", "Black Bear", "Moose", "Snowshoe Hare", "Coyote", "Woodland Caribou", "Canada Lynx")

boot_new <- boot_new |> filter(sp %in% sp.list)

sp.list <- sort(unique(boot$sp))

niter <- 1000
bs1 <- array(0, c(length(sp.list), 2, niter))

dimnames(bs1)[[1]] <- sp.list

bs.sum<-array(0, c(length(sp.list), 1, 3))
dimnames(bs.sum) <- list(sp.list, c("duration_GT_as_pct_of_R"), c("median","q5","q95"))

# Initialize an empty dataframe to store the bootstrapped values
df <- data.frame(matrix(ncol = length(sp.list), nrow = niter))
colnames(df) <- sp.list

# Set seed for reproducibility:
set.seed(12345)

for (sp in 1:length(sp.list)) {

  print(paste(sp, length(sp.list), date()))
  boot.sp <- boot[boot$sp == sp.list[sp], ]
  loc.list <- sort(unique(boot.sp$location))

  for (iter in 1:niter) {
    # Sample from the locations, with replacement.
    s <- sample(1:length(loc.list), length(loc.list), replace = TRUE)

    boot1 <- NULL

    for (j in 1:length(s))

      boot1 <- rbind(boot1, boot.sp[boot.sp$location == loc.list[s[j]], ])

    x <- mean(boot1$trail)
    bs1[sp.list[sp], 1, iter] <- as.numeric(x)

    x <- mean(boot1$random)
    bs1[sp.list[sp], 2, iter] <- as.numeric(x)

  }

  # Calculate ratio between the values of both cameras
  ratio1 <- bs1[sp, 2, ] / bs1[sp, 1, ] * 100
  ratio1 <- ratio1[!is.na(ratio1)]
  # Save to dataframe to store bootstrapped values
  df[[sp.list[sp]]] <- ratio1
  # Obtain median, 5% and 95% quantiles for CIs
  bs.sum[sp, 1, ] <- quantile(ratio1, c(0.5, 0.05, 0.95))

}

# Collect values from matrices into table
table <- data.frame(
  sp = sort(unique(boot$sp)),
  npairs = as.numeric(by(boot$location, boot$sp, length)),
  total_duration_GT = as.numeric(by(boot$trail, boot$sp, sum)),
  total_duration_R = as.numeric(by(boot$random, boot$sp, sum)),
  R_dur_as_pct_of_GT_median = bs.sum[,1,1],
  R_dur_as_pct_of_GT_lci = bs.sum[,1,2],
  R_dur_as_pct_of_GT_uci = bs.sum[,1,3]) |>
  # Round
  mutate(across(.cols = total_duration_GT:last_col(), .f = ~ round(., digits = 2)))






