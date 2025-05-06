#-----------------------------------------------------------------------------------------------------------------------

# Step 2: Compare number of images, series, and time in front of camera.

#-----------------------------------------------------------------------------------------------------------------------

library(tidyverse)

g_drive <- "G:/Shared drives/ABMI Mammals/"

# Load data.
df_22 <- read_csv(paste0(g_drive, "Data/Model Comparisons/Summary of Series 2022.csv"))
df_21 <- read_csv(paste0(g_drive, "Data/Model Comparisons/Summary of Series 2021.csv"))
df_20 <- read_csv(paste0(g_drive, "Data/Model Comparisons/Summary of Series 2020.csv"))
df_19 <- read_csv(paste0(g_drive, "Data/Model Comparisons/Summary of Series 2019.csv"))

df <- bind_rows(df_19, df_20, df_21, df_22)

check <- df %>%
  separate(location_cam, into = c("location", "camera_model"), sep = "_") %>%
  select(location, camera_model) %>%
  distinct()
# Missing 4-1B1-CA5, 8-1B1-CA2, 1021-SE, 1600-SW

missing <- c("4-1B1-CA5", "8-1B1-CA2", "1021-SE", "1600-SW")

unique(df$species_common_name)

species_of_interest <- c("Black Bear", "White-tailed Deer", "Mule Deer", "Moose", "Coyote",
                         "Elk (wapiti)", "Snowshoe Hare", "Fisher", "Red Fox",
                         "Canada Lynx", "Gray Wolf", "Marten", "Woodland Caribou")

# Summarise.
summary <- df %>%
  filter(common_name %in% species_of_interest) %>%
  separate(location_cam, into = c("location", "camera_model"), sep = "_") %>%
  arrange(species_common_name, location, camera_model) %>%
  group_by(camera_model, common_name, season) %>%
  summarise(total_duration = sum(total_duration),
            total_images = sum(total_images),
            total_series = sum(total_series)) %>%
  ungroup() %>%
  arrange(common_name, season, camera_model)

#-----------------------------------------------------------------------------------------------------------------------

# But let's ignore season for now and group it all together.

boot <- df %>%
  filter(species_common_name %in% species_of_interest) %>%
  group_by(location_cam, species_common_name) %>%
  summarise(total_duration = sum(total_duration),
            total_images = sum(total_images)) %>%
  ungroup() %>%
  separate(location_cam, into = c("location", "cam"), sep = "_") %>%
  filter(!location %in% missing) |>
  mutate(cam = ifelse(cam == "PC900 PROFESSIONAL", "PC900", "HF2")) %>%
  pivot_wider(id_cols = c(location, species_common_name),
              names_from = "cam",
              values_from = c("total_duration", "total_images")) %>%
  mutate(loc_sp = paste0(location, "_", species_common_name),
         sp = paste0(species_common_name)) %>%
  select(loc_sp, 3:6, location, sp) %>%
  # Remove deployment-species-season combos without any duration/images in both HF2 and PC900
  filter(total_duration_HF2 > 0 | total_duration_PC900 > 0)

loc.list <- sort(unique(boot$location))
sp.list <- sort(unique(boot$sp))

niter <- 1000
bs1 <- array(0, c(length(sp.list), 4, niter))

dimnames(bs1)[[1]] <- sp.list

for (iter in 1:niter) {

  # Sample from the locations, with replacement.
  s<-sample(1:length(loc.list), length(loc.list), replace=TRUE)

  boot1<-NULL
  for (j in 1:length(s))
    # My gosh this is funky.
    boot1<-rbind(boot1, boot[boot$location == loc.list[s[j]],])

  x <- by(boot1$total_images_PC900, boot1$sp, mean)
  bs1[names(x), 1, iter] <- as.numeric(x)

  x <- by(boot1$total_images_HF2, boot1$sp, mean)
  bs1[names(x), 2, iter] <- as.numeric(x)

  x <- by(boot1$total_duration_PC900, boot1$sp, mean)
  bs1[names(x), 3, iter] <- as.numeric(x)

  x <-by(boot1$total_duration_HF2, boot1$sp, mean)
  bs1[names(x), 4, iter] <- as.numeric(x)

}

bs.sum<-array(0, c(length(sp.list), 2, 3))

dimnames(bs.sum) <- list(sp.list,
                         c("total_images_pc900","total_duration_pc900"),
                         c("median","q5","q95"))

# Initialize an empty dataframe to store the bootstrapped values
df <- data.frame(matrix(ncol = length(sp.list), nrow = niter))

for (sp in 1:length(sp.list)) {
  ratio1<-bs1[sp,2,]/bs1[sp,1,]*100  # HF2 photos as percent of PC900 photos
  ratio1<-ratio1[!is.na(ratio1)]  # NAs for 0/0
  bs.sum[sp,1,]<-quantile(ratio1,c(0.5,0.05,0.95))
  ratio2<-bs1[sp,4,]/bs1[sp,3,]*100  # HF2 DurInd as percent of PC900 DurInd
  #ratio2<-ratio2[!is.na(ratio2)]  # NAs for 0/0
  # Save to dataframe to store bootstrapped values
  df[[sp.list[sp]]] <- ratio2
  bs.sum[sp,2,]<-quantile(ratio2,c(0.5,0.05,0.95), na.rm = TRUE)
}

q.sum.all <- data.frame(
  sp_season = sort(unique(boot$sp)),
  npairs = as.numeric(by(boot$location, boot$sp, length)),
  total_images_pc900 = as.numeric(by(boot$total_images_PC900, boot$sp, sum)),
  total_images_hf2 = as.numeric(by(boot$total_images_HF2, boot$sp, sum)),
  hf2_images_as_pct_of_pc900_median = bs.sum[,1,1],
  hf2_images_as_pct_of_pc900_lci = bs.sum[,1,2],
  hf2_images_as_pct_of_pc900_uci = bs.sum[,1,3],
  total_duration_pc900 = as.numeric(by(boot$total_duration_PC900, boot$sp, sum)),
  total_duration_hf2 = as.numeric(by(boot$total_duration_HF2, boot$sp, sum)),
  hf2_dur_as_pct_of_pc900_median = bs.sum[,2,1],
  hf2_dur_as_pct_of_pc900_lci = bs.sum[,2,2],
  hf2_dur_as_pct_of_pc900_uci = bs.sum[,2,3]
)

q.sum.all %>%
  tibble::remove_rownames() %>%
  write_csv(paste0(g_drive, "Data/Model Comparisons/Model Effects and Bootstrap By Species.csv"))

g_drive <- "G:/Shared drives/ABMI Mammals/"

df |> select(`Black Bear`:last_col()) |>
  write_csv(paste0(g_drive, "Data/Model Comparisons/Bootstrap Raw Values.csv"))

#-----------------------------------------------------------------------------------------------------------------------

