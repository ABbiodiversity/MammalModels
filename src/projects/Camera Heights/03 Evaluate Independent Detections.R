#-----------------------------------------------------------------------------------------------------------------------

# Title:       Compare differences in detection rate
# Description: Assess how the two camera height conditions performed in the number of independent detections they
#              captured.

# Author:      Marcus A Becker
# Date:        August 17, 2022

#-----------------------------------------------------------------------------------------------------------------------

# Attach packages
library(tidyverse)
library(fuzzyjoin)

g_drive <- "G:/Shared drives/ABMI Camera Mammals/"

# Evaluation of independent detections from script 01
df_series <- read_csv(paste0(g_drive, "data/processed/series-summary/camera-heights-series.csv"))

#-----------------------------------------------------------------------------------------------------------------------

# Summarise series by location and camera, then perform fuzzy join by time interval

df_series_summary <- df_series |>
  # Make adjustment to one deployment to line up times properly
  mutate(image_date_time = case_when(
    location == "760-NE_half" ~ image_date_time %m+% seconds(76),
    location == "1-2A3-CA1_full" ~ image_date_time %m+% seconds(45),
    location == "1-2A3-CA2_full" ~ image_date_time %m+% seconds(94),
    # BDT cameras
    location == "OG-ALPAC-602-2-NE_full" ~ image_date_time %m+% seconds(11),
    location == "OG-ALPAC-602-2-SW_full" ~ image_date_time %m+% seconds(34),
    location == "OG-ALPAC-637-2-SW_full" ~ image_date_time %m+% seconds(55),
    location == "OG-ALPAC-724-3-SE_full" ~ image_date_time %m-% seconds(63),
    location == "OG-ALPAC-724-4-NE_full" ~ image_date_time %m-% seconds(38),
    location == "OG-ALPAC-724-4-SE_full" ~ image_date_time %m+% seconds(15),
    location == "OG-ALPAC-724-6-NE_full" ~ image_date_time %m+% seconds(60),
    location == "OG-ALPAC-761-1-NE_full" ~ image_date_time %m+% seconds(43),
    location == "OG-ALPAC-761-1-SE_full" ~ image_date_time %m+% seconds(41),
    location == "OG-ALPAC-761-2-NE_full" ~ image_date_time %m-% seconds(22),
    location == "OG-ALPAC-761-3-NE_full" ~ image_date_time %m+% seconds(11),
    location == "OG-ALPAC-761-3-SE_full" ~ image_date_time %m-% seconds(18),
    location == "OG-ALPAC-787-2-NW_full" ~ image_date_time %m-% seconds(3576),
    location == "OG-ALPAC-787-2-SW_full" ~ image_date_time %m-% seconds(31),
    location == "OG-ALPAC-787-3-NE_full" ~ image_date_time %m+% seconds(72),
    location == "OG-ALPAC-787-3-SE_full" ~ image_date_time %m-% seconds(22),
    # NEW - 2023 Cameras
    location == "14-1A1-CA1_full" ~ image_date_time %m-% seconds(115),
    location == "14-1A1-CA3_full" ~ image_date_time %m-% seconds(55),
    location == "14-1C1-CA3_full" ~ image_date_time %m-% seconds(31),
    location == "14-1E1-CA3_full" ~ image_date_time %m-% seconds(100),
    location == "14-1E1-CA6_full" ~ image_date_time %m-% seconds(6),
    location == "14-1E2-CA1_full" ~ image_date_time %m-% seconds(87),
    location == "14-2A1-CA1_full" ~ image_date_time %m+% seconds(18),
    location == "14-2A1-CA4_full" ~ image_date_time %m-% seconds(45),
    location == "14-2C1-CA1_full" ~ image_date_time %m-% seconds(18),
    location == "14-2C1-CA4_full" ~ image_date_time %m+% seconds(51),
    location == "14-2E1-CA3_full" ~ image_date_time %m+% seconds(77),
    location == "14-2E1-CA5_full" ~ image_date_time %m-% seconds(25),
    location == "14-2E2-CA1_full" ~ image_date_time %m+% seconds(33),
    location == "14-2E2-CA3_full" ~ image_date_time %m+% seconds(6),
    location == "9-1A1-CA2_full" ~ image_date_time %m+% seconds(94),
    location == "9-1A1-CA4_full" ~ image_date_time %m+% seconds(48),
    location == "9-1C1-CA2_full" ~ image_date_time %m-% seconds(8),
    location == "9-1C1-CA6_full" ~ image_date_time %m-% seconds(61),
    location == "9-1E1-CA2_full" ~ image_date_time %m-% seconds(18),
    location == "9-1E1-CA5_full" ~ image_date_time %m-% seconds(23),
    location == "9-1E2-CA1_full" ~ image_date_time %m-% seconds(35),
    location == "9-1E2-CA3_full" ~ image_date_time %m+% seconds(47),
    location == "9-2A1-CA1_full" ~ image_date_time %m-% seconds(51),
    location == "9-2A1-CA4_full" ~ image_date_time %m-% seconds(18),
    location == "9-2C1-CA2_full" ~ image_date_time %m-% seconds(7),
    location == "9-2C1-CA3_full" ~ image_date_time %m+% seconds(187),
    location == "9-2E1-CA2_full" ~ image_date_time %m-% seconds(59),
    location == "9-2E1-CA7_full" ~ image_date_time %m-% seconds(13),
    TRUE ~ image_date_time
  )) |>
  group_by(series_num, location, species_common_name) |>
  summarise(start = min(image_date_time),
            end = max(image_date_time),
            n_images = n()) |>
  ungroup() |>
  # Add a 10-second buffer onto each side
  mutate(start = start %m-% seconds(10),
         end = end %m+% seconds(10)) |>
  select(series_num, location, species_common_name, start, end, n_images) |>
  separate(location, into = c("location", "height"), sep = "_") |>
  # Split into two lists with dataframe elements by project
  # group_by(height) |>
  group_split(height) |>
  # Fuzzy join - join is done by an overlap in the interval between start and end
  # Potential issue - camera projects operating at the same time.
  reduce(interval_full_join, by = c("start", "end"))

# Were there multiple matches for any series?
check_mult_matches <- df_series_summary |>
  group_by(series_num.y) |>
  tally() |>
  arrange(desc(n)) # Yes, there were.

# Can we calculate how many seconds of overlap? Just as a check.
df_int_len <- df_series_summary |>
  mutate(interval.x = interval(start = start.x, end = end.x),
         interval.y = interval(start = start.y, end = end.y),
         # Cool code.
         overlap = as.duration(intersect(interval.x, interval.y)))

#-----------------------------------------------------------------------------------------------------------------------

# Calculate number of 'detections' were recorded by each camera per event:

df <- df_series_summary |>
  # .x is the 0.5 camera
  group_by(series_num.x) |>
  add_count(name = "count_x") |>
  ungroup() |>
  # .y is the 1m camera
  group_by(series_num.y) |>
  add_count(name = "count_y") |>
  arrange() |>
  ungroup()

# First - a potential issue of mistaken classification with the tagging. Are there matched detections with diff sp?
diff_sp <- df |>
  filter(!species_common_name.x == species_common_name.y)
# Yes. This occurred 9 times. Not too big of a deal, something to correct eventually though.

# Let's look at the 1:1 matches
df_1to1_matches <- df |>
  filter(count_x == "1" & count_y == "1") |>
  mutate(total_images_from_x = n_images.x,
         total_images_from_y = n_images.y) # 532

# Now the many:1 matches (in either direction)
df_mult_matches <- df |>
  filter(!is.na(height.x),
         !is.na(height.y),
         !(count_x == "1" & count_y == "1")) |>
  # Calculate total number of images across multiple series ... kind of tricky here!
  group_by(series_num.x) |>
  mutate(total_images_from_y = sum(n_images.y)) |>
  ungroup() |>
  group_by(series_num.y) |>
  mutate(total_images_from_x = sum(n_images.x)) |>
  ungroup() |>
  # Now, I think I can just group by the respective series_num values and then filter so row_number() == 1 in each case
  group_by(series_num.x) |>
  filter(row_number() == 1) |>
  ungroup() |>
  group_by(series_num.y) |>
  filter(row_number() == 1) |>
  ungroup()
  # This works well. The start and end times don't mean anything now though; I would have had to take the start of the
  # first and the end of the last where there were multiple detections.

# Bind the matches together - 578 total detections.
df_matches <- bind_rows(df_1to1_matches, df_mult_matches) |>
  select(location = location.x, species_common_name = species_common_name.x, start_half_height = start.x, end_half_height = end.x,
         start_full_height = start.y, end_full_height = end.y, total_images_half_height = total_images_from_x,
         total_images_full_height = total_images_from_y)

# Now we assess the detections that were missed by one of the cameras.

# No detection at the half height camera.
df_no_half <- df |>
  filter(is.na(height.x)) |>
  mutate(total_images_half_height = 0) |>
  select(location = location.y, species_common_name = species_common_name.y, start_half_height = start.x, end_half_height = end.x,
         start_full_height = start.y, end_full_height = end.y, total_images_half_height,
         total_images_full_height = n_images.y)

# What's the sp distribution here? Lots of WTD. Weird.
check <- df_no_half |> group_by(species_common_name) |> tally() |> arrange(desc(n))

# No detection at the full height camera.
df_no_full <- df |>
  filter(is.na(height.y)) |>
  mutate(total_images_full_height = 0) |>
  select(location = location.x, species_common_name = species_common_name.x, start_half_height = start.x, end_half_height = end.x,
         start_full_height = start.y, end_full_height = end.y, total_images_half_height = n_images.x,
         total_images_full_height)

# What's the sp distribution here? Still lots of WTD. I guess it's a sheer volume thing.
check <- df_no_full |> group_by(species_common_name) |> tally() |> arrange(desc(n))

# Put it all together
df_detections <- bind_rows(df_matches, df_no_full, df_no_half)

#-----------------------------------------------------------------------------------------------------------------------

# Compare the number of images per detection using bootstrapping

# Only interested in a subset of species
sp_of_interest <- c("Black Bear", "White-tailed Deer", "Moose", "Coyote", "Cougar", "Marten",
                    "Snowshoe Hare", "Fisher", "Canada Lynx", "Gray Wolf", "Red Squirrel")

boot <- df_detections |>
  filter(species_common_name %in% sp_of_interest) |>
  # Only BDT
  #filter(str_detect(location, "OG-ALPAC")) |>
  group_by(species_common_name) |>
  # Create variable of series_num (by species) to use as unit of resampling
  mutate(series_num = row_number()) |>
  ungroup()

sp.list <- sort(unique(boot$species_common_name))

niter <- 500
bs1 <- array(0, c(length(sp.list), 2, niter))

dimnames(bs1)[[1]] <- sp.list

bs.sum<-array(0, c(length(sp.list), 1, 3))
dimnames(bs.sum) <- list(sp.list, c("images_full_as_pct_of_half"), c("median","q5","q95"))

set.seed(12345)

for (sp in 1:length(sp.list)) {

  print(paste(sp,length(sp.list),date()))
  boot.sp<-boot[boot$species_common_name==sp.list[sp],]
  series.list <- sort(unique(boot.sp$series_num))

  for (iter in 1:niter) {

    # Sample from the locations, with replacement.
    s <- sample(1:length(series.list), length(series.list), replace=TRUE)

    boot1<-NULL

    for (j in 1:length(s))

      boot1<-rbind(boot1, boot.sp[boot.sp$series_num == series.list[s[j]],])

    x <- mean(boot1$total_images_full_height)
    bs1[sp.list[sp], 1, iter] <- as.numeric(x)

    x <- mean(boot1$total_images_half_height)
    bs1[sp.list[sp], 2, iter] <- as.numeric(x)

  }

  # Calculate median, 5%, and 95% of resampling results.
  ratio1<-bs1[sp,2,]/bs1[sp,1,]*100
  ratio1<-ratio1[!is.na(ratio1)]
  bs.sum[sp,1,]<-quantile(ratio1,c(0.5,0.05,0.95))

}

# Collect values from matrices into table
table <- data.frame(
  species_common_name = sort(unique(boot$species_common_name)),
  npairs = as.numeric(by(boot$series_num, boot$species_common_name, length)),
  half_images_as_pct_of_full_median = bs.sum[,1,1],
  half_images_as_pct_of_full_lci = bs.sum[,1,2],
  half_images_as_pct_of_full_uci = bs.sum[,1,3]
)

# Create table for Appendix in report
table_final <- boot |>
  group_by(species_common_name) |>
  # Calculate total number of images by camera/species
  summarise(total_images_half = sum(total_images_half_height),
            total_images_full = sum(total_images_full_height),
            avg_images_half = round(mean(total_images_half_height), digits = 2),
            avg_images_full = round(mean(total_images_full_height), digits = 2)) |>
  # Join bootstrapping results
  left_join(table, by = "species_common_name") |>
  mutate(factor_median = round(half_images_as_pct_of_full_median / 100, digits = 2),
         factor_lci = round(half_images_as_pct_of_full_lci / 100, digits = 2),
         factor_uci = round(half_images_as_pct_of_full_uci / 100, digits = 2)) |>
  select(1, 3, 2, 5, 4, 10, 11, 12)

#-----------------------------------------------------------------------------------------------------------------------

# Write results

write_csv(df_detections, paste0(g_drive, "Results/Comparisons/Heights/heights-experiment_independent-detections-all.csv"))

# Two versions - just BDT, and with all the data.
write_csv(table_final, paste0(g_drive, "Results/Comparisons/Heights/heights-experiment_summary-of-images-all.csv"))

#-----------------------------------------------------------------------------------------------------------------------
