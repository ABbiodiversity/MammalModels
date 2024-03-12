#' Calculate total time in front of the camera, by deployment, project, and species.
#'
#' @param series Time by series dataframe
#' @param tbd Time by day summary; assume 'project_location' field.
#' @param snow Snow dates dataframe
#' @param season_spec Can be "new1", "new2", or "new3"
#'

sum_total_time <- function(series, tbd, season_spec) {

  series <- df_tt

  summer_green <- 143
  summer.start.j <- 106
  summer.end.j <- 288

  # Summarise total time

  if (season_spec == "new1") {

    tt <- series |>
      unite("project_location", project, location, sep = "_", remove = TRUE) |>
      mutate(julian = as.numeric(format(series_start, "%j"))) |>
      # Create season variables based on julian day, and calculate the proportion of individuals behind pole
      mutate(interval = interval(snow_start, snow_gone),
             intersect = series_start %within% interval,
             intersect = ifelse(is.na(intersect), "Not applicable", intersect),
             season_new1 = ifelse(series_start <= snow_gone, "snow", "nonsnow"),
             season_new1 = if_else(season_new1 == "snow" & intersect == FALSE, "nonsnow", season_new1),
             season_new1 = as.factor(ifelse(is.na(season_new1), "nonsnow", season_new1))) |>
      mutate_at(c("project_location", "species_common_name", "season_new1"), factor) |>
      group_by(project_location, species_common_name, season_new1, .drop = FALSE) |>
      summarise(total_duration = sum(series_total_time)) |>
      ungroup() |>
      mutate_if(is.factor, as.character) |>
      left_join(tbd, by = c("project_location"))

    # Unique species seen
    sp <- as.character(sort(unique(tt$species_common_name)))

    tt_nn <- tbd |>
      # Retrieve only those that had no images of animals
      anti_join(tt, by = "project_location") |>
      crossing(season_new1 = c("snow", "nonsnow"), species_common_name = sp) |>
      # Add total_duration column, which is zero in these cases
      mutate(total_duration = 0)

    tt_full <- tt |>
      bind_rows(tt_nn) |>
      arrange(project_location, species_common_name, season_new1) |>
      mutate(total_season_days = ifelse(season_new1 == "snow", total_snow_days, total_nonsnow_days)) |>
      separate(project_location, into = c("project", "location"), sep = "_", remove = TRUE, extra = "merge") |>
      select(project, location, species_common_name, season_new1, total_season_days, total_duration)

  } else if (season_spec == "new2") {

    tt <- series |>
      unite("project_location", project, location, sep = "_", remove = TRUE) |>
      mutate(julian = as.numeric(format(series_start, "%j"))) |>
      # Create season variables based on julian day, and calculate the proportion of individuals behind pole
      mutate(interval = interval(snow_start, snow_gone),
             intersect = series_start %within% interval,
             intersect = ifelse(is.na(intersect), "Not applicable", intersect),
             snow_gone_early = snow_gone %m-% days(10),
             season_new2 = ifelse(series_start <= snow_gone_early, "snow", "nonsnow"),
             season_new2 = if_else(season_new2 == "snow" & intersect == FALSE, "nonsnow", season_new2),
             season_new2 = as.factor(ifelse(is.na(season_new2), "nonsnow", season_new2))) |>
      mutate_at(c("project_location", "species_common_name", "season_new2"), factor) |>
      group_by(project_location, species_common_name, season_new2, .drop = FALSE) |>
      summarise(total_duration = sum(series_total_time)) |>
      ungroup() |>
      mutate_if(is.factor, as.character) |>
      left_join(tbd, by = c("project_location"))

    # Unique species seen
    sp <- as.character(sort(unique(tt$species_common_name)))

    tt_nn <- tbd |>
      # Retrieve only those that had no images of animals
      anti_join(tt, by = "project_location") |>
      crossing(season_new2 = c("snow", "nonsnow"), species_common_name = sp) |>
      # Add total_duration column, which is zero in these cases
      mutate(total_duration = 0)

    tt_full <- tt |>
      bind_rows(tt_nn) |>
      arrange(project_location, species_common_name, season_new2) |>
      mutate(total_season_days = ifelse(season_new2 == "snow", total_snow_days, total_nonsnow_days)) |>
      separate(project_location, into = c("project", "location"), sep = "_", remove = TRUE, extra = "merge") |>
      select(project, location, species_common_name, season_new2, total_season_days, total_duration)

  } else if (season_spec == "new3") {

    tt <- series |>
      unite("project_location", project, location, sep = "_", remove = TRUE) |>
      mutate(julian = as.numeric(format(series_start, "%j"))) |>
      # Create season variables based on julian day, and calculate the proportion of individuals behind pole
      mutate(interval = interval(snow_start, snow_gone),
             intersect = series_start %within% interval,
             intersect = ifelse(is.na(intersect), "Not applicable", intersect),
             snow_gone_early = snow_gone %m-% days(10),
             julian_sge = as.numeric(format(ymd(snow_gone_early), "%j")),
             season_new3 = case_when(
               julian >= julian_sge & julian <= summer_green ~ "spring",
               julian < julian_sge ~ "winter",
               julian > summer_green ~ "summer"),
             season_new3 = as.factor(ifelse(is.na(season_new3), "spring", season_new3))) |>
      mutate_at(c("project_location", "species_common_name", "season_new3"), factor) |>
      group_by(project_location, species_common_name, season_new3, .drop = FALSE) |>
      summarise(total_duration = sum(series_total_time)) |>
      ungroup() |>
      mutate_if(is.factor, as.character) |>
      left_join(tbd, by = c("project_location"))

    # Unique species seen
    sp <- as.character(sort(unique(tt$species_common_name)))

    tt_nn <- tbd |>
      # Retrieve only those that had no images of animals
      anti_join(tt, by = "project_location") |>
      crossing(season_new3 = c("winter", "spring", "summer"), species_common_name = sp) |>
      # Add total_duration column, which is zero in these cases
      mutate(total_duration = 0)

    tt_full <- tt |>
      bind_rows(tt_nn) |>
      arrange(project_location, species_common_name, season_new3) |>
      mutate(total_season_days = ifelse(season_new3 == "snow", total_snow_days, total_nonsnow_days)) |>
      separate(project_location, into = c("project", "location"), sep = "_", remove = TRUE, extra = "merge") |>
      select(project, location, species_common_name, season_new1, total_season_days, total_duration)

  }

  # Unique species seen
  sp <- as.character(sort(unique(tt$species_common_name)))

  tt_nn <- tbd |>
    # Retrieve only those that had no images of animals
    anti_join(tt, by = "project_location") |>
    crossing(season = c("summer", "winter"), common_name = sp) |>
    # Add total_duration column, which is zero in these cases
    mutate(total_duration = 0)

  tt_full <- tt |>
    bind_rows(tt_nn) |>
    arrange(project_location, species_common_name, season) |>
    mutate(total_season_days = ifelse(season == "summer", total_summer_days, total_winter_days)) |>
    separate(project_location, into = c("project", "location"), sep = "_", remove = TRUE, extra = "merge") |>
    select(project, location, species_common_name, season, total_season_days, total_duration)

  return(tt_full)

}
