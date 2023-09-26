#-----------------------------------------------------------------------------------------------------------------------

# Write new data and archive old data objects

#' @param data The data object
#' @param type Can be "tt", "tbd", "dl", or "dw"
#' @param project Projects
#' @param years Years

library(fs)

write_and_archive <- function(data, type, project, years) {

  g_drive <- "G:/Shared drives/ABMI Camera Mammals/"
  date <- Sys.Date()

  # Establish relevant path and file name
  if (type == "tt") {
    path <- "data/processed/time-in-cam-fov/"
    file <- "_fov-time-long_"
  }

  if (type == "tbd") {
    path <- "data/processed/time-by-day/"
    file <- "_tbd-summary_"
  }

  if (type == "dl") {
    path <- "results/density/deployments/"
    file <- "_density_long_"
  }

  if (type == "dw") {
    path <- "results/density/deployments/"
    file <- "_density_wide_"
  }

  # Write new data
  write_csv(data, paste0(g_drive, path, project, years, file, date, ".csv"))

  # List files
  files <- list.files(path = paste0(g_drive, path), full.names = TRUE) |>
    str_subset(pattern = paste0(project, years, file))

  # Find the most recent file (should always have date appended at the end)
  most_recent <- files |>
    str_sub(start = -14, end = -5) |> # Remember that *.csv is included in the position
    max()

  # List all the older files by removing most recent
  old_files <- files |>
    str_subset(pattern = most_recent, negate = TRUE)

  # Move them to archive folder
  file_move(old_files, paste0(g_drive, path, "archive"))

}

#-----------------------------------------------------------------------------------------------------------------------

