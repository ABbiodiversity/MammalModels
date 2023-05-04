#-----------------------------------------------------------------------------------------------------------------------

# Title:       Create a gif for camera trap images
# Description:
# Date:        May 2023
# Author:      Marcus Becker

#-----------------------------------------------------------------------------------------------------------------------

# Install required packages if needed
if (!requireNamespace("magick"))
  install.packages("magick")
if (!requireNamespace("purrr"))
  install.packages("purrr")

# Attach required packages
library(purrr)
library(magick)

#-----------------------------------------------------------------------------------------------------------------------

#' Create a gif
#' Using a folder with a group of images, create and save a gif
#'
#' @param images_folder The path to the folder where the target images are
#' @param file_type png or jpg
#' @param fps How many frames (images) per second for the gif
#' @param gif_name The name of the output gif file
#' @param gif_folder The path to the folder where you want the gif to be saved
#'

create_gif <- function(images_folder, file_type, fps, gif_name, gif_folder) {

  if (file_type == "jpg") {
    ext <- "*.jpg"
  } else {
    ext <- "*.png"
  }

  list.files(path = images_folder, pattern = ext, full.names = TRUE) |>
    purrr::map(image_read) |>
    image_join() |>
    image_animate(fps = fps) |>
    image_write(paste0(gif_folder, "/", gif_name, ".gif"))

}



