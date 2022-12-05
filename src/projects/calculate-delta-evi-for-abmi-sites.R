#-----------------------------------------------------------------------------------------------------------------------

# Title:            Calculate EVI
# Description:      Pull MODIS data using GEE and compute EVI at ABMI sites

# Authors:          Marcus Becker
# Date:             September 2022

# Previous Scripts: None

#-----------------------------------------------------------------------------------------------------------------------

# Attach packages
library(rgee)
library(googleCloudStorageR)
library(jsonlite)
library(sf)
library(dplyr)

g_drive <- "G:/Shared drives/ABMI Camera Mammals/"
my_dir <- paste0(g_drive, "data/spatial/rgee")

#ee_install()
ee_Initialize(user = "mabecker", drive = TRUE, gcs = TRUE)

roi_big <- st_read(paste0(g_drive, "data/spatial/ab_prov.shp")) |>
  st_bbox() |>
  st_as_sfc() |>
  sf_as_ee()

#-----------------------------------------------------------------------------------------------------------------------

# This function loads ALL of the existing MOD13A1 data
modis_all <- ee$ImageCollection("MODIS/006/MOD13A1")

# Filter by the date
Modis_filtered <- modis_all$
  filter(ee$Filter$date("2013-07-01", "2013-10-01"))

# Get information about the dataset
ee_print(Modis_filtered)

##Mask and Visualize:
VizParams <- list(palette = c(
  'FFFFFF', 'CE7E45', 'DF923D', 'F1B555', 'FCD163', '99B718', '74A901',
  '66A000', '529400', '3E8601', '207401', '056201', '004C00', '023B01',
  '012E01', '011D01', '011301'
))

##Create a mask layer. We want all poor quality pixels (-1, 2, 3) to become 0s
getmask <- function(image) {

  mask_img_0 <- image$eq(0) #any pixel that does not equal 0 turns into 0. The 0s become 1s
  mask_img_1 <- image$eq(1) #any pixel that does not equal 1 turns into 0. The 1s stay 1s.

  return(mask_img1and2 <- mask_img_1$add(mask_img_0)) #add them together so all good quality pixels = 1

}

MOD13Q1_clean <- function(image) {
  #clip first to the roi - have to use clip bc filter bounds doesn't work for global composites
  #aka MODIS data
  clip_img <- image$clip(roi_big)

  #might as well do other things within this function.
  #extract the NDVI band
  ndvi_values <- clip_img$select("EVI") #add this to scale between -1 and 1 $multiply(0.0001)

  #extract the quality band
  ndvi_qa <- clip_img$select("SummaryQA")

  #select pixels to mask
  quality_mask <- getmask(ndvi_qa)

  #All quality pixels with value 0 (so poor quality pixels) become 0s
  ndvi_values$updateMask(quality_mask)

  ##I need to mask out water bodies.
  #hansenImage = ee.Image('UMD/hansen/global_forest_change_2015')
  hansenImage <- ee$Image("UMD/hansen/global_forest_change_2015")

  #// Select the land/water mask.
  #var datamask = hansenImage.select('datamask');
  datamask <- hansenImage$select('datamask')

  #// Create a binary mask.
  #var mask = datamask.eq(1);
  #We use eq(1) to create a binary image in which all the pixels
  #that do not have the value of 1 in the datamask band
  #(those that are water or no data) get a value of 0 in the resulting image
  Hansenmask<-datamask$eq(1)

  #Update the composite mask with the water mask.
  ndvi_values$updateMask(Hansenmask)

}

##Now map the function over the image collection so that each image is processed.
##We don't have to worry about the fact that our roi is a different projection
##than MODIS data. GEE figures this out for you and when you download the final
##rasters you can specify the projection you want
NDVI_cleaned <- Modis_filtered$map(MOD13Q1_clean)

##Summarize the data into median EVI in summer and fall

##SUMMER##
#July 1 - August 1
NDVI_compositeSummer <- modis_all$
  filter(ee$Filter$date("2013-07-01", "2013-08-01"))$
  map(MOD13Q1_clean)$
  reduce(ee$Reducer$median())

#View it on a map
Map$addLayer(NDVI_compositeSummer, VizParams)

##FALL##
#Sept 1 - October 1
NDVI_compositeFall <- modis_all$
  filter(ee$Filter$date("2013-09-01", "2013-10-01"))$
  map(MOD13Q1_clean)$
  reduce(ee$Reducer$median())

Map$addLayer(NDVI_compositeFall, VizParams)

Delta2013<-NDVI_compositeSummer$subtract(NDVI_compositeFall)
Map$addLayer(Delta2013, VizParams)

#To download a single image
#When you use a reducer function you have to specify the scale and projection when you
#download the layer or you end up with the default - WGS-84 projection with 1-degree resolution
#which is huge and useless
ee_as_raster(Delta2013,
             region = roi_big,
             dsn = file.path(my_dir, "Delta2013.tif"),
             via = "drive",
             crs = "SR-ORG:6974",
             scale = 231.6564)

library(raster)

ras_delta2013 <- raster(paste0(g_drive, "data/spatial/rgee/Delta2013.tif"))
crs <- crs(ras_delta2013)

sf_abmi_sites <- st_read(paste0(g_drive, "data/spatial/sites/abmi_cam_locations_new.shp")) |>
  # Buffer by 1000m (1km)
  st_buffer(dist = 1000)

# Project home ranges into same prj as raster
sf_abmi_sites <- sf_abmi_sites |>
  st_transform(crs = crs)

# Extract mean delta EVI for each sites
sf_abmi_sites$deltaevi2013 <- raster::extract(ras_delta2013, sf_abmi_sites, fun = mean, na.rm = TRUE)








