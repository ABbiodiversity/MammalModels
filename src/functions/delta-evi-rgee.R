#-----------------------------------------------------------------------------------------------------------------------

# Title:       Functions for using RGEE (Google Earth Engine)
# Description:
# Date:        December 2022
# Author:      Marcus Becker (Melanie Dickie)

#-----------------------------------------------------------------------------------------------------------------------

# Create a mask layer. We want all poor quality pixels (-1, 2, 3) to become 0s
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



