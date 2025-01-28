# this file transforms the Pdelivery rasters created by Eric to make sure that they match and overlap correctly with the rest of the rasters

library(terra)
library(dplyr)
library(sf)

# load pdelivery raster from research drive
p_southEast <- rast("/Volumes/cgratton/scapetools/spatial_inputs/Pdeliv_regions/new/PdelivFactor_30m_southEastWI.tif")

#load existing raster from the same region
southEastRast <- rast("rasterOutputs/smartScape/southEastWI/modelInputs/southEastWI_awc_30m.tif")

# check extents
p_southEast
southEastRast

plot(p_southEast)
plot(southEastRast)

#match crs and extent
crs(p_southEast) <- terra::crs(southEastRast)
ext(p_southEast) <- terra::ext(southEastRast)
# resample pdeliv to align with ssurgo rasters
new_pdeliv <- terra::resample(p_southEast, southEastRast, method = "bilinear")

# recheck
new_pdeliv
southEastRast

plot(new_pdeliv)
plot(southEastRast)

#write raster
filename = "rasterOutputs/smartScape/southEastWI/modelInputs/southEastWI_pDelivFactor_30m.tiff"
# write the raster
terra::writeRaster(new_pdeliv, filename = filename,
                   overwrite = TRUE,
                   NAflag = -9999)
