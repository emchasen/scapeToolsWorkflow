# This script joins the distance to open water and distance to streams, 
# takes the smaller distance at each grid cell, and then creates a new raster: distanceToWaterWays.

library(sf)
library(terra)
library(tidyverse)
library(raster)

#Load distance to stream and distance to water rasters

stream <- raster("waterData/southEastWI_distanceToStreams.tiff")
water <- raster("waterData/southEastWI_distanceToWater.tiff")

#Plot data
plot(stream)
plot(water)

## Examine extent and match extents
streamExt = extent(stream)
streamExt
waterExt = extent(water)
waterExt

## Merge rasters
#Because the extents are not equal, resample one into the other so that the extents and grid cells align.
raster_blank <- raster(crs = crs(water),
                       vals = 0,
                       resolution = c(30,30),
                       ext = waterExt)
raster_blank
# resample
new_stream <- resample(stream, raster_blank, method = "bilinear")

## Take min value between the two layers
#This step makes one raster that uses the smallest value at each grid cell between the two rasters.
minFun <- function(x,y) {
 pmin(x,y)
}
distRaster <- overlay(new_stream, water, fun = minFun)
distRaster
plot(distRaster)

## Resample and cut using smartscape raster
#The raster needs to align perfectly with all of the other rasters in smartscape
# crop to learning hub shapefil
lh <- st_read("soilShapes/southEastWI.shp")
streamCRS <- st_crs(stream)
lh_stream <- st_transform(lh, streamCRS)
#load smartscape raster to align grid cells
ssRaster <- raster("rasterOutputs/smartScape/southEastWI/modelInputs/southEastWI_awc_30m.tif")
ssRaster
distRaster
plot(ssRaster)
# crop raster to smartscaperaster
dist_crop <- crop(distRaster, extent(lh_stream))
plot(dist_crop)
dist_crop2 <- mask(dist_crop, lh_stream)
plot(dist_crop2)
ssExtent = extent(ssRaster)
blank_raster <- raster(crs = crs(ssRaster),
                       vals = 0,
                       resolution = c(30,30),
                       ext = ssExtent)
new_distance <- resample(dist_crop2, blank_raster, method = "bilinear")
plot(new_distance)
new_distance
ssRaster
writeRaster(new_distance, "rasterOutputs/smartScape/southEastWI/modelInputs/southEastWI_distanceToWaterWays.tif",
            format = "GTiff",
            overwrite = TRUE,
            NAflag = -9999)


