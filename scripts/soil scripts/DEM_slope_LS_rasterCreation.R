# this script takes the DEM shapefiles for each learning hub that have been clipped in qgis
# and reprojects them into the correct crs and lays them out with the same grid as the soil rasters
# step 1. load DEM shapefile
# step 2. define CRS with raster base 
# step 3. load any soil shape from the region
# step 4. make blank raster with CRS and extent of soil shape
# step 5. resample the DEM to make elev 

# then create the slopePercent raster

# then create LS raster

library(terra)
library(dplyr)
library(sf)

 # elevation from DEM (fit to other rasters)------------------

# load base projection
CRS3857 <- rast("rasterOutputs/raster_base_projection.tif")
# load dem
dem = rast("rasterOutputs/southEastWI/southEastWI_DEM.tif")
# load one raster made from ssurgo data frame
shape = rast("rasterOutputs/southEastWI/grazeScape/southEastWI_slopelen_10m.tif")
#match crs and extent
crs(dem) <- terra::crs(shape)
ext(dem) <- terra::ext(shape)
# resample dem to align with ssurgo rasters
new_elev <- terra::resample(dem, shape, method = "bilinear")

filename = "rasterOutputs/southEastWI/grazeScape/southEastWI_elev_10m.tiff"
# write the raster
terra::writeRaster(new_elev, filename = filename,
            overwrite = TRUE,
            NAflag = -9999)

# dem to slope percent----------------------------

# load created raster 
dem <- rast("rasterOutputs/southEastWI/grazeScape/southEastWI_elev_10m.tiff")
# convert elevation  - feet to meters - 
dem_m <- dem * 0.3048
## convert to radians -------------------
slope_rad = terra::terrain(dem_m, v = 'slope', unit = 'radians', neighbors = 8)
summary(slope_rad)

## convert to percent  -----------------------------------------

slope_per <- app(slope_rad, fun = function(x) {round(tan(x)*100,2)})
slope_per
plot(slope_per)
filename = "rasterOutputs/southEastWI/grazeScape/southEastWI_slopePer_10m.tiff"
writeRaster(slope_per, filename,
            overwrite = TRUE,
            NAflag = -9999)

# slope and slopelength to LS--------------------

# load slopelength for the region and resolution
slopelen <- rast("rasterOutputs/southEastWI/grazeScape/southEastWI_slopelen_10m.tif")

# function for calculating factor
factorFun <- function(x) {
  ifelse(between(x, 3.01, 4), 0.4, 
         ifelse(between(x, 1, 3), 0.3,
                ifelse(x < 1, 0.2, 0.5)))
}

factor <- app(slope_per, fun = factorFun)

LSfun <- function(x,y,z) {
  round((((x/((10000+(x^2))^0.5))*4.56)+
           (x/(10000+(x^2))^0.5)^2*(65.41)+0.065)*((y*3.3)/72.6)^(z),2)
}

#LS <- lapp(slope_per, slopelen, factor, fun = LSfun)
LS <- LSfun(slope_per, slopelen, factor)

filename = paste0("rasterOutputs/southEastWI/grazeScape/southEastWI_LS_10m.tif")

writeRaster(LS, filename,
            overwrite = TRUE,
            NAflag = -9999)


