# this script is run on the remote computing service

library(sf)
library(rgdal)
library(tidyverse)
library(ggplot2)
library(raster)
library(sp)

# load openWater shapefile for learning hub

water <- st_read("eastCentralWI_openWater.shp")

# set the correct projection
baseRaster <- raster("../../stream/raster_base_projection.tif")
newCRS <- crs(baseRaster)
water_newCRS <- st_transform(water, crs = newCRS)

#Make blank raster
ext = extent(st_bbox(water_newCRS))
blank_raster <- raster(crs = newCRS,
                       vals = 0,
                       resolution = c(30,30),
                       ext = ext)
print(blank_raster)

#Convert to sp
#convert xym to xy spatial line
water_xy <- st_zm(water_newCRS$geometry)
water_xy
water_sp <- as_Spatial(water_xy)
print("water_sp completed")
water_sp

#Mask values in raster to the streams
maskedR <- mask(blank_raster, water_sp)
print("maskedR completed")
distanceToWater <- distance(maskedR)
print("distanceToWater completed")

writeRaster(distanceToWater, "eastCentralWI_distanceToWater.tiff",
            format = "GTiff",
            overwrite = TRUE,
            NAflag = -9999)