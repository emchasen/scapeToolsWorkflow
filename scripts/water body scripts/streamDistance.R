# this script is run on the remote computing service

library(sf)
library(rgdal)
library(tidyverse)
library(ggplot2)
library(raster)
library(sp)

# load data
Per_stream <- st_read("southEastWI_perStreams.shp")
summary(Per_stream)

# reproject into base raster
baseRaster <- raster("../raster_base_projection.tif")
newCRS <- crs(baseRaster)
stream_newCRS <- st_transform(Per_stream, crs = newCRS)
print(stream_newCRS)
#stream_sub <- stream_newCRS[1:10,]

# make blank raster
ext = extent(st_bbox(stream_newCRS))
#ext = extent(st_bbox(stream_sub))
blank_raster <- raster(crs = newCRS,
                       vals = 0,
                       resolution = c(30,30),
                       ext = ext)
print(blank_raster)

# convert stream to sp
#convert xym to xy spatial line
stream_xy <- st_zm(stream_newCRS$geometry)
#stream_xy <- st_zm(stream_sub$geometry)
print("stream_xy created")
stream_xy
stream_sp <- as_Spatial(stream_xy)
print("spatial stream created")
stream_sp

#mask values in raster to the streams
maskedR <- mask(blank_raster, stream_sp)
print("maskedR created")
distanceToStream <- distance(maskedR)
print("distance matrix created")

writeRaster(distanceToStream, "southEastWI_distanceToStreams.tiff",
            format = "GTiff",
            overwrite = TRUE,
            NAflag = -9999)
