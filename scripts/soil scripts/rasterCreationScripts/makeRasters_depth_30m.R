#load libraries
library(sf)
library(raster)
library(tidyverse)

print(sessionInfo())

# load shapefile
soil <- st_read("../southEastWI_soil.shp")
names(soil)
baseCRS <- raster("../../raster_base_projection.tif")
crs(baseCRS)


# subset shapefile data
sub <- soil %>%
  dplyr::select(c(ttl_dpt, geometry))
summary(sub)
#plot(sub)
# transform soil to baseCRS
subCRS <- st_transform(sub, crs = crs(baseCRS))


# make blank raster
ext = extent(st_bbox(subCRS))
CRS3857 <- crs(baseCRS)
raster_blank <- raster(crs = CRS3857,
                       vals = 0,
                       resolution = c(30,30),
                       ext = ext)
raster_blank

# rasterize shapefile
sub_raster <- rasterize(subCRS, raster_blank, field = subCRS$ttl_dpt)
sub_raster

# fill in raster holes-----------------
sub_raster.2 <- focal(sub_raster, w = matrix(1,75,75), fun = mean, 
                 pad = TRUE, na.rm = TRUE, NAonly = TRUE)
sub_raster.2

sub_raster.3 <- focal(sub_raster.2, w = matrix(1,75,75), fun = mean, 
                      pad = TRUE, na.rm = TRUE, NAonly = TRUE)
sub_raster.3


writeRaster(sub_raster.3, "southEastWI_depth_30m.tiff",
            format = "GTiff",
            overwrite = TRUE,
            NAflag = -9999)









