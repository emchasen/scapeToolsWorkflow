#load libraries
library(sf)
library(raster)
library(tidyverse)
library(terra)

print(sessionInfo())

# load shapefile
soil <- st_read("../southEastWI_soil.shp")
names(soil)
baseCRS <- raster("../../raster_base_projection.tif")
crs(baseCRS)

# subset shapefile data
sub <- soil %>%
  dplyr::select(c(drangcl, geometry))
summary(sub)
sub <- sub %>%
  mutate(drainage = recode(drangcl,
                           "Excessively drained" = "1", #
                           "Moderately well drained" = "2",
                           "Poorly drained" = "3",
                           "Somewhat excessively drained" = "4", #
                           "Somewhat poorly drained" = "5",
                           "Very poorly drained" = "6",
                           "Well drained" = "7")) %>%
  mutate(drainage = as.numeric(drainage))
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
sub_raster <- rasterize(subCRS, raster_blank, field = subCRS$drainage)
sub_raster
sub_raster <-  as(sub_raster, "SpatRaster")

# 
# writeRaster(sub_raster, "southEastWI_drainClass_30m.tiff",
#             format = "GTiff",
#             overwrite = TRUE,
#             NAflag = -9999)

drain2 <- terra::focal(sub_raster, 75, fun = 'modal', na.policy="only", na.rm=TRUE)

writeRaster(drain2, "southEastWI_drainClass_30m.tiff",
            overwrite = TRUE,
            NAflag = -9999)






