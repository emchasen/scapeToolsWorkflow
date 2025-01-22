#load libraries
library(sf)
library(rgdal)
library(raster)
library(dplyr) 
library(terra)

# load shapefile
soil <- st_read("../southEastWI_soil.shp")
names(soil)
baseCRS <- raster("../../raster_base_projection.tif")
crs(baseCRS)

# subset shapefile data
sub <- soil %>%
  dplyr::select(c(hydgrp, geometry)) %>%
  mutate(hydgrp.num = recode(hydgrp,
                             "A" = "1",
                             "A/D" = "1.5",
                             "B" = "2",
                             "B/D" = "2.5",
                             "C" = "3",
                             "C/D" = "3.5",
                             "D" = '4')) %>%
  mutate(hydgrp.num = as.numeric(hydgrp.num))
subCRS <- st_transform(sub, crs = crs(baseCRS))
summary(subCRS)
levels(as.factor(subCRS$hydgrp.num))

# make blank raster
ext = extent(st_bbox(subCRS))
CRS3857 <- crs(baseCRS)
raster_blank <- raster(crs = CRS3857,
                       vals = 0,
                       resolution = c(10,10),
                       ext = ext)
raster_blank

# rasterize shapefile
sub_raster <- rasterize(subCRS, raster_blank, field = subCRS$hydgrp.num)
sub_raster
sub_raster <-  as(sub_raster, "SpatRaster")
# writeRaster(sub_raster, "southEastWI_hydgrp_10m.tiff",
#             format = "GTiff",
#             overwrite = TRUE,
#             NAflag = -9999)


rast2 <- terra::focal(sub_raster, 75, fun = 'modal', na.policy="only", na.rm=TRUE)

writeRaster(rast2, "southEastWI_hydgrp_10m.tiff",
            overwrite = TRUE,
            NAflag = -9999)







