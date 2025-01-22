#load libraries
library(sf)
library(rgdal)
library(raster)
library(dplyr) 

print(sessionInfo())

# load shapefile
soil <- st_read("../southEastWI_soil.shp")
names(soil)
baseCRS <- raster("../../raster_base_projection.tif")
crs(baseCRS)

# subset shapefile data
sub <- soil %>%
  dplyr::select(c(nrrcpcl, geometry))
summary(sub)
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
sub_raster <- rasterize(subCRS, raster_blank, field = subCRS$nrrcpcl)
sub_raster

sub_raster <-  as(sub_raster, "SpatRaster")

# writeRaster(sub_raster, "southEastWI_landClass_30m.tiff",
#             format = "GTiff",
#             overwrite = TRUE,
#             NAflag = -9999)


rast2 <- terra::focal(sub_raster, 75, fun = 'modal', na.policy="only", na.rm=TRUE)

writeRaster(rast2, "southEastWI_landClass_30m_smooth.tiff",
            overwrite = TRUE,
            NAflag = -9999)






