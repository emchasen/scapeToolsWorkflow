#load libraries
library(sf)
library(rgdal)
library(raster)
library(dplyr) 

print(sessionInfo())

# load shapefile
soil <- st_read("../southEastWISoilN.shp")
names(soil)
baseCRS <- raster("../../raster_base_projection.tif")
print("loaded raster base")
crs(baseCRS)


# subset shapefile data
sub <- soil %>%
  dplyr::select(c(nrespns, geometry))
print("selected nresponse")
summary(sub)
sub <- sub %>%
  mutate(nresponse = recode(nrespns,
                "H" = "1",
                "M" = "2",
                "S" = "3")) %>%
  mutate(nresponse = as.numeric(nresponse))
print("recoded n response")
# transform soil to baseCRS
subCRS <- st_transform(sub, crs = crs(baseCRS))

# make blank raster
ext = extent(st_bbox(subCRS))
CRS3857 <- crs(baseCRS)
raster_blank <- raster(crs = CRS3857,
                       vals = 0,
                       resolution = c(10,10),
                       ext = ext)
print("made blank raster")
raster_blank

# rasterize shapefile
sub_raster <- rasterize(subCRS, raster_blank, field = subCRS$nresponse)
print("rasterized")
sub_raster



# writeRaster(sub_raster, "southEastWI_nResponse_10m.tiff",
#             format = "GTiff",
#             overwrite = TRUE,
#             NAflag = -9999)

# fill in raster holes-----------------
print("filling in holes")
sub_raster <-  as(sub_raster, "SpatRaster")
sub_raster.2 <- terra::focal(sub_raster, 75, fun = 'modal', na.policy="only", na.rm=TRUE)
sub_raster.2



writeRaster(sub_raster.2, "southEastWI_nResponse_10m_smooth.tiff",
            overwrite = TRUE,
            NAflag = -9999)







