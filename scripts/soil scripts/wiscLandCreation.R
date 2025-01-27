# this script creates a wiscland raster for the state of WI
# starting with the original wiscland layer (level 4), 
# then reclassifies it to only include the landcovers
# of interest in smartscape

library(raster)
library(sf)
library(terra)
library(tidyverse)


# this part is completed------------------------
# # load original wiscland level 4 raster
# land <- raster("WI/wiscland2/wiscland2_dataset/level4/wiscland2_level4.tif")
# #remove attribute table
# land <- setValues(raster(land), land[]) # makes the raster workable in R
# 
# # create reclass matrix
# rcl <- c(999, 1190, 1, # high density urban
#          1199, 1201, 2, # low density urban
#          2109, 2111, 3, # cg https://p.widencdn.net/8ghipa/Wiscland_2_User_Guide_September_2016
#          2119, 2121, 4, # cc https://p.widencdn.net/8ghipa/Wiscland_2_User_Guide_September_2016 (pg 49)
#          2129, 2131, 5, # dr
#          2139, 2141, 6, # potato/vegetable
#          2199, 2201, 7, # cranberry
#          3109, 3111, 8, # hay
#          3119, 3121, 9, # pasture
#          3199, 3900, 10, # grassland
#          3999, 4900, 11, # forest
#          4999, 5001, 12, # water
#          5999, 6500, 13, # wetland
#          6999, 7001, 14, # barren
#          7999, 8001, 15, # shrubland
#          8002, 65535, NA)
# rclmat <- matrix(rcl, ncol = 3, byrow = TRUE)
# 
# land_recl <- reclassify(land, rclmat)
# plot(land_recl)
# 
# writeRaster(land_recl, "wi/WL_reclass.tif",
#             NAflag = -9999,
#             format = "GTiff")
# 
# load base raster for crs
baseRaster = raster("rasterInputs/raster_base_projection.tif")
newCRS = crs(baseRaster)
#newLand = projectRaster(land_recl, crs = newCRS, method = "ngb")

# writeRaster(newLand, "wi/WL_reclass_3857.tif",
#             NAflag = -9999,
#             format = "GTiff",
#             overwrite = TRUE)


# next, load the WL_reclass_3857.tif into qgis or arcgis and clip to region
# in order to resample to fit with other raster layers

# start here------------------------
# load clipped wiscland layer

wl_region <- raster("rasterInputs/southEastWI_WL_3857.tif") 

summary(wl_region)

# load smartscape soil layer raster previously created for region

baseShape <- raster("rasterOutputs/smartScape/southEastWI/modelInputs/southEastWI_slopelen_30m.tiff")

# reproject into new raster with correct res
raster_blank <- raster(crs = crs(newCRS),
                       vals = 0,
                       resolution = c(30,30),
                       ext = extent(baseShape))
raster_blank

wl_newRes <- raster::resample(wl_region, raster_blank, method = "ngb")
plot(wl_newRes)
wl_newRes

# create filename: 
filename = paste0("rasterOutputs/smartScape/southEastWI/modelInputs/southEastWI_WiscLand_30m.tiff")
# resave wiscland raster
raster::writeRaster(wl_newRes, 
                    filename = filename,
                    NAflag = -9999,
                    format = 'GTiff', overwrite = TRUE)
