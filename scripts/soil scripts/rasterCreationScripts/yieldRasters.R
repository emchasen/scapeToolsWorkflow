#load libraries
library(sf)
library(terra)
library(tidyverse)
library(raster)

soy <- raster("rasterOutputs/soyYield_rfPred_2008to2017_x10_northCentralUS.tif")
corn <- raster("rasterOutputs/cornYield_rfPred_2008to2017-x10_northCentralUS.TIF")
baseRaster <- raster("rasterOutputs/raster_base_projection.tif")

# southEastWI-----------------

# load area shapefile
southEastShape <- st_read("soilShapes/southEastWI.shp")
# crop soy and corn to shape
southEast_yield <- st_transform(southEastShape, crs = crs(soy))
southEastSoy <- crop(soy, extent(southEast_yield))
plot(southEastSoy)
southEastSoy_mask <- mask(southEastSoy, southEast_yield)
plot(southEastSoy_mask)
southEastSoy_3857 <- projectRaster(southEastSoy_mask, crs = crs(baseRaster))
plot(southEastSoy_3857)
southEastCorn <- crop(corn, extent(southEast_yield))
plot(southEastCorn)
southEastCorn_mask <- mask(southEastCorn, southEast_yield)
plot(southEastCorn_mask)
southEastCorn_3857 <- projectRaster(southEastCorn_mask, crs = crs(baseRaster))
plot(southEastCorn_3857)
# fit into blank raster
# load raster for fitting
southEastRaster_10 <- raster("rasterOutputs/southEastWI/grazeScape/southEastWI_slopelen_10m.tif")
extPrj <- extent(southEastRaster_10)
blankRaster <- raster(crs = crs(baseRaster),
                      vals = 0,
                      resolution = c(10, 10),
                      ext = extPrj)
corn_new <- resample(southEastCorn_3857, blankRaster, method = "bilinear")
writeRaster(corn_new, "rasterOutputs/southEastWI/grazeScape/southEastWI_corn_10m.tif",
            overwrite = TRUE,
            NAflag = -9999)

soy_new <- resample(southEastSoy_3857, blankRaster, method = 'bilinear')
writeRaster(soy_new, "rasterOutputs/southEastWI/grazeScape/southEastWI_soy_10m.tif",
            overwrite = TRUE,
            NAflag = -9999)
# for smartscape
southEastRaster_30 <- raster("rasterOutputs/southEastWI/smartScape/southEastWI_slopelen_30m.tif")
extPrj <- extent(southEastRaster_30)
blankRaster <- raster(crs = crs(baseRaster),
                      vals = 0,
                      resolution = c(30, 30),
                      ext = extPrj)
corn_new <- resample(southEastCorn_3857, blankRaster, method = "bilinear")
writeRaster(corn_new, "rasterOutputs/southEastWI/smartScape/southEastWI_corn_30m.tif",
            format = "GTiff",
            overwrite = TRUE,
            NAflag = -9999)
soy_new <- resample(southEastSoy_3857, blankRaster, method = 'bilinear')
writeRaster(soy_new, "rasterOutputs/southEastWI/smartScape/southEastWI_soy_30m.tif",
            format = "GTiff",
            overwrite = TRUE,
            NAflag = -9999)


# eastCentralWI-----------------

# load area shapefile
eastCentralShape <- st_read("soilShapes/eastCentralWI.shp")
# crop soy and corn to shape
eastCentral_yield <- st_transform(eastCentralShape, crs = crs(soy))
eastCentralSoy <- crop(soy, extent(eastCentral_yield))
plot(eastCentralSoy)
eastCentralSoy_mask <- mask(eastCentralSoy, eastCentral_yield)
plot(eastCentralSoy_mask)
eastCentralSoy_3857 <- projectRaster(eastCentralSoy_mask, crs = crs(baseRaster))
eastCentralSoy_3857
eastCentralCorn <- crop(corn, extent(eastCentral_yield))
plot(eastCentralCorn)
eastCentralCorn_mask <- mask(eastCentralCorn, eastCentral_yield)
plot(eastCentralCorn_mask)
eastCentralCorn_3857 <- projectRaster(eastCentralCorn_mask, crs = crs(baseRaster))
eastCentralCorn_3857
# fit into blank raster
# load raster for fitting
eastCentralRaster_10 <- raster("rasterOutputs/eastCentralWI/grazeScape/eastCentralWI_slopelen_10m.tiff")
extPrj <- extent(eastCentralRaster_10)
blankRaster <- raster(crs = crs(baseRaster),
                      vals = 0,
                      resolution = c(10, 10),
                      ext = extPrj)
corn_new <- resample(eastCentralCorn_3857, blankRaster, method = "bilinear")
writeRaster(corn_new, "rasterOutputs/eastCentralWI/grazeScape/eastCentralWI_corn_10m.tif",
            overwrite = TRUE,
            NAflag = -9999)

soy_new <- resample(eastCentralSoy_3857, blankRaster, method = 'bilinear')
writeRaster(soy_new, "rasterOutputs/eastCentralWI/grazeScape/eastCentralWI_soy_10m.tif",
            overwrite = TRUE,
            NAflag = -9999)
# for smartscape
eastCentralRaster_30 <- raster("rasterOutputs/eastCentralWI/smartScape/eastCentralWI_slopelen_30m.tiff")
extPrj <- extent(eastCentralRaster_30)
blankRaster <- raster(crs = crs(baseRaster),
                      vals = 0,
                      resolution = c(30, 30),
                      ext = extPrj)
corn_new <- resample(eastCentralCorn_3857, blankRaster, method = "bilinear")
writeRaster(corn_new, "rasterOutputs/eastCentralWI/smartScape/eastCentralWI_corn_30m.tif",
            format = "GTiff",
            overwrite = TRUE,
            NAflag = -9999)
soy_new <- resample(eastCentralSoy_3857, blankRaster, method = 'bilinear')
writeRaster(soy_new, "rasterOutputs/eastCentralWI/smartScape/eastCentralWI_soy_30m.tif",
            format = "GTiff",
            overwrite = TRUE,
            NAflag = -9999)


# northeast WI--------------------------

# load area shapefile
northeastShape <- st_read("../Soil Data/areaShapefiles/northeastWI.shp")
# crop soy and corn to shape
northeast_yield <- st_transform(northeastShape, crs = crs(soy))
northeastSoy <- crop(soy, extent(northeast_yield))
plot(northeastSoy)
northeastSoy_mask <- mask(northeastSoy, northeast_yield)
plot(northeastSoy_mask)
northeastSoy_3857 <- projectRaster(northeastSoy_mask, crs = crs(baseRaster))
northeastSoy_3857
northeastCorn <- crop(corn, extent(northeast_yield))
plot(northeastCorn)
northeastCorn_mask <- mask(northeastCorn, northeast_yield)
plot(northeastCorn_mask)
northeastCorn_3857 <- projectRaster(northeastCorn_mask, crs = crs(baseRaster))
northeastCorn_3857
# fit into blank raster
# load raster for fitting
northeastRaster_10 <- raster("../spatialDataLayers/grazeScape/northeastWI/northeastWI_awc_10m.tif")
extPrj <- extent(northeastRaster_10)
blankRaster <- raster(crs = crs(baseRaster),
                      vals = 0,
                      resolution = c(10, 10),
                      ext = extPrj)
corn_new <- resample(northeastCorn_3857, blankRaster, method = "bilinear")
writeRaster(corn_new, "../spatialDataLayers/grazeScape/northeastWI/northeastWI_corn_10m.tif",
            format = "GTiff",
            overwrite = TRUE,
            NAflag = -9999)
soy_new <- resample(northeastSoy_3857, blankRaster, method = 'bilinear')
writeRaster(soy_new, "../spatialDataLayers/grazeScape/northeastWI/northeastWI_soy_10m.tif",
            format = "GTiff",
            overwrite = TRUE,
            NAflag = -9999)
# for smartscape
blankRaster <- raster(crs = crs(baseRaster),
                      vals = 0,
                      resolution = c(30, 30),
                      ext = extPrj)
corn_new <- resample(northeastCorn_3857, blankRaster, method = "bilinear")
writeRaster(corn_new, "../spatialDataLayers/smartScape/northeastWI/northeastWI_corn_30m.tif",
            format = "GTiff",
            overwrite = TRUE,
            NAflag = -9999)
soy_new <- resample(northeastSoy_3857, blankRaster, method = 'bilinear')
writeRaster(soy_new, "../spatialDataLayers/smartScape/northeastWI/northeastWI_soy_30m.tif",
            format = "GTiff",
            overwrite = TRUE,
            NAflag = -9999)

# uplands WI ----------------------------

# load area shapefile
uplandsShape <- st_read("../Soil Data/areaShapefiles/uplandsWI.shp")
# crop soy and corn to shape
uplands_yield <- st_transform(uplandsShape, crs = crs(soy))
uplandsSoy <- crop(soy, extent(uplands_yield))
plot(uplandsSoy)
uplandsSoy_mask <- mask(uplandsSoy, uplands_yield)
plot(uplandsSoy_mask)
uplandsSoy_3857 <- projectRaster(uplandsSoy_mask, crs = crs(baseRaster))
uplandsSoy_3857
uplandsCorn <- crop(corn, extent(uplands_yield))
plot(uplandsCorn)
uplandsCorn_mask <- mask(uplandsCorn, uplands_yield)
plot(uplandsCorn_mask)
uplandsCorn_3857 <- projectRaster(uplandsCorn_mask, crs = crs(baseRaster))
uplandsCorn_3857
# fit into blank raster
# load raster for fitting
uplandsRaster_10 <- raster("../spatialDataLayers/grazeScape/uplandsWI_ZJH_reexports/uplandsWI_clay_10m.tif")
uplandsRaster_10
extPrj <- extent(uplandsRaster_10)
blankRaster <- raster(crs = crs(baseRaster),
                      vals = 0,
                      resolution = c(10, 10),
                      ext = extPrj)
blankRaster
corn_new <- resample(uplandsCorn_3857, blankRaster, method = "bilinear")
plot(corn_new)
corn_new
writeRaster(corn_new, "../spatialDataLayers/grazeScape/uplandsCrops/uplandsWI_corn_10m.tif",
            format = "GTiff",
            overwrite = TRUE,
            NAflag = -9999)
soy_new <- resample(uplandsSoy_3857, blankRaster, method = 'bilinear')
soy_new
writeRaster(soy_new, "../spatialDataLayers/grazeScape/uplandsCrops/uplandsWI_soy_10m.tif",
            format = "GTiff",
            overwrite = TRUE,
            NAflag = -9999)
# for smartscape
blankRaster <- raster(crs = crs(baseRaster),
                      vals = 0,
                      resolution = c(30, 30),
                      ext = extPrj)
corn_new <- resample(uplandsCorn_3857, blankRaster, method = "bilinear")
writeRaster(corn_new, "../spatialDataLayers/smartScape/uplandsWI/uplandsWI_corn_30m.tif",
            format = "GTiff",
            overwrite = TRUE,
            NAflag = -9999)
soy_new <- resample(uplandsSoy_3857, blankRaster, method = 'bilinear')
writeRaster(soy_new, "../spatialDataLayers/smartScape/uplandsWI/uplandsWI_soy_30m.tif",
            format = "GTiff",
            overwrite = TRUE,
            NAflag = -9999)

# redcedar WI--------------------------

# load area shapefile
rcShape <- st_read("../Soil Data/areaShapefiles/redCedarWI.shp")
# crop soy and corn to shape
rc_yield <- st_transform(rcShape, crs = crs(soy))
rcSoy <- crop(soy, extent(rc_yield))
plot(rcSoy)
rcSoy_mask <- mask(rcSoy, rc_yield)
plot(rcSoy_mask)
rcSoy_3857 <- projectRaster(rcSoy_mask, crs = crs(baseRaster))
rcSoy_3857
rcCorn <- crop(corn, extent(rc_yield))
plot(rcCorn)
rcCorn_mask <- mask(rcCorn, rc_yield)
plot(rcCorn_mask)
rcCorn_3857 <- projectRaster(rcCorn_mask, crs = crs(baseRaster))
rcCorn_3857
# fit into blank raster
# load raster for fitting
rcRaster_10 <- raster("../spatialDataLayers/grazeScape/redCedarWI/redCedarWI_awc_10m.tif")
extPrj <- extent(rcRaster_10)
blankRaster <- raster(crs = crs(baseRaster),
                      vals = 0,
                      resolution = c(10, 10),
                      ext = extPrj)
corn_new <- resample(rcCorn_3857, blankRaster, method = "bilinear")
corn_new
plot(corn_new)
writeRaster(corn_new, "../spatialDataLayers/grazeScape/redCedarWI/redCedarWI_corn_10m.tif",
            format = "GTiff",
            overwrite = TRUE,
            NAflag = -9999)
soy_new <- resample(rcSoy_3857, blankRaster, method = 'bilinear')
soy_new
plot(soy_new)
writeRaster(soy_new, "../spatialDataLayers/grazeScape/redCedarWI/redCedarWI_soy_10m.tif",
            format = "GTiff",
            overwrite = TRUE,
            NAflag = -9999)
# for smartscape

blankRaster <- raster(crs = crs(baseRaster),
                      vals = 0,
                      resolution = c(30, 30),
                      ext = extPrj)
corn_new <- resample(rcCorn_3857, blankRaster, method = "bilinear")
corn_new
plot(corn_new)
writeRaster(corn_new, "../spatialDataLayers/smartScape/redCedarWI/redCedarWI_corn_30m.tif",
            format = "GTiff",
            overwrite = TRUE,
            NAflag = -9999)
soy_new <- resample(rcSoy_3857, blankRaster, method = 'bilinear')
writeRaster(soy_new, "../spatialDataLayers/smartScape/redCedarWI/redCedarWI_soy_30m.tif",
            format = "GTiff",
            overwrite = TRUE,
            NAflag = -9999)

# pineRiver MN--------------------------

# load area shapefile
prShape <- st_read("../Soil Data/areaShapefiles/pineRiverMN.shp")
# crop soy and corn to shape
pr_yield <- st_transform(prShape, crs = crs(soy))
prSoy <- crop(soy, extent(pr_yield))
plot(prSoy)
prSoy_mask <- mask(prSoy, pr_yield)
plot(prSoy_mask)
prSoy_3857 <- projectRaster(prSoy_mask, crs = crs(baseRaster))
prSoy_3857
prCorn <- crop(corn, extent(pr_yield))
plot(prCorn)
prCorn_mask <- mask(prCorn, pr_yield)
plot(prCorn_mask)
prCorn_3857 <- projectRaster(prCorn_mask, crs = crs(baseRaster))
prCorn_3857
# fit into blank raster
# load raster for fitting
prRaster_10 <- raster("/Volumes/My Passport/grassland2.0storage/spatialDataLayers/grazeScape/pineRiverMN/pineRiverMN_awc_10m.tif")
extPrj <- extent(prRaster_10)
blankRaster10 <- raster(crs = crs(baseRaster),
                        vals = 0,
                        resolution = c(10, 10),
                        ext = extPrj)
corn_new <- resample(prCorn_3857, blankRaster10, method = "bilinear")
corn_new
plot(corn_new)
writeRaster(corn_new, "../spatialDataLayers/grazeScape/pineRiverMN/pineRiverMN_corn_10m.tif",
            format = "GTiff",
            overwrite = TRUE,
            NAflag = -9999)
soy_new <- resample(prSoy_3857, blankRaster10, method = 'bilinear')
soy_new
plot(soy_new)
writeRaster(soy_new, "../spatialDataLayers/grazeScape/pineRiverMN/pineRiverMN_soy_10m.tif",
            format = "GTiff",
            overwrite = TRUE,
            NAflag = -9999)
# for smartscape

blankRaster30 <- raster(crs = crs(baseRaster),
                        vals = 0,
                        resolution = c(30, 30),
                        ext = extPrj)
corn_new <- resample(prCorn_3857, blankRaster30, method = "bilinear")
corn_new
plot(corn_new)
writeRaster(corn_new, "../spatialDataLayers/smartScape/pineRiverMN/corn_Yield_pineRiverMN.tif",
            format = "GTiff",
            overwrite = TRUE,
            NAflag = -9999)
soy_new <- resample(prSoy_3857, blankRaster30, method = 'bilinear')
writeRaster(soy_new, "../spatialDataLayers/smartScape/pineRiverMN/soy_Yield_pineRiverMN.tif",
            format = "GTiff",
            overwrite = TRUE,
            NAflag = -9999)

