# this script creates soil property rasters for the different regions 
# used in grazescape (10 m res) or smartscape (30 m res).

# different regions include:
# cloverBeltWI (counties Marathon, Clark, Taylor)
# southWestWI (counties Crawford, Vernon, Richland, Monroe, Lacrosse)
# uplandsWI (counties Sauk, Lafayette, Green, Grant, Iowa)
# northeastWI (counties Brown, Manitowoc, Kewaunee)
# eastCentralWI (Oconto, Outagamie, Shawano, Winnebago)

# variables include: 
# awc, clay, depth = ttl_dpt, kfact = k, landclass = nrrcpcl,
# ksat, om, ph, sand, silt, slopelength = slplns_

# load libraries
library(sf)
library(terra)
library(tidyverse) 

# you can load in an example soil shape file first to make sure that the variable
# is named correctly

#list of regions
regions <- c("southWestWI", "northeastWI", "cloverBeltWI", "uplandsWI")
# resolutions
res <- c(10, 30)

# load in base raster for correct CRS
baseCRS <- raster("Raster Base Projection/raster_base_projection.tif")

makeRasters <- function(prop, region, res) {
  #load in soil shape file (created in MakeSoilShape.R)
  soil <- st_read(paste0("soilShapeFiles/", region, "_soil.shp"))
  # subset shapefile data
  sub <- soil %>%
    dplyr::select(c(all_of(prop), geometry))
  # transform soil to baseCRS
  subCRS <- st_transform(sub, crs = crs(baseCRS))
  # make blank raster
  ext = extent(st_bbox(subCRS))
  CRS3857 <- crs(baseCRS)
  raster_blank <- raster(crs = CRS3857,
                         vals = 0,
                         resolution = c(res,res),
                         ext = ext)
  # rasterize shapefile
  sub_raster <- rasterize(subCRS, raster_blank, field = subCRS[[prop]])
 
  # fix variable names for filename
  if(prop == 'ttl_dpt') {prop = "depth"}
  if(prop == "k") {prop = "kfact"}
  if(prop == "nrrcpcl") {prop = "landClass"}
  if(prop == "slplns_") {prop = "slopelen"}
  filename = paste0(region, "_", prop, "_", res, "m.tiff")
  # save and write raster
  print(filename)
  writeRaster(sub_raster, filename,
              format = "GTiff",
              overwrite = TRUE,
              NAflag = -9999)
}

# run this function with whichever soil property, region and resolution needed

makeRasters(prop = 'awc', region = "northeastWI", res = 30) # soil property is a string

