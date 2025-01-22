# this script creates soil property rasters for the different regions 
# used in grazescape (10 m res) or smartscape (30 m res).

# different regions include:
# cloverBeltWI (counties Marathon, Clark, Taylor)
# southWestWI (counties Crawford, Vernon, Richland, Monroe, Lacrosse)
# uplandsWI (counties Sauk, Lafayette, Green, Grant, Iowa)
# northeastWI (counties Brown, Manitowoc, Kewaunee)

# variables include: 
# hydgrp, farmclass = frmlndc, drainclass = drngcls, N response = nrespns

# load libraries
library(sf)
library(raster)
library(tidyverse) 

# you can load in an example soil shape file first to make sure that the variable
# is named correctly

#list of regions
#regions <- c("southWestWI", "northeastWI", "cloverBeltWI", "uplandsWI")
# resolutions
#res <- c(10, 30)

# load in base raster for correct CRS
baseCRS <- raster("Raster Base Projection/raster_base_projection.tif")

##TODO define region, res, and prop from lists
region <- ""
res <- c()
prop <- ""


#load in soil shape file (created in MakeSoilShape.R)
soil <- st_read(paste0("soilShapeFiles/", region, "_soil.shp"))
# subset shapefile data
sub <- soil %>%
  dplyr::select(c(all_of(prop), geometry))

##TODO depending on which property you are making a raster for, use the respective code
# if prop = hydgrp,-------------------- 
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


# if prop = farmclass,----------------
##TODO levels(as.factor(sub$frmlndc)), make sure all levels are accounted for below
sub <- sub %>%
  mutate(farmClass = recode(frmlndc,
                            "All areas are prime farmland" = "1",
                            "Farmland of statewide importance" = "2",
                            "Not prime farmland" = "3",
                            "Prime farmland if drained" = "4",
                            "Prime farmland if drained and either protected from flooding or not frequently flooded during the growing season" = "5",
                            #"Prime farmland if protected from flooding or not frequently flooded during the growing season" = "6",
                            #"Prime farmland if subsoiled, completely removing the root inhibiting soil layer" = "6"
  )) %>%
  mutate(farmClass = as.numeric(farmClass))

# if prop = drainclass,-------------- 
sub <- sub %>%
  mutate(drainage = recode(drangcl,
                           "Excessively drained" = "1",
                           "Moderately well drained" = "2",
                           "Poorly drained" = "3",
                           "Somewhat excessively drained" = "4",
                           "Somewhat poorly drained" = "5",
                           "Very poorly drained" = "6",
                           "Well drained" = "7")) %>%
  mutate(drainage = as.numeric(drainage))


# if prop = N response---------------------
sub <- sub %>%
  mutate(nresponse = recode(nrespns,
                            "H" = "1",
                            "M" = "2",
                            "S" = "3")) %>%
  mutate(nresponse = as.numeric(nresponse))


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
# change prop name for filename
if(prop == "frmlndc") {prop = "farmClass"}
if(prop == "drngcls") {prop = "drainClass"}
if(prop == "nrespns") {prop = "nResponse"}
  
filename = paste0(region, "_", prop, "_", res, "m.tiff")
# save and write raster
writeRaster(sub_raster, filename,
            format = "GTiff",
            overwrite = TRUE,
            NAflag = -9999)





