
#  cut in QGIS
# step 1. Load DEM, set project CRS to 3857
# step 2. Cut WI county shapefile in native CRS
# step 3. load learning hub shapefile into Q and extract raster by shapefile

library(terra)
library(sf)
library(tidyverse)
# load DEM
#  dem <- raster("slopeAndLScalcs/WIS_Statewide_DEM_10m_3857.tif")
#  crs(dem)
# # load shapefile
wi <- st_read("soilShapes/WI_Counties2010/WI_Counties2010.shp")
crs(wi)

# wi_new <- st_transform(wi, crs = crs(dem))
# summary(wi)
# levels(as.factor(wi$NAME))


# crawford, vernon, richland ----------------------------------------------

wi_cut <- wi3857 %>%
  filter(NAME == "Crawford"
         | NAME == "Vernon"
         | NAME == "Richland")



# crop raster to shapefile
dem_crop <- crop(dem, extent(wi_cut))
dem_crop2 <- mask(dem_crop, wi_cut)

plot(dem_crop)
plot(dem_crop2)

writeRaster(dem_crop2, "CrawVernRich_DEM.tif",
            format = "GTiff",
            overwrite = TRUE,
            NAflag = -9999)


# marathon,clark,taylor ---------------------------------------------------


wi_cut <- wi3857 %>%
  filter(NAME == "Marathon"
         | NAME == "Clark"
         | NAME == "Taylor")

plot(wi_cut)
st_write(wi_cut, "../areaShapefiles/cloverBelt.shp",
         driver = "ESRI Shapefile")

# crop raster to shapefile
dem_crop <- crop(dem, extent(wi_cut))
dem_crop2 <- mask(dem_crop, wi_cut)

plot(dem_crop)
plot(dem_crop2)

writeRaster(dem_crop2, "slopeAndLScalcs/cloverBelt_DEM.tif",
            format = "GTiff",
            overwrite = TRUE,
            NAflag = -9999)


# Lacrosse and Monroe -----------------------------------------------------

wi_cut <- wi3857 %>%
  filter(NAME == "La Crosse"
         | NAME == "Monroe")

# crop raster to shapefile
dem_crop <- crop(dem, extent(wi_cut))
dem_crop2 <- mask(dem_crop, wi_cut)

plot(dem_crop)
plot(dem_crop2)

writeRaster(dem_crop2, "slopeAndLScalcs/LacrosseMonroe_DEM.tif",
            format = "GTiff",
            overwrite = TRUE,
            NAflag = -9999)


# Crawford, Vernon, Richland, Monroe, Lacrosse ----------------------------

wi_cut <- wi3857 %>%
  filter(NAME == "La Crosse"
         | NAME == "Monroe"
         | NAME == "Crawford"
         | NAME == "Vernon"
         | NAME == "Richland")

#save shapefile
class(wi_cut)
st_write(wi_cut, "../areaShapefiles/southWestWI.shp", driver = "ESRI Shapefile")


# crop raster to shapefile
dem_crop <- crop(dem, extent(wi_cut))
dem_crop2 <- mask(dem_crop, wi_cut)

plot(dem_crop)
plot(dem_crop2)

writeRaster(dem_crop2, "slopeAndLScalcs/SouthwestWI_DEM.tif",
            format = "GTiff",
            overwrite = TRUE,
            NAflag = -9999)


# SouthCentral WI ---------------------------------------------------------


wi_cut <- wi3857 %>%
  filter(NAME == "Columbia"
         | NAME == 'Dane'
         | NAME == "Grant"
         | NAME == "Iowa"
         | NAME == "Lafayette"
         | NAME == "Sauk")

st_crs(wi_cut)$proj4string
plot(wi_cut)
st_write(wi_cut, "../areaShapefiles/southCentralWI.shp", driver = "ESRI Shapefile")

# crop raster to shapefile
dem_crop <- crop(dem, extent(wi_cut))
dem_crop2 <- mask(dem_crop, wi_cut)

plot(dem_crop)
plot(dem_crop2)

writeRaster(dem_crop2, "slopeAndLScalcs/SouthCentralWI_DEM.tif",
            format = "GTiff",
            overwrite = TRUE,
            NAflag = -9999)


# uplands -----------------------------------------------------------------

wi_cut <- wi %>%
  filter(NAME == "Green"
         | NAME == "Grant"
         | NAME == "Iowa"
         | NAME == "Lafayette"
         | NAME == "Sauk")

st_write(wi_cut, "../areaShapefiles/uplandsWI.shp", driver = "ESRI Shapefile")



# northeast WI ------------------------------------------------------------

wi_cut <- wi %>%
  filter(NAME == "Brown"
         | NAME == "Manitowoc"
         | NAME == "Kewaunee")

st_write(wi_cut, "../areaShapefiles/northeastWI.shp", driver = "ESRI Shapefile")

# redCedar WI-------------------------------------

counties <- c("Barron", "Burnett", "Chippewa", "Dunn", "Pierce", "Polk", "Rusk", "St. Croix", "Sawyer", "Washburn")

wi_cut <- wi %>%
  filter(NAME %in% counties)

st_write(wi_cut, "../areaShapefiles/redCedarWI.shp", driver = "ESRI Shapefile")

# PineRiver MN -----------------------

mn <- st_read("shp_bdry_counties_in_minnesota/mn_county_boundaries_500.shp")
head(mn)
levels(as.factor(mn$CTY_NAME))

counties <- c("Crow Wing", "Cass", "Hubbard", "Aitkin")

mn_cut <- mn %>%
  filter(CTY_NAME %in% counties) %>%
  select(CTY_NAME)

st_write(mn_cut, "../areaShapefiles/pineRiverMN.shp", driver = "ESRI Shapefile", append = FALSE)

# east Central WI---------------
counties <- c("Oconto", "Outagamie", "Shawano", "Winnebago") 

wi_cut <- wi %>%
  filter(NAME %in% counties)

st_write(wi_cut, "soilShapes/eastCentralWI.shp", driver = "ESRI Shapefile")

# south east WI---------------
counties <- c("Sheboygan", "Fond du Lac","Green Lake", "Dodge", "Washington", "Waukesha", "Ozaukee", "Calumet")

wi_cut <- wi %>%
  filter(NAME %in% counties)

st_write(wi_cut, "soilShapes/southEastWI.shp", driver = "ESRI Shapefile")
