library(raster)
library(tidyverse)
library(sf)
library(rgdal)
library(tidymodels)
library(randomForest)


# load data and create soils data set -------------------------------------

##TODO replace raster names as needed

# load soil rasters
slope <- raster("../southEastWI_slopePer_30m.tiff")
elev <- raster("../southEastWI_DEM_30m.tiff")
sand <- raster("../southEastWI_sand_30m.tif")
clay <- raster("../southEastWI_clay_30m.tif")
om <- raster("../southEastWI_om_30m.tif")
ksat <- raster("../southEastWI_ksat_30m.tif")
awc <- raster("../southEastWI_awc_30m.tif")
depth <- raster("../southEastWI_depth_30m.tif")
ph <- raster("../southEastWI_ph_30m.tif")
silt <- raster("../southEastWI_silt_30m.tif")
print('done loading rasters')

slope_xy <- as.data.frame(slope, xy = TRUE)
elev_xy <- as.data.frame(elev, xy = TRUE)
sand_xy <- as.data.frame(sand, xy = TRUE)
clay_xy <- as.data.frame(clay, xy = TRUE)
om_xy <- as.data.frame(om, xy = TRUE)
ksat_xy <- as.data.frame(ksat, xy = TRUE)
awc_xy <- as.data.frame(awc, xy = TRUE)
depth_xy <- as.data.frame(depth, xy = TRUE)
ph_xy <- as.data.frame(ph, xy = TRUE)
silt_xy <- as.data.frame(silt, xy = TRUE)
print('converted to data frames')

soil_xy <- left_join(slope_xy, elev_xy) %>%
  left_join(sand_xy) %>%
  left_join(clay_xy) %>%
  left_join(om_xy) %>%
  left_join(ksat_xy) %>%
  left_join(awc_xy) %>%
  left_join(depth_xy) %>%
  left_join(ph_xy) %>%
  left_join(silt_xy)

soil_xy <- soil_xy %>%
  rename("slope" = "lyr.1",
         "elev" = "southEastWI_DEM",
         "sand" = "southEastWI_sand_30m",
         "clay" = "southEastWI_clay_30m",
         "om" = "southEastWI_om_30m",
         "ksat" = "southEastWI_ksat_30m",
         "awc" = "southEastWI_awc_30m",
         "total.depth" = "southEastWI_depth_30m",
         "ph" = "southEastWI_ph_30m",
         "silt" = "southEastWI_silt_30m")

# convert elevation from feet to meters
soil_xy <- soil_xy %>%
  mutate(elev = 0.3048*elev)

# load random forest model ------------------------------------------------

# load models
mod <- readRDS("tidyPastureALLWInoCec.rds")
#mod <- readRDS("../grass growth model files/tidyPasture.rds")

# levels of cropname
species_df <- data.frame(cropname = c("Bluegrass-clover","Orchardgrass-clover", "Timothy-clover"),
                         variety = c("low", "high", "medium"))


species.name = list()
for(i in 1:nrow(species_df)) {
  species.name[[i]] = paste('pasture_Yield', 
                            species_df[i,2],
                            "southEastWI.tiff", sep = "_")
}

baseRaster <- raster("../../Raster Base Projection/raster_base_projection.tif")
CRS3857 <- crs(baseRaster)

for(i in 1:3){
  print(i)
  pred_df <- species_df[i,] %>% 
    slice(rep(1:n(), each=nrow(soil_xy))) %>% 
    bind_cols(soil_xy) %>%
    drop_na()
  
  print(summary(pred_df))
  
  yield_df <- mod %>% 
    predict(pred_df) %>%
    bind_cols(pred_df) %>%
    mutate(grassYield = round(.pred, 4)) %>%
    dplyr::select(-.pred)
  
  summary(yield_df)
  
  #rasterize
  yield_raster <- yield_df %>%
    dplyr::select(c(x,y,grassYield)) %>%
    rasterFromXYZ(res = c(30, 30), crs = CRS3857)
  
  # save raster
  writeRaster(yield_raster, filename = species.name[[i]],
              format = "GTiff",
              NAflag = -9999,
              overwrite = TRUE) 
  print(paste("rasterized yield", i))
}
