library(raster)
library(tidyverse)
library(sf)
#library(rgdal)
library(tidymodels)
library(randomForest)
library(foreach)
library(doParallel)


# load data and create soils data set -------------------------------------

soil_xy <- read_csv("../southEastSoilXY.csv.gz")
nrow(soil_xy)
head(na.omit(soil_xy))

# load random forest model ------------------------------------------------

# load models
erosion_mod <- readRDS("pt_erosion_southEastWI.rds")
#erosion_mod <- readRDS("modelOutputs/southEastWI/erosion/pt_erosion_southEastWI.rds")
pi_mod <- readRDS("pt_ploss_southEastWI.rds")
ffcn_mod <- readRDS("pt_ffcn_southEastWI.rds")
sci_mod <- readRDS("pt_sci_southEastWI.rds")


# create management data set ----------------------------------------------

rotational <- factor(erosion_mod$preproc$xlevels$rotational)
density <- factor(erosion_mod$preproc$xlevels$density)
manure <- c(0,0,100, 200, 25, 150, 50, 0)
fert <- c(0, 100, 0, 0, 50, 0, 50, 50)
manureFert <- as.factor(paste(manure, fert))
level_df <- expand_grid(density, rotational, manureFert)
summary(level_df)

# load percent P to total P and DM
pneeds <- read.csv("../../totalPandDM_R.csv")
#pneeds <- read.csv("csvs/totalPandDM_R.csv")
pneeds <- pneeds %>%
  filter(crop == "pt") %>%
  mutate_if(is.character, as.factor) %>%
  dplyr::select(c(density, manureFert, total_DM_lbs,  totalP2O5_lbs))
summary(pneeds)

# only the options from snapplus simulations
rotation_density_options <- c("cn hi", "cn lo", "rt rt")


# add total_DM_lbs 
level_df <- left_join(level_df, pneeds) %>%
  mutate(rotation_density = paste(rotational, density))%>%
  filter(rotation_density %in% rotation_density_options) %>%
  mutate_if(is.character, as.factor) %>%
  mutate(rotationLabel = as.character(rotational), # for naming structure
         densityLabel = as.character(density)) %>%
  separate(manureFert, c("manure", "fertilizer"), remove = FALSE)  %>%
  mutate(initialP = 45) 

summary(level_df)


#create naming structures
erosion.name = list()
for(i in 1:nrow(level_df)) {
  erosion.name[[i]] = paste("pasture_Erosion",
                            level_df[i,9], # rotation label
                            level_df[i,10], #density label
                            level_df[i,4], # manure
                            level_df[i,5], #fertilizer
                            "southEastWI.tiff", sep = "_")
}


erosion.name[1]


pi.name = list()
for(i in 1:nrow(level_df)) {
  pi.name[[i]] = paste("pasture_PI",
                       level_df[i,9], # rotation label
                       level_df[i,10], #density label
                       level_df[i,4], # manure
                       level_df[i,5], #fertilizer
                       "southEastWI.tiff", sep = "_")
}


pi.name[1]

cn.name = list()
for(i in 1:nrow(level_df)) {
  cn.name[[i]] = paste("pasture_CN",
                       level_df[i,9], # rotation label
                       level_df[i,10], #density label
                       level_df[i,4], # manure
                       level_df[i,5], #fertilizer
                       "southEastWI.tiff", sep = "_")
}


cn.name[1]

sci.name = list()
for(i in 1:nrow(level_df)) {
  sci.name[[i]] = paste("pasture_SCI",
                        level_df[i,9], # rotation label
                        level_df[i,10], #density label
                        level_df[i,4], # manure
                        level_df[i,5], #fertilizer
                        "southEastWI.tiff", sep = "_")
}

baseRaster <- raster("../../Raster Base Projection/raster_base_projection.tif")
#baseRaster <- raster("rasterOutputs/raster_base_projection.tif")
CRS3857 <- crs(baseRaster)

# parallel processing
n_cores <- detectCores()
print(paste("cores", n_cores))
cluster <- makeCluster(4) ##THIS IS A GUESS AT HOW MANY CORES TO RUN
registerDoParallel(cluster)

#system.time( 
foreach(i = 1:nrow(level_df), .packages = c("tidyverse", "raster", "tidymodels", "randomForest"),
        .verbose = TRUE) %dopar% {
#for(i in 1:nrow(level_df)) {
  # combine management with soil for prediction data frame
  pred_df <- level_df[i,] %>%
   slice(rep(1:n(), each=nrow(soil_xy))) %>%
   bind_cols(soil_xy) %>%
   drop_na(c(slope, slopelenusle.r, sand, silt, clay, k, LSsurgo))
  # print(paste("pred_df",i))
  # print(summary(pred_df))
  #create erosion data frame
  erosion_df <- erosion_mod %>%
   predict(pred_df) %>%
   bind_cols(pred_df) %>%
   mutate(Erosion = round(.pred, 4)) %>%
   dplyr::select(-.pred)
  # print(paste("erosion_df", i))
  # print(summary(erosion_df))
  # rasterize erosion
  erosion_raster <- erosion_df %>%
   dplyr::select(c(x,y,Erosion)) %>%
   rasterFromXYZ(res = c(30, 30), crs = CRS3857)
  #print(erosion_raster)
  # write erosion raster
  writeRaster(erosion_raster, filename = erosion.name[[i]],
            format = "GTiff",
            NAflag = -9999,
            overwrite = TRUE)
  #print(paste("rasterized erosion", i))
  # create PI data frame
  PI_df <- pi_mod %>%
   predict(erosion_df) %>%
   bind_cols(erosion_df) %>%
   #mutate(PI = case_when(.pred < 0 ~ 0,
    #                     TRUE ~round(.pred, 3)))
   mutate(PI = round(.pred,3))
 # print(paste("PI_df", i))
 # print(summary(PI_df))
 # rasterize PI
 PI_raster <- PI_df %>%
   dplyr::select(c(x,y,PI)) %>%
   rasterFromXYZ(res = c(30, 30), crs = CRS3857)
 #print(PI_raster)
 # write PI raster
 writeRaster(PI_raster, filename = pi.name[[i]],
            format = "GTiff",
            NAflag = -9999,
            overwrite = TRUE)
 #print(paste("rasterized PI", i))
 # create SCI data frame
 SCI_df <- sci_mod %>%
   predict(erosion_df) %>%
   bind_cols(erosion_df) %>%
   mutate(SCI = round(.pred,3))
 # print(paste("SCI_df", i))
 # print(summary(SCI_df))
 # rasterize SCI
 SCI_raster <- SCI_df %>%
  dplyr::select(c(x,y,SCI)) %>%
  rasterFromXYZ(res = c(30, 30), crs = CRS3857)
 #print(SCI_raster)
 # write SCI raster
 writeRaster(SCI_raster, filename = sci.name[[i]],
            format = "GTiff",
            NAflag = -9999,
            overwrite = TRUE)
 #print(paste("rasterized SCI", i))
 # create curve number data frame
 cn_pred_df <- pred_df %>%
  drop_na(hydgrp)

 CN_df <- ffcn_mod %>%
   predict(cn_pred_df) %>%
   bind_cols(cn_pred_df) %>%
   mutate(cn_mc3 = if_else(.pred*exp(0.00673*(100-.pred)) < 99, .pred*exp(0.00673*(100-.pred)), 99),
         pre_cnAdj = (cn_mc3-.pred)/3*(1-2*exp(-13.86*slope))+.pred,
         cnAdj = if_else(slope < 0.05, .pred,
                         if_else(pre_cnAdj > 99, 99, pre_cnAdj))
    )

 #summary(CN_df)
 # rasterize CN
 cn_raster <- CN_df %>%
   dplyr::select(c(x,y, cnAdj)) %>%
   rasterFromXYZ(res = c(30, 30), crs = CRS3857)
 #print(cn_raster)
 # write CN raster
 writeRaster(cn_raster, filename = cn.name[[i]],
            format = "GTiff",
            NAflag = -9999,
            overwrite = TRUE)
 #print(paste("rasterized CN", i))
 }

stopCluster(cl = cluster)


unregister_dopar <- function() {
 env <- foreach:::.foreachGlobals
 rm(list=ls(name=env), pos=env)
}

unregister_dopar()
