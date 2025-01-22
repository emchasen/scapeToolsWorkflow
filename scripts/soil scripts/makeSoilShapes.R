# this file makes the shapefile of soil map units with associated properties to convert to matrices
# make one shapefile for each region

#load libraries
library(tidyverse) 
library(sf)

# make a string containing the list of counties -----------
# use label name (folder name)

EC_counties <- c("Oconto", "Outagamie", "Shawano", "Winnebago") # waiting on shawano, oconto
SE_counties <- c("Sheboygan", "FondDuLac","GreenLake", "Dodge", "Washington", "MilwaukeeWaukesha", "Ozaukee", "CalumetManitowoc")

# function definitions------------------
# get map with mapunits and attach county name
get_map <- function(counties){
  map <- list()
  for(i in 1:length(counties)) {
    filename = paste0("EXTRACTIONS/", counties[i], "/SSURGO/", counties[i], "_SSURGO_Mapunits.shp")
    #filename = eval(parse(text = counties[i]))
    map[[i]] <- st_read(filename) %>%
      mutate(mukey = as.integer(MUKEY),
             County = counties[i]) 
  }
  map <- do.call(rbind.data.frame, map)
}

read_mapunit <- function(counties){
  mapunit <- list()
  for(i in 1:length(counties)){
    filename = paste0("EXTRACTIONS/", counties[i], "/SSURGO/", counties[i], "_SSURGO_mapunit.csv")
    mapunit[[i]] <- read.csv(file = filename, na.strings = c(" ", "", "NA", "NaN"))
  }
  mapunit <- do.call(rbind.data.frame, mapunit)
}

read_horizon <- function(counties){
  horizon <- list()
  for(i in 1:length(counties)){
    filename = paste0("EXTRACTIONS/", counties[i], "/SSURGO/", counties[i], "_SSURGO_chorizon.csv")
    horizon[[i]] <- read.csv(file = filename, na.strings = c(" ", "", "NA", "NaN"))
  }
  horizon <- do.call(rbind.data.frame, horizon)
}

#extract component layers
read_component <- function(counties){
  component <- list()
  for(i in 1:length(counties)){
    filename = paste0("EXTRACTIONS/", counties[i], "/SSURGO/", counties[i], "_SSURGO_component.csv")
    component[[i]] <- read.csv(file = filename, na.strings = c(" ", " ", "NA", "NaN"))
  }
  component <- do.call(rbind.data.frame, component)
}

# east central region----------
## load data---------------
# use the county name string
chorizon_EC <- read_horizon(EC_counties)

component_EC <- read_component(EC_counties)

map_EC <- get_map(EC_counties) %>%
  mutate_if(is.character, as.factor)

mapunit_EC <- read_mapunit(EC_counties) %>%
  mutate_if(is.character, as.factor) %>%
  dplyr::select(c(mukey, farmlndcl, muacres))

## clean component table ---------------------------------------------------

component_EC <- component_EC %>%
  filter(majcompflag == "Yes") %>%
  dplyr::select(c(mukey, cokey, comppct.r, compname, majcompflag, compkind, hydgrp, slopelenusle.r,
                  nirrcapcl,nirrcapscl, drainagecl)) %>% 
  mutate(nonIrrLandClass = paste(nirrcapcl, nirrcapscl)) %>%
  mutate_if(is.character, as.factor)

# keep the component with the greatest percent
comp_high_EC <- component_EC %>%
  group_by(mukey) %>%
  filter(comppct.r == max(comppct.r))

#summary(component)

## clean horizon table ---------------------------------------------------

depth_EC <- chorizon_EC %>%
  group_by(cokey) %>%
  summarise(total.depth = max(hzdepb.r))

## * filter to remove horizons that start below 30 cm --------------------
chorizon_EC <- chorizon_EC %>%
  filter(hzdept.r < 31) %>%
  droplevels()

chorizon_EC <- chorizon_EC %>%
  dplyr::select(hzdepb.r, hzdept.r, cokey, sandtotal.r, silttotal.r, claytotal.r, om.r, ksat.r, kffact,
                cec7.r, ph1to1h2o.r, awc.r)

## * take weighted means of attributes of interest -----------------------

chorizon_EC <- chorizon_EC %>%
  mutate(thick = ifelse(hzdepb.r > 30, 30 - hzdept.r, hzdepb.r - hzdept.r)) %>%
  group_by(cokey) %>%
  summarise(sand = round(weighted.mean(sandtotal.r, thick, na.rm = TRUE),2),
            silt = round(weighted.mean(silttotal.r, thick, na.rm = TRUE),2),
            clay = round(weighted.mean(claytotal.r, thick, na.rm = TRUE),2),
            om = round(weighted.mean(om.r, thick, na.rm = TRUE),2),
            ksat = round(weighted.mean(ksat.r, thick, na.rm = TRUE),2),
            k = round(weighted.mean(kffact, thick, na.rm = TRUE),2),
            cec = round(weighted.mean(cec7.r, thick, na.rm = TRUE),2),
            ph = round(weighted.mean(ph1to1h2o.r, thick, na.rm = TRUE),2),
            awc = round(weighted.mean(awc.r, thick, na.rm = TRUE), 2))
# add deepest soil depth back
chorizon_EC <- left_join(chorizon_EC, depth_EC, by = "cokey")

## join component, horizon and mapunit -------------------------------------

comp_horizon_EC <- left_join(comp_high_EC, chorizon_EC)

comp_horizon_mu_EC <- left_join(map_EC, comp_horizon_EC) %>% 
  left_join(mapunit_EC) %>%
  dplyr::select(-c(SPATIALVER, MUKEY))
#summary(comp_horizon_mu)

# south east region----------
## load data---------------
chorizon_SE <- read_horizon(SE_counties)

component_SE <- read_component(SE_counties)

map_SE <- get_map(SE_counties) %>%
  mutate_if(is.character, as.factor)

mapunit_SE <- read_mapunit(SE_counties) %>%
  mutate_if(is.character, as.factor) %>%
  dplyr::select(c(mukey, farmlndcl, muacres))

## clean component table ---------------------------------------------------

component_SE <- component_SE %>%
  filter(majcompflag == "Yes") %>%
  dplyr::select(c(mukey, cokey, comppct.r, compname, majcompflag, compkind, hydgrp, slopelenusle.r,
                  nirrcapcl,nirrcapscl, drainagecl)) %>% 
  mutate(nonIrrLandClass = paste(nirrcapcl, nirrcapscl)) %>%
  mutate_if(is.character, as.factor)

# keep the component with the greatest percent
comp_high_SE <- component_SE %>%
  group_by(mukey) %>%
  filter(comppct.r == max(comppct.r))

#summary(component)

## clean horizon table ---------------------------------------------------

depth_SE <- chorizon_SE %>%
  group_by(cokey) %>%
  summarise(total.depth = max(hzdepb.r))

## * filter to remove horizons that start below 30 cm --------------------
chorizon_SE <- chorizon_SE %>%
  filter(hzdept.r < 31) %>%
  droplevels()

chorizon_SE <- chorizon_SE %>%
  dplyr::select(hzdepb.r, hzdept.r, cokey, sandtotal.r, silttotal.r, claytotal.r, om.r, ksat.r, kffact,
                cec7.r, ph1to1h2o.r, awc.r)

## * take weighted means of attributes of interest -----------------------

chorizon_SE <- chorizon_SE %>%
  mutate(thick = ifelse(hzdepb.r > 30, 30 - hzdept.r, hzdepb.r - hzdept.r)) %>%
  group_by(cokey) %>%
  summarise(sand = round(weighted.mean(sandtotal.r, thick, na.rm = TRUE),2),
            silt = round(weighted.mean(silttotal.r, thick, na.rm = TRUE),2),
            clay = round(weighted.mean(claytotal.r, thick, na.rm = TRUE),2),
            om = round(weighted.mean(om.r, thick, na.rm = TRUE),2),
            ksat = round(weighted.mean(ksat.r, thick, na.rm = TRUE),2),
            k = round(weighted.mean(kffact, thick, na.rm = TRUE),2),
            cec = round(weighted.mean(cec7.r, thick, na.rm = TRUE),2),
            ph = round(weighted.mean(ph1to1h2o.r, thick, na.rm = TRUE),2),
            awc = round(weighted.mean(awc.r, thick, na.rm = TRUE), 2))
# add deepest soil depth back
chorizon_SE <- left_join(chorizon_SE, depth_SE, by = "cokey")

## join component, horizon and mapunit -------------------------------------

comp_horizon_SE <- left_join(comp_high_SE, chorizon_SE)

comp_horizon_mu_SE <- left_join(map_SE, comp_horizon_SE) %>% 
  left_join(mapunit_SE) %>%
  dplyr::select(-c(SPATIALVER, MUKEY))
#summary(comp_horizon_mu)

## * save file --------------------------------------------------

st_write(comp_horizon_mu_SE, "soilShapes/southEastWI_soil.shp")

# add nresponse property-----------------------

# spreadsheet from Laura
soilN <- read_csv("csvs/soilsdata_a2809mapunits.csv")

## east central WI ----------------------------------------------------------------
EC_counties <- c("Oconto", "Outagamie", "Shawano", "Winnebago") 

# filter soil N to region and rename so that it matches soil shape
EC_n <- soilN %>%
 filter(county %in% EC_counties) %>%
 rename("awc_class" = "awc") %>%
 droplevels()

# check
levels(as.factor(EC_n$county))

# load soil shape 
EC_soil <- st_read("soilShapes/eastCentralWI_soil.shp")

soilNames <- EC_soil %>%
 st_drop_geometry() %>%
 group_by(MUSYM, mukey, cokey, County, compnam) %>%
 tally()

# check that mapunit keys are in correct range
range(EC_soil$mukey)
range(EC_n$mapunitkey)

# all compnames to lower case
EC_soil <- EC_soil %>%
 mutate(compnam = tolower(compnam))

EC_n <- EC_n %>%
 mutate(soilseries = tolower(soilseries))

newEC <- left_join(EC_soil, EC_n, by = c("mukey" = "mapunitkey")) %>%
 dplyr::select(-c(county))

# find which mapunits are missing N reponse
na_soilshape <- newEC %>%
 filter(is.na(nresponse)) %>%
 st_drop_geometry()

missingNresponse <- na_soilshape %>%
 group_by(MUSYM, mukey, cokey, County, compnam) %>%
 tally() #28

levels(as.factor(missingNresponse$compnam))

# create a list based on the compname that includes all of the compnames that are not soils
nonSoils <- c("dumps", "limestone quarries", "pits", "rock outcrop",  
              "urban land", "water")

missingNresponse2 <- missingNresponse %>%
 filter(!compnam %in% nonSoils) %>%
 droplevels() %>%
 ungroup() %>%
 dplyr::select(-(n)) #8 soils that do not have n responses

#write.csv(missingNresponse2, "missingNresponse_southwestWI.csv", row.names = FALSE, quote = FALSE)

names(newEC)


st_write(newEC, "soilShapes/eastCentralWISoilN.shp", append = FALSE)

## southEast WI ----------------------------------------------------------------
SE_counties <- c("Sheboygan", "Fond du Lac","Green Lake", "Dodge", "Washington", "Waukesha", "Ozaukee", "Calumet")

# filter soil N to region and rename so that it matches soil shape
SE_n <- soilN %>%
 filter(county %in% SE_counties) %>%
 rename("awc_class" = "awc") %>%
 droplevels()

# check
levels(as.factor(SE_n$county))

# load soil shape 
SE_soil <- st_read("soilShapes/southEastWI_soil.shp")

soilNames <- SE_soil %>%
 st_drop_geometry() %>%
 group_by(MUSYM, mukey, cokey, County, compnam) %>%
 tally() # 958

# check that mapunit keys are in correct range
range(SE_soil$mukey)
range(SE_n$mapunitkey)

# all compnames to lower case
SE_soil <- SE_soil %>%
 mutate(compnam = tolower(compnam))

SE_n <- SE_n %>%
 mutate(soilseries = tolower(soilseries))

newSE <- left_join(SE_soil, SE_n, by = c("mukey" = "mapunitkey")) %>%
 dplyr::select(-c(county))

# find which mapunits are missing N reponse
na_soilshape <- newSE %>%
 filter(is.na(nresponse)) %>%
 st_drop_geometry()

missingNresponse <- na_soilshape %>%
 group_by(MUSYM, mukey, cokey, County, compnam) %>%
 tally() #172

levels(as.factor(missingNresponse$compnam))

# create a list based on the compname that includes all of the compnames that are not soils
nonSoils <- c( "beaches", "borrow area","borrow pits","cut and fill land","dune land", "made land", "old beaches", "pits", 
               "rock land", "rock outcrop",  "rough broken land","sandy and gravelly land", "sandy lake beaches", "stony land", 
              "urban land", "water")

missingNresponse2 <- missingNresponse %>%
 filter(!compnam %in% nonSoils) %>%
 droplevels() %>%
 ungroup() %>%
 dplyr::select(-(n)) #109 soils without n response variable

#write.csv(missingNresponse2, "missingNresponse_southwestWI.csv", row.names = FALSE, quote = FALSE)

names(newSE)


st_write(newSE, "soilShapes/southEastWISoilN.shp", append = FALSE)
