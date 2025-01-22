# this script creates functions that will clean the soil data needed for raster and model creations

library(tidyverse)

# make a string containing the list of counties -----------
# use label name (folder name)

counties <- c("CalumetManitowoc", "Dodge", "FondDuLac", "GreenLake", "Oconto", "Outagamie", 
              "Ozaukee", "Shawano", "Sheboygan", "Washington", "MilwaukeeWaukesha","Winnebago")

# define functions --------------------------------------------------------
# if soils downloaded manually, will need to edit filename in each of the functions below

read_horizon <- function(){
 horizon <- list()
 for(i in 1:length(counties)){
  filename = paste0("EXTRACTIONS/", counties[i], "/SSURGO/", counties[i], "_SSURGO_chorizon.csv") 
  horizon[[i]] <- read.csv(file = filename, na.strings = c(" ", "", "NA", "NaN"))
 }
 horizon <- do.call(rbind.data.frame, horizon)
}

read_component <- function(){
 component <- list()
 for(i in 1:length(counties)){
  filename = paste0("EXTRACTIONS/", counties[i], "/SSURGO/", counties[i], "_SSURGO_component.csv")
  component[[i]] <- read.csv(file = filename, na.strings = c(" ", " ", "NA", "NaN"))
 }
 component <- do.call(rbind.data.frame, component)
}

read_mapunit <- function(){
 mapunit <- list()
 for(i in 1:length(counties)){
  filename = paste0("EXTRACTIONS/", counties[i], "/SSURGO/", counties[i], "_SSURGO_mapunit.csv")
  mapunit[[i]] <- read.csv(file = filename, na.strings = c(" ", "NA", "NaN", ""))
  mapunit[[i]]$county <- counties[i]
 }
 mapunit <- do.call(rbind.data.frame, mapunit)
}

# upload and clean data ---------------------------------------------------

# * clean horizon ---------------------------------------------------------

chorizon <- read_horizon()

#deepest horizon bottom of each component
depth <- chorizon %>%
 group_by(cokey) %>%
 summarise(total.depth = max(hzdepb.r))

#filter to remove horizons that start below 30 cm
chorizon <- chorizon %>%
 filter(hzdept.r < 31) %>%
 droplevels()

#colnames(chorizon)

chorizon <- chorizon %>%
 dplyr::select(hzdepb.r, hzdept.r, cokey, sandtotal.r, silttotal.r, claytotal.r, om.r, ksat.r, kffact, 
               cec7.r, ph1to1h2o.r, awc.r)

#summary(chorizon)

#weighted means 
# aggregate data to 30 cm 
chorizon <- chorizon %>%
 mutate(thick = ifelse(hzdepb.r > 30, 30 - hzdept.r, hzdepb.r - hzdept.r)) %>%  
 group_by(cokey) %>%
 summarise(sand = round(weighted.mean(sandtotal.r, thick, na.rm = TRUE),2),
           silt = round(weighted.mean(silttotal.r, thick, na.rm = TRUE),2),
           clay = round(weighted.mean(claytotal.r, thick, na.rm = TRUE),2),
           om = round(weighted.mean(om.r, thick, na.rm = TRUE),2),
           k = round(weighted.mean(kffact, thick, na.rm = TRUE),2),
           ksat = round(weighted.mean(ksat.r, thick, na.rm = TRUE), 2),
           ph = round(weighted.mean(ph1to1h2o.r, thick, na.rm = TRUE), 2),
           awc = round(weighted.mean(awc.r, thick, na.rm = TRUE), 2)) 

#add deepest soil depth back
chorizon <- left_join(chorizon, depth, by = "cokey")
summary(chorizon)


# * clean component -------------------------------------------------------

component <- read_component()

#summary(component)

component <- component %>%
 dplyr::select(c(comppct.r, compname, compkind, majcompflag, slope.r, slopelenusle.r,
                 mukey, cokey, hydgrp)) %>%
 filter(compkind == "Series" | compkind == "Taxadjunct"| compkind == "Variant" | compkind == "Family") %>%
 filter(majcompflag == "Yes") %>%
 droplevels()

#length(levels(as.factor(component$cokey)))
# keep the component with the greatest percent
comp_high <- component %>%
 group_by(mukey) %>%
 filter(comppct.r == max(comppct.r))

#join component and horizon by cokey
component_horizon <- left_join(comp_high, chorizon, by = c("cokey")) %>%
 mutate_if(is.character, as.factor)

summary(component_horizon)

# check for missing data
NA_DF <- component[rowSums(is.na(component)) > 0,]
NA_DF <- component_horizon[rowSums(is.na(component_horizon)) > 0,] # this all get cleaned out because of high OM??

# slopelength is NA for some non mucky soils but the slope is so low (0 or 1, I could make the slopelenth 76)
component_horizon <- component_horizon %>%
  mutate(slopelenusle.r  = replace_na(slopelenusle.r , 76))
# k = NA but that will get filled in in the next step (when joined with snapplus)

# * clean mapunit ---------------------------------------------------------

mapunit <- read_mapunit()

mapunit <- mapunit %>%
 dplyr::select(c(musym, muacres, mukey, county)) 

map_sum <- mapunit %>%
 group_by(musym, mukey) %>%
 tally()


# join horizon, component and mapunit -------------------------------------

full_soil <- left_join(component_horizon, mapunit, by = c("mukey"))

# check again for NAs
NA_DF <- full_soil[rowSums(is.na(full_soil)) > 0,] # these are all muck soils - cleaned out of models because of high OM

#if there are blanks, make NA
full_soil <- full_soil %>%
 mutate_if(is.character, as.factor)

soil_sum <- full_soil %>%
 group_by(compname, musym) %>%
 tally()

summary(full_soil)

# save clean data-------------------------
write_csv(full_soil, "csvs/soil.csv")
# make sure it saved properly
test <- read_csv("csvs/soil.csv")

