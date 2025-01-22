# this script attaches the N response for each soil to the soil shape files
# depending on soil name/mapunit

library(tidyverse)
library(raster)
library(sf)

# spreadsheet from Laura
soilN <- read_csv("Nresponse/soilsdata_a2809mapunits.csv")

summary(soilN)
levels(as.factor(soilN$nresponse))
levels(as.factor(soilN$county))

# soils with missing Nresponse in ssurgo:
# Reading p.15-17, Table 4.1, and p. 37-38 in A2809 will give you what you need to query for.  
# There is, however, one more complicating factor – around 2014, a year after the current A2809 went out,
# the NRCS made a change in the way they mapped some “densic” horizons in some common soils in the NE part of the state 
# and that change lowered the calculated Available water capacity and put these soils into a medium yield N response category. 


# kickapoo ----------------------------------------------------------------
counties <- c("Crawford", "Vernon", "Richland", "Monroe", "La Crosse") # done

# filter soil N to region and rename so that it matches soil shape
kickapoo_n <- soilN %>%
  filter(county %in% counties) %>%
  rename("awc_class" = "awc") %>%
  droplevels()

# check
levels(as.factor(kickapoo_n$county))

# load soil shape 
kickapoo_soil <- st_read("soilShapeFiles/ridgeValley_soil.shp")
summary(kickapoo_soil)

soilNames <- kickapoo_soil %>%
  st_drop_geometry() %>%
  group_by(MUSYM, mukey, cokey, County, compnam) %>%
  tally()

# check that mapunit keys are in correct range
range(kickapoo_soil$mukey)
range(kickapoo_n$mapunitkey)

# all compnames to lower case
kickapoo_soil <- kickapoo_soil %>%
  mutate(compnam = tolower(compnam))

kickapoo_n <- kickapoo_n %>%
  mutate(soilseries = tolower(soilseries))

newKickapoo <- left_join(kickapoo_soil, kickapoo_n, by = c("mukey" = "mapunitkey")) %>%
  dplyr::select(-c(county))

# find which mapunits are missing N reponse
na_soilshape <- newKickapoo %>%
  filter(is.na(nresponse)) %>%
  st_drop_geometry()

missingNresponse <- na_soilshape %>%
  group_by(MUSYM, mukey, cokey, County, compnam) %>%
  tally()

levels(as.factor(missingNresponse$compnam))

nonSoils <- c("coal pile", "dam", "landfill", "pits", "riverwash",  "urban land", "water")

missingNresponse2 <- missingNresponse %>%
  filter(!compnam %in% nonSoils) %>%
  droplevels() %>%
  ungroup() %>%
  dplyr::select(-(n)) #23

#write.csv(missingNresponse2, "missingNresponse_southwestWI.csv", row.names = FALSE, quote = FALSE)

names(newKickapoo)


st_write(newKickapoo, "soilShapeFiles/ridgeVallySoilN.shp", append = FALSE)
# clover belt -------------------------------------------------------------


counties <- sort(c("Clark", "Marathon", "Taylor")) # done

clover_n <- soilN %>%
  filter(county %in% counties) %>%
  rename("awc_class" = "awc") %>%
  droplevels()

# check
levels(as.factor(clover_n$county))

# load soil shape 
clover_soil <- st_read("soilShapeFiles/cloverBeltWI_soil.shp")
summary(clover_soil)


# check that mapunit keys are in correct range
range(clover_soil$mukey)
range(clover_n$mapunitkey)

# all compnames to lower case
clover_soil <- clover_soil %>%
  mutate(compnam = tolower(compnam))

clover_n <- clover_n %>%
  mutate(soilseries = tolower(soilseries))

##TODO figure out why there are more in the newClover than in clover_soil
newClover <- left_join(clover_soil, clover_n, by = c("mukey" = "mapunitkey")) %>%
  dplyr::select(-c(county))

# find which mapunits are missing N reponse
na_soilshape <- newClover %>%
  filter(is.na(nresponse)) %>%
  st_drop_geometry()

missingNresponse <- na_soilshape %>%
  group_by(MUSYM, mukey, cokey, County, compnam) %>%
  tally() #21

levels(as.factor(missingNresponse$compnam))

nonSoils <- c("Dam", "Pits", "Urban land", "Water")

missingNresponse2 <- missingNresponse %>%
  filter(!compnam %in% nonSoils) %>%
  droplevels() %>%
  ungroup() %>%
  dplyr::select(-(n)) #10

write.csv(missingNresponse2, "missingNresponse_cloverBeltWI.csv", row.names = FALSE, quote = FALSE)

check <- newClover %>%
  dplyr::select(c(mukey, compnam, MUSYM, soilmu, cokey, componentkey))
names(newClover)
st_write(newClover, "soilShapeFiles/cloverBeltWI_soil2.shp", append = FALSE)

# northeast wi ------------------------------------------------------------


counties <- c("Brown", "Manitowoc", "Kewaunee") # done

northeast_n <- soilN %>%
  filter(county %in% counties) %>%
  rename("awc_class" = "awc") %>%
  droplevels()

# check
levels(as.factor(northeast_n$county))

# load soil shape 
northeast_soil <- st_read("soilShapeFiles/northEastWI_soil.shp")
summary(northeast_soil)

northeast_soil <- northeast_soil %>%
  mutate(compnam = tolower(compnam))

northeast_n <- northeast_n %>%
  mutate(soilseries = tolower(soilseries))

# check that mapunit keys are in correct range
range(northeast_soil$mukey)
range(northeast_n$mapunitkey)

newNortheast <- left_join(northeast_soil, northeast_n, by = c("mukey" = "mapunitkey")) %>%
  dplyr::select(-c(county))

# find which mapunits are missing N reponse
na_soilshape <- newNortheast %>%
  filter(is.na(nresponse)) %>%
  st_drop_geometry()

missingNresponse <- na_soilshape %>%
  group_by(MUSYM, mukey, cokey, County, compnam) %>%
  tally() #31

levels(as.factor(missingNresponse$compnam))

nonSoils <- c("Alluvial land","Alluvial land, wet" , "Borrow area", "Dumps", "Dune land","Fill land",  "Marsh", "Pits",
              "Rough broken land", "Stony and rocky land", "Urban land" , "Water")

missingNresponse2 <- missingNresponse %>%
  filter(!compnam %in% nonSoils) %>%
  droplevels() %>%
  ungroup() %>%
  dplyr::select(-(n)) #9

check <- newNortheast %>%
  dplyr::select(c(mukey, compnam, MUSYM, soilmu, cokey, componentkey))
names(newNortheast)
st_write(newNortheast, "soilShapeFiles/northEastWI_soil2.shp", append = FALSE)


# uplands wi --------------------------------------------------------------


counties <- c("Sauk", "Lafayette", "Green", "Grant", "Iowa")

uplands_n <- soilN %>%
  filter(county %in% counties) %>%
  rename("awc_class" = "awc") %>%
  droplevels()

uplands_n <- uplands_n %>%
  mutate(soilseries = tolower(soilseries))
levels(as.factor(uplands_n$soilseries))

# check
levels(as.factor(uplands_n$county))

# load soil shape 
uplands_soil <- st_read("soilShapeFiles/uplandsWI_soil.shp")
uplands_soil <- uplands_soil %>%
  mutate(compnam = tolower(compnam))
levels(as.factor(uplands_soil$compnam))

summary(uplands_soil)

# check that mapunit keys are in correct range
range(uplands_soil$mukey)
range(uplands_n$mapunitkey)

newUplands<- left_join(uplands_soil, uplands_n, by = c("mukey" = "mapunitkey")) %>%
  dplyr::select(-c(county))

na <- newUplands %>% filter(is.na(nresponse)) %>% st_drop_geometry()
missingN <- na %>%
  group_by(MUSYM, mukey, cokey, County, compnam) %>%
  mutate(compnam = as.factor(tolower(compnam))) %>%
  tally() #64

levels(as.factor(missingN$compnam))

nonSoils <- c("riverwash","pits", "dumps" , "landfill" ,  "alluvial land" , "urban land" , "water" , "marsh" , "mine pits and dumps",
              "rock outcrop" , "terrace escarpments")

missingN <- missingN %>%
  filter(!compnam %in% nonSoils) %>%
  droplevels() %>%
  ungroup() %>%
  dplyr::select(-(n)) # 30

check <- newUplands %>%
  dplyr::select(c(mukey, compnam, MUSYM, soilmu, cokey, componentkey))
names(newUplands)
st_write(newUplands, "soilShapeFiles/uplandsWI_soil2.shp", append = FALSE)

# redCedar-----------------------------------

counties <- c("Barron", "Burnett", "Chippewa", "Dunn", "Pierce", "Polk", "Rusk", "St. Croix", "Sawyer", "Washburn")

redCedar_n <- soilN %>%
  filter(county %in% counties) %>%
  rename("awc_class" = "awc") %>%
  droplevels()

# load soil shape 
redCedar_soil <- st_read("soilShapeFiles/redCedarWI_soil.shp")

summary(redCedar_soil)

# check that mapunit keys are in correct range
range(redCedar_soil$mukey)
range(redCedar_n$mapunitkey)

newredCedar <- left_join(redCedar_soil, redCedar_n, by = c("mukey" = "mapunitkey")) %>%
  dplyr::select(-c(county))

na <- newredCedar %>% filter(is.na(nresponse)) %>% st_drop_geometry()

missingN <- na %>%
  group_by(MUSYM, mukey, cokey, County, compnam) %>%
  mutate(compnam = as.factor(tolower(compnam))) %>%
  tally() #64

levels(as.factor(missingN$compnam))

nonSoils <- c("rubble land", "riverwash", "pits", "landfill", "urban land", "water")

missingN <- missingN %>%
  filter(!compnam %in% nonSoils) 

soils <- redCedar_soil %>%
  st_drop_geometry() %>%
  group_by(MUSYM, mukey, cokey, County, compnam) %>%
  tally()

st_write(newredCedar, "soilShapeFiles/redCedarWI_nResponse.shp", append = FALSE)

# compare new and old soil shapes for missing N response--------------------------

## southwest------------------------------

sw_old <- st_read("soilShapeFiles/southWestWI_soil.shp")
sw_new <- st_read("soilShapeFiles/southWestWI_soil2.shp")

# find which mapunits are missing N reponse
sw_old_na <- sw_old %>%
  filter(is.na(nrespns)) %>%
  st_drop_geometry()

missingNresponse <- sw_old_na %>%
  group_by(MUSYM, mukey, cokey, County, compnam) %>%
  mutate(compnam = as.factor(tolower(compnam))) %>%
  tally()

levels(as.factor(missingNresponse$compnam))

nonSoils <- c("blownout land", "coal pile", "dam", "landfill", "pits", "riverwash", "rock outcrop", "urban land", "water")

missingNresponse <- missingNresponse %>%
  filter(!compnam %in% nonSoils) %>%
  droplevels() %>%
  ungroup() %>%
  dplyr::select(-(n)) # 66

sw_new_na <- sw_new %>%
  filter(is.na(nrespns)) %>%
  st_drop_geometry()

missingNresponse <- sw_new_na %>%
  group_by(MUSYM, mukey, cokey, County, compnam) %>%
  mutate(compnam = as.factor(tolower(compnam))) %>%
  tally()

levels(as.factor(missingNresponse$compnam))

missingNresponse <- missingNresponse %>%
  filter(!compnam %in% nonSoils) %>%
  droplevels() %>%
  ungroup() %>%
  dplyr::select(-(n)) # 23

## clover belt-----------------

cb_old <- st_read("soilShapeFiles/cloverBeltWI_soil.shp")
cb_new <- st_read("soilShapeFiles/cloverBeltWI_soil2.shp")

# find which mapunits are missing N reponse
cb_old_na <- cb_old %>%
  filter(is.na(nrespns)) %>%
  st_drop_geometry()

missingNresponse <- cb_old_na %>%
  group_by(MUSYM, mukey, cokey, County, compnam) %>%
  mutate(compnam = as.factor(tolower(compnam))) %>%
  tally()

levels(as.factor(missingNresponse$compnam))

nonSoils <- c("blownout land", "coal pile", "dam", "landfill", "pits", "riverwash", "rock outcrop", "urban land", "water")

missingNresponse <- missingNresponse %>%
  filter(!compnam %in% nonSoils) %>%
  droplevels() %>%
  ungroup() %>%
  dplyr::select(-(n)) # 22

cb_new_na <- cb_new %>%
  filter(is.na(nrespns)) %>%
  st_drop_geometry()

missingNresponse <- cb_new_na %>%
  group_by(MUSYM, mukey, cokey, County, compnam) %>%
  mutate(compnam = as.factor(tolower(compnam))) %>%
  tally()

levels(as.factor(missingNresponse$compnam))

missingNresponse <- missingNresponse %>%
  filter(!compnam %in% nonSoils) %>%
  droplevels() %>%
  ungroup() %>%
  dplyr::select(-(n)) # 10

## northeast---------------------

new_old <- st_read("soilShapeFiles/northeastWI_soil.shp")
ne_new <- st_read("soilShapeFiles/northeastWI_soil2.shp")

# find which mapunits are missing N reponse
old_na <- new_old %>%
  filter(is.na(nrespns)) %>%
  st_drop_geometry()

missingNresponse <- old_na %>%
  group_by(MUSYM, mukey, cokey, County, compnam) %>%
  mutate(compnam = as.factor(tolower(compnam))) %>%
  tally()

levels(as.factor(missingNresponse$compnam))

nonSoils <- c("alluvial land", "alluvial land, wet", "borrow area", "dune land", "dumps","fill land", "pits","urban land" ,"water",               
               "marsh","rock outcrop", "rough broken land" , "stony and rocky land")

missingNresponse <- missingNresponse %>%
  filter(!compnam %in% nonSoils) %>%
  droplevels() %>%
  ungroup() %>%
  dplyr::select(-(n)) # 12

new_na <- ne_new %>%
  filter(is.na(nrespns)) %>%
  st_drop_geometry()

missingNresponse <- new_na %>%
  group_by(MUSYM, mukey, cokey, County, compnam) %>%
  mutate(compnam = as.factor(tolower(compnam))) %>%
  tally()

levels(as.factor(missingNresponse$compnam))

missingNresponse <- missingNresponse %>%
  filter(!compnam %in% nonSoils) %>%
  droplevels() %>%
  ungroup() %>%
  dplyr::select(-(n)) # 12 

# don't redo northeast

## uplands-------------------

old <- st_read("soilShapeFiles/uplandsWI_soil.shp")
new <- st_read("soilShapeFiles/uplandsWI_soil2.shp")

# find which mapunits are missing N reponse
old_na <- old %>%
  filter(is.na(nrespns)) %>%
  st_drop_geometry()

missingNresponse <- old_na %>%
  group_by(MUSYM, mukey, cokey, County, compnam) %>%
  mutate(compnam = as.factor(tolower(compnam))) %>%
  tally()

levels(as.factor(missingNresponse$compnam))

nonSoils <- c("rock outcrop", "blownout land", "riverwash", "pits", "dumps", "landfill", "alluvial land", "urban land",
              "water", "marsh", "mine pits and dumps", "blown-out land", "dune land", "terrace escarpments")

missingNresponse <- missingNresponse %>%
  filter(!compnam %in% nonSoils) %>%
  droplevels() %>%
  ungroup() %>%
  dplyr::select(-(n)) # 66

new_na <- new %>%
  filter(is.na(nrespns)) %>%
  st_drop_geometry()

missingNresponse <- new_na %>%
  group_by(MUSYM, mukey, cokey, County, compnam) %>%
  mutate(compnam = as.factor(tolower(compnam))) %>%
  tally()

levels(as.factor(missingNresponse$compnam))

missingNresponse <- missingNresponse %>%
  filter(!compnam %in% nonSoils) %>%
  droplevels() %>%
  ungroup() %>%
  dplyr::select(-(n)) # 12 
