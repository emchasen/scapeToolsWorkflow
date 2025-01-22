# this is the file that joins data outputs from snapplus batch runs with the ssurgo data

library(tidyverse)

# create full data sets from snapPlus outout-------------------
# loop through the folder: snapPlus output
# bind all data sets within a county folder to make one data set per county

for(i in 1:length(list.files(path = "snapplusOutput/"))) {
  #print(list.files(path = "snapplusOutput/")[i])
  # new file pathway
  pathway = paste0( "snapplusOutput/", list.files(path = "snapplusOutput/")[i])
  #print(pathway)
  df <- list()
  for(j in 1:length(list.files(pathway))) {
    #print(list.files(pathway)[j])
    #print(paste0(pathway, "/", list.files(pathway)[j]))
    df[[j]] <- read_csv(paste0(pathway, "/", list.files(pathway)[j]))
  }
  newName <- str_split(str_split(pathway, pattern = "/")[[1]][2], pattern = " ")[[1]][1]
  #print(newName)
  fullData <- bind_rows(df)
  #print(dim(fullData))
  fileName <- paste0(pathway, "/", newName, "_full.csv.gz")
  #print(fileName)
  write_csv(x = fullData, file = fileName)
}

# oconto-------------------

# add ssurgo soils, pdata and snapplus data ----------------------------------

pdata <- read.csv("csvs/percentPtoTotalP.csv")

# join snapplus and pdata---------------

oconto <- read_csv("snapplusOutput/Oconto Co/Oconto_full.csv.gz") %>% 
 mutate_if(is.character, as.factor)
summary(oconto)
oconto_snap <- left_join(oconto, pdata, by = c("crop", "cover", "ManureApp", "SyntheticApp", "density"))
summary(oconto_snap)

# prepare for binding with ssurgo data
oconto_snap$SoilSeries <- tolower(oconto_snap$SoilSeries)
oconto_snap$County <- tolower(oconto_snap$County)
oconto_snap <- oconto_snap %>%
 mutate_if(is.character, as.factor)

soil <- read_csv("csvs/soil.csv")
oconto_soil <- soil %>% filter(county == "Oconto") %>%
 mutate(county = "oconto") # 75 soils

#removed non major components 
oconto_soil <- oconto_soil %>%
 select(c(compname, slopelenusle.r, hydgrp, sand, silt, clay, om, k, total.depth, musym, county, mukey)) %>%
 mutate(compname = tolower(compname), 
        county = tolower(county))

summary(oconto_soil)

# have to check this out by hand and see if there are too many missing soils
k_na <- oconto_soil %>%
 filter(is.na(k)) %>%
 mutate_if(is.character, as.factor) %>%
 droplevels() # 3 missing k values, all high OM

oconto_soil <- oconto_soil %>%
 mutate(k = replace_na(k, 0.02))  # per email with Laura

oconto_SnapSurgo <- left_join(oconto_snap, oconto_soil, by = c("SoilSeries" = "compname", "SoilSymbol" = "musym", "County" = "county")) %>%
 mutate_if(is.character, as.factor)
summary(oconto_SnapSurgo)

k_na <- oconto_SnapSurgo %>%
 filter(is.na(k)) %>%
 mutate_if(is.character, as.factor) %>%
 droplevels()

which_soils <- k_na %>%
 group_by(SoilSymbol, SoilSeries, mukey) %>%
 tally() # 10 soils


# recalculate factor and LS based on Snap Slope
oconto_SnapSurgo <- oconto_SnapSurgo %>% 
 drop_na(k) %>% # see above note in "which_soils"
 rowwise %>%
 mutate(factor = ifelse(between(slope, 3.01, 4), 0.4, 
                        ifelse(between(slope, 1, 3), 0.3,
                               ifelse(slope < 1, 0.2, 0.5)))) %>%
 # LS = (((slope/((10000+(slope^2))^0.5))*4.56)+(slope/(10000+(slope^2))^0.5)^2*(65.41)+0.065)*(slopelength/72.6)^(factor)
 mutate(LSsurgo = round((((slope/((10000+(slope^2))^0.5))*4.56)+(slope/(10000+(slope^2))^0.5)^2*(65.41)+0.065)*((slopelenusle.r*3.3)/72.6)^(factor),2)) %>%
 dplyr::select(-c(factor))

oconto_SnapSurgo <- oconto_SnapSurgo %>%
 mutate_if(is.character, as.factor) 
summary(oconto_SnapSurgo)
#add R factor ??? it is in the weather/county file but I don't think we use this at all 
#kewa_SnapSurgo <- kewa_SnapSurgo %>%
#mutate(R_factor = 97.69)

write_csv(oconto_SnapSurgo, "csvs/ocontoJoinSnapSurgo.csv.gz")

# winnebago-------------------

# add ssurgo soils, pdata and snapplus data ----------------------------------

pdata <- read.csv("csvs/percentPtoTotalP.csv")

# join snapplus and pdata---------------

winnebago <- read_csv("snapplusOutput/Winnebago Co/Winnebago_full.csv.gz") %>% 
 mutate_if(is.character, as.factor)
summary(winnebago)
winnebago_snap <- left_join(winnebago, pdata, by = c("crop", "cover", "ManureApp", "SyntheticApp", "density"))
summary(winnebago_snap)

# prepare for binding with ssurgo data
winnebago_snap$SoilSeries <- tolower(winnebago_snap$SoilSeries)
winnebago_snap$County <- tolower(winnebago_snap$County)
winnebago_snap <- winnebago_snap %>%
 mutate_if(is.character, as.factor)

soil <- read_csv("csvs/soil.csv")
winnebago_soil <- soil %>% filter(county == "Winnebago") %>%
 mutate(county = "winnebago") # 71 soils

#removed non major components 
winnebago_soil <- winnebago_soil %>%
 select(c(compname, slopelenusle.r, hydgrp, sand, silt, clay, om, k, total.depth, musym, county, mukey)) %>%
 mutate(compname = tolower(compname), 
        county = tolower(county))

summary(winnebago_soil)

# have to check this out by hand and see if there are too many missing soils
k_na <- winnebago_soil %>%
 filter(is.na(k)) %>%
 mutate_if(is.character, as.factor) %>%
 droplevels() # 6 missing k values, all high OM

winnebago_soil <- winnebago_soil %>%
 mutate(k = replace_na(k, 0.02))  # per email with Laura

winnebago_SnapSurgo <- left_join(winnebago_snap, winnebago_soil, by = c("SoilSeries" = "compname", "SoilSymbol" = "musym", "County" = "county")) %>%
 mutate_if(is.character, as.factor)
summary(winnebago_SnapSurgo)

k_na <- winnebago_SnapSurgo %>%
 filter(is.na(k)) %>%
 mutate_if(is.character, as.factor) %>%
 droplevels()

which_soils <- k_na %>%
 group_by(SoilSymbol, SoilSeries, mukey) %>%
 tally() # 2 soils


# recalculate factor and LS based on Snap Slope
winnebago_SnapSurgo <- winnebago_SnapSurgo %>% 
 drop_na(k) %>% # see above note in "which_soils"
 rowwise %>%
 mutate(factor = ifelse(between(slope, 3.01, 4), 0.4, 
                        ifelse(between(slope, 1, 3), 0.3,
                               ifelse(slope < 1, 0.2, 0.5)))) %>%
 # LS = (((slope/((10000+(slope^2))^0.5))*4.56)+(slope/(10000+(slope^2))^0.5)^2*(65.41)+0.065)*(slopelength/72.6)^(factor)
 mutate(LSsurgo = round((((slope/((10000+(slope^2))^0.5))*4.56)+(slope/(10000+(slope^2))^0.5)^2*(65.41)+0.065)*((slopelenusle.r*3.3)/72.6)^(factor),2)) %>%
 dplyr::select(-c(factor))

winnebago_SnapSurgo <- winnebago_SnapSurgo %>%
 mutate_if(is.character, as.factor) %>%
 drop_na(sand)
summary(winnebago_SnapSurgo)
#add R factor ??? it is in the weather/county file but I don't think we use this at all 
#kewa_SnapSurgo <- kewa_SnapSurgo %>%
#mutate(R_factor = 97.69)

write_csv(winnebago_SnapSurgo, "csvs/winnebagoJoinSnapSurgo.csv.gz")

# outagamie-------------------

# add ssurgo soils, pdata and snapplus data ----------------------------------

pdata <- read.csv("csvs/percentPtoTotalP.csv")

# join snapplus and pdata---------------

outagamie <- read_csv("snapplusOutput/Outagamie Co/Outagamie_full.csv.gz") %>% 
 mutate_if(is.character, as.factor)
summary(outagamie)
outagamie_snap <- left_join(outagamie, pdata, by = c("crop", "cover", "ManureApp", "SyntheticApp", "density"))
summary(outagamie_snap)

# prepare for binding with ssurgo data
outagamie_snap$SoilSeries <- tolower(outagamie_snap$SoilSeries)
outagamie_snap$County <- tolower(outagamie_snap$County)
outagamie_snap <- outagamie_snap %>%
 mutate_if(is.character, as.factor)

soil <- read_csv("csvs/soil.csv")
outagamie_soil <- soil %>% filter(county == "Outagamie") %>%
 mutate(county = "outagamie") # 81 soils

#removed non major components 
outagamie_soil <- outagamie_soil %>%
 select(c(compname, slopelenusle.r, hydgrp, sand, silt, clay, om, k, total.depth, musym, county, mukey)) %>%
 mutate(compname = tolower(compname), 
        county = tolower(county))

summary(outagamie_soil)

# have to check this out by hand and see if there are too many missing soils
k_na <- outagamie_soil %>%
 filter(is.na(k)) %>%
 mutate_if(is.character, as.factor) %>%
 droplevels() # 6 missing k values, all high OM

outagamie_soil <- outagamie_soil %>%
 mutate(k = replace_na(k, 0.02))  # per email with Laura

outagamie_SnapSurgo <- left_join(outagamie_snap, outagamie_soil, by = c("SoilSeries" = "compname", "SoilSymbol" = "musym", "County" = "county")) %>%
 mutate_if(is.character, as.factor)
summary(outagamie_SnapSurgo)

k_na <- outagamie_SnapSurgo %>%
 filter(is.na(k)) %>%
 mutate_if(is.character, as.factor) %>%
 droplevels()

which_soils <- k_na %>%
 group_by(SoilSymbol, SoilSeries, mukey) %>%
 tally() # 6 soils


# recalculate factor and LS based on Snap Slope
outagamie_SnapSurgo <- outagamie_SnapSurgo %>% 
 drop_na(k) %>% # see above note in "which_soils"
 rowwise %>%
 mutate(factor = ifelse(between(slope, 3.01, 4), 0.4, 
                        ifelse(between(slope, 1, 3), 0.3,
                               ifelse(slope < 1, 0.2, 0.5)))) %>%
 # LS = (((slope/((10000+(slope^2))^0.5))*4.56)+(slope/(10000+(slope^2))^0.5)^2*(65.41)+0.065)*(slopelength/72.6)^(factor)
 mutate(LSsurgo = round((((slope/((10000+(slope^2))^0.5))*4.56)+(slope/(10000+(slope^2))^0.5)^2*(65.41)+0.065)*((slopelenusle.r*3.3)/72.6)^(factor),2)) %>%
 dplyr::select(-c(factor))

outagamie_SnapSurgo <- outagamie_SnapSurgo %>%
 mutate_if(is.character, as.factor)
summary(outagamie_SnapSurgo)
#add R factor ??? it is in the weather/county file but I don't think we use this at all 
#kewa_SnapSurgo <- kewa_SnapSurgo %>%
#mutate(R_factor = 97.69)

write_csv(outagamie_SnapSurgo, "csvs/outagamieJoinSnapSurgo.csv.gz")

# calumet--------------------
# add ssurgo soils, pdata and snapplus data ----------------------------------

pdata <- read.csv("csvs/percentPtoTotalP.csv")

# join snapplus and pdata---------------

calumet <- read_csv("snapplusOutput/Calumet Co/Calumet_full.csv.gz") %>% 
 mutate_if(is.character, as.factor)
summary(calumet)
calumet_snap <- left_join(calumet, pdata, by = c("crop", "cover", "ManureApp", "SyntheticApp", "density"))
summary(calumet_snap)

# prepare for binding with ssurgo data
calumet_snap$SoilSeries <- tolower(calumet_snap$SoilSeries)
calumet_snap$County <- tolower(calumet_snap$County)
calumet_snap <- calumet_snap %>%
 mutate_if(is.character, as.factor)

soil <- read_csv("csvs/soil.csv")
cal_soil <- soil %>% filter(county == "CalumetManitowoc") %>%
 mutate(county = "Calumet")

#removed non major components 
cal_soil <- cal_soil %>%
 select(c(compname, slopelenusle.r, hydgrp, sand, silt, clay, om, k, total.depth, musym, county, mukey)) %>%
 mutate(compname = tolower(compname), 
        county = tolower(county))

summary(cal_soil)

# have to check this out by hand and see if there are too many missing soils
k_na <- cal_soil %>%
 filter(is.na(k)) %>%
 mutate_if(is.character, as.factor) %>%
 droplevels()

cal_soil <- cal_soil %>%
 mutate(k = replace_na(k, 0.02))  # per email with Laura

cal_SnapSurgo <- left_join(calumet_snap, cal_soil, by = c("SoilSeries" = "compname", "SoilSymbol" = "musym", "County" = "county")) %>%
 mutate_if(is.character, as.factor)
summary(cal_SnapSurgo)

k_na <- cal_SnapSurgo %>%
 filter(is.na(k)) %>%
 mutate_if(is.character, as.factor) %>%
 droplevels()

which_soils <- k_na %>%
 group_by(SoilSymbol, SoilSeries, mukey) %>%
 tally() # checked a handful of areas on websoil survey and could not find this soils present.


# recalculate factor and LS based on Snap Slope
cal_SnapSurgo <- cal_SnapSurgo %>% 
 drop_na(sand) %>% # see above note in "which_soils"
 rowwise %>%
 mutate(factor = ifelse(between(slope, 3.01, 4), 0.4, 
                        ifelse(between(slope, 1, 3), 0.3,
                               ifelse(slope < 1, 0.2, 0.5)))) %>%
 # LS = (((slope/((10000+(slope^2))^0.5))*4.56)+(slope/(10000+(slope^2))^0.5)^2*(65.41)+0.065)*(slopelength/72.6)^(factor)
 mutate(LSsurgo = round((((slope/((10000+(slope^2))^0.5))*4.56)+(slope/(10000+(slope^2))^0.5)^2*(65.41)+0.065)*((slopelenusle.r*3.3)/72.6)^(factor),2)) %>%
 dplyr::select(-c(factor))

cal_SnapSurgo <- cal_SnapSurgo %>%
 mutate_if(is.character, as.factor)
summary(cal_SnapSurgo)
#add R factor ??? it is in the weather/county file but I don't think we use this at all 
#kewa_SnapSurgo <- kewa_SnapSurgo %>%
 #mutate(R_factor = 97.69)

write_csv(cal_SnapSurgo, "csvs/calumetJoinSnapSurgo.csv.gz")

# dodge-------------------

# add ssurgo soils, pdata and snapplus data ----------------------------------

pdata <- read.csv("csvs/percentPtoTotalP.csv")

# join snapplus and pdata---------------

dodge <- read_csv("snapplusOutput/Dodge Co/Dodge_full.csv.gz") %>% 
 mutate_if(is.character, as.factor)
summary(dodge)
dodge_snap <- left_join(dodge, pdata, by = c("crop", "cover", "ManureApp", "SyntheticApp", "density"))
summary(dodge_snap)

# prepare for binding with ssurgo data
dodge_snap$SoilSeries <- tolower(dodge_snap$SoilSeries)
dodge_snap$County <- tolower(dodge_snap$County)
dodge_snap <- dodge_snap %>%
 mutate_if(is.character, as.factor)

soil <- read_csv("csvs/soil.csv")
dodge_soil <- soil %>% filter(county == "Dodge") %>%
 mutate(county = "dodge")

#removed non major components 
dodge_soil <- dodge_soil %>%
 select(c(compname, slopelenusle.r, hydgrp, sand, silt, clay, om, k, total.depth, musym, county, mukey)) %>%
 mutate(compname = tolower(compname), 
        county = tolower(county))

summary(dodge_soil)

# have to check this out by hand and see if there are too many missing soils
k_na <- dodge_soil %>%
 filter(is.na(k)) %>%
 mutate_if(is.character, as.factor) %>%
 droplevels()

dodge_soil <- dodge_soil %>%
 mutate(k = replace_na(k, 0.02))  # per email with Laura

dodge_SnapSurgo <- left_join(dodge_snap, dodge_soil, by = c("SoilSeries" = "compname", "SoilSymbol" = "musym", "County" = "county")) %>%
 mutate_if(is.character, as.factor)
summary(dodge_SnapSurgo)

k_na <- dodge_SnapSurgo %>%
 filter(is.na(k)) %>%
 mutate_if(is.character, as.factor) %>%
 droplevels()

which_soils <- k_na %>%
 group_by(SoilSymbol, SoilSeries, mukey) %>%
 tally() # only 4 soils... just gonna let it go


# recalculate factor and LS based on Snap Slope
dodge_SnapSurgo <- dodge_SnapSurgo %>% 
 drop_na(k) %>% # see above note in "which_soils"
 rowwise %>%
 mutate(factor = ifelse(between(slope, 3.01, 4), 0.4, 
                        ifelse(between(slope, 1, 3), 0.3,
                               ifelse(slope < 1, 0.2, 0.5)))) %>%
 # LS = (((slope/((10000+(slope^2))^0.5))*4.56)+(slope/(10000+(slope^2))^0.5)^2*(65.41)+0.065)*(slopelength/72.6)^(factor)
 mutate(LSsurgo = round((((slope/((10000+(slope^2))^0.5))*4.56)+(slope/(10000+(slope^2))^0.5)^2*(65.41)+0.065)*((slopelenusle.r*3.3)/72.6)^(factor),2)) %>%
 dplyr::select(-c(factor))

dodge_SnapSurgo <- dodge_SnapSurgo %>%
 mutate_if(is.character, as.factor)
summary(dodge_SnapSurgo)
#add R factor ??? it is in the weather/county file but I don't think we use this at all 
#kewa_SnapSurgo <- kewa_SnapSurgo %>%
#mutate(R_factor = 97.69)

write_csv(dodge_SnapSurgo, "csvs/dodgeJoinSnapSurgo.csv.gz")


# fond du lac-------------------

# add ssurgo soils, pdata and snapplus data ----------------------------------

pdata <- read.csv("csvs/percentPtoTotalP.csv")

# join snapplus and pdata---------------

fondDuLac <- read_csv("snapplusOutput/Fond du Lac Co/Fond_full.csv.gz") %>% 
 mutate_if(is.character, as.factor)
summary(fondDuLac)
fondDuLac_snap <- left_join(fondDuLac, pdata, by = c("crop", "cover", "ManureApp", "SyntheticApp", "density"))
summary(fondDuLac_snap)

# prepare for binding with ssurgo data
fondDuLac_snap$SoilSeries <- tolower(fondDuLac_snap$SoilSeries)
fondDuLac_snap$County <- tolower(fondDuLac_snap$County)
fondDuLac_snap <- fondDuLac_snap %>%
 mutate_if(is.character, as.factor)

soil <- read_csv("csvs/soil.csv")
fondDuLac_soil <- soil %>% filter(county == "FondDuLac") %>%
 mutate(county = "fond du lac")

#removed non major components 
fondDuLac_soil <- fondDuLac_soil %>%
 select(c(compname, slopelenusle.r, hydgrp, sand, silt, clay, om, k, total.depth, musym, county, mukey)) %>%
 mutate(compname = tolower(compname), 
        county = tolower(county))

summary(fondDuLac_soil)

# have to check this out by hand and see if there are too many missing soils
k_na <- fondDuLac_soil %>%
 filter(is.na(k)) %>%
 mutate_if(is.character, as.factor) %>%
 droplevels()

fondDuLac_soil <- fondDuLac_soil %>%
 mutate(k = replace_na(k, 0.02))  # per email with Laura

fondDuLac_SnapSurgo <- left_join(fondDuLac_snap, fondDuLac_soil, by = c("SoilSeries" = "compname", "SoilSymbol" = "musym", "County" = "county")) %>%
 mutate_if(is.character, as.factor)
summary(fondDuLac_SnapSurgo)

k_na <- fondDuLac_SnapSurgo %>%
 filter(is.na(k)) %>%
 mutate_if(is.character, as.factor) %>%
 droplevels()

which_soils <- k_na %>%
 group_by(SoilSymbol, SoilSeries, mukey) %>%
 tally() # 21 soils out of 186, ok


# recalculate factor and LS based on Snap Slope
fondDuLac_SnapSurgo <- fondDuLac_SnapSurgo %>% 
 drop_na(k) %>% # see above note in "which_soils"
 rowwise %>%
 mutate(factor = ifelse(between(slope, 3.01, 4), 0.4, 
                        ifelse(between(slope, 1, 3), 0.3,
                               ifelse(slope < 1, 0.2, 0.5)))) %>%
 # LS = (((slope/((10000+(slope^2))^0.5))*4.56)+(slope/(10000+(slope^2))^0.5)^2*(65.41)+0.065)*(slopelength/72.6)^(factor)
 mutate(LSsurgo = round((((slope/((10000+(slope^2))^0.5))*4.56)+(slope/(10000+(slope^2))^0.5)^2*(65.41)+0.065)*((slopelenusle.r*3.3)/72.6)^(factor),2)) %>%
 dplyr::select(-c(factor))

fondDuLac_SnapSurgo <- fondDuLac_SnapSurgo %>%
 drop_na(clay) %>%
 mutate_if(is.character, as.factor)
summary(fondDuLac_SnapSurgo)
#add R factor ??? it is in the weather/county file but I don't think we use this at all 
#kewa_SnapSurgo <- kewa_SnapSurgo %>%
#mutate(R_factor = 97.69)

write_csv(fondDuLac_SnapSurgo, "csvs/fondDuLacJoinSnapSurgo.csv.gz")

# green lake------------------

# add ssurgo soils, pdata and snapplus data ----------------------------------

pdata <- read.csv("csvs/percentPtoTotalP.csv")

# join snapplus and pdata---------------

greenLake <- read_csv("snapplusOutput/Green Lake Co/Green_full.csv.gz") %>% 
 mutate_if(is.character, as.factor)
summary(greenLake)
greenLake_snap <- left_join(greenLake, pdata, by = c("crop", "cover", "ManureApp", "SyntheticApp", "density"))
summary(greenLake_snap)

# prepare for binding with ssurgo data
greenLake_snap$SoilSeries <- tolower(greenLake_snap$SoilSeries)
greenLake_snap$County <- tolower(greenLake_snap$County)
greenLake_snap <- greenLake_snap %>%
 mutate_if(is.character, as.factor)

soil <- read_csv("csvs/soil.csv")
greenLake_soil <- soil %>% filter(county == "GreenLake") %>%
 mutate(county = "green lake")

#removed non major components 
greenLake_soil <- greenLake_soil %>%
 select(c(compname, slopelenusle.r, hydgrp, sand, silt, clay, om, k, total.depth, musym, county, mukey)) %>%
 mutate(compname = tolower(compname), 
        county = tolower(county))

summary(greenLake_soil)

# have to check this out by hand and see if there are too many missing soils
k_na <- greenLake_soil %>%
 filter(is.na(k)) %>%
 mutate_if(is.character, as.factor) %>%
 droplevels()

greenLake_soil <- greenLake_soil %>%
 mutate(k = replace_na(k, 0.02))  # per email with Laura

greenLake_SnapSurgo <- left_join(greenLake_snap, greenLake_soil, by = c("SoilSeries" = "compname", "SoilSymbol" = "musym", "County" = "county")) %>%
 mutate_if(is.character, as.factor)
summary(greenLake_SnapSurgo)

k_na <- greenLake_SnapSurgo %>%
 filter(is.na(k)) %>%
 mutate_if(is.character, as.factor) %>%
 droplevels()

which_soils <- k_na %>%
 group_by(SoilSymbol, SoilSeries, mukey) %>%
 tally() # 1 soil out of 103


# recalculate factor and LS based on Snap Slope
greenLake_SnapSurgo <- greenLake_SnapSurgo %>% 
 drop_na(k) %>% # see above note in "which_soils"
 rowwise %>%
 mutate(factor = ifelse(between(slope, 3.01, 4), 0.4, 
                        ifelse(between(slope, 1, 3), 0.3,
                               ifelse(slope < 1, 0.2, 0.5)))) %>%
 # LS = (((slope/((10000+(slope^2))^0.5))*4.56)+(slope/(10000+(slope^2))^0.5)^2*(65.41)+0.065)*(slopelength/72.6)^(factor)
 mutate(LSsurgo = round((((slope/((10000+(slope^2))^0.5))*4.56)+(slope/(10000+(slope^2))^0.5)^2*(65.41)+0.065)*((slopelenusle.r*3.3)/72.6)^(factor),2)) %>%
 dplyr::select(-c(factor))

greenLake_SnapSurgo <- greenLake_SnapSurgo %>%
 drop_na(clay) %>%
 mutate_if(is.character, as.factor)
summary(greenLake_SnapSurgo)
#add R factor ??? it is in the weather/county file but I don't think we use this at all 
#kewa_SnapSurgo <- kewa_SnapSurgo %>%
#mutate(R_factor = 97.69)

write_csv(greenLake_SnapSurgo, "csvs/greenLakeJoinSnapSurgo.csv.gz")

# ozaukee------------------

# add ssurgo soils, pdata and snapplus data ----------------------------------

pdata <- read.csv("csvs/percentPtoTotalP.csv")

# join snapplus and pdata---------------

ozaukee <- read_csv("snapplusOutput/Ozaukee Co/Ozaukee_full.csv.gz") %>% 
 mutate_if(is.character, as.factor)
summary(ozaukee)
ozaukee_snap <- left_join(ozaukee, pdata, by = c("crop", "cover", "ManureApp", "SyntheticApp", "density"))
summary(ozaukee_snap)

# prepare for binding with ssurgo data
ozaukee_snap$SoilSeries <- tolower(ozaukee_snap$SoilSeries)
ozaukee_snap$County <- tolower(ozaukee_snap$County)
ozaukee_snap <- ozaukee_snap %>%
 mutate_if(is.character, as.factor)

soil <- read_csv("csvs/soil.csv")
ozaukee_soil <- soil %>% filter(county == "Ozaukee") %>%
 mutate(county = "ozaukee")

#removed non major components 
ozaukee_soil <- ozaukee_soil %>%
 select(c(compname, slopelenusle.r, hydgrp, sand, silt, clay, om, k, total.depth, musym, county, mukey)) %>%
 mutate(compname = tolower(compname), 
        county = tolower(county))

summary(ozaukee_soil)

# have to check this out by hand and see if there are too many missing soils
k_na <- ozaukee_soil %>%
 filter(is.na(k)) %>%
 mutate_if(is.character, as.factor) %>%
 droplevels()

ozaukee_soil <- ozaukee_soil %>%
 mutate(k = replace_na(k, 0.02))  # per email with Laura

ozaukee_SnapSurgo <- left_join(ozaukee_snap, ozaukee_soil, by = c("SoilSeries" = "compname", "SoilSymbol" = "musym", "County" = "county")) %>%
 mutate_if(is.character, as.factor)
summary(ozaukee_SnapSurgo)

k_na <- ozaukee_SnapSurgo %>%
 filter(is.na(k)) %>%
 mutate_if(is.character, as.factor) %>%
 droplevels()

which_soils <- k_na %>%
 group_by(SoilSymbol, SoilSeries, mukey) %>%
 tally() # 13 soil out of 80


# recalculate factor and LS based on Snap Slope
ozaukee_SnapSurgo <- ozaukee_SnapSurgo %>% 
 drop_na(k) %>% # see above note in "which_soils"
 rowwise %>%
 mutate(factor = ifelse(between(slope, 3.01, 4), 0.4, 
                        ifelse(between(slope, 1, 3), 0.3,
                               ifelse(slope < 1, 0.2, 0.5)))) %>%
 # LS = (((slope/((10000+(slope^2))^0.5))*4.56)+(slope/(10000+(slope^2))^0.5)^2*(65.41)+0.065)*(slopelength/72.6)^(factor)
 mutate(LSsurgo = round((((slope/((10000+(slope^2))^0.5))*4.56)+(slope/(10000+(slope^2))^0.5)^2*(65.41)+0.065)*((slopelenusle.r*3.3)/72.6)^(factor),2)) %>%
 dplyr::select(-c(factor))

ozaukee_SnapSurgo <- ozaukee_SnapSurgo %>%
 drop_na(clay) %>%
 mutate_if(is.character, as.factor)
summary(ozaukee_SnapSurgo)
#add R factor ??? it is in the weather/county file but I don't think we use this at all 
#kewa_SnapSurgo <- kewa_SnapSurgo %>%
#mutate(R_factor = 97.69)

write_csv(ozaukee_SnapSurgo, "csvs/ozaukeeJoinSnapSurgo.csv.gz")

# sheboygan------------------

# add ssurgo soils, pdata and snapplus data ----------------------------------

pdata <- read.csv("csvs/percentPtoTotalP.csv")

# join snapplus and pdata---------------

sheboygan <- read_csv("snapplusOutput/Sheboygan Co/Sheboygan_full.csv.gz") %>% 
 mutate_if(is.character, as.factor)
summary(sheboygan)
sheboygan_snap <- left_join(sheboygan, pdata, by = c("crop", "cover", "ManureApp", "SyntheticApp", "density"))
summary(sheboygan_snap)

# prepare for binding with ssurgo data
sheboygan_snap$SoilSeries <- tolower(sheboygan_snap$SoilSeries)
sheboygan_snap$County <- tolower(sheboygan_snap$County)
sheboygan_snap <- sheboygan_snap %>%
 mutate_if(is.character, as.factor)

soil <- read_csv("csvs/soil.csv")
sheboygan_soil <- soil %>% filter(county == "Sheboygan") %>%
 mutate(county = "sheboygan")

#removed non major components 
sheboygan_soil <- sheboygan_soil %>%
 select(c(compname, slopelenusle.r, hydgrp, sand, silt, clay, om, k, total.depth, musym, county, mukey)) %>%
 mutate(compname = tolower(compname), 
        county = tolower(county))

summary(sheboygan_soil)

# have to check this out by hand and see if there are too many missing soils
k_na <- sheboygan_soil %>%
 filter(is.na(k)) %>%
 mutate_if(is.character, as.factor) %>%
 droplevels()

sheboygan_soil <- sheboygan_soil %>%
 mutate(k = replace_na(k, 0.02))  # per email with Laura

sheboygan_SnapSurgo <- left_join(sheboygan_snap, sheboygan_soil, by = c("SoilSeries" = "compname", "SoilSymbol" = "musym", "County" = "county")) %>%
 mutate_if(is.character, as.factor)
summary(sheboygan_SnapSurgo)

k_na <- sheboygan_SnapSurgo %>%
 filter(is.na(k)) %>%
 mutate_if(is.character, as.factor) %>%
 droplevels()

which_soils <- k_na %>%
 group_by(SoilSymbol, SoilSeries, mukey) %>%
 tally() # 14 soil out of 85


# recalculate factor and LS based on Snap Slope
sheboygan_SnapSurgo <- sheboygan_SnapSurgo %>% 
 drop_na(k) %>% # see above note in "which_soils"
 rowwise %>%
 mutate(factor = ifelse(between(slope, 3.01, 4), 0.4, 
                        ifelse(between(slope, 1, 3), 0.3,
                               ifelse(slope < 1, 0.2, 0.5)))) %>%
 # LS = (((slope/((10000+(slope^2))^0.5))*4.56)+(slope/(10000+(slope^2))^0.5)^2*(65.41)+0.065)*(slopelength/72.6)^(factor)
 mutate(LSsurgo = round((((slope/((10000+(slope^2))^0.5))*4.56)+(slope/(10000+(slope^2))^0.5)^2*(65.41)+0.065)*((slopelenusle.r*3.3)/72.6)^(factor),2)) %>%
 dplyr::select(-c(factor))

sheboygan_SnapSurgo <- sheboygan_SnapSurgo %>%
 drop_na(clay) %>%
 mutate_if(is.character, as.factor)
summary(sheboygan_SnapSurgo)
#add R factor ??? it is in the weather/county file but I don't think we use this at all 
#kewa_SnapSurgo <- kewa_SnapSurgo %>%
#mutate(R_factor = 97.69)

write_csv(sheboygan_SnapSurgo, "csvs/sheboyganJoinSnapSurgo.csv.gz")

# washington------------------

# add ssurgo soils, pdata and snapplus data ----------------------------------

pdata <- read.csv("csvs/percentPtoTotalP.csv")

# join snapplus and pdata---------------

washington <- read_csv("snapplusOutput/Washington Co/Washington_full.csv.gz") %>% 
 mutate_if(is.character, as.factor)
summary(washington)
washington_snap <- left_join(washington, pdata, by = c("crop", "cover", "ManureApp", "SyntheticApp", "density"))
summary(washington_snap)

# prepare for binding with ssurgo data
washington_snap$SoilSeries <- tolower(washington_snap$SoilSeries)
washington_snap$County <- tolower(washington_snap$County)
washington_snap <- washington_snap %>%
 mutate_if(is.character, as.factor)

soil <- read_csv("csvs/soil.csv")
washington_soil <- soil %>% filter(county == "Washington") %>%
 mutate(county = "washington")

#removed non major components 
washington_soil <- washington_soil %>%
 select(c(compname, slopelenusle.r, hydgrp, sand, silt, clay, om, k, total.depth, musym, county, mukey)) %>%
 mutate(compname = tolower(compname), 
        county = tolower(county))

summary(washington_soil)

# have to check this out by hand and see if there are too many missing soils
k_na <- washington_soil %>%
 filter(is.na(k)) %>%
 mutate_if(is.character, as.factor) %>%
 droplevels()

washington_soil <- washington_soil %>%
 mutate(k = replace_na(k, 0.02))  # per email with Laura

washington_SnapSurgo <- left_join(washington_snap, washington_soil, by = c("SoilSeries" = "compname", "SoilSymbol" = "musym", "County" = "county")) %>%
 mutate_if(is.character, as.factor)
summary(washington_SnapSurgo)

k_na <- washington_SnapSurgo %>%
 filter(is.na(k)) %>%
 mutate_if(is.character, as.factor) %>%
 droplevels()

which_soils <- k_na %>%
 group_by(SoilSymbol, SoilSeries, mukey) %>%
 tally() # 18 soil out of 101


# recalculate factor and LS based on Snap Slope
washington_SnapSurgo <- washington_SnapSurgo %>% 
 drop_na(k) %>% # see above note in "which_soils"
 rowwise %>%
 mutate(factor = ifelse(between(slope, 3.01, 4), 0.4, 
                        ifelse(between(slope, 1, 3), 0.3,
                               ifelse(slope < 1, 0.2, 0.5)))) %>%
 # LS = (((slope/((10000+(slope^2))^0.5))*4.56)+(slope/(10000+(slope^2))^0.5)^2*(65.41)+0.065)*(slopelength/72.6)^(factor)
 mutate(LSsurgo = round((((slope/((10000+(slope^2))^0.5))*4.56)+(slope/(10000+(slope^2))^0.5)^2*(65.41)+0.065)*((slopelenusle.r*3.3)/72.6)^(factor),2)) %>%
 dplyr::select(-c(factor))

washington_SnapSurgo <- washington_SnapSurgo %>%
 drop_na(clay) %>%
 mutate_if(is.character, as.factor)
summary(washington_SnapSurgo)
#add R factor ??? it is in the weather/county file but I don't think we use this at all 
#kewa_SnapSurgo <- kewa_SnapSurgo %>%
#mutate(R_factor = 97.69)

write_csv(washington_SnapSurgo, "csvs/washingtonJoinSnapSurgo.csv.gz")

# kewaunee ----------------------------------------------------------------


kewa0 <- read.csv("KewauneeCounty/Full_Kewaunee_No_Contour.csv") %>%
  mutate_if(is.character, as.factor)

 # kewa_sum0 <- kewa0 %>%
 #   group_by(SoilSeries, SoilSymbol, crop) %>%
 #   tally()
 # 
 # kewa_soil0 <- kewa0 %>%
 #   group_by(SoilSeries, SoilSymbol) %>%
 #   tally()

kewa1contour <- read.csv("KewauneeCounty/Merged - Kewaunee Co CONTOUR - Batch Run 2021-08-03.csv") %>%
  mutate_if(is.character, as.factor)
# summary(kewa1contour) #456435 x 20
# 
# kewa_sum1 <- kewa1contour %>%
#   group_by(SoilSeries, SoilSymbol, crop) %>%
#   tally()
# # 
# kewa_soil1 <- kewa1contour %>%
#   group_by(SoilSeries, SoilSymbol) %>%
#   tally()

kewaDL <- read.csv("KewauneeCounty/Merged - Kewaunee Co Dry Lot - Batch Run 2021-09-15.csv") %>%
  mutate_if(is.character, as.factor) ##TODO missing 2 soils

# kewaDL_sum <- kewaDL %>%
#   group_by(SoilSeries, SoilSymbol) %>%
#   tally() 
# 
# kewaDL_sum2 <- kewaDL %>%
#   mutate(FertManure = as.factor(paste(SyntheticApp, ManureApp))) %>%
#   group_by(SoilSeries, SoilSymbol, slope, density, initialP, FertManure) %>%
#   tally() 

kewa <- bind_rows(kewa0, kewa1contour, kewaDL)
#rm(list = c("kewa0", "kewa1contour", "kewaDL"))

write.csv(kewa, "Kewaunee.csv", row.names = FALSE, quote = FALSE)


# marathon county ---------------------------------------------------------


mara0contour <- read.csv("MarathonCounty/Merged - Marathon Co - Batch Run 2021-05-19.csv") %>%
  mutate_if(is.character, as.factor)
#summary(mara0contour) #316710 * 20

# marasum0 <- mara0contour %>%
#   group_by(SoilSymbol, SoilSeries, crop) %>%
#   tally()
# 
# marasoil0 <- mara0contour %>%
#   group_by(SoilSymbol, SoilSeries) %>%
#   tally() ##TODO RsB Antigo is missing a fertilizer run -
# 
# check <- mara0contour %>%
#   filter(SoilSeries == "ANTIGO" & SoilSymbol == "RsB") %>%
#   droplevels() %>%
#   mutate(FertManure = as.factor(paste(SyntheticApp, ManureApp))) %>%
#   group_by(crop, cover, tillage, density, rotational, slope, initialP, FertManure) %>%
#   tally() ##TODO yes it is missing a fertilizer run (0 150)

mara1contour <- read.csv("MarathonCounty/Merged - Marathon Co CONTOUR - Batch Run 2021-07-28.csv") %>%
  mutate_if(is.character, as.factor)
# summary(mara1contour)
# 
# marasum1 <- mara1contour %>%
#   group_by(SoilSymbol, SoilSeries, crop) %>%
#   tally()
# 
# marasoil1 <- mara1contour %>%
#   group_by(SoilSymbol, SoilSeries) %>%
#   tally() # looks good

maraDL <- read.csv("MarathonCounty/Merged - Marathon Co Dry Lot - Batch Run 2021-09-13.csv")%>%
  mutate_if(is.character, as.factor)

# maraDLsoil <- maraDL %>%
#   group_by(SoilSymbol, SoilSeries) %>%
#   tally() # looks good

mara <- bind_rows(mara0contour, mara1contour, maraDL)
write.csv(mara, "Marathon.csv", row.names = FALSE, quote = FALSE)
rm(list = ls())


# clark county ------------------------------------------------------------


clark0 <- read.csv("ClarkCounty/Merged - Clark Co - Batch Run 2021-08-16.csv") %>%
  mutate_if(is.character, as.factor)

# clark0sum <- clark0 %>%
#   group_by(SoilSymbol, SoilSeries, crop) %>%
#   tally()
# clark0soil <- clark0 %>%
#   group_by(SoilSymbol, SoilSeries) %>%
#   tally() ##TODO BoC Boone and Cd Citypoint missing some
# # 
# # boc0 <- clark0 %>%
#   filter(SoilSeries == "BOONE" & SoilSymbol == "BoC") %>%
#   mutate(FertManure = as.factor(paste(SyntheticApp, ManureApp))) %>%
#   group_by(crop, cover, tillage, density, rotational, initialP, slope, FertManure) %>%
#   tally() # missing a slope (only 6 and 15)
# 
# cd0 <- clark0 %>%
#   filter(SoilSeries == "CITYPOINT" & SoilSymbol == "Cd") %>%
#   mutate(FertManure = as.factor(paste(SyntheticApp, ManureApp))) %>%
#   group_by(crop, cover, tillage, density, rotational, initialP,slope) %>%
#   tally() # missing a slope (only 0.5 and 1)

clark1 <- read.csv("ClarkCounty/Merged - Clark Co CONTOUR - Batch Run 2021-08-22.csv") %>%
  mutate_if(is.character, as.factor)

# clark1sum <- clark1 %>%
#   group_by(SoilSymbol, SoilSeries, crop) %>%
#   tally()
# clark1soil <- clark1 %>%
#   group_by(SoilSymbol, SoilSeries) %>%
#   tally() ##TODO BoC Boone and Cd Citypoint and 679A Ettrick missing some
# 
# boc1 <- clark1 %>%
#   filter(SoilSeries == "BOONE" & SoilSymbol == "BoC") %>%
#   mutate(FertManure = as.factor(paste(SyntheticApp, ManureApp))) %>%
#   group_by(crop, cover, tillage, density, rotational, initialP, slope, FertManure) %>%
#   tally() # missing a slope (only 6 and 15)
# 
# cd1 <- clark1 %>%
#   filter(SoilSeries == "CITYPOINT" & SoilSymbol == "Cd") %>%
#   mutate(FertManure = as.factor(paste(SyntheticApp, ManureApp))) %>%
#   group_by(crop, cover, tillage, density, rotational, initialP,slope, FertManure) %>%
#   tally() # missing a slope (only 0.5 and 1)
# 
# ettric <- clark1 %>%
#   filter(SoilSeries == "ETTRICK" & SoilSymbol == "679A") %>%
#   mutate(FertManure = as.factor(paste(SyntheticApp, ManureApp))) %>%
#   group_by(crop, cover, tillage, density, rotational, initialP, slope, FertManure) %>%
#   tally() # missing a fertilizer treatment (0 150)

clarkDL <- read.csv("ClarkCounty/Merged - Clark Co DryLot - Batch Run 2021-09-13.csv") %>%
  mutate_if(is.character, as.factor)

# clarkDLsoil <- clarkDL %>%
#   group_by(SoilSymbol, SoilSeries) %>%
#   tally() ##TODO BoC Boone and Cd Citypoint missing some
# 
# bocDL <- clarkDL %>%
#   filter(SoilSeries == "BOONE" & SoilSymbol == "BoC") %>%
#   mutate(FertManure = as.factor(paste(SyntheticApp, ManureApp))) %>%
#   group_by(crop, cover, tillage, density, rotational, initialP, slope, FertManure) %>%
#   tally() # missing a slope (only 6 and 15)
# 
# cdDL <- clarkDL %>%
#   filter(SoilSeries == "CITYPOINT" & SoilSymbol == "Cd") %>%
#   mutate(FertManure = as.factor(paste(SyntheticApp, ManureApp))) %>%
#   group_by(crop, cover, tillage, density, rotational, initialP,slope, FertManure) %>%
#   tally() # missing a slope (only 0.5 and 1)

clark <- bind_rows(clark1, clark0, clarkDL)
write.csv(clark, "Clark.csv", row.names = FALSE, quote = FALSE)
rm(list = ls())


# taylor ------------------------------------------------------------------

tay0 <- read.csv("TaylorCounty/Merged - Taylor Co - Batch Run 2021-08-28.csv") %>%
  mutate_if(is.character, as.factor)

# tay0soil <- tay0 %>%
#   group_by(SoilSeries, SoilSymbol) %>%
#   tally() # cathro 408A, Besemen 414A, Loxley 414A, Loxley 9055A, Lupton 408A
# 
# cat <- tay0 %>%
#   filter(SoilSeries == "CATHRO" & SoilSymbol == "408A") %>%
#   mutate(FertManure = as.factor(paste(SyntheticApp, ManureApp))) %>%
#   group_by(crop, cover, tillage, density, rotational, initialP, slope) %>%
#   tally()
# 
# bes <- tay0 %>%
#   filter(SoilSeries == "BESEMAN" & SoilSymbol == "414A") %>%
#   mutate(FertManure = as.factor(paste(SyntheticApp, ManureApp))) %>%
#   group_by(crop, cover, tillage, density, rotational, initialP, slope, FertManure) %>%
#   tally()
# 
# lox <- tay0 %>%
#   filter(SoilSeries == "LOXLEY" & SoilSymbol == "414A") %>%
#   mutate(FertManure = as.factor(paste(SyntheticApp, ManureApp))) %>%
#   group_by(crop, cover, tillage, density, rotational, initialP, slope, FertManure) %>%
#   tally()
# 
# lox2 <- tay0 %>%
#   filter(SoilSeries == "LOXLEY" & SoilSymbol == "9055A") %>%
#   mutate(FertManure = as.factor(paste(SyntheticApp, ManureApp))) %>%
#   group_by(crop, cover, tillage, density, rotational, initialP, slope, FertManure) %>%
#   tally()
# 
# lupton <- tay0 %>%
#   filter(SoilSeries == "LUPTON" & SoilSymbol == "408A") %>%
#   mutate(FertManure = as.factor(paste(SyntheticApp, ManureApp))) %>%
#   group_by(crop, cover, tillage, density, rotational, initialP, slope, FertManure) %>%
#   tally()

tay1 <- read.csv("TaylorCounty/Merged - Taylor Co CONTOUR - Batch Run 2021-09-03.csv")

# tay1soil <- tay1 %>%
#   group_by(SoilSeries, SoilSymbol) %>%
#   tally() # cathro 408A, Besemen 414A, Loxley 414A, Loxley 9055A, Lupton 408A, Pelesier 9197C, worcester 192A
# 
# cat1 <- tay1 %>%
#   filter(SoilSeries == "CATHRO" & SoilSymbol == "408A") %>%
#   mutate(FertManure = as.factor(paste(SyntheticApp, ManureApp))) %>%
#   group_by(crop, cover, tillage, density, rotational, initialP, slope) %>%
#   tally()
# 
# bes1 <- tay1 %>%
#   filter(SoilSeries == "BESEMAN" & SoilSymbol == "414A") %>%
#   mutate(FertManure = as.factor(paste(SyntheticApp, ManureApp))) %>%
#   group_by(crop, cover, tillage, density, rotational, initialP, slope, FertManure) %>%
#   tally()
# 
# lox1 <- tay1 %>%
#   filter(SoilSeries == "LOXLEY" & SoilSymbol == "414A") %>%
#   mutate(FertManure = as.factor(paste(SyntheticApp, ManureApp))) %>%
#   group_by(crop, cover, tillage, density, rotational, initialP, slope, FertManure) %>%
#   tally()
# 
# lox1 <- tay1 %>%
#   filter(SoilSeries == "LOXLEY" & SoilSymbol == "9055A") %>%
#   mutate(FertManure = as.factor(paste(SyntheticApp, ManureApp))) %>%
#   group_by(crop, cover, tillage, density, rotational, initialP, slope, FertManure) %>%
#   tally()
# 
# lupton1 <- tay1 %>%
#   filter(SoilSeries == "LUPTON" & SoilSymbol == "408A") %>%
#   mutate(FertManure = as.factor(paste(SyntheticApp, ManureApp))) %>%
#   group_by(crop, cover, tillage, density, rotational, initialP, slope, FertManure) %>%
#   tally()
# 
# pel <- tay1 %>%
#   filter(SoilSeries == "PELISSIER" & SoilSymbol == "9197C") %>%
#   mutate(FertManure = as.factor(paste(SyntheticApp, ManureApp))) %>%
#   group_by(crop, cover, tillage, density, rotational, initialP, slope, FertManure) %>%
#   tally()
# 
# wor <- tay1 %>%
#   filter(SoilSeries == "WORCESTER" & SoilSymbol == "192A") %>%
#   mutate(FertManure = as.factor(paste(SyntheticApp, ManureApp))) %>%
#   group_by(crop, cover, tillage, density, rotational, initialP, slope, FertManure) %>%
#   tally()

tayDL <- read.csv("TaylorCounty/Merged - Taylor Co Dry Lot - Batch Run 2021-09-14.csv")

# tayDLsoil <- tayDL %>%
#   group_by(SoilSeries, SoilSymbol) %>%
#   tally() #cathro 408A, Besemen 414A, Loxley 414A, Loxley 9055A, Lupton 408A
# 
# catDL <- tayDL %>%
#   filter(SoilSeries == "CATHRO" & SoilSymbol == "408A") %>%
#   mutate(FertManure = as.factor(paste(SyntheticApp, ManureApp))) %>%
#   group_by(crop, cover, tillage, density, rotational, initialP, slope, FertManure) %>%
#   tally()
# 
# besDL <- tayDL %>%
#   filter(SoilSeries == "BESEMAN" & SoilSymbol == "414A") %>%
#   mutate(FertManure = as.factor(paste(SyntheticApp, ManureApp))) %>%
#   group_by(crop, cover, tillage, density, rotational, initialP, slope, FertManure) %>%
#   tally()
# 
# loxDL <- tayDL %>%
#   filter(SoilSeries == "LOXLEY" & SoilSymbol == "414A") %>%
#   mutate(FertManure = as.factor(paste(SyntheticApp, ManureApp))) %>%
#   group_by(crop, cover, tillage, density, rotational, initialP, slope, FertManure) %>%
#   tally()
# 
# loxDL2 <- tayDL %>%
#   filter(SoilSeries == "LOXLEY" & SoilSymbol == "9055A") %>%
#   mutate(FertManure = as.factor(paste(SyntheticApp, ManureApp))) %>%
#   group_by(crop, cover, tillage, density, rotational, initialP, slope, FertManure) %>%
#   tally()
# 
# luptonDL <- tayDL %>%
#   filter(SoilSeries == "LUPTON" & SoilSymbol == "408A") %>%
#   mutate(FertManure = as.factor(paste(SyntheticApp, ManureApp))) %>%
#   group_by(crop, cover, tillage, density, rotational, initialP, slope, FertManure) %>%
#   tally()

taylor <- bind_rows(tay0, tay1, tayDL)
write.csv(taylor, "Taylor.csv", row.names = FALSE, quote = FALSE)
rm(list = ls())

# load complete snapplus batches ------------------------------------------

kewa <- read.csv("Kewaunee.csv") #105 soils
mara <- read.csv("Marathon.csv") #73 soils
clark <- read.csv("Clark.csv") #124 soils
taylor <- read.csv("Taylor.csv") #133
taySum <- taylor %>%
  group_by(SoilSeries, SoilSymbol) %>%
  tally()

# add ssurgo, pdata and weather to soils ----------------------------------

setwd("/Volumes/GoogleDrive/My Drive/grassland2.0/grazescape/Soil Data/SSURGO data")
kewa_soil <- read.table("Final Clean Soil/kewaunee_soil.txt", header = TRUE, sep = "|") #105 
mara_soil <- read.table("Final Clean Soil/marathon_soil.txt", header = TRUE, sep = "|") #76
clark_soil <- read.table("Final Clean Soil/clark_soil.txt", header = TRUE, sep = "|") #131
taylor_soil <- read.table("Final Clean Soil/Taylor_soil.txt", header = TRUE, sep = "|") #131
setwd("/Volumes/GoogleDrive/My Drive/grassland2.0/grazescape/SnapPlus Module/Batch Simulator/R scripts")
pdata <- read.csv("../csvs/percentPtoTotalP.csv")
weather <- read.csv("../modelCreation/data/weatherByCounty.csv")

# create kewaunee data set ------------------------------------------------

#join P data and snap data
kewa_snap <- left_join(kewa, pdata, by = c("crop", "cover", "ManureApp", "SyntheticApp", "density"))

# prepare for binding with ssurgo data
kewa_snap$SoilSeries <- tolower(kewa_snap$SoilSeries)
kewa_snap$County <- tolower(kewa_snap$County)
kewa_snap <- kewa_snap %>%
  mutate_if(is.character, as.factor)

#removed non major components 
kewa_soil <- kewa_soil %>%
  select(c(compname, slopelenusle.r, hydgrp, sand, silt, clay, om, k, total.depth, musym, county)) %>%
  mutate(compname = tolower(compname), 
         county = tolower(county))

k_na <- kewa_soil %>%
  filter(is.na(k)) %>%
  mutate_if(is.character, as.factor) %>%
  droplevels()

kewa_soil <- kewa_soil %>%
  mutate(k = replace_na(k, 0.02)) %>% # per email with Laura
  mutate(sand = replace_na(sand, 0),
         silt = replace_na(silt, 0),
         clay = replace_na(clay, 0))

summary(kewa_soil)

kewa_SnapSurgo <- left_join(kewa_snap, kewa_soil, by = c("SoilSeries" = "compname", "SoilSymbol" = "musym", "County" = "county")) %>%
  mutate_if(is.character, as.factor)
summary(kewa_SnapSurgo)
# recalculate factor and LS based on Snap Slope
kewa_SnapSurgo <- kewa_SnapSurgo %>% rowwise %>%
  mutate(factor = ifelse(between(slope, 3.01, 4), 0.4, 
                         ifelse(between(slope, 1, 3), 0.3,
                                ifelse(slope < 1, 0.2, 0.5)))) %>%
  # LS = (((slope/((10000+(slope^2))^0.5))*4.56)+(slope/(10000+(slope^2))^0.5)^2*(65.41)+0.065)*(slopelength/72.6)^(factor)
  mutate(LSsurgo = round((((slope/((10000+(slope^2))^0.5))*4.56)+(slope/(10000+(slope^2))^0.5)^2*(65.41)+0.065)*((slopelenusle.r*3.3)/72.6)^(factor),2)) %>%
  dplyr::select(-c(factor))
kewa_SnapSurgo <- kewa_SnapSurgo %>%
  mutate_if(is.character, as.factor)
summary(kewa_SnapSurgo)
#add R factor
kewa_SnapSurgo <- kewa_SnapSurgo %>%
  mutate(R_factor = 97.69)

write.csv(kewa_SnapSurgo, "../modelCreation/data/kewaJoinSnapSurgo.csv", quote = FALSE, row.names = FALSE)
test <- read_csv("../modelCreation/data/kewaJoinSnapSurgo.csv")
summary(test)

# create marathon data set ------------------------------------------------

#join P data and snap data
mara_snap <- left_join(mara, pdata, by = c("crop", "cover", "ManureApp", "SyntheticApp", "density"))

# prepare for binding with ssurgo data
mara_snap$SoilSeries <- tolower(mara_snap$SoilSeries)
mara_snap$County <- tolower(mara_snap$County)
mara_snap <- mara_snap %>%
  mutate_if(is.character, as.factor)

#removed non major components 
mara_soil <- mara_soil %>%
  select(c(compname, slopelenusle.r, hydgrp, sand, silt, clay, om, k, total.depth, musym, county)) %>%
  mutate(compname = tolower(compname), 
         county = tolower(county))
summary(mara_soil)
k_na <- mara_soil %>%
  filter(is.na(k)) %>%
  mutate_if(is.character, as.factor) %>%
  droplevels()
summary(k_na)
# remove pits and water, replace NA with 0.02 for mucky soils k
mara_soil <- mara_soil %>%
  mutate(k = replace_na(k, 0.02))
names(mara_soil)
mara_soil <- mara_soil %>%
  group_by(county, compname, musym, hydgrp) %>%
  summarise(slopelenusle.r = mean(slopelenusle.r),
            sand = mean(sand),
            silt = mean(silt),
            clay = mean(clay),
            om = mean(om),
            k = mean(k),
            total.depth = mean(total.depth))
mara_SnapSurgo <- left_join(mara_snap, mara_soil, by = c("SoilSeries" = "compname", "SoilSymbol" = "musym", "County" = "county")) %>%
  mutate_if(is.character, as.factor)
summary(mara_SnapSurgo)
# recalculate factor and LS based on Snap Slope
mara_SnapSurgo <- mara_SnapSurgo %>% rowwise %>%
  mutate(factor = ifelse(between(slope, 3.01, 4), 0.4, 
                         ifelse(between(slope, 1, 3), 0.3,
                                ifelse(slope < 1, 0.2, 0.5)))) %>%
  # LS = (((slope/((10000+(slope^2))^0.5))*4.56)+(slope/(10000+(slope^2))^0.5)^2*(65.41)+0.065)*(slopelength/72.6)^(factor)
  mutate(LSsurgo = round((((slope/((10000+(slope^2))^0.5))*4.56)+(slope/(10000+(slope^2))^0.5)^2*(65.41)+0.065)*((slopelenusle.r*3.3)/72.6)^(factor),2)) %>%
  mutate(R_factor = 122.49) %>%
  dplyr::select(-c(factor)) %>%
  mutate_if(is.character, as.factor)
summary(mara_SnapSurgo)


write.csv(mara_SnapSurgo, "../modelCreation/data/maraJoinSnapSurgo.csv",  quote = FALSE, row.names = FALSE)
test <- read_csv("../modelCreation/data/maraJoinSnapSurgo.csv")
summary(test)

# create clark data set ------------------------------------------------

#join P data and snap data
clark_snap <- left_join(clark, pdata, by = c("crop", "cover", "ManureApp", "SyntheticApp", "density"))

# prepare for binding with ssurgo data
clark_snap$SoilSeries <- tolower(clark_snap$SoilSeries)
clark_snap$County <- tolower(clark_snap$County)
clark_snap <- clark_snap %>%
  mutate_if(is.character, as.factor)

#removed non major components 
clark_soil <- clark_soil %>%
  select(c(compname, slopelenusle.r, hydgrp, sand, silt, clay, om, k, total.depth, musym, county)) %>%
  mutate(compname = tolower(compname), 
         county = tolower(county))
summary(clark_soil)
k_na <- clark_soil %>%
  filter(is.na(k)) %>%
  mutate_if(is.character, as.factor) %>%
  droplevels()
summary(k_na)
clark_soil <- clark_soil %>%
  mutate(k = replace_na(k, 0.02),
         sand = replace_na(sand, 0),
         silt = replace_na(silt, 0),
         clay = replace_na(clay,0))
summary(clark_soil)
clark_soil <- clark_soil %>%
  group_by(county, compname, musym, hydgrp) %>%
  summarise(slopelenusle.r = mean(slopelenusle.r),
            sand = mean(sand),
            silt = mean(silt),
            clay = mean(clay),
            om = mean(om),
            k = mean(k),
            total.depth = mean(total.depth))
# change crystal lake cyb to cub
clark_soil <- clark_soil %>%
  mutate(musym = case_when(
    compname == "crystal lake" ~ "CuB",
    TRUE ~ musym))

clark_SnapSurgo <- left_join(clark_snap, clark_soil, by = c("SoilSeries" = "compname", "SoilSymbol" = "musym", "County" = "county")) %>%
  mutate_if(is.character, as.factor)
summary(clark_SnapSurgo)
# recalculate factor and LS based on Snap Slope
clark_SnapSurgo <- clark_SnapSurgo %>% rowwise %>%
  mutate(factor = ifelse(between(slope, 3.01, 4), 0.4, 
                         ifelse(between(slope, 1, 3), 0.3,
                                ifelse(slope < 1, 0.2, 0.5)))) %>%
  # LS = (((slope/((10000+(slope^2))^0.5))*4.56)+(slope/(10000+(slope^2))^0.5)^2*(65.41)+0.065)*(slopelength/72.6)^(factor)
  mutate(LSsurgo = round((((slope/((10000+(slope^2))^0.5))*4.56)+(slope/(10000+(slope^2))^0.5)^2*(65.41)+0.065)*((slopelenusle.r*3.3)/72.6)^(factor),2)) %>%
  dplyr::select(-c(factor)) %>%
  mutate(R_factor = 132.95) %>%
  mutate_if(is.character, as.factor)
summary(clark_SnapSurgo)


write.csv(clark_SnapSurgo, "../modelCreation/data/clarkJoinSnapSurgo.csv", quote = FALSE, row.names = FALSE)
test <- read_csv("../modelCreation/data/clarkJoinSnapSurgo.csv")


# create taylor dataset ---------------------------------------------------

#join P data and snap data
tay_snap <- left_join(taylor, pdata, by = c("crop", "cover", "ManureApp", "SyntheticApp", "density"))

# prepare for binding with ssurgo data
tay_snap$SoilSeries <- tolower(tay_snap$SoilSeries)
tay_snap$County <- tolower(tay_snap$County)
tay_snap <- tay_snap %>%
  mutate_if(is.character, as.factor)

#removed non major components 
taylor_soil <- taylor_soil %>%
  select(c(compname, slopelenusle.r, hydgrp, sand, silt, clay, om, k, total.depth, musym, county)) %>%
  mutate(compname = tolower(compname), 
         county = tolower(county))
summary(taylor_soil)
k_na <- taylor_soil %>%
  filter(is.na(k)) %>%
  mutate_if(is.character, as.factor) %>%
  droplevels()
summary(k_na)
taylor_soil <- taylor_soil %>%
  mutate(k = replace_na(k, 0.02))
summary(taylor_soil)
taylor_soil <- taylor_soil %>%
  group_by(county, compname, musym, hydgrp) %>%
  summarise(slopelenusle.r = mean(slopelenusle.r),
            sand = mean(sand),
            silt = mean(silt),
            clay = mean(clay),
            om = mean(om),
            k = mean(k),
            total.depth = mean(total.depth))

taylor_SnapSurgo <- left_join(tay_snap, taylor_soil, by = c("SoilSeries" = "compname", "SoilSymbol" = "musym", "County" = "county")) %>%
  mutate_if(is.character, as.factor)
summary(taylor_SnapSurgo)
which_na <- taylor_SnapSurgo %>%
  filter(is.na(total.depth)) %>%
  mutate_if(is.character, as.factor) %>%
  droplevels()
summary(which_na)
# drop NAs because they aren't in SSURGO
taylor_SnapSurgo1 <- taylor_SnapSurgo %>%
  drop_na(total.depth)
summary(taylor_SnapSurgo1)
# still have slopelength NAs
sl_na <- taylor_SnapSurgo1 %>%
  filter(is.na(slopelenusle.r)) %>%
  mutate_if(is.character, as.factor) %>%
  droplevels()
summary(sl_na) # will have to drop 
taylor_SnapSurgo <- taylor_SnapSurgo %>%
  drop_na(slopelenusle.r)

# recalculate factor and LS based on Snap Slope
taylor_SnapSurgo <- taylor_SnapSurgo %>% rowwise %>%
  mutate(factor = ifelse(between(slope, 3.01, 4), 0.4, 
                         ifelse(between(slope, 1, 3), 0.3,
                                ifelse(slope < 1, 0.2, 0.5)))) %>%
  # LS = (((slope/((10000+(slope^2))^0.5))*4.56)+(slope/(10000+(slope^2))^0.5)^2*(65.41)+0.065)*(slopelength/72.6)^(factor)
  mutate(LSsurgo = round((((slope/((10000+(slope^2))^0.5))*4.56)+(slope/(10000+(slope^2))^0.5)^2*(65.41)+0.065)*((slopelenusle.r*3.3)/72.6)^(factor),2)) %>%
  dplyr::select(-c(factor)) %>%
  mutate(R_factor = 122.83) %>%
  mutate_if(is.character, as.factor)
summary(taylor_SnapSurgo)

write.csv(taylor_SnapSurgo, "../modelCreation/data/taylorJoinSnapSurgo.csv", quote = FALSE, row.names = FALSE)
test <- read_csv("../modelCreation/data/taylorJoinSnapSurgo.csv")
