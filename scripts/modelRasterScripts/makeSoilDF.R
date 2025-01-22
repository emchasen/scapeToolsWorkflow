library(raster)
library(tidyverse)

# load data and create soils data set -------------------------------------

# load soil rasters
slope <- raster("eastCentralWI_slopePer_30m.tiff")
slope_xy <- as.data.frame(slope, xy = TRUE)
sand <- raster("eastCentralWI_sand_30m.tiff")
sand_xy <- as.data.frame(sand, xy = TRUE)
clay <- raster("eastCentralWI_clay_30m.tiff")
clay_xy <- as.data.frame(clay, xy = TRUE)
slopelen <- raster("eastCentralWI_slopelen_30m.tiff")
slopelen_xy <- as.data.frame(slopelen, xy = TRUE)
silt <- raster("eastCentralWI_silt_30m.tiff")
silt_xy <- as.data.frame(silt, xy = TRUE)
kfact <- raster("eastCentralWI_kfact_30m.tiff")
k_xy <- as.data.frame(kfact, xy = TRUE)
om <- raster("eastCentralWI_om_30m.tiff")
om_xy <- as.data.frame(om, xy = TRUE)
hydgrp <- raster("eastCentralWI_hydgrp_30m.tiff")
hydgrp_xy <- as.data.frame(hydgrp, xy = TRUE)
depth <- raster("eastCentralWI_depth_30m.tiff")
depth_xy <- as.data.frame(depth, xy = TRUE)
LS <- raster("eastCentralWI_LS_30m.tif")
ls_xy <- as.data.frame(LS, xy = TRUE)


hydgrp_xy <- hydgrp_xy %>%
  mutate(hydgrp.num = as.factor(recode(eastCentralWI_hydgrp_30m,
                                       "1" = "A",
                                       #"1.25" = "A/D",
                                       "1.5" = "A/D",
                                       #"1.75" = "A/D",
                                       "2" = "B",
                                       # "2.25" = "B/D",
                                       "2.5" = "B/D",
                                       # "2.75" = "B/D",
                                       "3" = "C",
                                       # "3.25" = "C/D",
                                       "3.5" = "C/D",
                                       #"3.75" = "C/D",
                                       "4" = 'D'))) %>%
  dplyr::select(c(x,y,hydgrp.num))

soil_xy <- left_join(slope_xy, sand_xy) %>%
  rename("slope" = "lyr.1") %>%
  left_join(clay_xy) %>%
  left_join(slopelen_xy) %>%
  left_join(silt_xy) %>%
  left_join(k_xy) %>%
  left_join(om_xy) %>%
  left_join(hydgrp_xy) %>%
  left_join(depth_xy) %>%
  left_join(ls_xy) %>%
  rename("LSsurgo" = "lyr.1")

soil_xy <- soil_xy %>%
  rename("slopelenusle.r" = "eastCentralWI_slopelen_30m",
         "sand" = "eastCentralWI_sand_30m",
         "silt" = "eastCentralWI_silt_30m",
         "clay" = "eastCentralWI_clay_30m",
         "k" = "eastCentralWI_kfact_30m",
         "OM" = "eastCentralWI_om_30m",
         "hydgrp" = "hydgrp.num",
         "total.depth" = "eastCentralWI_depth_30m")

print(summary(soil_xy))

write_csv(soil_xy, "eastCentralSoilXY.csv.gz")
nrow(soil_xy)
print(head(na.omit(soil_xy)))






