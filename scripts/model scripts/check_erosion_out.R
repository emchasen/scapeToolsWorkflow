# this script compares erosion models from southWest snap plus runs
# rmse in script is calculated from full data while rmse in plot is from test set (in condor)

#load libraries
library(tidyverse)
library(randomForest)
library(tidymodels)


##load data files from each county in learning hub---------
sheb <- read_csv("csvs/sheboyganJoinSnapSurgo.csv.gz", col_types = list( "SoilSymbol" = col_character()))
#head(sheb)
fdl <- read_csv("csvs/fondDuLacJoinSnapSurgo.csv.gz", col_types = list( "SoilSymbol" = col_character()))
#head(fdl)
gl <- read_csv("csvs/greenLakeJoinSnapSurgo.csv.gz", col_types = list( "SoilSymbol" = col_character()))
#head(gl)
dod <- read_csv("csvs/dodgeJoinSnapSurgo.csv.gz", col_types = list( "SoilSymbol" = col_character()))
#head(dod)
wash <- read_csv("csvs/washingtonJoinSnapSurgo.csv.gz", col_types = list( "SoilSymbol" = col_character()))
#head(wash)
ozak <- read_csv("csvs/ozaukeeJoinSnapSurgo.csv.gz", col_types = list( "SoilSymbol" = col_character()))
#head(ozak)
cal <- read_csv("csvs/calumetJoinSnapSurgo.csv.gz", col_types = list( "SoilSymbol" = col_character()))
#head(cal)

# create one data frame-------------
full <- bind_rows(sheb, fdl, gl, dod, wash, ozak, cal)
#print(paste('total data is', dim(full)))
rm(list = c("sheb", "fdl", "gl", "dod", "wash", "ozak", "cal"))



# cont corn ---------------------------------------------------------------

cc <- full %>%
  filter(crop == "cc") %>%
  mutate_if(is.character, as.factor) %>%
  dplyr::select(c(Erosion, cover, tillage, Contour, total_DM_lbs, slope, slopelenusle.r, sand, silt, clay, k)) %>%
  mutate(Contour = as.factor(Contour)) %>%
  distinct() %>%
  drop_na() %>%
  droplevels()

# null hypothesis
cc.best.guess <- round(mean(cc$Erosion),2) 

# Evaluate RMSE
cc.RMSE.baseline <- round(sqrt(mean((cc.best.guess-cc$Erosion)^2)),2)


#load model
cc_mod <- readRDS("modelOutputs/eastCentralWI/erosion/cc_erosion_eastCentralWI.rds")
cc_mod <- readRDS("/Volumes/cgratton/scapetools/serverReady/grazeScape/modelFiles/southWestWI/cc_erosion_southWestWI.rds")
cc_mod

# predictions
pred_cc <- cc_mod %>%
  predict(cc) %>%
  bind_cols(cc)

cc_rmse <- round(sqrt(mean((pred_cc$.pred - pred_cc$Erosion)^2)),3) 
# plot data
ggplot(pred_cc, aes(x = Erosion, y = .pred)) +
  geom_point() 

rss <- sum((pred_cc$.pred - pred_cc$Erosion) ^ 2)  ## residual sum of squares
tss <- sum((pred_cc$Erosion - mean(pred_cc$Erosion)) ^ 2)  ## total sum of squares
rsq <- 1 - rss/tss

# corn grain --------------------------------------------------------------

# clean data
cg <- full %>%
  filter(crop == "cg") %>%
  mutate_if(is.character, as.factor) %>%
  dplyr::select(c(Erosion, cover, tillage, Contour, total_DM_lbs, slope, slopelenusle.r, sand, silt, clay, k)) %>%
  mutate(Contour = as.factor(Contour)) %>%
  distinct() %>%
  drop_na() %>%
  droplevels()

# null hypothesis
cg.best.guess <- round(mean(cg$Erosion),2) 

# Evaluate RMSE
cg.RMSE.baseline <- round(sqrt(mean((cg.best.guess-cg$Erosion)^2)),2)

#load model
cg_mod <- readRDS("modelOutputs/eastCentralWI/erosion/cg_erosion_eastCentralWI.rds")
cg_mod <- readRDS("/Volumes/cgratton/scapetools/serverReady/grazeScape/modelFiles/southWestWI/cg_erosion_southWestWI.rds")
cg_mod
cg_mod$fit$importance

# predictions
pred_cg <- cg_mod %>%
  predict(cg) %>%
  bind_cols(cg)

cg_rmse <- round(sqrt(mean((pred_cg$.pred - pred_cg$Erosion)^2)),3) 
# plot data
ggplot(pred_cg, aes(x = Erosion, y = .pred)) +
  geom_point() 

rss <- sum((pred_cg$.pred - pred_cg$Erosion) ^ 2)  ## residual sum of squares
tss <- sum((pred_cg$Erosion - mean(pred_cg$Erosion)) ^ 2)  ## total sum of squares
rsq <- 1 - rss/tss

# cso ---------------------------------------------------------------------

# clean data
cso <- full %>%
  filter(crop == "cso") %>%
  mutate_if(is.character, as.factor) %>%
  dplyr::select(c(Erosion, cover, tillage, Contour, total_DM_lbs, slope, slopelenusle.r, sand, silt, clay, k)) %>%
  mutate(Contour = as.factor(Contour)) %>%
  drop_na() %>%
  distinct() %>%
  droplevels()

# null hypothesis
cso.best.guess <- round(mean(cso$Erosion),2) 

# Evaluate RMSE
cso.RMSE.baseline <- round(sqrt(mean((cso.best.guess-cso$Erosion)^2)),2)

#load model
cso_mod <- readRDS("modelOutputs/eastCentralWI/erosion/cso_erosion_eastCentralWI.rds")
cso_mod <- readRDS("/Volumes/cgratton/scapetools/serverReady/grazeScape/modelFiles/southWestWI/cso_erosion_southWestWI.rds")
cso_mod

cso_mod$fit$importance

# predictions
pred_cso <- cso_mod %>%
  predict(cso) %>%
  bind_cols(cso)

cso_rmse <- round(sqrt(mean((pred_cso$.pred - pred_cso$Erosion)^2)),3) 

# plot data
ggplot(pred_cso, aes(x = Erosion, y = .pred)) +
  geom_point()

rss <- sum((pred_cso$.pred - pred_cso$Erosion) ^ 2)  ## residual sum of squares
tss <- sum((pred_cso$Erosion - mean(pred_cso$Erosion)) ^ 2)  ## total sum of squares
rsq <- 1 - rss/tss

# dr ----------------------------------------------------------------------

# clean data
dr <- full %>%
  filter(crop == "dr") %>%
  mutate_if(is.character, as.factor) %>%
  dplyr::select(c(Erosion, cover, tillage, Contour, total_DM_lbs, slope, slopelenusle.r, sand, silt, clay, k)) %>%
  mutate(Contour = as.factor(Contour),
         tillage = recode(tillage, 
                          "sm" = "sv")) %>%
  distinct() %>%
  drop_na() %>%
  droplevels()

summary(dr)

# null hypothesis
dr.best.guess <- round(mean(dr$Erosion),2) 

# Evaluate RMSE
dr.RMSE.baseline <- round(sqrt(mean((dr.best.guess-dr$Erosion)^2)),2)

#load model
dr_mod <- readRDS("modelOutputs/southEastWI/erosion/dr_erosion_southEastWI_ero4.rds")
dr_mod <- readRDS("/Volumes/cgratton/scapetools/serverReady/grazeScape/modelFiles/southWestWI/dr_erosion_southWestWI.rds")
dr_mod
dr_mod$fit$importance

# predictions
pred_dr <- dr_mod %>%
  predict(dr) %>%
  bind_cols(dr)

dr_rmse <- round(sqrt(mean((pred_dr$.pred - pred_dr$Erosion)^2)),3) 

# plot data
ggplot(pred_dr, aes(x = Erosion, y = .pred)) +
  geom_point() 

rss <- sum((pred_dr$.pred - pred_dr$Erosion) ^ 2)  ## residual sum of squares
tss <- sum((pred_dr$Erosion - mean(pred_dr$Erosion)) ^ 2)  ## total sum of squares
rsq <- 1 - rss/tss


# ps ----------------------------------------------------------------------

  # clean data
  ps <- full %>%
  filter(crop == "ps") %>%
  mutate_if(is.character, as.factor) %>%
  dplyr::select(c(Erosion, tillage, Contour, total_DM_lbs, slope, slopelenusle.r, sand, silt, clay, k)) %>%
  mutate(Contour = as.factor(Contour)) %>%
  distinct() %>%
  droplevels()

# null hypothesis
ps.best.guess <- round(mean(ps$Erosion),2) 

# Evaluate RMSE
ps.RMSE.baseline <- round(sqrt(mean((ps.best.guess-ps$Erosion)^2)),2)


#load model
ps_mod <- readRDS("modelsFromCondor/southWest/erosion/ps_erosion_southWestWI.rds")
ps_mod <- readRDS("/Volumes/cgratton/scapetools/serverReady/grazeScape/modelFiles/southWestWI/ps_erosion_southWestWI.rds")
ps_mod

ps_mod$fit$importance

# predictions
pred_ps <- ps_mod %>%
  predict(ps) %>%
  bind_cols(ps)

ps_rmse <- round(sqrt(mean((pred_ps$.pred - pred_ps$Erosion)^2)),3) 
# plot data
ggplot(pred_ps, aes(x = Erosion, y = .pred)) +
  geom_point() 

rss <- sum((pred_ps$.pred - pred_ps$Erosion) ^ 2)  ## residual sum of squares
tss <- sum((pred_ps$Erosion - mean(pred_ps$Erosion)) ^ 2)  ## total sum of squares
rsq <- 1 - rss/tss

# pt ----------------------------------------------------------------------

# clean data
pt <- full %>%
  filter(crop == "pt", 
         Contour == "0") %>%
  #filter(OM < 20) %>%
  mutate_if(is.character, as.factor) %>%
  dplyr::select(c(Erosion, rotational, density, total_DM_lbs, slope, slopelenusle.r, sand, silt, clay, k)) %>%
  distinct() %>%
  drop_na() %>%
  droplevels()

# null hypothesis
pt.best.guess <- round(mean(pt$Erosion),2) 

# Evaluate RMSE
pt.RMSE.baseline <- round(sqrt(mean((pt.best.guess-pt$Erosion)^2)),2)

#load model
pt_mod <- readRDS("modelOutputs/eastCentralWI/erosion/pt_erosion_eastCentralWI.rds")
pt_mod <- readRDS("/Volumes/cgratton/scapetools/serverReady/grazeScape/modelFiles/southWestWI/pt_erosion_southWestWI.rds")
pt_mod

pt_mod$fit$importance

# predictions
pred_pt <- pt_mod %>%
  predict(pt) %>%
  bind_cols(pt)
 
pt_rmse <- round(sqrt(mean((pred_pt$.pred - pred_pt$Erosion)^2)),3)

# plot data
ggplot(pred_pt, aes(x = Erosion, y = .pred)) +
  geom_point() 

rss <- sum((pred_pt$.pred - pred_pt$Erosion) ^ 2)  ## residual sum of squares
tss <- sum((pred_pt$Erosion - mean(pred_pt$Erosion)) ^ 2)  ## total sum of squares
rsq <- 1 - rss/tss

# dl ----------------------------------------------------------------------

# clean data
dl <- full %>%
  filter(crop == "dl") %>%
  mutate_if(is.character, as.factor) %>%
  dplyr::select(c(Erosion, density, total_DM_lbs, slope, slopelenusle.r, sand, silt, clay, k, R_factor)) %>%
  distinct() %>%
  droplevels()

# null hypothesis
dl.best.guess <- round(mean(dl$Erosion),2) 

# Evaluate RMSE
dl.RMSE.baseline <- round(sqrt(mean((dl.best.guess-dl$Erosion)^2)),2)

#load model
dl_mod <- readRDS("modelsFromCondor/southWest/erosion/dl_erosion_southWestWI.rds")
dl_mod <- readRDS("/Volumes/cgratton/scapetools/serverReady/grazeScape/modelFiles/southWestWI/dl_erosion_southWestWI.rds")
dl_mod
dl_mod$fit$importance

# predictions
pred_dl <- dl_mod %>%
  predict(dl) %>%
  bind_cols(dl)

dl_rmse <- round(sqrt(mean((pred_dl$.pred - pred_dl$Erosion)^2)),3) 
# plot data
ggplot(pred_dl, aes(x = Erosion, y = .pred)) +
  geom_point() 


rss <- sum((pred_dl$.pred - pred_dl$Erosion) ^ 2)  ## residual sum of squares
tss <- sum((pred_dl$Erosion - mean(pred_dl$Erosion)) ^ 2)  ## total sum of squares
rsq <- 1 - rss/tss


