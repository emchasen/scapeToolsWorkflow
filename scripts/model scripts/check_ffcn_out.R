# this script checks the metrics of the ffcn models created from the southWest snapplus runs

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

#load data
full <- read.csv("../snapPlus/data/TainterSnapSurgo.csv")

# cont corn ---------------------------------------------------------------

# clean data
cc <- full %>%
  filter(crop == "cc") %>%
  mutate_if(is.character, as.factor) %>%
  dplyr::select(c(ffCN,cover, tillage, hydgrp, total_DM_lbs, sand, silt, clay)) %>%
  distinct() %>%
 drop_na() %>%
  droplevels()

# null hypothesis
cc.best.guess <- round(mean(cc$ffCN),2) 

# Evaluate RMSE
cc.RMSE.baseline <- round(sqrt(mean((cc.best.guess-cc$ffCN)^2)),2)

#load model
cc_mod <- readRDS("modelOutputs/southEastWI/cn/cc_ffcn_southEastWI.rds")
cc_mod

cc_mod$fit$importance

# predictions
pred_cc <- cc_mod %>%
  predict(cc) %>%
  bind_cols(cc)

cc_rmse <- round(sqrt(mean((pred_cc$.pred - pred_cc$ffCN)^2)),3) #0.619

# plot data
ggplot(pred_cc, aes(x = ffCN, y = .pred)) +
  geom_point() 

rss <- sum((pred_cc$.pred - pred_cc$ffCN) ^ 2)  ## residual sum of squares
tss <- sum((pred_cc$ffCN - mean(pred_cc$ffCN)) ^ 2)  ## total sum of squares
rsq <- 1 - rss/tss

# corn grain --------------------------------------------------------------


# clean data
cg <- full %>%
  filter(crop == "cg") %>%
  mutate_if(is.character, as.factor) %>%
  dplyr::select(c(ffCN,cover, tillage, hydgrp, total_DM_lbs, sand, silt, clay)) %>%
  distinct() %>%
 drop_na() %>%
  droplevels()

# null hypothesis
cg.best.guess <- round(mean(cg$ffCN),2) 

# Evaluate RMSE
cg.RMSE.baseline <- round(sqrt(mean((cg.best.guess-cg$ffCN)^2)),2)

#load model
cg_mod <- readRDS("modelOutputs/southEastWI/cn/cg_ffcn_southEastWI.rds")
cg_mod

cg_mod$fit$importance

# predictions
pred_cg <- cg_mod %>%
  predict(cg) %>%
  bind_cols(cg)

cg_rmse <- round(sqrt(mean((pred_cg$.pred - pred_cg$ffCN)^2)),3) 

# plot data
ggplot(pred_cg, aes(x = ffCN, y = .pred)) +
  geom_point() 

rss <- sum((pred_cg$.pred - pred_cg$ffCN) ^ 2)  ## residual sum of squares
tss <- sum((pred_cg$ffCN - mean(pred_cg$ffCN)) ^ 2)  ## total sum of squares
rsq <- 1 - rss/tss


# cso ---------------------------------------------------------------------

# clean data
cso <- full %>%
  filter(crop == "cso") %>%
  mutate_if(is.character, as.factor) %>%
  dplyr::select(c(ffCN,cover, tillage, hydgrp, total_DM_lbs, sand, silt, clay)) %>%
  distinct() %>%
 drop_na() %>%
  droplevels()

# null hypothesis
cso.best.guess <- round(mean(cso$ffCN),2) 

# Evaluate RMSE
cso.RMSE.baseline <- round(sqrt(mean((cso.best.guess-cso$ffCN)^2)),2)

#load model
cso_mod <- readRDS("modelOutputs/southEastWI/cn/cso_ffcn_southEastWI.rds")
cso_mod
cso_mod$fit$importance

# predictions
pred_cso <- cso_mod %>%
  predict(cso) %>%
  bind_cols(cso)

cso_rmse <- round(sqrt(mean((pred_cso$.pred - pred_cso$ffCN)^2)),3) 

# plot data
ggplot(pred_cso, aes(x = ffCN, y = .pred)) +
  geom_point() 

rss <- sum((pred_cso$.pred - pred_cso$ffCN) ^ 2)  ## residual sum of squares
tss <- sum((pred_cso$ffCN - mean(pred_cso$ffCN)) ^ 2)  ## total sum of squares
rsq <- 1 - rss/tss



# dr ----------------------------------------------------------------------

# clean data
dr <- full %>%
  filter(crop == "dr") %>%
  mutate_if(is.character, as.factor) %>%
  mutate(Contour = as.factor(Contour),
         tillage = recode(tillage, 
                          "sm" = "sv")) %>%
  dplyr::select(c(ffCN,cover, tillage, hydgrp, total_DM_lbs, sand, silt, clay)) %>%
 drop_na() %>%
  distinct() %>%
  droplevels()

# null hypothesis
dr.best.guess <- round(mean(dr$ffCN),2) 

# Evaluate RMSE
dr.RMSE.baseline <- round(sqrt(mean((dr.best.guess-dr$ffCN)^2)),2)

#load model
dr_mod <- readRDS("modelOutputs/southEastWI/cn/dr_ffcn_southEastWI.rds")
dr_mod
dr_mod$fit$importance

# predictions
pred_dr <- dr_mod %>%
  predict(dr) %>%
  bind_cols(dr)

dr_rmse <- round(sqrt(mean((pred_dr$.pred - pred_dr$ffCN)^2)),3) 

# plot data
ggplot(pred_dr, aes(x = ffCN, y = .pred)) +
  geom_point() 

rss <- sum((pred_dr$.pred - pred_dr$ffCN) ^ 2)  ## residual sum of squares
tss <- sum((pred_dr$ffCN - mean(pred_dr$ffCN)) ^ 2)  ## total sum of squares
rsq <- 1 - rss/tss


# ps ----------------------------------------------------------------------

# # clean data
# ps <- full %>%
#   filter(crop == "ps") %>%
#   mutate_if(is.character, as.factor) %>%
#   dplyr::select(c(ffCN, tillage, hydgrp, total_DM_lbs, sand, silt, clay)) %>%
#   distinct() %>%
#   droplevels()
# 
# # null hypothesis
# ps.best.guess <- round(mean(ps$ffCN),2) 
# 
# # Evaluate RMSE
# ps.RMSE.baseline <- round(sqrt(mean((ps.best.guess-ps$ffCN)^2)),2)
# 
# #load model
# ps_mod <- readRDS("modelsFromCondor/pineRiver/ffcn/ps_ffcn_pineRiverMN.rds")
# ps_mod
# ps_mod$fit$importance
# 
# # predictions
# pred_ps <- ps_mod %>%
#   predict(ps) %>%
#   bind_cols(ps)
# 
# ps_rmse <- round(sqrt(mean((pred_ps$.pred - pred_ps$ffCN)^2)),3) #0.514
# 
# # plot data
# ggplot(pred_ps, aes(x = ffCN, y = .pred)) +
#   geom_point() 
# 
# rss <- sum((pred_ps$.pred - pred_ps$ffCN) ^ 2)  ## residual sum of squares
# tss <- sum((pred_ps$ffCN - mean(pred_ps$ffCN)) ^ 2)  ## total sum of squares
# rsq <- 1 - rss/tss


# pt ----------------------------------------------------------------------

# clean data
pt <- full %>%
  filter(crop == "pt") %>%
  mutate_if(is.character, as.factor) %>%
  dplyr::select(c(ffCN, rotational, density, hydgrp, total_DM_lbs, sand, silt, clay)) %>%
  distinct() %>%
 drop_na() %>%
  droplevels()

# null hypothesis
pt.best.guess <- round(mean(pt$ffCN),2) 

# Evaluate RMSE
pt.RMSE.baseline <- round(sqrt(mean((pt.best.guess-pt$ffCN)^2)),2)

#load model
pt_mod <- readRDS("modelOutputs/southEastWI/cn/pt_ffcn_southEastWI.rds")
pt_mod
pt_mod$fit$importance

# predictions
pred_pt <- pt_mod %>%
  predict(pt) %>%
  bind_cols(pt)

pt_rmse <- round(sqrt(mean((pred_pt$.pred - pred_pt$ffCN)^2)),3) 

# plot data
ggplot(pred_pt, aes(x = ffCN, y = .pred)) +
  geom_point() 

rss <- sum((pred_pt$.pred - pred_pt$ffCN) ^ 2)  ## residual sum of squares
tss <- sum((pred_pt$ffCN - mean(pred_pt$ffCN)) ^ 2)  ## total sum of squares
rsq <- 1 - rss/tss



# dl ----------------------------------------------------------------------

# # clean data
# dl <- full %>%
#   filter(crop == "dl") %>%
#   mutate_if(is.character, as.factor) %>%
#   dplyr::select(c(ffCN, density, hydgrp, total_DM_lbs, sand, silt, clay, R_factor)) %>%
#   distinct() %>%
#   droplevels()
# 
# # null hypothesis
# dl.best.guess <- round(mean(dl$ffCN),2) 
# 
# # Evaluate RMSE
# dl.RMSE.baseline <- round(sqrt(mean((dl.best.guess-dl$ffCN)^2)),2)
# 
# #load model
# dl_mod <- readRDS("modelsFromCondor/pineRiver/ffcn/dl_ffcn_pineRiverMN.rds")
# dl_mod
# dl_mod$fit$importance
# 
# # predictions
# pred_dl <- dl_mod %>%
#   predict(dl) %>%
#   bind_cols(dl)
# 
# dl_rmse <- round(sqrt(mean((pred_dl$.pred - pred_dl$ffCN)^2)),3) 
# 
# # plot data
# ggplot(pred_dl, aes(x = ffCN, y = .pred)) +
#   geom_point() 
# 
# rss <- sum((pred_dl$.pred - pred_dl$ffCN) ^ 2)  ## residual sum of squares
# tss <- sum((pred_dl$ffCN - mean(pred_dl$ffCN)) ^ 2)  ## total sum of squares
# rsq <- 1 - rss/tss

