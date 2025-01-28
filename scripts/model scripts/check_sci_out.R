# this script compares sci models from southWest snap plus runs

#load libraries
library(tidyverse)
library(randomForest)
library(tidymodels)


##load data files from each county in learning hub---------
oco <- read_csv("csvs/ocontoJoinSnapSurgo.csv.gz", col_types = list( "SoilSymbol" = col_character()))
head(oco)
out <- read_csv("csvs/outagamieJoinSnapSurgo.csv.gz", col_types = list( "SoilSymbol" = col_character()))
head(out)
winn <- read_csv("csvs/winnebagoJoinSnapSurgo.csv.gz", col_types = list( "SoilSymbol" = col_character()))
head(winn)

# create one data frame-------------
full <- bind_rows(oco, out, winn)
#print(paste('total data is', dim(full)))
rm(list = c("oco", "out", "winn"))


# cont corn ---------------------------------------------------------------

# clean data
cc <- full %>%
  mutate_if(is.character, as.factor) %>%
  filter(crop == "cc",
         initialP == 25) %>%
  dplyr::select(SCI, Erosion, cover, tillage, sand, silt, clay, Contour, total_DM_lbs) %>%
  mutate(Contour = as.factor(Contour)) %>%
 distinct() %>%
  drop_na() %>%
  droplevels()

# null hypothesis
cc.best.guess <- round(mean(cc$SCI),2) 

# Evaluate RMSE
cc.RMSE.baseline <- round(sqrt(mean((cc.best.guess-cc$SCI)^2)),2)

#load model
cc_mod <- readRDS("modelOutputs/southEastWI/sci/cc_sci_southEastWI.rds")
cc_mod

pred_cc <- cc_mod %>%
  predict(cc) %>%
  bind_cols(cc)

cc_rmse <- round(sqrt(mean((pred_cc$.pred - pred_cc$SCI)^2)),3) 
# plot data
ggplot(pred_cc, aes(x = SCI, y = .pred)) +
  geom_point(alpha = 0.5) 

rss <- sum((pred_cc$.pred - pred_cc$SCI) ^ 2)  ## residual sum of squares
tss <- sum((pred_cc$SCI - mean(pred_cc$SCI)) ^ 2)  ## total sum of squares
rsq <- 1 - rss/tss


# corn grain --------------------------------------------------------------

# clean data
cg <- full %>%
  mutate_if(is.character, as.factor) %>%
  filter(crop == "cg",
         initialP == 25) %>%
  dplyr::select(SCI, Erosion, cover, tillage, sand, silt, clay, Contour, total_DM_lbs) %>%
  mutate(Contour = as.factor(Contour)) %>%
  drop_na() %>%
  droplevels()

# null hypothesis
cg.best.guess <- round(mean(cg$SCI),2) 

# Evaluate RMSE
cg.RMSE.baseline <- round(sqrt(mean((cg.best.guess-cg$SCI)^2)),2)

#load model
cg_mod <- readRDS("modelOutputs/southEastWI/sci/cg_sci_southEastWI4.rds")
cg_mod
cg_mod$fit$importance

# predictions
pred_cg <- cg_mod %>%
  predict(cg) %>%
  bind_cols(cg)

cg_rmse <- round(sqrt(mean((pred_cg$.pred - pred_cg$SCI)^2)),3) 
# plot data
ggplot(pred_cg, aes(x = SCI, y = .pred)) +
  geom_point() +
  geom_jitter(alpha = 0.4) 

rss <- sum((pred_cg$.pred - pred_cg$SCI) ^ 2)  ## residual sum of squares
tss <- sum((pred_cg$SCI - mean(pred_cg$SCI)) ^ 2)  ## total sum of squares
rsq <- 1 - rss/tss


# cso ---------------------------------------------------------------------

# clean data
cso <- full %>%
  filter(crop == "cso") %>%
  mutate_if(is.character, as.factor) %>%
  dplyr::select(c(SCI, Erosion, tillage, cover, sand, silt, clay, Contour, total_DM_lbs)) %>%
  mutate(Contour = as.factor(Contour)) %>%
  distinct() %>%
  drop_na() %>%
  droplevels()

# null hypothesis
cso.best.guess <- round(mean(cso$SCI),2) 

# Evaluate RMSE
cso.RMSE.baseline <- round(sqrt(mean((cso.best.guess-cso$SCI)^2)),2)

#load model
cso_mod <- readRDS("modelOutputs/southEastWI/sci/cso_sci_southEastWI3.rds")
cso_mod
cso_mod$fit$importance

# predictions
pred_cso <- cso_mod %>%
  predict(cso) %>%
  bind_cols(cso)

cso_rmse <- round(sqrt(mean((pred_cso$.pred - pred_cso$SCI)^2)),3) 

# plot data
ggplot(pred_cso, aes(x = SCI, y = .pred)) +
  geom_point() 

rss <- sum((pred_cso$.pred - pred_cso$SCI) ^ 2)  ## residual sum of squares
tss <- sum((pred_cso$SCI - mean(pred_cso$SCI)) ^ 2)  ## total sum of squares
rsq <- 1 - rss/tss


# dr ----------------------------------------------------------------------

# clean data
dr <- full %>%
  filter(crop == "dr") %>%
  mutate_if(is.character, as.factor) %>%
  dplyr::select(c(SCI, Erosion, tillage, cover, sand, silt, clay, Contour, total_DM_lbs)) %>%
  mutate(Contour = as.factor(Contour),
         tillage = recode(tillage, 
                          "sm" = "sv")) %>%
  distinct() %>%
  drop_na() %>%
  droplevels()

# null hypothesis
dr.best.guess <- round(mean(dr$SCI),2) 

# Evaluate RMSE
dr.RMSE.baseline <- round(sqrt(mean((dr.best.guess-dr$SCI)^2)),2)

#load model
dr_mod <- readRDS("modelOutputs/southEastWI/sci/dr_sci_southEastWI4.rds")
dr_mod
dr_mod$fit$importance

# predictions
pred_dr <- dr_mod %>%
  predict(dr) %>%
  bind_cols(dr)

dr_rmse <- round(sqrt(mean((pred_dr$.pred - pred_dr$SCI)^2)),3) 

# plot data
ggplot(pred_dr, aes(x = SCI, y = .pred)) +
  geom_jitter(alpha = 0.5) 

rss <- sum((pred_dr$.pred - pred_dr$SCI) ^ 2)  ## residual sum of squares
tss <- sum((pred_dr$SCI - mean(pred_dr$SCI)) ^ 2)  ## total sum of squares
rsq <- 1 - rss/tss


# ps ----------------------------------------------------------------------

# 
#   # clean data
#   ps <- full %>%
#   mutate_if(is.character, as.factor) %>%
#   filter(crop == "ps",
#          initialP == 25) %>%
#   mutate(Contour = as.factor(Contour)) %>%
#   dplyr::select(SCI, Erosion, tillage, sand, silt, clay, Contour, total_DM_lbs) %>%
#   distinct() %>%
#   droplevels()
# 
# # null hypothesis
# ps.best.guess <- round(mean(ps$SCI),2) 
# 
# # Evaluate RMSE
# ps.RMSE.baseline <- round(sqrt(mean((ps.best.guess-ps$SCI)^2)),2)
# 
# #load model
# ps_mod <- readRDS("modelsFromCondor/southWest/sci/ps_sci_southWestWI.rds")
# ps_mod
# 
# ps_mod$fit$importance
# 
# # predictions
# pred_ps <- ps_mod %>%
#   predict(ps) %>%
#   bind_cols(ps)
# 
# ps_rmse <- round(sqrt(mean((pred_ps$.pred - pred_ps$SCI)^2)),3) 
# # plot data
# ggplot(pred_ps, aes(x = SCI, y = .pred)) +
#   geom_point() 
# 
# rss <- sum((pred_ps$.pred - pred_ps$SCI) ^ 2)  ## residual sum of squares
# tss <- sum((pred_ps$SCI - mean(pred_ps$SCI)) ^ 2)  ## total sum of squares
# rsq <- 1 - rss/tss

# pt ----------------------------------------------------------------------

# clean data
pt <- full %>%
  filter(crop == "pt", 
         Contour == "0",
         OM < 20) %>%
  #filter(OM < 20) %>%
  mutate_if(is.character, as.factor) %>%
  dplyr::select(c(SCI, Erosion, density, rotational, sand, silt, clay, total_DM_lbs)) %>%
  distinct() %>%
  drop_na() %>%
  droplevels()

# null hypothesis
pt.best.guess <- round(mean(pt$SCI),2) 

# Evaluate RMSE
pt.RMSE.baseline <- round(sqrt(mean((pt.best.guess-pt$SCI)^2)),2)

#load model
pt_mod <- readRDS("modelOutputs/southEastWI/sci/pt_sci_southEastWI.rds")
pt_mod

pt_mod$fit$importance

# predictions
pred_pt <- pt_mod %>%
  predict(pt) %>%
  bind_cols(pt)
 
pt_rmse <- round(sqrt(mean((pred_pt$.pred - pred_pt$SCI)^2)),3) 

# plot data
ggplot(pred_pt, aes(x = SCI, y = .pred)) +
  geom_point() 

rss <- sum((pred_pt$.pred - pred_pt$SCI) ^ 2)  ## residual sum of squares
tss <- sum((pred_pt$SCI - mean(pred_pt$SCI)) ^ 2)  ## total sum of squares
rsq <- 1 - rss/tss



# dl ----------------------------------------------------------------------

# # clean data
# dl <- full%>%
#   mutate_if(is.character, as.factor) %>%
#   filter(crop == "dl",
#          initialP == 25) %>%
#   dplyr::select(SCI, Erosion, density, sand, silt, clay, total_DM_lbs) %>%
#   distinct() %>%
#   droplevels()
# 
# # null hypothesis
# dl.best.guess <- round(mean(dl$SCI),2) 
# 
# # Evaluate RMSE
# dl.RMSE.baseline <- round(sqrt(mean((dl.best.guess-dl$SCI)^2)),2)
# 
# #load model
# dl_mod <- readRDS("modelsFromCondor/southWest/sci/dl_sci_southWestWI.rds")
# dl_mod
# dl_mod$fit$importance
# 
# # predictions
# pred_dl <- dl_mod %>%
#   predict(dl) %>%
#   bind_cols(dl)
# 
# dl_rmse <- round(sqrt(mean((pred_dl$.pred - pred_dl$SCI)^2)),3) 
# # plot data
# ggplot(pred_dl, aes(x = SCI, y = .pred)) +
#   geom_point() 
# 
# rss <- sum((pred_dl$.pred - pred_dl$SCI) ^ 2)  ## residual sum of squares
# tss <- sum((pred_dl$SCI - mean(pred_dl$SCI)) ^ 2)  ## total sum of squares
# rsq <- 1 - rss/tss
# 


