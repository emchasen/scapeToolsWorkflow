# this script compares PI model output from clover belt snapplus runs
# and creates final model for use in grazescape/smartscape

#load libraries
library(tidyverse)
library(tidymodels)
library(MASS)
library(caret)


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

#clean data
cc <- full %>%
  filter(crop == "cc", 
         OM < 20) %>%
  mutate_if(is.character, as.factor) %>%
  dplyr::select(c(PI, Erosion, cover, tillage, Contour, initialP, total_DM_lbs, totalP2O5_lbs, slope, slopelenusle.r, LSsurgo,
                  total.depth, OM, silt, k)) %>%
  mutate(Contour = as.factor(Contour)) %>%
 filter(PI > -1) %>%
  distinct() %>%
  droplevels()

summary(cc)

ccBack <- readRDS("modelOutputs/eastCentralWI/pi/condorOutput/ccPI_stepBack_eastCentralWI.rds")

# null hypothesis
cc.best.guess <- round(mean(cc$PI),2) #southEast WI: 2.96; eastCentral WI: 2.08

#partition data
set.seed(123)
inTrain <- createDataPartition(y = cc$PI, p = 0.7, list = FALSE)
train <- cc[inTrain,]
test <- cc[-inTrain,]

# Evaluate RMSE
cc.RMSE.baseline <- round(sqrt(mean((cc.best.guess-test$PI)^2)),2) #southEast WI: 3.29; eastCentral WI: 2.46

ccBack$anova
#define the model
lm_mod <- 
  linear_reg() %>% 
  set_engine("lm")

tidy_cc <- 
  lm_mod %>%
  fit(PI ~ Erosion + cover + tillage + Contour + initialP + total_DM_lbs + 
               totalP2O5_lbs + slope + slopelenusle.r + LSsurgo + total.depth + 
               OM + silt + k + Erosion:cover + Erosion:tillage + Erosion:Contour + 
               Erosion:initialP + Erosion:total_DM_lbs + Erosion:totalP2O5_lbs + 
               Erosion:slope + Erosion:slopelenusle.r + Erosion:LSsurgo + 
               Erosion:total.depth + Erosion:OM + Erosion:silt + Erosion:k + 
               cover:tillage + cover:Contour + cover:initialP + cover:total_DM_lbs + 
               cover:totalP2O5_lbs + cover:slope + cover:slopelenusle.r + 
               cover:total.depth + cover:OM + cover:silt + cover:k + tillage:Contour + 
               tillage:initialP + tillage:total_DM_lbs + tillage:totalP2O5_lbs + 
               tillage:slope + tillage:slopelenusle.r + tillage:LSsurgo + 
               tillage:total.depth + tillage:OM + tillage:silt + tillage:k + 
               Contour:initialP + Contour:total_DM_lbs + Contour:totalP2O5_lbs + 
               Contour:slope + Contour:slopelenusle.r + Contour:OM + Contour:silt + 
               Contour:k + initialP:total_DM_lbs + initialP:totalP2O5_lbs + 
               initialP:slope + initialP:slopelenusle.r + initialP:LSsurgo + 
               initialP:total.depth + initialP:OM + initialP:silt + initialP:k + 
               total_DM_lbs:totalP2O5_lbs + total_DM_lbs:slope + total_DM_lbs:slopelenusle.r + 
               total_DM_lbs:LSsurgo + total_DM_lbs:silt + total_DM_lbs:k + 
               totalP2O5_lbs:slope + totalP2O5_lbs:slopelenusle.r + totalP2O5_lbs:LSsurgo + 
               totalP2O5_lbs:total.depth + totalP2O5_lbs:OM + totalP2O5_lbs:silt + 
               totalP2O5_lbs:k + slope:slopelenusle.r + slope:LSsurgo + 
               slope:total.depth + slope:OM + slope:silt + slope:k + slopelenusle.r:LSsurgo + 
               slopelenusle.r:total.depth + slopelenusle.r:OM + slopelenusle.r:silt + 
               slopelenusle.r:k + LSsurgo:total.depth + LSsurgo:OM + LSsurgo:silt + 
               LSsurgo:k + total.depth:OM + total.depth:silt + total.depth:k + 
               OM:silt + OM:k + silt:k,
      data = train)

cc_pred <- tidy_cc %>%
  predict(test) %>%
  bind_cols(test)

ggplot(cc_pred, aes(x = PI, y = .pred)) +
  geom_point()

cc_rmse <- round(sqrt(mean((cc_pred$.pred - cc_pred$PI)^2)),3)
#southEast WI: 0.334; eastCentral WI: 0.33

saveRDS(tidy_cc, "modelOutputs/eastCentralWI/pi/cc_ploss_eastCentralWI.rds")

rss <- sum((cc_pred$.pred - cc_pred$PI) ^ 2)  ## residual sum of squares
tss <- sum((cc_pred$PI - mean(cc_pred$PI)) ^ 2)  ## total sum of squares
rsq <- 1 - rss/tss

# corn grain --------------------------------------------------------------


#clean data
cg <- full %>%
  filter(crop == "cg",
         OM < 20) %>%
  mutate_if(is.character, as.factor) %>%
  dplyr::select(c(PI, Erosion, cover, tillage, Contour, initialP, total_DM_lbs, totalP2O5_lbs, slope, slopelenusle.r, LSsurgo,
                  total.depth, OM, silt, k)) %>%
  mutate(Contour = as.factor(Contour)) %>%
  drop_na(PI) %>%
  distinct() %>%
  droplevels()

summary(cg)

# null hypothesis
cg.best.guess <- round(mean(cg$PI),2) #southEast WI: 3.72; eastCentral WI: 2.63

#partition data
set.seed(123)
inTrain <- createDataPartition(y = cg$PI, p = 0.7, list = FALSE)
train <- cg[inTrain,]
test <- cg[-inTrain,]

# Evaluate RMSE
cg.RMSE.baseline <- round(sqrt(mean((cg.best.guess-test$PI)^2)),2) #southEast WI: 4.09; eastCentral WI: 3.13

cg_mod <- readRDS("modelOutputs/eastCentralWI/pi/condorOutput/cgPI_stepBack_eastCentralWI.rds")

cg_mod$anova

lm_mod <- 
 linear_reg() %>% 
 set_engine("lm")

##TODO
# copy and paste final model into fit below

tidy_cg <- 
  lm_mod %>%
  fit(PI ~ Erosion + cover + tillage + Contour + initialP + total_DM_lbs + 
       totalP2O5_lbs + slope + slopelenusle.r + LSsurgo + total.depth + 
       OM + silt + k + Erosion:cover + Erosion:tillage + Erosion:Contour + 
       Erosion:initialP + Erosion:total_DM_lbs + Erosion:totalP2O5_lbs + 
       Erosion:slope + Erosion:slopelenusle.r + Erosion:LSsurgo + 
       Erosion:total.depth + Erosion:OM + Erosion:silt + Erosion:k + 
       cover:tillage + cover:Contour + cover:initialP + cover:totalP2O5_lbs + 
       cover:slope + cover:slopelenusle.r + cover:LSsurgo + cover:total.depth + 
       cover:OM + cover:silt + cover:k + tillage:Contour + tillage:initialP + 
       tillage:total_DM_lbs + tillage:totalP2O5_lbs + tillage:slope + 
       tillage:slopelenusle.r + tillage:LSsurgo + tillage:total.depth + 
       tillage:OM + tillage:silt + tillage:k + Contour:initialP + 
       Contour:total_DM_lbs + Contour:totalP2O5_lbs + Contour:slope + 
       Contour:slopelenusle.r + Contour:LSsurgo + Contour:OM + Contour:silt + 
       Contour:k + initialP:total_DM_lbs + initialP:slope + initialP:slopelenusle.r + 
       initialP:LSsurgo + initialP:total.depth + initialP:OM + initialP:silt + 
       initialP:k + total_DM_lbs:totalP2O5_lbs + total_DM_lbs:slope + 
       total_DM_lbs:slopelenusle.r + total_DM_lbs:total.depth + 
       total_DM_lbs:OM + total_DM_lbs:silt + total_DM_lbs:k + totalP2O5_lbs:slope + 
       totalP2O5_lbs:slopelenusle.r + totalP2O5_lbs:total.depth + 
       totalP2O5_lbs:OM + totalP2O5_lbs:silt + totalP2O5_lbs:k + 
       slope:slopelenusle.r + slope:LSsurgo + slope:total.depth + 
       slope:OM + slope:silt + slope:k + slopelenusle.r:LSsurgo + 
       slopelenusle.r:total.depth + slopelenusle.r:OM + slopelenusle.r:silt + 
       slopelenusle.r:k + LSsurgo:total.depth + LSsurgo:OM + LSsurgo:silt + 
       LSsurgo:k + total.depth:OM + total.depth:silt + total.depth:k + 
       OM:silt + OM:k + silt:k,
      data = train)

cg_pred <- tidy_cg %>%
  predict(test) %>%
  bind_cols(test)

ggplot(cg_pred, aes(x = PI, y = .pred)) +
  geom_point()

cg_rmse <- round(sqrt(mean((cg_pred$.pred - cg_pred$PI)^2)),3) #southEast WI: 0.363; eastCentral WI: 0.365

saveRDS(tidy_cg, "modelOutputs/eastCentralWI/pi/cg_ploss_eastCentralWI.rds")

rss <- sum((cg_pred$.pred - cg_pred$PI) ^ 2)  ## residual sum of squares
tss <- sum((cg_pred$PI - mean(cg_pred$PI)) ^ 2)  ## total sum of squares
rsq <- 1 - rss/tss


# corn soy oats -----------------------------------------------------------

#clean data
cso <- full %>%
  filter(crop == "cso",
         OM < 20) %>%
  mutate_if(is.character, as.factor) %>%
  dplyr::select(c(PI, Erosion, cover, tillage, Contour, initialP, total_DM_lbs, totalP2O5_lbs, slope, slopelenusle.r, LSsurgo,
                  total.depth, OM, silt, k)) %>%
  mutate(Contour = as.factor(Contour)) %>%
  distinct() %>%
  droplevels()

# null hypothesis
cso.best.guess <- round(mean(cso$PI),2) #southEast WI: 3.58; eastCentral WI: 2.5

#partition data
set.seed(123)
inTrain <- createDataPartition(y = cso$PI, p = 0.7, list = FALSE)
train <- cso[inTrain,]
test <- cso[-inTrain,]

# Evaluate RMSE
cso.RMSE.baseline <- round(sqrt(mean((cso.best.guess-test$PI)^2)),2) #southEast WI: 3.92l eastCentral WI: 2.94

cso_mod <- readRDS("modelOutputs/eastCentralWI/pi/condorOutput/csoPI_stepBack_eastCentralWI.rds")

cso_mod$anova 

lm_mod <- 
 linear_reg() %>% 
 set_engine("lm")

##TODO
# copy and paste final model into fit below

tidy_cso <- 
  lm_mod %>%
  fit(PI ~ Erosion + cover + tillage + Contour + initialP + total_DM_lbs + 
       totalP2O5_lbs + slope + slopelenusle.r + LSsurgo + total.depth + 
       OM + silt + k + Erosion:cover + Erosion:tillage + Erosion:Contour + 
       Erosion:initialP + Erosion:total_DM_lbs + Erosion:totalP2O5_lbs + 
       Erosion:slope + Erosion:slopelenusle.r + Erosion:LSsurgo + 
       Erosion:total.depth + Erosion:OM + Erosion:silt + Erosion:k + 
       cover:tillage + cover:Contour + cover:initialP + cover:total_DM_lbs + 
       cover:totalP2O5_lbs + cover:slope + cover:slopelenusle.r + 
       cover:LSsurgo + cover:silt + cover:k + tillage:Contour + 
       tillage:initialP + tillage:total_DM_lbs + tillage:totalP2O5_lbs + 
       tillage:slope + tillage:slopelenusle.r + tillage:LSsurgo + 
       tillage:total.depth + tillage:OM + tillage:silt + tillage:k + 
       Contour:initialP + Contour:total_DM_lbs + Contour:totalP2O5_lbs + 
       Contour:slope + Contour:slopelenusle.r + Contour:LSsurgo + 
       Contour:total.depth + Contour:OM + Contour:silt + Contour:k + 
       initialP:total_DM_lbs + initialP:slopelenusle.r + initialP:LSsurgo + 
       initialP:total.depth + initialP:OM + initialP:silt + initialP:k + 
       total_DM_lbs:totalP2O5_lbs + total_DM_lbs:slope + total_DM_lbs:slopelenusle.r + 
       total_DM_lbs:LSsurgo + total_DM_lbs:total.depth + total_DM_lbs:OM + 
       total_DM_lbs:silt + total_DM_lbs:k + totalP2O5_lbs:slope + 
       totalP2O5_lbs:slopelenusle.r + totalP2O5_lbs:LSsurgo + totalP2O5_lbs:total.depth + 
       totalP2O5_lbs:OM + totalP2O5_lbs:silt + totalP2O5_lbs:k + 
       slope:slopelenusle.r + slope:LSsurgo + slope:OM + slope:silt + 
       slope:k + slopelenusle.r:LSsurgo + slopelenusle.r:total.depth + 
       slopelenusle.r:OM + slopelenusle.r:silt + slopelenusle.r:k + 
       LSsurgo:total.depth + LSsurgo:OM + LSsurgo:silt + LSsurgo:k + 
       total.depth:OM + total.depth:silt + total.depth:k + OM:silt + 
       OM:k + silt:k,
      data = train)

cso_pred <- tidy_cso %>%
  predict(test) %>%
  bind_cols(test)

ggplot(cso_pred, aes(x = PI, y = .pred)) +
  geom_point()

cso_rmse <- round(sqrt(mean((cso_pred$.pred - cso_pred$PI)^2)),3) # southEast WI: 0.36; eastCentral WI: 0.367

saveRDS(tidy_cso, "modelOutputs/eastCentralWI/pi/cso_ploss_eastCentralWI.rds")

rss <- sum((cso_pred$.pred - cso_pred$PI) ^ 2)  ## residual sum of squares
tss <- sum((cso_pred$PI - mean(cso_pred$PI)) ^ 2)  ## total sum of squares
rsq <- 1 - rss/tss


# dairy rotation ----------------------------------------------------------

#clean data
dr <- full %>%
  filter(crop == "dr",
         OM < 20) %>%
  mutate_if(is.character, as.factor) %>%
  dplyr::select(c(PI, Erosion, cover, tillage, Contour, initialP, total_DM_lbs, totalP2O5_lbs, slope, slopelenusle.r, LSsurgo,
                  total.depth, OM, silt, k)) %>%
  mutate(Contour = as.factor(Contour),
         tillage = recode(tillage, 
                          "sm" = "sv")) %>%
  distinct() %>%
  droplevels()

levels(dr$tillage)

summary(dr)

# null hypothesis
dr.best.guess <- round(mean(dr$PI),2) #southEastWI: 3.13; eastCentral WI: 2.17

#partition data
set.seed(123)
inTrain <- createDataPartition(y = dr$PI, p = 0.7, list = FALSE)
train <- dr[inTrain,]
test <- dr[-inTrain,]

# Evaluate RMSE
dr.RMSE.baseline <- round(sqrt(mean((dr.best.guess-test$PI)^2)),2) #southEast WI: 3.08; eastCentral WI: 2.34

dr_mod <- readRDS("modelOutputs/eastCentralWI/pi/condorOutput/drPI_stepBack_eastCentralWI.rds")

dr_mod$anova

lm_mod <- 
 linear_reg() %>% 
 set_engine("lm")

##TODO
# copy and past final model into fit below

tidy_dr <- 
  lm_mod %>%
  fit(PI ~ Erosion + cover + tillage + Contour + initialP + total_DM_lbs + 
       totalP2O5_lbs + slope + slopelenusle.r + LSsurgo + total.depth + 
       OM + silt + k + Erosion:cover + Erosion:tillage + Erosion:Contour + 
       Erosion:initialP + Erosion:total_DM_lbs + Erosion:totalP2O5_lbs + 
       Erosion:slope + Erosion:slopelenusle.r + Erosion:LSsurgo + 
       Erosion:total.depth + Erosion:OM + Erosion:silt + Erosion:k + 
       cover:tillage + cover:Contour + cover:initialP + cover:totalP2O5_lbs + 
       cover:slope + cover:slopelenusle.r + cover:LSsurgo + cover:silt + 
       cover:k + tillage:Contour + tillage:initialP + tillage:total_DM_lbs + 
       tillage:totalP2O5_lbs + tillage:slope + tillage:slopelenusle.r + 
       tillage:LSsurgo + tillage:total.depth + tillage:OM + tillage:silt + 
       tillage:k + Contour:initialP + Contour:total_DM_lbs + Contour:totalP2O5_lbs + 
       Contour:slope + Contour:slopelenusle.r + Contour:LSsurgo + 
       Contour:OM + Contour:silt + Contour:k + initialP:total_DM_lbs + 
       initialP:slope + initialP:slopelenusle.r + initialP:total.depth + 
       initialP:OM + initialP:silt + initialP:k + total_DM_lbs:totalP2O5_lbs + 
       total_DM_lbs:slope + total_DM_lbs:slopelenusle.r + total_DM_lbs:LSsurgo + 
       total_DM_lbs:total.depth + total_DM_lbs:OM + total_DM_lbs:silt + 
       total_DM_lbs:k + totalP2O5_lbs:slope + totalP2O5_lbs:slopelenusle.r + 
       totalP2O5_lbs:LSsurgo + totalP2O5_lbs:total.depth + totalP2O5_lbs:OM + 
       totalP2O5_lbs:silt + totalP2O5_lbs:k + slope:slopelenusle.r + 
       slope:LSsurgo + slope:total.depth + slope:OM + slope:silt + 
       slope:k + slopelenusle.r:LSsurgo + slopelenusle.r:total.depth + 
       slopelenusle.r:OM + slopelenusle.r:silt + slopelenusle.r:k + 
       LSsurgo:total.depth + LSsurgo:OM + LSsurgo:silt + LSsurgo:k + 
       total.depth:OM + total.depth:silt + total.depth:k + OM:silt + 
       OM:k + silt:k,
      data = train)

dr_pred <- tidy_dr %>%
  predict(test) %>%
  bind_cols(test)

ggplot(dr_pred, aes(x = PI, y = .pred)) +
  geom_point()

dr_rmse <- round(sqrt(mean((dr_pred$.pred - dr_pred$PI)^2)),3) #southEast WI: 0.342; eastCentral WI: 0.357

saveRDS(tidy_dr, "modelOutputs/eastCentralWI/pi/dr_ploss_eastCentralWI.rds")

rss <- sum((dr_pred$.pred - dr_pred$PI) ^ 2)  ## residual sum of squares
tss <- sum((dr_pred$PI - mean(dr_pred$PI)) ^ 2)  ## total sum of squares
rsq <- 1 - rss/tss

# pasture seeding ------------------------------

# # clean data
# ps <- full %>%
#  filter(crop == "ps",
#         OM < 20) %>%
#  mutate_if(is.character, as.factor)%>%
#  dplyr::select(c(PI, Erosion, tillage, Contour, initialP, total_DM_lbs, totalP2O5_lbs, slope, slopelenusle.r, LSsurgo,
#                  total.depth, OM, silt, k, R_factor)) %>%
#  mutate(Contour = as.factor(Contour)) %>%
#  distinct() %>%
#  droplevels()
# 
# # null hypothesis
# ps.best.guess <- round(mean(ps$PI),2) # southeast WI = 1.57; eastCentral WI: 1.16
# 
# #partition data
# set.seed(123)
# inTrain <- createDataPartition(y = ps$PI, p = 0.7, list = FALSE)
# train <- ps[inTrain,]
# test <- ps[-inTrain,]
# 
# # Evaluate RMSE
# ps.RMSE.baseline <- round(sqrt(mean((ps.best.guess-test$PI)^2)),2)
# 
# ps_mod <- readRDS("/Volumes/cgratton/scapetools/serverReady/grazeScape/modelFiles/southWestWI/ps_ploss_southWestWI.rds")
# 
# ps_pred <- ps_mod %>%
#  predict(test) %>%
#  bind_cols(test)
# 
# ggplot(ps_pred, aes(x = PI, y = .pred)) +
#  geom_point()
# 
# ps_rmse <- round(sqrt(mean((ps_pred$.pred - ps_pred$PI)^2)),3)
# 
# rss <- sum((ps_pred$.pred - ps_pred$PI) ^ 2)  ## residual sum of squares
# tss <- sum((ps_pred$PI - mean(ps_pred$PI)) ^ 2)  ## total sum of squares
# rsq <- 1 - rss/tss

# pasture ---------------------------------------------------------
  
  #clean data
  pt <- full %>%
  filter(crop == "pt", 
         Contour == "0",
         OM < 20) %>%
  mutate_if(is.character, as.factor) %>%
  dplyr::select(c(PI, Erosion, density, rotational, initialP, total_DM_lbs, totalP2O5_lbs, slope, slopelenusle.r, LSsurgo,
                  total.depth, OM, silt, k)) %>%
  distinct() %>%
  droplevels()

# null hypothesis
pt.best.guess <- round(mean(pt$PI),2) # southeast WI = 1.57; eastCentral WI: 1.16

#partition data
set.seed(123)
inTrain <- createDataPartition(y = pt$PI, p = 0.7, list = FALSE)
train <- pt[inTrain,]
test <- pt[-inTrain,]

# Evaluate RMSE
pt.RMSE.baseline <- round(sqrt(mean((pt.best.guess-test$PI)^2)),2) # southeast WI = 1.8; eastCentral WI: 1.35

pt_mod <- readRDS("modelOutputs/eastCentralWI/pi/condorOutput/ptPI_stepBack_eastCentralWI.rds")

pt_mod$anova

lm_mod <- 
  linear_reg() %>% 
  set_engine("lm")

##TODO
# in the anova output is the final mod - copy and paste the final model into the fit
tidy_pt <- 
  lm_mod %>%
  fit(PI ~ Erosion + density + initialP + total_DM_lbs + totalP2O5_lbs + 
       slope + slopelenusle.r + LSsurgo + total.depth + OM + silt + 
       k + Erosion:density + Erosion:initialP + Erosion:total_DM_lbs + 
       Erosion:totalP2O5_lbs + Erosion:slope + Erosion:slopelenusle.r + 
       Erosion:LSsurgo + Erosion:total.depth + Erosion:OM + Erosion:silt + 
       Erosion:k + density:initialP + density:totalP2O5_lbs + density:slope + 
       density:slopelenusle.r + density:LSsurgo + density:total.depth + 
       density:OM + density:silt + density:k + initialP:slope + 
       initialP:slopelenusle.r + initialP:LSsurgo + initialP:total.depth + 
       initialP:OM + initialP:silt + initialP:k + total_DM_lbs:totalP2O5_lbs + 
       total_DM_lbs:total.depth + total_DM_lbs:silt + total_DM_lbs:k + 
       totalP2O5_lbs:slopelenusle.r + totalP2O5_lbs:OM + totalP2O5_lbs:k + 
       slope:slopelenusle.r + slope:LSsurgo + slope:total.depth + 
       slope:OM + slope:silt + slope:k + slopelenusle.r:LSsurgo + 
       slopelenusle.r:total.depth + slopelenusle.r:OM + slopelenusle.r:silt + 
       slopelenusle.r:k + LSsurgo:total.depth + LSsurgo:OM + LSsurgo:silt + 
       LSsurgo:k + total.depth:OM + total.depth:silt + total.depth:k + 
       OM:silt + OM:k + silt:k,
      data = train)

pt_pred <- tidy_pt %>%
  predict(test) %>%
  bind_cols(test)

ggplot(pt_pred, aes(x = PI, y = .pred)) +
  geom_point()

pt_rmse <- round(sqrt(mean((pt_pred$.pred - pt_pred$PI)^2)),3) # southEast WI = 0.294; eastCentral = 0.299

saveRDS(tidy_pt, "modelOutputs/eastCentralWI/pi/pt_ploss_eastCentralWI.rds")

rss <- sum((pt_pred$.pred - pt_pred$PI) ^ 2)  ## residual sum of squares
tss <- sum((pt_pred$PI - mean(pt_pred$PI)) ^ 2)  ## total sum of squares
rsq <- 1 - rss/tss

# examine data
lowPI <- pt_pred %>% 
 filter(between(PI, 0.01, 4),
        .pred > 0.01)
predsOff <- lowPI %>% 
 mutate(error = abs(PI/.pred))
summary(predsOff$error)

predsOff <- filter(predsOff, error > 2)


ggplot(predsOff, aes(x = PI, y = .pred, color = density)) +
 geom_point()
ggplot(predsOff, aes(x = PI, y = .pred, color = rotational)) +
 geom_point()
ggplot(predsOff, aes(x = OM, y = error)) + 
 geom_point()

# dry lot-----------------

# #clean data
# dl <- full %>%
#  filter(crop == "dl",
#         OM < 20) %>%
#  mutate_if(is.character, as.factor) %>%
#  dplyr::select(c(PI, Erosion, density, initialP, total_DM_lbs, totalP2O5_lbs, slope, slopelenusle.r, LSsurgo,
#                  total.depth, OM, silt, k, R_factor)) %>%
#  distinct() %>%
#  droplevels()
# 
# # null hypothesis
# dl.best.guess <- round(mean(dl$PI),2) #9.17
# 
# #partition data
# set.seed(123)
# inTrain <- createDataPartition(y = dl$PI, p = 0.7, list = FALSE)
# train <- dl[inTrain,]
# test <- dl[-inTrain,]
# 
# # Evaluate RMSE
# dl.RMSE.baseline <- round(sqrt(mean((dl.best.guess-test$PI)^2)),2) #10.85
# 
# dl_mod <- readRDS("/Volumes/cgratton/scapetools/serverReady/grazeScape/modelFiles/southWestWI/dl_ploss_southWestWI.rds")
# 
# dl_pred <- dl_mod %>%
#  predict(test) %>%
#  bind_cols(test)
# 
# ggplot(dl_pred, aes(x = PI, y = .pred)) +
#  geom_point()
# 
# dl_rmse <- round(sqrt(mean((dl_pred$.pred - dl_pred$PI)^2)),3)
# 
# rss <- sum((dl_pred$.pred - dl_pred$PI) ^ 2)  ## residual sum of squares
# tss <- sum((dl_pred$PI - mean(dl_pred$PI)) ^ 2)  ## total sum of squares
# rsq <- 1 - rss/tss
