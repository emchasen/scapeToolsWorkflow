# this is the script that created the model that we have in place for SCI - cash grain, in the clover belt

library(tidyverse)
library(tidymodels)
library(randomForest)

###load data files from each county in learning hub---------
sheb <- read_csv("../../sheboyganJoinSnapSurgo.csv.gz", col_types = list( "SoilSymbol" = col_character()))
#head(sheb)
fdl <- read_csv("../../fondDuLacJoinSnapSurgo.csv.gz", col_types = list( "SoilSymbol" = col_character()))
#head(fdl)
gl <- read_csv("../../greenLakeJoinSnapSurgo.csv.gz", col_types = list( "SoilSymbol" = col_character()))
#head(gl)
dod <- read_csv("../../dodgeJoinSnapSurgo.csv.gz", col_types = list( "SoilSymbol" = col_character()))
#head(dod)
wash <- read_csv("../../washingtonJoinSnapSurgo.csv.gz", col_types = list( "SoilSymbol" = col_character()))
#head(wash)
ozak <- read_csv("../../ozaukeeJoinSnapSurgo.csv.gz", col_types = list( "SoilSymbol" = col_character()))
#head(ozak)
cal <- read_csv("../../calumetJoinSnapSurgo.csv.gz", col_types = list( "SoilSymbol" = col_character()))
#head(cal)

# create one data frame-------------
full <- bind_rows(sheb, fdl, gl, dod, wash, ozak, cal)
#print(paste('total data is', dim(full)))
rm(list = c("sheb", "fdl", "gl", "dod", "wash", "ozak", "cal"))

drFull <- full %>%
  mutate_if(is.character, as.factor) %>%
  filter(crop == "dr",
         initialP == 25,
         OM < 20) %>%
  select(SCI, Erosion, cover, tillage, sand, silt, clay, Contour, total_DM_lbs) %>%
  mutate(Contour = as.factor(Contour),
         tillage = recode(tillage, 
                          "sm" = "sv")) %>%
  distinct() %>%
  droplevels()

dim(drFull)

drFull <- drFull %>%
  drop_na()

dim(drFull)
summary(drFull)
rm(full)

# sample 1
set.seed(123)
split <- initial_split(drFull, prop = .7, strata = Erosion)
dr <- training(split)
dr_test <- testing(split)

print(paste('new dr data is', dim(dr)))

#partition data
set.seed(123)
split2 <- initial_split(dr, strata = SCI)
train <- training(split2)
test <- testing(split2)

cc_rec <- recipe(SCI ~ ., data = train)

cc_prep <- prep(cc_rec)
juiced <- juice(cc_prep)

tune_spec <- rand_forest(
  mtry = tune(),
  trees = 60,
  min_n = tune()
) %>%
  set_mode("regression") %>%
  set_engine("randomForest")

tune_wf <- workflow() %>%
  add_recipe(cc_rec) %>%
  add_model(tune_spec)

# train hyperparameters
set.seed(234)
folds <- vfold_cv(train, v = 5)

rf_grid <- grid_regular(
  mtry(range = c(3, 7)),
  min_n(range = c(5, 8)),
  levels = 2
)

set.seed(456)
regular_res <- tune_grid(
  tune_wf,
  resamples = folds,
  grid = rf_grid
)

metrics <- regular_res %>%
  collect_metrics()
write.csv(metrics, "dr_sci_southEastmetrics.csv", row.names = FALSE, quote = FALSE)

#choose best model
best_rmse <- select_best(regular_res, "rmse")

final_rf <- finalize_model(
  tune_spec,
  best_rmse
)

mod <- final_rf %>%
  set_engine("randomForest") %>%
  fit(SCI ~ .,
      data = train
  )

saveRDS(mod, "dr_sci_southEastWI.rds")

testRMSE <- mod %>%
  predict(test) %>%
  bind_cols(test) 

RMSE <- round(sqrt(mean((testRMSE$.pred - testRMSE$SCI)^2)),5)

print(paste("RMSE is", RMSE))

testRMSE2 <- mod %>%
 predict(dr_test) %>%
 bind_cols(dr_test) 

RMSE2 <- round(sqrt(mean((testRMSE2$.pred - testRMSE2$SCI)^2)),5)

print(paste("RMSE is on dr test", RMSE2))
