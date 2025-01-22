# this script tests the SCI mod in continuous corn using sand, silt, and clay, Contour, total_DM_lbs
# this is the script that I used to make the model we are using

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

csoFull <- full %>%
  mutate_if(is.character, as.factor) %>%
  filter(crop == "cso",
         initialP == 25,
         OM < 20) %>%
  select(SCI, Erosion, cover, tillage, sand, silt, clay, Contour, total_DM_lbs) %>%
  mutate(Contour = as.factor(Contour)) %>%
 distinct() %>%
  droplevels()

dim(csoFull)

csoFull <- csoFull %>%
  drop_na()

dim(csoFull)
summary(csoFull)
rm(full)

# sample 1
set.seed(123)
split <- initial_split(csoFull, prop = .7, strata = Erosion)
cso <- training(split)
cso_test <- testing(split)

print(paste('new cso data is', dim(cso)))

#partition data
set.seed(123)
split <- initial_split(cso, strata = SCI)
train <- training(split)
test <- testing(split)

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
write.csv(metrics, "cso_sci_southEastWImetrics.csv", row.names = FALSE, quote = FALSE)

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

saveRDS(mod, "cso_sci_southEastWI.rds")

testRMSE <- mod %>%
  predict(test) %>%
  bind_cols(test) 

RMSE <- round(sqrt(mean((testRMSE$.pred - testRMSE$SCI)^2)),5)

print(paste("RMSE is", RMSE))

testRMSE2 <- mod %>%
 predict(cso_test) %>%
 bind_cols(cso_test) 

RMSE2 <- round(sqrt(mean((testRMSE2$.pred - testRMSE2$SCI)^2)),5)

print(paste("RMSE is on cso test", RMSE2))