#load libraries
library(tidyverse)
library(randomForest)
library(tidymodels)

#load data files from each county in learning hub---------
oco <- read_csv("../../ocontoJoinSnapSurgo.csv.gz", col_types = list( "SoilSymbol" = col_character()))
head(oco)
out <- read_csv("../../outagamieJoinSnapSurgo.csv.gz", col_types = list( "SoilSymbol" = col_character()))
head(out)
winn <- read_csv("../../winnebagoJoinSnapSurgo.csv.gz", col_types = list( "SoilSymbol" = col_character()))
head(gl)

# create one data frame-------------
full <- bind_rows(oco, out, winn)
print(paste('total data is', dim(full)))
rm(list = c("oco", "out", "winn"))

# without R ---------------------------------------------------------------


cc <- full %>%
  filter(crop == "cc",
         initialP == 25,
         Contour == 0) %>%
  mutate_if(is.character, as.factor) %>%
  select(c(ffCN,cover, tillage, hydgrp, total_DM_lbs, sand, silt, clay)) %>%
  distinct() %>%
  drop_na() %>%
  droplevels()

summary(cc)
rm(full)

#partition data
set.seed(123)
split <- initial_split(cc, strata = ffCN)
train <- training(split)
test <- testing(split)

cc_rec <- recipe(ffCN ~ ., data = train)

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
folds <- vfold_cv(train)

rf_grid <- grid_regular(
  mtry(range = c(3, 6)),
  min_n(range = c(3,5)),
  levels = 4
)

set.seed(456)
regular_res <- tune_grid(
  tune_wf,
  resamples = folds,
  grid = rf_grid
)

metrics <- regular_res %>%
  collect_metrics()
write.csv(metrics, "ContCornFFCN_MetricseastCentralWI.csv", row.names = FALSE, quote = FALSE)

#choose best model
best_rmse <- select_best(regular_res, "rmse")

final_rf <- finalize_model(
  tune_spec,
  best_rmse
)

mod <- final_rf %>%
  set_engine("randomForest", importance = TRUE) %>%
  fit(ffCN ~ .,
      data = train
  )

saveRDS(mod, "cc_ffcn_eastCentralWI.rds")

testRMSE <- mod %>%
  predict(test) %>%
  bind_cols(test) 

RMSE <- round(sqrt(mean((testRMSE$.pred - testRMSE$ffCN)^2)),5)

print(paste("RMSE without R factor is", RMSE))

