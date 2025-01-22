# this script tests the SCI mod in continuous corn using sand, silt, and clay, Contour, total_DM_lbs

library(tidyverse)
library(tidymodels)
library(randomForest)


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

df <- full %>%
  mutate_if(is.character, as.factor) %>%
  filter(crop == "pt",
         initialP == 25,
         OM < 20) %>%
  select(SCI, Erosion, density, rotational, sand, silt, clay, total_DM_lbs) %>%
  distinct() %>%
  droplevels()

dim(df)

df <- df %>%
  drop_na()

dim(df)
summary(df)
rm(full)

#partition data
set.seed(123)
split <- initial_split(df, strata = SCI)
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
folds <- vfold_cv(train)

rf_grid <- grid_regular(
  mtry(range = c(3, 7)),
  min_n(range = c(5, 8)),
  levels = 3
)

set.seed(456)
regular_res <- tune_grid(
  tune_wf,
  resamples = folds,
  grid = rf_grid
)

metrics <- regular_res %>%
  collect_metrics()
write.csv(metrics, "pt_sci_eastCentralWImetrics.csv", row.names = FALSE, quote = FALSE)

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

saveRDS(mod, "pt_sci_eastCentralWI.rds")

testRMSE <- mod %>%
  predict(test) %>%
  bind_cols(test) 

RMSE <- round(sqrt(mean((testRMSE$.pred - testRMSE$SCI)^2)),5)

print(paste("RMSE is", RMSE))
