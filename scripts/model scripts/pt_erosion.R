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

pt <- full %>%
 filter(crop == "pt", 
        Contour == "0") %>%
 mutate_if(is.character, as.factor) %>%
 drop_na() %>%
 #filter(OM < 20) %>%
 select(c(Erosion, rotational, density, total_DM_lbs, slope, slopelenusle.r, sand, silt, clay, k)) %>%
 distinct() %>%
 droplevels()


rm(full)

summary(pt)
print(paste('pt data is', dim(pt)))

#partition data
set.seed(123)
split <- initial_split(pt, strata = Erosion)
train <- training(split)
test <- testing(split)

pt_rec <- recipe(Erosion ~ ., data = train)

pt_prep <- prep(pt_rec)
juiced <- juice(pt_prep)

tune_spec <- rand_forest(
 mtry = tune(),
 trees = 60,
 min_n = tune()
) %>%
 set_mode("regression") %>%
 set_engine("randomForest")

tune_wf <- workflow() %>%
 add_recipe(pt_rec) %>%
 add_model(tune_spec)

# train hyperparameters
set.seed(234)
folds <- vfold_cv(train)

rf_grid <- grid_regular(
 mtry(range = c(3, 7)),
 min_n(range = c(5, 8)),
 levels = 4
)

print('begin paramterizing')
set.seed(456)
regular_res <- tune_grid(
 tune_wf,
 resamples = folds,
 grid = rf_grid
)

metrics <- regular_res %>%
 collect_metrics()
write.csv(metrics, "pt_ErosionMetrics_eastCentralWI.csv", row.names = FALSE, quote = FALSE)

#choose best model
best_rmse <- select_best(regular_res, "rmse")

final_rf <- finalize_model(
 tune_spec,
 best_rmse
)

mod <- final_rf %>%
 set_engine("randomForest", importance = TRUE) %>%
 fit(Erosion ~ .,
     data = train
 )

saveRDS(mod, "pt_erosion_eastCentralWI.rds")

testRMSE <- mod %>%
 predict(test) %>%
 bind_cols(test) 

RMSE <- round(sqrt(mean((testRMSE$.pred - testRMSE$Erosion)^2)),5)

print(paste("RMSE without R factor is", RMSE))


