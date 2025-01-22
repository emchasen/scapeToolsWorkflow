#load libraries
library(tidyverse)
library(randomForest)
library(tidymodels)

##load data files from each county in learning hub---------
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


dr <- full %>%
 filter(crop == "dr") %>%
 mutate_if(is.character, as.factor) %>%
 drop_na() %>%
 #filter(OM < 20) %>%
 dplyr::select(c(Erosion, cover, tillage, Contour, total_DM_lbs, slope, slopelenusle.r, sand, silt, clay, k)) %>%
 mutate(Contour = as.factor(Contour)) %>%
 distinct() %>%
 droplevels()

rm(full)

summary(dr)
print(paste('dr data is', dim(dr)))

#partition data
set.seed(123)
split <- initial_split(dr, strata = Erosion)
train <- training(split)
test <- testing(split)

dr_rec <- recipe(Erosion ~ ., data = train)

dr_prep <- prep(dr_rec)
juiced <- juice(dr_prep)

tune_spec <- rand_forest(
 mtry = tune(),
 trees = 60,
 min_n = tune()
) %>%
 set_mode("regression") %>%
 set_engine("randomForest")

tune_wf <- workflow() %>%
 add_recipe(dr_rec) %>%
 add_model(tune_spec)

# train hyperparameters
set.seed(234)
folds <- vfold_cv(train)

rf_grid <- grid_regular(
 mtry(range = c(3, 7)),
 min_n(range = c(5, 8)),
 levels = 3 #was 4
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
write.csv(metrics, "dr_ErosionMetrics_southEastWI.csv", row.names = FALSE, quote = FALSE)

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

saveRDS(mod, "dr_erosion_southEastWI.rds")

testRMSE <- mod %>%
 predict(test) %>%
 bind_cols(test) 

RMSE <- round(sqrt(mean((testRMSE$.pred - testRMSE$Erosion)^2)),5)

print(paste("RMSE without R factor is", RMSE))


