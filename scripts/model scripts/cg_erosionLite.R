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


cgFull <- full %>%
 filter(crop == "cg") %>%
 mutate_if(is.character, as.factor) %>%
 drop_na() %>%
 #filter(OM < 20) %>%
 dplyr::select(c(Erosion, cover, tillage, Contour, total_DM_lbs, slope, slopelenusle.r, sand, silt, clay, k)) %>%
 mutate(Contour = as.factor(Contour)) %>%
 distinct() %>%
 droplevels()

rm(full)

summary(cgFull)
print(paste('cg data is', dim(cgFull)))

# sample
# sample 1
set.seed(123)
split <- initial_split(cgFull, prop = .7, strata = Erosion)
cg <- training(split)
cg_test <- testing(split)

print(paste('new cg data is', dim(cg)))

#partition data
set.seed(123)
split2 <- initial_split(cg, strata = Erosion)
train <- training(split2)
test <- testing(split2)

cg_rec <- recipe(Erosion ~ ., data = train)

cg_prep <- prep(cg_rec)
juiced <- juice(cg_prep)

tune_spec <- rand_forest(
 mtry = tune(),
 trees = 60,
 min_n = tune()
) %>%
 set_mode("regression") %>%
 set_engine("randomForest")

tune_wf <- workflow() %>%
 add_recipe(cg_rec) %>%
 add_model(tune_spec)

# train hyperparameters
set.seed(234)
folds <- vfold_cv(train, v = 5)

rf_grid <- grid_regular(
 mtry(range = c(3, 7)),
 min_n(range = c(5, 8)),
 levels = 2 # was 4
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
write.csv(metrics, "cg_ErosionMetrics_southEastWI.csv", row.names = FALSE, quote = FALSE)

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

saveRDS(mod, "cg_erosion_southEastWI.rds")

testRMSE <- mod %>%
 predict(test) %>%
 bind_cols(test) 

RMSE <- round(sqrt(mean((testRMSE$.pred - testRMSE$Erosion)^2)),5)

print(paste("RMSE on cg", RMSE))

testRMSE2 <- mod %>%
 predict(cg_test) %>%
 bind_cols(cg_test) 

RMSE2 <- round(sqrt(mean((testRMSE2$.pred - testRMSE2$Erosion)^2)),5)

print(paste("RMSE on cg_test", RMSE2))


