library(tidyverse)
library(MASS)
library(caret)

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


dat <- full %>%
  filter(crop == "pt", 
         Contour == "0") %>%
  mutate_if(is.character, as.factor) %>%
  dplyr::select(c(PI, Erosion, density, rotational, initialP, total_DM_lbs, totalP2O5_lbs, slope, slopelenusle.r, LSsurgo,
                  total.depth, OM, silt, k)) %>%
  distinct() %>%
  drop_na() %>%
  droplevels()

rm(full)
summary(dat)

set.seed(0731)
inTrain <- createDataPartition(y = dat$PI, p = 0.7, list = FALSE)
train <- dat[inTrain,]
test <- dat[-inTrain,]

# nointerx.lm <- lm(PI~., data = train)
# stepForward <- stepAIC(nointerx.lm, scope = . ~ .^2, direction = "forward")
# saveRDS(stepForward, "ptPI_stepForward_redCedar.rds")

allinterx.lm <- lm(PI~.^2,data = train)
stepBack <- stepAIC(allinterx.lm, direction = "backward")
saveRDS(stepBack, "ptPI_stepBack_eastCentralWI.rds")

