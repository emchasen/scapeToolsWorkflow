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
  filter(crop == "cc") %>%
  mutate_if(is.character, as.factor) %>%
  dplyr::select(c(PI, Erosion, cover, tillage, Contour, initialP, total_DM_lbs, totalP2O5_lbs, slope, slopelenusle.r, LSsurgo,
                  total.depth, OM, silt, k)) %>%
  mutate(Contour = as.factor(Contour)) %>%
  drop_na() %>%
  distinct() %>%
  droplevels()

summary(dat)

rm(full)

set.seed(0731)
inTrain <- createDataPartition(y = dat$PI, p = 0.7, list = FALSE)
train <- dat[inTrain,]
test <- dat[-inTrain,]

# nointerx.lm <- lm(PI~., data = train)
# stepForward <- stepAIC(nointerx.lm, scope = . ~ .^2, direction = "forward")
# saveRDS(stepForward, "ccPI_stepForward_redCedar.rds")

allinterx.lm <- lm(PI~.^2,data = train)
stepBack <- stepAIC(allinterx.lm, direction = "backward")
saveRDS(stepBack, "ccPI_stepBack_eastCentralWI.rds")



