library(tidyverse)

# soilP data from https://uwlab.webhosting.cals.wisc.edu/wp-content/uploads/sites/17/2016/06/P_05-09.pdf
# land area from Wikipedia (sq miles)

# southwest WI -----------------------------
countySoilP <- data.frame(county = c("lacrosse", "vernon", "crawford", "richland", "monroe"),
                          land_area = c(452, 792, 571, 586, 901),
                          soilP = c(60, 36, 32, 56, 50)) %>%
  mutate(soilP_area = land_area * soilP)

countySoilP

totalSoilP = sum(countySoilP$soilP_area)
averageSoilP = totalSoilP/sum(countySoilP$land_area)

averageSoilP
# 46


# clover belt -------------------------------------------------------------

countySoilP <- data.frame(county = c("marathon", "clark", "taylor"),
                          land_area = c(1545, 1210, 975),
                          soilP = c(47, 37, 33)) %>%
  mutate(soilP_area = land_area * soilP)

totalSoilP = sum(countySoilP$soilP_area)
averageSoilP = totalSoilP/sum(countySoilP$land_area) # 40

# northest wi ---------------------------------------------

countySoilP <- data.frame(county = c("brown", "kewaunee", "manitowoc"),
                          land_area = c(530, 343, 589),
                          soilP = c(36, 37, 36)) %>%
  mutate(soilP_area = land_area * soilP)

totalSoilP = sum(countySoilP$soilP_area)
averageSoilP = totalSoilP/sum(countySoilP$land_area) # 36

# uplands wi ------------------------

countySoilP <- data.frame(county = c("grant", "iowa", "lafayette", 'sauk', 'green'),
                          land_area = c(1147, 763, 634, 831, 584),
                          soilP = c(41, 38, 45,58, 45)) %>%
  mutate(soilP_area = land_area * soilP)

totalSoilP = sum(countySoilP$soilP_area)
averageSoilP = totalSoilP/sum(countySoilP$land_area) # 45

# redCedarWI------------------

countySoilP <- data.frame(county = c("barron", "burnett", "chippewa", "Dunn", "pierce", "polk", "rusk", "st. croix", "sawyer", "washburn"),
                          land_area = c(863, 872, 1008, 850, 574, 914, 914, 722, 1257, 797),
                          soilP = c(55, 50, 50, 59, 41, 45, 45, 41, 55, 57)) %>%
  mutate(soilP_area = land_area * soilP)

totalSoilP = sum(countySoilP$soilP_area)
averageSoilP = totalSoilP/sum(countySoilP$land_area) # 50

# eastCentral WI----------------

countySoilP <- data.frame(county = c("Oconto", "Shawano", "Outagamie", "Winnebago"),
                          land_area = c(998, 893, 638, 434),
                          soilP = c(47, 43, 42, 47)) %>%
  mutate(soilP_area = land_area * soilP)

totalSoilP = sum(countySoilP$soilP_area)
averageSoilP = totalSoilP/sum(countySoilP$land_area) # 45

# southEast WI----------------

countySoilP <- data.frame(county = c("Calumet", "Dodge", "Fond du Lac", "Green Lake", "Ozaukee",
                                     "Sheboygan", "Washington", "Waukesha"),
                          land_area = c(318, 876, 720, 349, 233, 511, 431, 550),
                          soilP = c(36, 51, 44, 53, 37, 46, 44, 53)) %>%
 mutate(soilP_area = land_area * soilP)

totalSoilP = sum(countySoilP$soilP_area)
averageSoilP = totalSoilP/sum(countySoilP$land_area) # 47
