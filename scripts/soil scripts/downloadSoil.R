# this script downloads soils data from SSURGO using the fedData package
# running the code will create new local directories on your computer that store the county level soil data
# EXTRACTIONS/SSURGO/'label'
# you can also download the soil direction from web soil survey. It's just that the column labels have to be manually added when downloaded from the web

# if you haven't installed the libraries previously
#install.packages("FedData") 
#install.packages("tidyverse")
library(FedData)
library(tidyverse)

# download data-----------------------------------
# use websoil survey to locate the area code needed to fill in the template code https://websoilsurvey.nrcs.usda.gov/app/

CalumetManitowocCounty <- get_ssurgo(template = "WI600", label = "CalumetManitowoc")
DodgeCounty <- get_ssurgo(template=c('WI027'), label='Dodge')
FondDuLacCounty <- get_ssurgo(template=c('WI039'), label='FondDuLac')
GreenLakeCounty <- get_ssurgo(template = 'WI047', label = "GreenLake")
OcontoCounty <- get_ssurgo(template = 'WI083', label = "Oconto")
OutagamieCounty <- get_ssurgo(template = "WI087", label = "Outagamie")
OzaukeeCounty <- get_ssurgo(template = "WI089", label = "Ozaukee")
ShawanoCounty <- get_ssurgo(template=c('WI115'), label='Shawano')
SheboyganCounty <- get_ssurgo(template = "WI117", label = "Sheboygan")
WashingtonCounty <- get_ssurgo(template = "WI131", label = "Washington")
MilwaukeeWaukeshaCounty <- get_ssurgo(template = "WI602", label = "MilwaukeeWaukesha")
WinnebagoCounty <- get_ssurgo(template = "WI139", label = "Winnebago")

