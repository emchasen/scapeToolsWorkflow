library(sf)
library(tidyverse)

# load data
# load shapefiles
wi <- st_read("soilShapes/WI_Counties2010/WI_Counties2010.shp")
#transform to stream crs
stream <- st_read("scripts/water body scripts/data/perStreams.shp")
streamCRS <- st_crs(stream)
wi_stream <- st_transform(wi, streamCRS)

# east central wi--------------------
##Cut WI to learning hub regions and contiguous counties----------
EC_counties <- c("Oconto", "Outagamie", "Shawano", "Winnebago", "Marinette", "Forest", "Langlade",
                 "Menominee", "Marathon", "Portage", "Waupaca", "Waushara", "Green Lake", "Fond du Lac",
                 "Calumet", "Brown") 

ec_wi <- wi_stream %>%
 dplyr::filter(NAME %in% EC_counties)

## write file----------------
st_write(ec_wi, "scripts/water body scripts/data/eachCentralWI_shape.shp")

# south east counties--------
##Cut WI to learning hub regions and contiguous counties----------
SE_counties <- c("Sheboygan", "Fond du Lac","Green Lake", "Dodge", "Washington", "Waukesha", "Ozaukee", "Calumet",
                 "Manitowoc", "Brown", "Outagamie", "Winnebago", "Waushara", "Marquette", "Columbia", "Dane", 
                 "Jefferson", "Walworth", "Racine", "Milwaukee")

se_wi <- wi_stream %>%
 dplyr::filter(NAME %in% SE_counties)

## write file----------------
st_write(se_wi, "scripts/water body scripts/data/southEastWI_shape.shp")
