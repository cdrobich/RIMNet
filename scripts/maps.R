
# Resources ---------------------------------------------------------------
# https://rkabacoff.github.io/datavis/Maps.html
# https://chiajung-yeh.github.io/Spatial-Analysis/data-visualization-with-maps.html
# https://rfortherestofus.com/2022/03/heatmaps/
# https://trucvietle.me/r/tutorial/2017/01/18/spatial-heat-map-plotting-using-r.html

# Mapping -----------------------------------------------------------------

library(tidyverse)
library(sf)
library(mapview)

data <- read.csv('data/all_facilities.csv')
ottawa_map <- read_sf('data/OWB/ottawa_river_watershed.shp')


## creating the Ottawa River watershed shapefile
# map <- read_sf("data/OWB/ONT_WSHED_BDRY.shp")
# 
# map %>% mapview()
# 
# colnames(map)
# class(map)
# 
# ottawa_river <- c("Lower Ottawa River", "Central Ottawa River","Upper Ottawa River")
# 
# ottawa_map <- map %>% filter(NAME2 %in% ottawa_river)
# ottawa_map %>% mapview()
# 
# st_write(ottawa_map, "data/OWB/ottawa_river_watershed.shp")

## turn data into shapefile
colnames(data)

data_test <- data %>% select(year:sample_type_en, 
                             substance_name_en, 
                             statistical_quantity_en, 
                             result, units, sample_category)

data_shp <- st_as_sf(data_test, coords = c("longitude","latitude"), crs = 4269)

colnames(data_shp)

st_write(data_shp, "data/OWB/all_facilities.shp")

ggplot() + 
  geom_sf(data = ottawa_map) +
  geom_point(data = data,
             aes(x = longitude, y = latitude, colour = facility_name),
             size = 5) 

ottawa_data_join <- st_join(ottawa_map, data_shp)

m <- mapview(data_shp)
leafem::addFeatures(m, ottawa_map)

########### I'd like to make a map that looks like this but you can click 
# and see different summary values for different substances
mapview(data_shp,
        zcol = c("sample_type_en", "substance_name_en", "result"),
        burst = T) +
  mapview(ottawa_map, alpha.regions = 0.3, legend = F)
