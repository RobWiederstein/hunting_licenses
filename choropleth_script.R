# Geospatial data available at the geojson format
library(geojsonio)
file <- "./data_pure/shp/nhgis0007_shape/nhgis0007_shapefile_tl2000_us_state_1960/US_state_1960.shp"
spdf <- geojson_read(file,  what = "sp")

#probably need to get rid of AL & HA


plot(spdf)
# Since it is a bit too much data, I select only a subset of it:
spdf <- spdf[ substr(spdf@data$code,1,2)  %in% c("06", "83", "13", "30", "34", "11", "66") , ]
# I need to fortify the data AND keep trace of the commune code! (Takes ~2 minutes)
library(broom)
spdf_fortified <- tidy(spdf, region = "code")
# Now I can plot this shape easily as described before:
library(ggplot2)
ggplot() +
        geom_polygon(data = spdf_fortified, aes( x = long, y = lat, group = group), fill="white", color="grey") +
        theme_void() +
        coord_map()


