# load geospatial libraries
x <- c("ggmap", "rgdal", "rgeos", "maptools", "dplyr", "tidyr", "tmap", "sf",
       "mapproj")
# install.packages(x)
lapply(x, library, character.only = TRUE)
# load data
file <- "./data_tidy/hunting_licenses_by_state_1960_and_2020.csv"
df.00 <- read.csv(file = file, header = T, colClasses = "character")
# format data
df.00$year <- as.integer(df.00$year)
df.00$value <- as.numeric(df.00$value)
#find % change in hunters license by state 1960 to 2020
df.01 <- dplyr::filter(df.00, year %in% c(1960, 2020)) #get year
df.02 <- dplyr::filter(df.01, key == "per_capita_hunting_license") #variable
df.03 <- tidyr::spread(df.02, year, value)
#add column pct change in per capita
df.03$avg_annual_pct_change <- (df.03$'2020' - df.03$'1960') / df.03$'1960'
df.03$avg_annual_pct_change <- df.03$avg_annual_pct_change/(2020 - 1960)
df.03$avg_annual_pct_change <- round(df.03$avg_annual_pct_change * 100, 1)

# add polygons
dsn <- "./data_pure/shp/cb_2019_us_all_20m/cb_2019_us_state_20m"
sl.00 <- rgdal::readOGR(dsn = dsn, layer = "cb_2019_us_state_20m", stringsAsFactors = F)

#omit DC & PR
sl.01 <- sl.00[-grep("DC|PR", sl.00$STUSPS), ]

#bring alaska and hawaii to the US
sl.02 <- spTransform(sl.01, CRS("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"))
sl.02@data$id = rownames(sl.02@data)

#alaska
alaska = sl.02[sl.02$STATEFP=="02",]
alaska = elide(alaska, rotate=-50)
alaska = elide(alaska, scale=max(apply(bbox(alaska), 1, diff)) / 2.3)
alaska = elide(alaska, shift=c(-2100000, -2500000))
proj4string(alaska) = proj4string(sl.02)
#hawaii
hawaii = sl.02[sl.02$STATEFP=="15",]
hawaii = elide(hawaii, rotate=-35)
hawaii = elide(hawaii, shift=c(5400000, -1400000))
proj4string(hawaii) = proj4string(sl.02)
#Drop Alaska & Hawaii from dataframe
sl.02 = sl.02[!sl.02$STATEFP %in% c("02", "15"), ]
#Add new Alaska & Hawaii
sl.02 = rbind(sl.02, alaska, hawaii)

#join sp with dataframe
#http://www.nickeubank.com/wp-content/uploads/2015/10/RGIS2_MergingSpatialData_part1_Joins.html
sf.00 <- merge(sl.02, df.03, by.x = "STATEFP", by.y = "fips")
class(sf.00)

us_states <- sf.00
tmap_mode("plot")

us_states_map = 
      tm_shape(us_states) + 
      tm_polygons("avg_annual_pct_change",
                  midpoint = NA,
                  palette = "RdYlGn", 
                  minimize = T, 
                  contrast = c(0, 0.8),
                  legend.hist = TRUE
                  ) + 
      tm_layout(main.title = "US Avg. Annual Pct. Change in Hunters Per Capita \n 1960 - 2020",
                main.title.size = 1,
                main.title.position = "center",
                bg.color = "white",
                frame = FALSE,
                legend.text.color = "black",
                #legend.position = c(.9, 0),
                legend.outside = TRUE
                ) + 
      tm_text("abb", size = .5)

filename <- "./figs/us-avg-annual-pct-change-per-capita-hunter-1960-2020.jpg"
jpeg(filename = filename, width = 7, height = 5, units = "in", res = 300)
us_states_map
dev.off()
