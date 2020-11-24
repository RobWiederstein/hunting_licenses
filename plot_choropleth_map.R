# load geospatial libraries
x <- c("ggmap", "rgdal", "rgeos", "maptools", "dplyr", "tidyr", "tmap", "sf")
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
df.03$pct_change <- (df.03$'2020' - df.03$'1960') / df.03$'1960'
df.03$pct_change <- round(df.03$pct_change * 100, 1)
#septiles
df.03$septile <- ggplot2::cut_interval(df.03$pct_change, n = 7,
                                      labels = paste("S", 1:7, sep = "")
)


# add polygons
dsn <- "./data_pure/shp/cb_2019_us_all_20m/cb_2019_us_state_20m"
sl.00 <- rgdal::readOGR(dsn = dsn, layer = "cb_2019_us_state_20m", stringsAsFactors = F)
#omit DC & PR
sl.01 <- sl.00[-grep("DC|PR", sl.00$STUSPS), ]

# keep county name
keep.col <- c(1, 3, 5)
cl.01 <- cl.00[, keep.col]
colnames(cl.01@data) <- tolower(colnames(cl.01@data))
colnames(cl.01@data)[3] <- "location"
ogrDrivers()
dsn <- system.file("vectors", package = "rgdal")[1]
ogrListLayers(dsn)
ogrInfo(dsn)
ogrInfo(dsn=dsn, layer="cities")
owd <- getwd()
setwd(dsn)
ogrInfo(dsn="cities.shp")
ogrInfo(dsn="cities.shp", layer="cities")
setwd(owd)
ow <- options("warn")$warn
options("warn"=1)
cities <- readOGR(dsn=dsn, layer="cities")
str(slot(cities, "data"))
cities$POPULATION <- type.convert(as.character(cities$POPULATION),
                                  na.strings="-99", numerals="no.loss")
str(slot(cities, "data"))
cities <- readOGR(dsn=dsn, layer="cities", GDAL1_integer64_policy=TRUE)
str(slot(cities, "data"))
options("warn"=ow)
summary(cities)
table(Encoding(as.character(cities$NAME)))
ogrInfo(dsn=dsn, layer="kiritimati_primary_roads")
OGRSpatialRef(dsn=dsn, layer="kiritimati_primary_roads")
kiritimati_primary_roads <- readOGR(dsn=dsn, layer="kiritimati_primary_roads")
summary(kiritimati_primary_roads)
ogrInfo(dsn=dsn, layer="scot_BNG")
OGRSpatialRef(dsn=dsn, layer="scot_BNG")
scot_BNG <- readOGR(dsn=dsn, layer="scot_BNG")
summary(scot_BNG)
if ("GML" %in% ogrDrivers()$name) {
        dsn <- system.file("vectors/airports.gml", package = "rgdal")[1]
        airports <- try(readOGR(dsn=dsn, layer="airports"))
        if (!inherits(airports, "try-error")) summary(airports)
}
dsn <- system.file("vectors/ps_cant_31.MIF", package = "rgdal")[1]
ogrInfo(dsn=dsn, layer="ps_cant_31")
ps_cant_31 <- readOGR(dsn=dsn, layer="ps_cant_31")
summary(ps_cant_31)
sapply(as(ps_cant_31, "data.frame"), class)
ps_cant_31 <- readOGR(dsn=dsn, layer="ps_cant_31", stringsAsFactors=FALSE)
summary(ps_cant_31)
sapply(as(ps_cant_31, "data.frame"), class)
dsn <- system.file("vectors/Up.tab", package = "rgdal")[1]
ogrInfo(dsn=dsn, layer="Up")
Up <- readOGR(dsn=dsn, layer="Up")
summary(Up)
dsn <- system.file("vectors/test_trk2.gpx", package = "rgdal")[1]
test_trk2 <- try(readOGR(dsn=dsn, layer="tracks"))
if (!inherits(test_trk2, "try-error")) summary(test_trk2)
test_trk2pts <- try(readOGR(dsn=dsn, layer="track_points"))
if (!inherits(test_trk2pts, "try-error")) summary(test_trk2pts)
dsn <- system.file("vectors", package = "rgdal")[1]
ogrInfo(dsn=dsn, layer="trin_inca_pl03")
birds <- readOGR(dsn=dsn, layer="trin_inca_pl03")
summary(birds)
dsn <- system.file("vectors/PacoursIKA2.TAB", package = "rgdal")[1]
try(ogrInfo(dsn, "PacoursIKA2"))
ogrInfo(dsn, "PacoursIKA2", require_geomType="wkbPoint")
plot(readOGR(dsn, "PacoursIKA2", require_geomType="wkbLineString"), col="red")
plot(readOGR(dsn, "PacoursIKA2", require_geomType="wkbPoint"), add=TRUE)
odir <- getwd()
setwd(system.file("vectors", package = "rgdal")[1])
ow <- options("warn")$warn
options("warn"=1)
ogrInfo("test64.vrt", "test64")
str(readOGR("test64.vrt", "test64", verbose=FALSE, integer64="allow.loss")$val)
str(readOGR("test64.vrt", "test64", verbose=FALSE, integer64="warn.loss")$val)
str(readOGR("test64.vrt", "test64", verbose=FALSE, integer64="no.loss")$val)
str(readOGR("test64.vrt", "test64", verbose=FALSE, stringsAsFactors=FALSE,
            integer64="no.loss")$val)
setwd(odir)
options("warn"=ow)