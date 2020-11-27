#uscb inflation adjuster from bls
path <- "./data_pure/uscb/CPI_U_RS.xlsx"
df.inf.01 <- readxl::read_excel(path = path, skip = 2)
df.inf.01 <- na.omit(df.inf.01)
#usfw conservation revenue
file <- "./data_pure/usfw/WRApportionmentsHE-1939-2020.csv"
df.wr <- data.table::fread(input = file, data.table = F, skip = 1, 
                           header = T, strip.white  = T)
#colnames
colnames(df.wr) <- tolower(colnames(df.wr))
colnames(df.wr)[2:ncol(df.wr)] <- gsub(" ", "", colnames(df.wr)[2:ncol(df.wr)])
 #set space
df.wr <- df.wr[1:grep("WYOMING", df.wr$state), 1:grep("fy2020", colnames(df.wr))]
#format columns
df.wr[, 2:ncol(df.wr)] <- apply(df.wr[, 2:ncol(df.wr)], 2, function(x)stringr::str_trim(x, "both"))
df.wr[, 2:ncol(df.wr)] <- apply(df.wr[, 2:ncol(df.wr)], 2, function(x)gsub("\\$|,", "", x))
df.wr[, 2:ncol(df.wr)] <- apply(df.wr[, 2:ncol(df.wr)], 2, function(x)as.numeric(x))      
#title case
df.wr[, 1] <- stringr::str_to_title(df.wr[, 1])
#convert to long
df.wr <- tidyr::gather(df.wr, key = year, value = value, -state)
df.wr$year <- gsub("fy", "", df.wr$year)
df.wr$year <- as.integer(df.wr$year)

df.wr$inf_adj_wr_allocation <- priceR::afi(df.wr$value, from_date = df.wr$year, to_date = 2019, country = "US")
#actual dollars by year
df.wr.01 <-
        df.wr %>%
        group_by(year) %>%
        summarize(total = sum(value))
#2019 /constant dollars by year
df.wr.02 <-
        df.wr %>%
        group_by(year) %>%
        summarize(total = sum(inf_adj_wr_allocation))

p <- ggplot(df.wr.01, aes(year, total))
p <- p + geom_line()
p <- p + geom_smooth()
p <- p + geom_line(data = df.wr.02, aes(year, total), color = "red")
p

df.cons <- 
library(zoo)
library(quantmod)
library(lubridate)
quantmod::getSymbols("CPIAUCSL", src='FRED')
# make an `xts` object of prices
set.seed(1)
p <- xts(rnorm(63, mean=10, sd=3), seq(from=as.Date('1950-12-01'), by='years', length.out=63))
colnames(p) <- "price"
avg.cpi <- apply.yearly(CPIAUCSL, mean)
cf <- avg.cpi/as.numeric(avg.cpi['2008']) #using 2008 as the base year
dat <- merge(p, cf, all=FALSE)
dat$adj <- dat[, 1] * dat[, 2]
tail(dat)


#
set.seed(123)
prices <- rnorm(200, mean=10, sd=3)
years <- round(rnorm(200, mean=2006, sd=5))
df <- data.frame(prices, years)

library(priceR)
adjust_for_inflation(prices, years, "US", to_date = 2008)
# [1]  6.707112  8.102301 16.228195  9.785813 11.795624 17.197669 13.589684  7.210790  6.744690  9.250294 14.267029 11.561430  9.921566 12.007162 10.244619