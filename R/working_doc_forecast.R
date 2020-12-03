#forecast hunting licenses
library(forecast)
library(lubridate)

file <- "./data_tidy/hunting_licenses_by_state_1960_and_2020.csv"
colClasses <- c(rep("character", 3), "integer", "character", "numeric")
df.00 <- data.table::fread(file = file, data.table = FALSE, colClasses = colClasses)
df.01 <- dplyr::filter(df.00, key == "certified_paid_hunting_license_holders")
df.02 <- df.01 %>%
            group_by(year) %>%
            summarize(tot_hunting_lic_holders = sum(value))
#df.02$year <- 2000:2006
df.02$year <- paste(df.02$year, "-06-30-", sep = "")
df.02$year <- as.Date(df.02$year, format = "%Y-%m-%d")
df.02 <- data.frame(df.02)
df.02 <- ts(df.02)
df.02 %>% forecast %>% plot
