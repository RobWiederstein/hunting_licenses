library(choroplethr)
file <- "./data_tidy/hunting_licenses_by_state_2000_and_2020.csv"
df.00 <- read.csv(file = file, stringsAsFactors = F, colClasses = "character")
keep <- c("per_capita_certified_paid_hunting_license_holders_2000_to_population_2000",
          "per_capita_certified_paid_hunting_license_holders_2020_to_population_2020"
)
df.01 <- subset(df.00, key %in% keep)
df.01$value <- as.numeric(df.01$value)
df.01$year <- as.integer(df.01$year)
df.01$value <- df.01$value * 100

           
library(ggplot2)
p <- ggplot(df.01, aes(year, value, group = state))
p <- p + geom_line()
p

#plot median state per capita hunting license to population
library(dplyr)
library(magritr)
df.02 <- 
      df.01 %>%
      group_by(key, year) %>%
      summarize(median_per_capita = median(value))

p <- ggplot(df.02, aes(year, median_per_capita)) 
p <- p + geom_line()
p <- p + scale_y_continuous(limits = c(1, 10))
p

#plot density
p <- ggplot(df.01, aes(value, group = year, color = year))
p <- p + geom_density()
p
