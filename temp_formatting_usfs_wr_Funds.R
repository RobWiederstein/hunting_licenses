file <- "./data_pure/usfw/WRApportionmentsHE-1939-2020.csv" 
df.00 <- read.csv(file = file, 
                  header = T, 
                  colClasses = "character", 
                  strip.white = T, 
                  skip = 1,
                  nrow = 55
                  )
df.00 <- df.00[, 1:83]
df.00[, 1:ncol(df.00)] <- apply(df.00[, 1: ncol(df.00)], 2, function(x) stringr::str_trim(x, "both"))
df.00[, 1:ncol(df.00)] <- lapply(df.00[, 1:ncol(df.00)], function(x) gsub("\\$|,", "", x))
colnames(df.00) <- tolower(colnames(df.00))
colnames(df.00) <- gsub("fy\\.", "", colnames(df.00))
colnames(df.00)[2:ncol(df.00)] <- paste("wildlife_restoration_apportionment_funds", 1939:2020, sep = "_")
df.00$state <- tolower(df.00$state)
df.00$state <- stringr::str_to_title(df.00[, 1])
omit_terr <- c("American Samoa|Guam|N. Mariana Islands|Puerto Rico|U.s. Virgin Islands")
df.00 <- df.00[-grep(omit_terr, df.00$state), ]
df.01 <- tidyr::gather(df.00, key = key, value = value, -state)
df.01$year <- stringr::str_sub(df.01$key, start = -4)
df.01$key <- gsub("_[1-2][9|0][0-9][0-9]$", "", df.01$key)
df.02 <- dplyr::select(df.01, state, year, key, value)
df.02$year <- as.integer(df.02$year)
df.02$value <- as.numeric(df.02$value)

#convert to 2010 dollars

#plot

df.03 <- 
        df.02 %>%
        group_by(year) %>%
        summarize(total_funds = sum(value))

p <- ggplot(df.03, aes(year, total_funds))
p <- p + geom_line(color = "blue")
p
