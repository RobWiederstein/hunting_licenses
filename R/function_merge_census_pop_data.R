merge_census_pop_data <- function(){
      get_pop_2000_us_census <- function(){
      file <- "./data_pure/st-est00int-01.csv"
      df <- read.csv(file = file, header = F, skip = 9, nrows = 51, colClasses = "character")
      df.01 <- df[, -2] #omit April, 2000
      df.01 <- df.01[-grep("District of Columbia", df.01$V1), ] #omit D.C.
      df.01[, 1] <- gsub("\\.", "", df.01[, 1]) #eliminate "."
      df.01[, 2:13] <- apply(df.01[, 2:13], 2, function(x){gsub(",", "", x)}) #no ","
      df.01[, 2:13] <- apply(df.01[, 2:13], 2, function(x){as.numeric(x)}) #numeric
      df.02 <- df.01[1:2]
      colnames(df.02) <- c("state", "pop_2000")
      df.02
}
      get_pop_2019_us_census <- function(){
            file <- "./data_pure/nst-est2019-01.csv"
            df.00 <- read.csv(file = file, 
                              skip = 9, 
                              nrow = 51, 
                              header = F,
                              colClasses = "character")
            df.00[, 1] <- gsub("\\.", "", df.00[, 1]) #lose "."
            df.00 <- df.00[-grep("District of Columbia", df.00[, 1])] #omit DC
            df.00 <- df.00[, c(1, 12)] #subset to states and 2019
            df.00[, 2] <- gsub(",", "", df.00[, 2]) #omit ","
            df.00[, 2] <- as.numeric(df.00[, 2]) #convert to numeric
            colnames(df.00) <- c("state", "pop_2019") #labels
            df.00
      }
      df <- merge(get_pop_2000_us_census(),
                  get_pop_2019_us_census()
            )
      df
}
df <- merge_census_pop_data()

# df$diff <- df[, 3] - df[, 2]
# df$pct_chg <- df$diff / df$pop_2000
# df$pct_chg <- df$pct_chg * 100
# df$pct_chg <- round(df$pct_chg, 1)
# df <- dplyr::arrange(df, -pct_chg)
# df$rank <- 1:nrow(df)

