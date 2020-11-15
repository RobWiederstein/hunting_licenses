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