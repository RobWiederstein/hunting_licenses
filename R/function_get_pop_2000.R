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

