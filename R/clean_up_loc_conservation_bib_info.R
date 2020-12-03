file <- "./data_pure/loc/library_of_congress_legal_directory_american_conservation_movement.csv"
df <- data.table::fread(file = file, data.table = F, header = T)
df <- df[, -ncol(df)]
names(df) <- c("url", "name", "created", "summary", "notes")
#date
df$date <- stringr::str_sub(df$created, -10)
df$date <- gsub("ffice |Globe ", "", df$date)
df$date[which(nchar(df$date) == 4)] <- paste(df$date[which(nchar(df$date) == 4)], " 01 01", sep = "")
df$date <- as.Date(df$date)
#amt
df$amt <- 1
