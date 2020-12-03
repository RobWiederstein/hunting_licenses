file <- "./data_pure/loc/library_of_congress_legal_directory_american_conservation_movement.csv"
df <- data.table::fread(file = file, data.table = F, header = T)
df <- df[, -ncol(df)]
names(df) <- c("url", "cite", "created", "summary", "notes")
#date
df$date <- stringr::str_sub(df$created, -10)
df$date <- gsub("ffice |Globe ", "", df$date)
df$date[which(nchar(df$date) == 4)] <- paste(df$date[which(nchar(df$date) == 4)], " 01 01", sep = "")
df$date <-gsub(" ", "-", df$date)
df$date <- as.Date(df$date)
#amt
df$amt <- 1
#classify cites into the following categories:
df$type <- ""
classifications <-c("Debates and Proceedings", 
                    "Senate Report", 
                    "House Report", 
                    "Senate Miscellaneous Document", 
                    "Senate Executive Document", 
                    "Senate Document",
                    "Joint Resolution",
                    "An [Aa]ct",
                    "Proclamation"
  )
#554 out of 555 classified
l.cat <- sapply(classifications, grep, df$cite)
df$type[l.cat$`Debates and Proceedings`] <- 1
df$type[l.cat$`Senate Report`] <- 2
df$type[l.cat$`House Report`] <- 3
df$type[l.cat$`Senate Miscellaneous Document`] <- 4
df$type[l.cat$`Senate Executive Document`] <- 5
df$type[l.cat$`Senate Document`] <- 6
df$type[l.cat$`Joint Resolution`] <- 7
df$type[l.cat$`An [Aa]ct`] <- 8
df$type[l.cat$Proclamation] <- 9
df$type[which(df$type == "")] <- 7 #all accounted for
df$type <- factor(df$type, labels = classifications)

#created
df$created_1 <- df$created
df$created_1 <- gsub("\nCREATED/PUBLISHED ", "", df$created_1)
df$created_1 <- str_extract(df$created_1, "[^1|2]+")
a <- str_split(df$created_1, ":|\\.")
a <- lapply(a, stringr::str_trim, side = "both")
#country
df$country <- unlist(lapply(a, "[", 1))
#state
df$state <- unlist(lapply(a, "[", 2))
df$state[which(df$state == "District of Columbia  Washington Government Printing Office")] <- "District of Columbia"
#publisher
df$publisher <- unlist(lapply(a, "[", 3))
df$publisher <- gsub("Boston Little, Brown and Company", "Boston Little, Brown, and Company", df$publisher)
df$publisher <- gsub("Washington \\[n", "unknown", df$publisher)
#summary
df$summary_1 <- gsub("\nSUMMARY ", "", df$summary) #title
df$summary_1 <- gsub("\"", "", df$summary_1)
df$summary_1 <- stringr::str_trim(side = "both")
df$summary_1[sample(1:nrow(df), 5)]

df$title <- stringr::str_extract(df$cite, 'An Act.*[.]')
file <- "./data_tidy/loc_bibliography_conservation.csv"
write.csv(df, file = file, row.names = F)

