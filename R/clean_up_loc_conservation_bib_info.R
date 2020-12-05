library(stringr)
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
df$action <- ""
classifications <-c("Debates and Proceedings", 
                    "Senate Report", 
                    "House Report", 
                    "Senate Miscellaneous Document", 
                    "Senate Executive Document", 
                    "Senate Document",
                    "Joint Resolution",
                    "An Act",
                    "Proclamation"
  )
#554 out of 555 classified
l.cat <- sapply(classifications, grep, df$cite)
df$action[l.cat$`Debates and Proceedings`] <- 1
df$action[l.cat$`Senate Report`] <- 2
df$action[l.cat$`House Report`] <- 3
df$action[l.cat$`Senate Miscellaneous Document`] <- 4
df$action[l.cat$`Senate Executive Document`] <- 5
df$action[l.cat$`Senate Document`] <- 6
df$action[l.cat$`Joint Resolution`] <- 7
df$action[l.cat$`An Act`] <- 8
df$action[l.cat$Proclamation] <- 9
df$action[which(df$action == "")] <- 7 #all accounted for
df$action <- factor(df$action, labels = classifications)
levels(df$action)[8] <- "Law Passed"
#created
df$created_1 <- df$created
df$created_1 <- gsub("\nCREATED/PUBLISHED ", "", df$created_1)
df$created_1 <- str_extract(df$created_1, "[^1|2]+")
a <- str_split(df$created_1, ":|\\.")
a <- lapply(a, str_trim, side = "both")
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
df$summary_1 <- stringr::str_trim(df$summary_1, side = "both")
#notes
df$notes_1 <- gsub("\nNOTES", "", df$notes)
#title
df$title <- stringr::str_extract(df$cite, 'An Act.*[.]')
#cite_1 wrap in <a href = [url]>cite</a>
df$cite_1 <- paste("<a href=", df$url, ">", df$cite, "</a>", sep = "")
#write-out
file <- "./data_tidy/loc_bibliography_conservation.csv"
write.csv(df, file = file, row.names = F)

