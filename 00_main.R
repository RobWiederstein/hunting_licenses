compute_pct_change_hunting_licenses_from_2000_to_2020 <- function(){
   merge_sources_for_license_and_population <- function(){
      merge_hunters_license_data <- function(){
         get_hunters_license_2000_fw <- function(){
            file <- "./data_pure/tabula-2020-11-08_fw_hunting_licenses_2000.csv"
            df.00 <- read.csv(file = file, as.is = T, header = F)
            df.01 <- df.00[, -c(6, 8, 10)] #drop "$" columns
            df.01[, 2:ncol(df.01)] <- apply(df.01[, 2:ncol(df.01)], 2, function(x){gsub(",| ", "", x)})
            df.01[, 2:ncol(df.01)] <- apply(df.01[, 2:ncol(df.01)], 2, function(x){as.numeric(x)})
            #add var names
            my_col_names <- c(
               "state",
               "certified_paid_hunting_license_holders_2000",
               "resident_hunting_licenses_tags_permits_stamps_2000",
               "non-resident_hunting_licenses_tags_permits_stamps_2000",
               "total_hunting_licenses_tags_permits_stamps_2000",
               "gross_cost_to_resident_hunters_2000",
               "gross_cost_to_non-resident_hunters_2000",
               "total_gross_cost_to_hunters_2000"
            )
            colnames(df.01) <- my_col_names
            df.01
      }
         get_hunters_license_2020_fw <- function(){
            file <- "./data_pure/tabula-2020-11-08_fw_hunting_licenses_2020.csv"
            df.00 <- read.csv(file = file, as.is = T, header = T)
            colnames(df.00) <- paste("V", 1:ncol(df.00), sep = "") #shorten vars
            df.00 <- df.00[-grep("AS|DC|GU|MP|PR|VI|Total:", df.00$V1), ] #omit territories
            df.00[, 2:ncol(df.00)] <- apply(df.00[, 2:ncol(df.00)], 2, function(x){gsub(",|\\$| ", "", x)})
            df.00[, 2:ncol(df.00)] <- apply(df.00[, 2:ncol(df.00)], 2, function(x){as.numeric(x)})
            my_col_names <- c(
               "state",
               "certified_paid_hunting_license_holders_2020",
               "resident_hunting_licenses_tags_permits_stamps_2020",
               "non-resident_hunting_licenses_tags_permits_stamps_2020",
               "total_hunting_licenses_tags_permits_stamps_2020",
               "gross_cost_to_resident_hunters_2020",
               "gross_cost_to_non-resident_hunters_2020",
               "total_gross_cost_to_hunters_2020"
            )
            colnames(df.00) <- my_col_names
            #state names
            df.states <- data.frame(state = state.name, abb = state.abb, stringsAsFactors = F)
            colnames(df.00)[1] <- "abb"
            df.00 <- merge(df.00, df.states)
            df.00
         }
         merge(
            get_hunters_license_2000_fw(),
            get_hunters_license_2020_fw()
         )
      }
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
      df.00 <- merge(merge_hunters_license_data(),
                     merge_census_pop_data()
                     )
   }
   df.00 <- merge_sources_for_license_and_population()
   #add new variables
   df.00$per_capita_certified_paid_hunting_license_holders_2000_to_population_2000 <- df.00$certified_paid_hunting_license_holders_2000/df.00$pop_2000
   df.00$per_capita_certified_paid_hunting_license_holders_2020_to_population_2020 <- df.00$certified_paid_hunting_license_holders_2020/df.00$pop_2019
   df.00$diff_in_resident_hunting_license_per_capita_2000_to_2020 <- df.00$per_capita_certified_paid_hunting_license_holders_2020_to_population_2020 - df.00$per_capita_certified_paid_hunting_license_holders_2000_to_population_2000
   df.00$divided_by_base_year_2000 <- df.00$diff_in_resident_hunting_license_per_capita_2000_to_2020 / df.00$per_capita_certified_paid_hunting_license_holders_2000_to_population_2000
   df.00$pct_change_per_capita_resident_hunting_license_2020 <- df.00$divided_by_base_year_2000 * 100
   df.00$pct_change_per_capita_resident_hunting_license_2020 <- round(df.00$pct_change_per_capita_resident_hunting_license, 1)
   df.00
}

df.00 <- compute_pct_change_hunting_licenses_from_2000_to_2020()

#add fips codes for choropleth map
merge_with_state_fips_codes <- function(){
   file <- "./data_pure/state_fips_codes.csv"
   df <- read.csv(file = file, header = T, colClasses = "character")
   colnames(df) <- c("state", "fips", "abb")
   omit.terr <- c("DC", "AS", "GU", "MP", "PR", "UM", "VI")
   df <- df[-which(df[, 3]  %in% omit.terr), ]
   df$fips[which(nchar(df$fips) == 1)] <- paste("0", df$fips[which(nchar(df$fips) == 1)], sep = "")
   df
}

df.01 <- merge(df.00, merge_with_state_fips_codes())
#reorder columns
library(magrittr)
library(dplyr)
df.02 <- df.01[, c(24, 1, 2:8, 10:20, 23)]
#convert to long
df.03 <-
   df.02 %>%
   gather(key = "key", value = value, -fips, -state, -abb)
   
#create year column
get_last_four <- function(x){
   start = nchar(x) - 3
   stop = nchar(x)
   substr(x, start = start, stop = stop)
}
df.03$year <- get_last_four(df.03$key)
#reorder columns again!
df.04 <- df.03[, c(1:3, 6, 4, 5)]
#save file
file <- "./data_tidy/hunting_licenses_by_state_2000_and_2020.csv"
write.csv(df.04, file = file, row.names = F)
