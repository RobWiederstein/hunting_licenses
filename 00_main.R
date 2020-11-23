tidy_up_and_save_data <- function(){
      combine_data_sources <- function(){
      merge_hunters_license_data <- function(){
         get_hunters_license_1960_fw <- function(){
            file <- "./data_pure/usfw/tabula-2020-11-17-fw_hunting_licenses_1960.csv"
            df.00 <- read.csv(file = file, as.is = T, header = F)
            df.00$V2 <- gsub(",| |\\$", "", df.00$V2)
            df.00$V2 <- as.numeric(df.00$V2)
            df.00[, 3:8] <- NA
            #add var names
            my_col_names <- c(
               "state",
               "certified_paid_hunting_license_holders_1960",
               "resident_hunting_licenses_tags_permits_stamps_1960",
               "non-resident_hunting_licenses_tags_permits_stamps_1960",
               "total_hunting_licenses_tags_permits_stamps_1960",
               "gross_cost_to_resident_hunters_1960",
               "gross_cost_to_non-resident_hunters_1960",
               "total_gross_cost_to_hunters_1960"
            )
            colnames(df.00) <- my_col_names
            df.00
         }
         get_hunters_license_1970_fw <- function(){
            file <- "./data_pure/usfw/tabula-2020-11-18-fw_hunting_licenses_1970.csv"
            df.00 <- read.csv(file = file, as.is = T, header = F)
            df.00[, 2:ncol(df.00)] <- apply(df.00[, 2:ncol(df.00)], 2, function(x){gsub(",| |\\$", "", x)})
            df.00[, 2:ncol(df.00)] <- apply(df.00[, 2:ncol(df.00)], 2, function(x){as.numeric(x)})
            #add var names
            my_col_names <- c(
               "state",
               "certified_paid_hunting_license_holders_1970",
               "resident_hunting_licenses_tags_permits_stamps_1970",
               "non-resident_hunting_licenses_tags_permits_stamps_1970",
               "total_hunting_licenses_tags_permits_stamps_1970",
               "gross_cost_to_resident_hunters_1970",
               "gross_cost_to_non-resident_hunters_1970",
               "total_gross_cost_to_hunters_1970"
            )
            colnames(df.00) <- my_col_names
            df.00
         }
         get_hunters_license_1980_fw <- function(){
            file <- "./data_pure/usfw/tabula-2020-11-17-fw_hunting_licenses_1980.csv"
            df.00 <- read.csv(file = file, as.is = T, header = F)
            df.00[, 2:ncol(df.00)] <- apply(df.00[, 2:ncol(df.00)], 2, function(x){gsub(",| |\\$", "", x)})
            df.00[, 2:ncol(df.00)] <- apply(df.00[, 2:ncol(df.00)], 2, function(x){as.numeric(x)})
            #add var names
            my_col_names <- c(
               "state",
               "certified_paid_hunting_license_holders_1980",
               "resident_hunting_licenses_tags_permits_stamps_1980",
               "non-resident_hunting_licenses_tags_permits_stamps_1980",
               "total_hunting_licenses_tags_permits_stamps_1980",
               "gross_cost_to_resident_hunters_1980",
               "gross_cost_to_non-resident_hunters_1980",
               "total_gross_cost_to_hunters_1980"
            )
            colnames(df.00) <- my_col_names
            df.00
         }
         get_hunters_license_1990_fw <- function(){
            file <- "./data_pure/usfw/tabula-2020-11-18-fw_hunting_licenses_1990.csv"
            df.00 <- read.csv(file = file, as.is = T, header = F)
            df.00[, 2:ncol(df.00)] <- apply(df.00[, 2:ncol(df.00)], 2, function(x){gsub(",| |\\$", "", x)})
            df.00[, 2:ncol(df.00)] <- apply(df.00[, 2:ncol(df.00)], 2, function(x){as.numeric(x)})
            #add var names
            my_col_names <- c(
               "state",
               "certified_paid_hunting_license_holders_1990",
               "resident_hunting_licenses_tags_permits_stamps_1990",
               "non-resident_hunting_licenses_tags_permits_stamps_1990",
               "total_hunting_licenses_tags_permits_stamps_1990",
               "gross_cost_to_resident_hunters_1990",
               "gross_cost_to_non-resident_hunters_1990",
               "total_gross_cost_to_hunters_1990"
            )
            colnames(df.00) <- my_col_names
            df.00
         }
         get_hunters_license_2000_fw <- function(){
            file <- "./data_pure/usfw/tabula-2020-11-08_fw_hunting_licenses_2000.csv"
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
         get_hunters_license_2010_fw <- function(){
            file <- "./data_pure/usfw/tabula-2020-11-18-fw_hunting_licenses_2010.csv"
            df.00 <- read.csv(file = file, as.is = T, header = F)
            #there's only six columns/variables in this year
            df.00 <- df.00[-grep("AS|DC|GU|MP|PR|VI|Total:", df.00$V1), ] #omit territories
            df.00[, 2:ncol(df.00)] <- apply(df.00[, 2:ncol(df.00)], 2, function(x){gsub(",| |\\$", "", x)})
            df.00[, 2:ncol(df.00)] <- apply(df.00[, 2:ncol(df.00)], 2, function(x){as.numeric(x)})
            #add var names
            my_col_names <- c(
               "state",
               "certified_paid_hunting_license_holders_2010",
               "resident_hunting_licenses_tags_permits_stamps_2010",
               "non-resident_hunting_licenses_tags_permits_stamps_2010",
               "total_hunting_licenses_tags_permits_stamps_2010",
               #"gross_cost_to_resident_hunters_2010",
               #"gross_cost_to_non-resident_hunters_2010",
               "total_gross_cost_to_hunters_2010"
            )
            colnames(df.00) <- my_col_names
            #state names
            df.states <- data.frame(state = state.name, abb = state.abb, stringsAsFactors = F)
            colnames(df.00)[1] <- "abb"
            df.00 <- merge(df.00, df.states)
            df.00
         }
         get_hunters_license_2020_fw <- function(){
            file <- "./data_pure/usfw/tabula-2020-11-08_fw_hunting_licenses_2020.csv"
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
         
         #add additional years to list
         my_list_df <- list(get_hunters_license_1960_fw(),
                            get_hunters_license_1970_fw(),
                            get_hunters_license_1980_fw(),
                            get_hunters_license_1990_fw(),
                            get_hunters_license_2000_fw(),
                            get_hunters_license_2010_fw(),
                            get_hunters_license_2020_fw()
                            )
         #apply merge to a list of dataframes
         df.00 <- Reduce(function(x, y) merge(x, y, all=TRUE), my_list_df)
         
      }
      merge_census_pop_data <- function(){
         get_pop_1960_us_census <- function(){
            file <- "./data_pure/uscb/nhgis0004_ds91_1960_state.csv"
            df <- read.csv(file = file, header = T, colClasses = "character")
            colnames(df) <- tolower(colnames(df))
            df <- df[-grep("District", df$state), ]
            #from code sheet:  B5O001 == Total population
            my_cols <- c("state", "b5o001")
            df <- dplyr::select(df, my_cols)
            df$b5o001 <- as.numeric(df$b5o001)
            colnames(df) <- c("state", "pop_1960")
            df
         }
         get_pop_1970_us_census <- function(){
            file <- "./data_pure/uscb/nhgis0006_ts_nominal_state.csv"
            df <- read.csv(file = file, header = T, colClasses = "character")
            df.00 <- df[-grep("Puerto|District", df$STATE), ]
            #from code sheet:  AV0AA1970 == Total population
            my_cols <- c("STATE", "AV0AA1970")
            df.00 <- dplyr::select(df.00, my_cols)
            df.00$AV0AA1970 <- as.numeric(df.00$AV0AA1970)
            colnames(df.00) <- c("state", "pop_1970")
            df.00
         }
         get_pop_1980_us_census <- function(){
            file <- "./data_pure/uscb/nhgis0004_ds116_1980_state.csv"
            df <- read.csv(file = file, header = T, colClasses = "character")
            colnames(df) <- tolower(colnames(df))
            df <- df[-grep("District", df$state), ]
            #from code sheet: C6W001 == Total pop
            my_cols <- c("state", "c6w001")
            df <- dplyr::select(df, my_cols)
            df$c6w001 <- as.numeric(df$c6w001)
            colnames(df) <- c("state", "pop_1980")
            df
         }
         get_pop_1990_us_census <- function(){
            file <- "./data_pure/uscb/nhgis0006_ts_nominal_state.csv"
            df <- read.csv(file = file, header = T, colClasses = "character")
            df.00 <- df[-grep("Puerto|District", df$STATE), ]
            #from code sheet:  AV0AA1990 == Total population
            my_cols <- c("STATE", "AV0AA1990")
            df.00 <- dplyr::select(df.00, my_cols)
            df.00$AV0AA1990 <- as.numeric(df.00$AV0AA1990)
            colnames(df.00) <- c("state", "pop_1990")
            df.00
         }
         get_pop_2000_us_census <- function(){
            file <- "./data_pure/uscb/nhgis0004_ds146_2000_state.csv" 
            df <- read.csv(file = file, header = T, colClasses = "character")
            colnames(df) <- tolower(colnames(df))
            df <- df[-grep("District", df$state), ]
            #from code sheet: FL5001 == Total pop
            my_cols <- c("state", "fl5001")
            df <- dplyr::select(df, my_cols)
            df$fl5001 <- as.numeric(df$fl5001)
            colnames(df) <- c("state", "pop_2000")
            df
         }
         get_pop_2010_us_census <- function(){
            file <- "./data_pure/uscb/nhgis0006_ts_nominal_state.csv"
            df <- read.csv(file = file, header = T, colClasses = "character")
            df.00 <- df[-grep("Puerto|District", df$STATE), ]
            #from code sheet:  AV0AA2010 == Total population
            my_cols <- c("STATE", "AV0AA2010")
            df.00 <- dplyr::select(df.00, my_cols)
            df.00$AV0AA2010 <- as.numeric(df.00$AV0AA2010)
            colnames(df.00) <- c("state", "pop_2010")
            df.00
         }
         get_pop_2020_us_census <- function(){
            file <-  "./data_pure/uscb/NationalProjections_ProjectedTotalPopulation_2020-2040_Updated12-2018.csv"
            df <- read.csv(file = file, header = T, colClasses = "character")
            colnames(df) <- tolower(colnames(df))
            df <- df[-grep("District|Puerto", df$state), ]
            df$y2020 <- gsub(",", "", df$y2020)
            df$y2020 <- as.numeric(df$y2020)
            colnames(df) <- c("state", "pop_2020")
            df
         }
         #add additional years to list
         my_list_df <- list(get_pop_1960_us_census(),
                            get_pop_1970_us_census(),
                            get_pop_1980_us_census(),
                            get_pop_1990_us_census(),
                            get_pop_2000_us_census(),
                            get_pop_2010_us_census(),
                            get_pop_2020_us_census()
         )
         #apply merge to a list of dataframes
         df.01 <- Reduce(function(x, y) merge(x, y, all=TRUE), my_list_df)
         df.01
      }
      create_per_capita_columns <- function(){
            hl_cols <- grep("certified", colnames(merge_hunters_license_data()))
            df <- merge_hunters_license_data()[, hl_cols]/
                  merge_census_pop_data()[, -1]
            old.names <- colnames(df)
            end_str <- nchar(old.names)
            start_str <- end_str - 3
            years <- substr(old.names, start_str, end_str)
            new.names <- paste("per_capita_hunting_license_", years, sep = "")
            colnames(df) <- new.names
            df$state <- merge_census_pop_data()[, 1]
            df
      }
      merge_with_state_fips_codes <- function(){
            file <- "./data_pure/fips/state_fips_codes.csv"
            df <- read.csv(file = file, header = T, colClasses = "character")
            colnames(df) <- c("state", "fips", "abb")
            omit.terr <- c("DC", "AS", "GU", "MP", "PR", "UM", "VI")
            df <- df[-which(df[, 3]  %in% omit.terr), ]
            df$fips[which(nchar(df$fips) == 1)] <- paste("0", df$fips[which(nchar(df$fips) == 1)], sep = "")
            df
      }
      #add additional years to list
      my_list_df <- list(merge_hunters_license_data(),
                         merge_census_pop_data(),
                         create_per_capita_columns(),
                         merge_with_state_fips_codes()
      )
      #apply merge to a list of dataframes
      df.01 <- Reduce(function(x, y) merge(x, y, all=TRUE), my_list_df)
      df.01
}
      df.00 <- combine_data_sources()
      #reorder columns
      df.01 <- dplyr::select(df.00, state, abb, fips, everything())
      #convert to long
      df.02 <- tidyr::gather(df.01, key = "key", value = value, -fips, -state, -abb)
      #create year column
      df.02$year <- stringr::str_sub(df.02$key, start = -4)
      #nix year from variable name
      df.02$key <- gsub("_[12][90][0-9][0-9]", "", df.02$key)
      #reorder columns again!
      df.03 <- dplyr::select(df.02, fips, state, abb, year, key, value)
      #omit incomplete data fields
      df.04 <- na.omit(df.03)
      #save file
      file <- "./data_tidy/hunting_licenses_by_state_1960_and_2020.csv"
      write.csv(df.04, file = file, row.names = F)
}
tidy_up_and_save_data()
