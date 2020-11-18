get_hunters_license_1980_fw <- function()
      {file <- "./data_pure/tabula-2020-11-17-fw_hunting_licenses_1980.csv"
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
