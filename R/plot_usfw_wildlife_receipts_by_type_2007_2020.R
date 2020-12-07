plot_receipts_and_deductions_usfw_wl_rest <- function(){
      plot_usfw_wildlife_restoration_receipts <- function(){
      #downloaded from https://www.fws.gov/wsfrprograms/Subpages/GrantPrograms/WR/WR-Receipts.pdf
      #numbers in thousands
      file <- "./data_pure/usfw/tabula-2020-12-05-wildlife_restoration_receipts.csv"
      df.00 <- read.csv(file = file, header = T, nrows = 4, stringsAsFactors = F)
      colnames(df.00) <- tolower(colnames(df.00))
      df.00$entry <- "receipt"
      df.01 <- gather(df.00, key = key, value = value, -entry, -gross.receipts)
      names(df.01) <- c("tax", "entry", "year", "value")
      df.01 <- dplyr::select(df.01, entry, year, tax, value)
      #year
      df.01$year <- gsub("fy\\.", "20", df.01$year)
      df.01$year <-as.Date(df.01$year, format = "%Y")
      #factor
      df.01$tax <- gsub(" ", "", df.01$tax)
      df.01$tax <- gsub("and", "-", df.01$tax)
      df.01$tax <- tolower(df.01$tax)
      df.01$tax <- factor(df.01$tax, levels = c("bows-arrows", "pistols-revolvers", "firearms", "ammunition"))
      #value
      df.01$value <- gsub(",", "", df.01$value)
      df.01$value <- as.numeric(df.01$value)
      df.01$value <- df.01$value * 1000
      names(df.01)[grep("tax", names(df.01))] <- "key"
      df.01
}
      plot_usfw_wildlife_restoration_deduct <- function(){
            file <- "./data_pure/usfw/tabula-2020-12-05-wildlife_restoration_receipts.csv"
            df.00 <- read.csv(file = file, header = T, skip = 5, stringsAsFactors = F)
            df.00$entry <- "deducts"
            colnames(df.00) <- tolower(colnames(df.00))
            df.01 <- gather(df.00, key = key, value = value, -deductions, -entry)
            names(df.01) <- c("deduction", "entry", "year", "value")
            df.01 <- dplyr::select(df.01, entry, year, deduction, value)
            #year
            df.01$year <- gsub("fy\\.", "20", df.01$year)
            df.01$year <- as.Date(df.01$year, format = "%Y")
            #value
            df.01$value <- gsub(",", "", df.01$value)
            df.01$value <- as.integer(df.01$value)
            df.01$value <- df.01$value * 1000
            #other
            names(df.01)[grep("deduction", names(df.01))] <- "key"
            df.01$key <- tolower(df.01$key)
            df.01$key <- gsub(" ", "-", df.01$key)
            
            
            df.01
      }
      df.00 <- rbind(plot_usfw_wildlife_restoration_receipts(), 
                     plot_usfw_wildlife_restoration_deduct()
      )
      df.00$entry <- factor(df.00$entry, levels = c("receipt", "deducts"))
      #plot
      p <- ggplot(data = df.00)
      p <- p + geom_col(aes(x = entry, y = value, fill = key))
      p <- p + facet_grid(. ~ year)
      p <- p + scale_x_discrete(name = "")
      p <- p + scale_y_continuous(name = "",
                                  breaks = c(0, 250e6, 500e6, 750e6),
                                  labels = c("$0", "$250m", "$500m", "$750m"))
      p <- p + ggtitle("Gross Receipts Wildlife Restoration Fund by Type\n2007 - 2020")
      p <- p + ggthemes::theme_stata(scheme = "s1color")
      p
      filename <- "./figs/gross_receipts_wildlife_restoration_fund_by_type_2007_2020.jpg"
      ggsave(p, filename = filename, height = 5, width = 8, unit = "in", dpi = 300)
      p
}