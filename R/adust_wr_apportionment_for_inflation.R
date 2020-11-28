#usfw wildlife conservation revenue by year
#https://www.fws.gov/wsfrprograms/Subpages/GrantPrograms/WR/WRApportionmentsHE-1939-2020.xlsx
#downloaded November 24, 2020
file <- "./data_pure/usfw/WRApportionmentsHE-1939-2020.csv"
df.wr <- data.table::fread(input = file, data.table = F, skip = 1, 
                           header = T, strip.white  = T)
#colnames
colnames(df.wr) <- tolower(colnames(df.wr))
colnames(df.wr)[2:ncol(df.wr)] <- gsub(" ", "", colnames(df.wr)[2:ncol(df.wr)])
 #set space
df.wr <- df.wr[1:grep("WYOMING", df.wr$state), 1:grep("fy2020", colnames(df.wr))]
#format columns
df.wr[, 2:ncol(df.wr)] <- apply(df.wr[, 2:ncol(df.wr)], 2, function(x)stringr::str_trim(x, "both"))
df.wr[, 2:ncol(df.wr)] <- apply(df.wr[, 2:ncol(df.wr)], 2, function(x)gsub("\\$|,", "", x))
df.wr[, 2:ncol(df.wr)] <- apply(df.wr[, 2:ncol(df.wr)], 2, function(x)as.numeric(x))      
#title case
df.wr[, 1] <- stringr::str_to_title(df.wr[, 1])
#omit territories and DC
omit.terr <- "American Samoa|Guam|N. Mariana Islands|U.s. Virgin Islands|Puerto Rico"
df.wr <- df.wr[-grep(omit.terr, df.wr$state), ]
#convert to long
df.wr <- tidyr::gather(df.wr, key = key, value = value, -state)
#add year column
df.wr$year <-gsub("fy", "", df.wr$key)
df.wr$key <- "wildlife_rest_real_dollars"
df.wr <- dplyr::select(df.wr, state, year, key, value)
df.wr$year <- as.integer(df.wr$year)
df.wr$wildlife_rest_2019_dollars <- priceR::afi(df.wr$value, 
                                                from_date = df.wr$year, 
                                                to_date = 2019, 
                                                country = "US"
                                                )

df.wr.01 <- dplyr::select(df.wr, state, year, wildlife_rest_2019_dollars)
df.wr.01 <- tidyr::gather(df.wr.01, key = key, value = value, -state, -year)
df.wr.02 <- rbind(df.wr[, 1:4], df.wr.01)

#actual dollars by year
df.wr.03 <-
        df.wr.02 %>%
        group_by(year, key) %>%
        summarize(total = sum(value))

p <- ggplot(df.wr.03, aes(year, total, group = key, color = key))
p <- p + geom_line()
p <- p + ggtitle("Total Annual Wildlife Restoration Funds \n Real vs Current Dollars\n1939-2020")
p <- p + scale_y_continuous(breaks = c(0, 250e6, 500e6, 750e6),
                            labels = c("0", "$250m", "$500m", "$750m"),
                            name = "")
p <- p + scale_x_continuous(name = "")
p
filename <- "./img/wildlife_restoration_funds_real_versus_current_dollars_1939_2020.jpg"
ggsave(filename = filename, height = 5, width = 7, dpi = 300, unit = "in")

#save table
df.wr.04 <- tidyr::spread(df.wr.03, key = key, value = total)
file <- "./tbls/usfw_total_annual_restoration_funds_1939_2020.csv"
write.csv(df.wr.04, file = file, row.names = F)

#totals by state over life of program--inflation adjusted
df.wr.05 <- dplyr::filter(df.wr.02, key == "wildlife_rest_2019_dollars")
df.wr.06 <-
      df.wr.05 %>%
      group_by(state) %>%
      summarize(total_wildlife_rest_funds_infl_adj = sum(value)) %>%
      arrange(-total_wildlife_rest_funds_infl_adj)
df.wr.06$rank <- 1:nrow(df.wr.06)
df.wr.06 <- dplyr::select(df.wr.06, rank, state, total_wildlife_rest_funds_infl_adj)
df.wr.06$total_wildlife_rest_funds_infl_adj <- round(df.wr.06$total_wildlife_rest_funds_infl_adj, 2)
file <- "./tbls/usfw_total_restoration_funds_by_state_inflation_adjusted.csv"
write.csv(df.wr.06, file = file, row.names = F)
head(df.wr.06)

#plot as facet grid by state over time
#so table needs to be almost the full table
#add state abbs for facet label
#add state abb
file <- "./data_pure/misc/state_fips_codes.csv"
df.states <- read.csv(file = file, header = T, colClasses = "character")
df.states$FIPS[which(nchar(df.states$FIPS) == 1)] <- paste("0", df.states$FIPS[which(nchar(df.states$FIPS) == 1)], sep = "")
colnames(df.states)[1] <- "state"
colnames(df.states)[2] <- "state.abb"
df.wr.02 <- merge(df.wr.02, df.states[, 1:2])
df.wr.02 <- dplyr::select(df.wr.02, state, state.abb, year, key, value)
#add additional column for factor variable
df.factor <- dplyr::filter(df.wr.02, year == 2020 & key == "wildlife_rest_2019_dollars")
df.factor <-
      df.factor %>%
      arrange(-value)
df.factor$rank <- 1:nrow(df.factor)
df.wr.02 <- merge(df.wr.02, df.factor[, c(1, 6)])
df.wr.02$state.abb <- factor(df.wr.02$state.abb, levels = dput(df.factor$state.abb))

#plot
p <- ggplot(df.wr.02, aes(year, value, group = key, colour = key))
p <- p + geom_line()
p <- p + scale_y_continuous(limits = c(0, 40e6),
                            breaks = c(0, 10e6, 20e6, 30e6, 40e6),
                            labels = c("$0.0", "$10m", "$20m", "$30m", "$40m"),
                            name = "")
p <- p + scale_x_continuous(limits = c(1940, 2020),
                            breaks = c(1950, 2000),
                            labels = c("1950", "2000"),
                            name = "")
#p <- p + scale_color_hue(labels = c("real", "constant"))
p <- p + scale_colour_manual(values = c("#4582EC", "#ffa600"),
                             labels = c("constant", "real"),
                             )
p <- p + labs(color = "Dollars")
p <- p + ggtitle("Annual Wildlife Restoration Funding 1939 - 2020\nReal vs. Constant Dollars\nbase year = 2019")
p <- p + theme_gdocs()
p <- p + theme(plot.title = element_text(hjust = 0.5))
p <- p + facet_wrap(facets = vars(state.abb))
p 
filename <- "./figs/annual_wildlife_restoration_funding_by_state_1939_2020.jpg"
ggsave(p, filename = filename, height = 10, width = 10, dpi = 300, units = "in")
#can you add to appendix in rmarkdown?
