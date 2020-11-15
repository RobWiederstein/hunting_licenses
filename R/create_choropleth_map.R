library(choroplethr)
file <- "./data_tidy/hunting_licenses_by_state_2000_and_2020.csv"
df.00 <- read.csv(file = file, stringsAsFactors = F, colClasses = "character")
df.01 <- subset(df.00, key == "pct_change_per_capita_resident_hunting_license_2020")
df.01$value <- as.numeric(df.01$value)
df.02 <- df.01[, c(2, 6)]
df.02$state <- tolower(df.02$state)
colnames(df.02) <- c("region", "value")

state_choropleth(df.02, title = "US Pct. Change Hunting License 2000 to 2020",
                 legend = "", 
                 num_colors = 7,
                 zoom = NULL, 
                 reference_map = FALSE
                 )
