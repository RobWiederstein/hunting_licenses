input <- "./data_tidy/hunting_licenses_by_state_1960_and_2020.csv"
colClasses <- c(rep("character", 3), "integer", "character", "numeric")
df.00 <- data.table::fread(input = input, data.table = F, colClasses = colClasses)

#start table
dt <- data.frame(fips = unique(df.00$fips), abb = unique(df.00$abb))

#hunters per 1000 in 2020
df.pchl <- 
      df.00 %>% 
      dplyr::filter(key == "per_capita_hunting_license" & year == 2020) %>% 
      arrange(abb) %>%
      select(abb, value) %>%
      group_by(abb) %>%
      summarize(hunters_per_1000_2020 = value *1000, .groups = "keep")
dt <- merge(dt, df.pchl)
dt$hunters_per_1000_2020 <-round(dt$hunters_per_1000_2020, 1)
#wr_apportionment dollars per hunter
df.wp <- 
      df.00 %>%
      dplyr::filter(key == "wr_apportionment_real_dollars" & year == 2020)
df.hl <-
      df.00 %>%
      dplyr::filter(key == "certified_paid_hunting_license_holders"  & year == 2020)
df.pcwp <- left_join(df.wp, df.hl)
#ugh

file <- "./data_tidy/appendix_state_fish_wildlife_metrics.csv"
write.csv(dt, file = file, row.names = F)


                                 