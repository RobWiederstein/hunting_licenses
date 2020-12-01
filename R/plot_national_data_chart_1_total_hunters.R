par(mar = c(4, 4, .1, .1))

#plot 1 -- total hunters
plot_total_hunting_licenses <- function(){
        #Add main dataframe
        file <- "./data_tidy/hunting_licenses_by_state_1960_and_2020.csv"
        colClasses <- c(rep("character", 3), "integer", "character", "numeric")
        df.00 <- data.table::fread(file = file, data.table = F, colClasses = colClasses)
        df.01 <- dplyr::filter(df.00, key == "certified_paid_hunting_license_holders")
        df.02 <-        df.01 %>%
                        group_by(year) %>%
                        summarize(total_licenses = sum(value))
        df.03 <- tidyr::gather(df.02, key = key, value = value, -year)
        df.03$key <- "tot_hunters_licenses"
        # Add FHWAR line plot
        file <-  "./data_pure/usfw/2020-11-20-fw_nat_survey_fhwar_1955_2016.csv"
        colClasses <- c("integer", "character", "integer", "character", "integer", "character", "numeric")
        df.fhwar <- read.csv(file = file, header = T, colClasses = colClasses)
        df.fhwar <- select(df.fhwar, year, total_all_hunters)
        df.fhwar <- tidyr::gather(df.fhwar, key = key, value = value, -year)
        df.fhwar$key[which(df.fhwar$year %in% 1955:1990)] <- "tot_hunters_pre_1991"
        df.fhwar$key[which(df.fhwar$year %in% 1990:2020)] <- "tot_hunters_post_1991"
        #rbind
        df.04 <- rbind(df.03, df.fhwar)
        df.04$key <- factor(df.04$key, levels = c("tot_hunters_licenses",
                                                     "tot_hunters_pre_1991",
                                                     "tot_hunters_post_1991")
                               )
        #plot total number of hunting licenses
        p <- ggplot(df.04, aes(year, value, group = key, colour = key))
        p <- p + geom_line()
        p <- p + geom_point()
        p <- p + scale_y_continuous(limits = c(0, 22000000),
                                    name = "",
                                    breaks = c(0, 50e5, 100e5, 150e5, 200e5),
                                    labels = c("0", "5m", "10m", "15m", "20m")
        )
        p <- p + scale_x_continuous(name = "")
        p <- p + scale_colour_manual(values = c("#4582ec", "#ffa600", "#ff5ca4"))
        p <- p + ggtitle("Total U.S. Hunters from License and Surveys \n1955 - 2020")
        p <- p + theme_gdocs()
        filename <- "./figs/total_us_hunters_from_license_and_survey_data_1955_2020.jpg"
        ggsave(p, filename = filename, height = 5, width = 8, dpi = 300, units = "in")
        p
}
plot_total_hunting_licenses()
#plot 2 -- participaton rate
plot_pct_hunters_from_FHWAR_and_hunters_licenses <- function(){
        calculate_pct_hunting_licenses <- function(){
        #Add main dataframe
        file <- "./data_tidy/hunting_licenses_by_state_1960_and_2020.csv"
        colClasses <- c(rep("character", 3), "integer", "character", "numeric")
        df.00 <- data.table::fread(file = file, data.table = F, colClasses = colClasses)
        #total up the number of hunting licenses
        df.01 <- dplyr::filter(df.00, key == "certified_paid_hunting_license_holders")
        df.02 <-        df.01 %>%
                group_by(year) %>%
                summarize(total_licenses = sum(value))
        #total up the pop among the states
        df.03 <- dplyr::filter(df.00, key == "pop")
        df.04 <- df.03 %>%
                group_by(year) %>%
                summarize(tot_pop = sum(value))
        #figure out a per capita on the totals
        df.04$pct_hunting_licenses <- round((df.02$total_licenses / df.04$tot_pop) * 100, 2)
        df.04 <- df.04[, c(1,3)]
        df.04 <- tidyr::gather(df.04, key = key, value = value, -year)
        df.04
}
        calculate_pct_hunters_from_survey <- function(){
                # Add FHWAR line plot
                file <-  "./data_pure/usfw/2020-11-20-fw_nat_survey_fhwar_1955_2016.csv"
                colClasses <- c("integer", "character", "integer", "character", "integer", "character", "numeric")
                df.fhwar <- read.csv(file = file, header = T, colClasses = colClasses)
                df.fhwar <- select(df.fhwar, year, "participation_rate_calculated")
                df.fhwar$participation_rate_calculated <- round(df.fhwar$participation_rate_calculated *100, 2)
                df.fhwar <- tidyr::gather(df.fhwar, key = key, value = value, -year)
                df.fhwar$key[which(df.fhwar$year %in% 1955:1990)] <- "part_rate_pre_1991"
                df.fhwar$key[which(df.fhwar$year %in% 1990:2020)] <- "part_rate_post_1991"
                df.fhwar
        }
        df <- rbind(calculate_pct_hunters_from_survey(), calculate_pct_hunting_licenses())
        df$key <- factor(df$key, levels = c("pct_hunting_licenses", "part_rate_pre_1991", "part_rate_post_1991"))
        
        p <- ggplot(df, aes(year, value, group = key, colour = key))
        p <- p + geom_line()#4582EC
        p <- p + geom_point(size = 3)
        p <- p + scale_y_continuous(limits = c(0, 12),
                                    name = "",
                                    breaks = c(0, 3.0, 6.0, 9.0, 12),
                                    labels = c("0.0%", "3.0%", "6.0%", "9.0%", "12.0%")
        )
        p <- p + scale_x_continuous(name = "")
        p <- p + scale_colour_manual(values = c("#4582ec", "#ffa600", "#ff5ca4"))
        p <- p + ggtitle("Hunting Licenses vs. Survey Participation Rate")
        p <- p + theme_gdocs()
        filename <- "./figs/hunting_licenses_vs_survey_part_rate_1955_2020.jpg"
        ggsave(p, filename = filename, height = 5, width = 8, dpi = 300, units = "in")
        p
}
#plot 3 -- population to hunter growth
Plot_change_in_hunters_license_to_population <- function(){
        file <- "./data_pure/total_annual_hl_and_pop_1960-2020.csv"
        colClasses <- c("integer", "integer", "integer", "numeric", "numeric")
        df.pct <- read.csv(file = file, header = T, colClasses = colClasses)
        df.pct <- tidyr::gather(df.pct, key = key, value = value, -year)
        df.pct <- dplyr::filter(df.pct, key == "pct_increase_population" | key == "pct_increase_hunting_lic")
        p <- ggplot(df.pct, aes(year, value, group = key, color = key))
        p <- p + geom_line()
        p <- p + geom_point(size = 3)
        p <- p + scale_color_manual(values=c("#4582EC", "#ffa600"))
        p <- p + theme_gdocs()
        p <- p + scale_x_continuous(name = "")
        p <- p + ggtitle("Pct. Increase in Population and Hunting Licenses \nbase year = 1960")
        filename <- "./figs/pct_increase_in_population_and_hunting_licenses_1960_to_2020.jpg"
        ggsave(p, filename = filename, height = 5, width = 8, dpi = 300, unit = "in")
        p
}

#plot 4 -- 
        #first series is total hunting revenue per hunter (or total population?) in 2020 dollars
        #total hunters / total revenue over time
        #second series is wr apportionment by hunter (or total population?) in 2020 dollars over time
        #total hunters / wr apportionment over time

#need to convert current dollars to constant dollars--this may not be that hard

#Add main dataframe
file <- "./data_tidy/hunting_licenses_by_state_1960_and_2020.csv"
colClasses <- c(rep("character", 3), "integer", "character", "numeric")
df.00 <- data.table::fread(file = file, data.table = F, colClasses = colClasses)
#total cost to hunters
df.01 <- dplyr::filter(df.00, key == "total_gross_cost_to_hunters")
df.02 <-df.01 %>%
        group_by(year) %>%
        summarize(total_cost = sum(value))
#total hunters by year
df.03 <- dplyr::filter(df.00, key == "certified_paid_hunting_license_holders")
df.04 <- df.03 %>%
        group_by(year) %>%
        summarize(total_hunters = sum(value))
#merge
df.05 <- merge(df.02, df.04)
#total cost per hunter
df.05$cost_per_hunter <- df.05$total_cost / df.05$total_hunters
df.05 <- dplyr::select(df.05, year, cost_per_hunter)
df.06 <- tidyr::gather(df.05, key = key, value = value, -year)
#total cost per person
df.07 <- dplyr::filter(df.00, key == "pop")
df.08 <- df.07 %>%
        group_by(year) %>%
        summarize(total_pop = sum(value))
df.09 <- merge(df.02, df.08)
df.09$cost_per_person <- df.09$total_cost / df.09$total_pop
df.09 <- dplyr::select(df.09, year, cost_per_person)
df.09 <- tidyr::gather(df.09, key = key, value = value, -year)
#join cost_per_hunter to cost_per_person
df.10 <- rbind(df.06, df.09)

#build inflation adjusted series for comparison
df.infl <- data.frame(total_cost_1970 = 101608879,
                      const_dol_2020 = 681900329.51,
                      total_hunt_1970 = 15658318,
                      total_hunt_2020 = 15151724,
                      total_pop_1970 = 202455416,
                      total_pop_2020 = 331794992,
                      infl_adj_cost_per_hunter_1970 = 101608879 / 15658318,
                      infl_adj_cost_per_hunter_2020 = 681900329.51 / 15151724,
                      infl_adj_cost_per_person_1970 = 101608879 / 202455416,
                      infl_adj_cost_per_person_2020 = 681900329.51 / 331794992
)
df.infl <- tidyr::gather(df.infl, key = key, value = value)
df.infl$year <- stringr::str_sub(df.infl$key, -4)
df.infl$year <- as.integer(df.infl$year)       
df.infl$key  <-gsub("_1970|_2020", "", df.infl$key)
df.infl <- df.infl[grep("infl_adj", df.infl$key), ]
#plot
p <- ggplot(df.10, aes(year, value, group = key, colour = key))
p <- p + geom_line()
p <- p + geom_point(size = 3)
p <- p + scale_y_continuous(name = "",
                        limits = c(0, 60),
                        breaks = c(0, 15, 30, 45, 60)
                        )
p <- p + scale_color_manual(values=c("#4582EC", "#ffa600", "#ff5ca4", "#c86edb"))
p <- p + geom_line(data = df.infl, aes(year, value, group = key, colour = key))
p <- p + geom_point(data = df.infl, aes(year, value, group = key, colour = key), size = 3)
p <- p + theme_gdocs()
p <- p + scale_x_continuous(name = "", 
                            limits = c(1960, 2020))
p <- p + ggtitle("Gross Cost Per Hunter and Per Capita \n1970-2020")
filename <- "./figs/gross_cost_per_hunter_per_capita_1970_2020.jpg"
ggsave(p, filename = filename, height = 5, width = 8, dpi = 300, units = "in")
p






