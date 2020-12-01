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


df.01 <- dplyr::filter(df.00, key == "per_capita_hunting_license")

df.02 <- 
        df.01 %>%
        group_by(year) %>%
        summarize(per_capita_license = median(value))
df.02$per_capita_license <- round(df.02$per_capita_license * 1000, 0)

p <- ggplot(df.02, aes(year, per_capita_license))
p <- p + geom_line(color = "#4582EC")
p <- p + geom_point(color = "#4582EC", size = 3)
p <- p + scale_y_continuous(limits = c(0, 120),
                            name = "licenses/thousand")
p <- p + scale_x_continuous(name = "")
p <- p + ggtitle("Median State Issued Hunting Licenses \n 1960 - 2020")
p <- p + theme_gdocs()
p
