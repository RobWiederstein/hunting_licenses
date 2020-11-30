par(mar = c(4, 4, .1, .1))

#plot 1 -- total hunters

#still don't like --needs refining.  Add key to plot?  Is that possible? Sure!
# It may not be that hard!
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
}
plot_total_hunting_licenses()
#plot 2 -- participaton rate

#plot 3 -- population to hunter growth

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
