#plot types of federal action by year and type
library(ggthemes)
file <- "./data_tidy/loc_bibliography_conservation.csv"
df <- read.csv(file =  file, header = T, stringsAsFactors = F)
df.01 <- dplyr::select(df, date, type, amt)
df.01$date <- as.Date(df.01$date, format = "%Y-%m-%d")
df.01$type[grep("Senate", df.01$type)] <- "Senate Document"
df.02 <-    df.01 %>%
            mutate(year = format(date, "%Y")) %>%
            group_by(year, type) %>%
            summarise(total = sum(amt))
df.02$year <- as.Date(df.02$year, format = "%Y")
#plot
p <- ggplot(df.02, aes(fill=type, y=total, x=year))
p <- p + geom_bar(position="stack", stat="identity")
p <- p + scale_x_date(name = "",
                            limits = as.Date(c("1860", "1921"), format = "%Y"),
                            breaks = as.Date(c("1860", "1880", "1900", "1920"), format = "%Y"),
                            #date_breaks = "10 years",
                            date_labels = "%Y"
                      )
p <- p + scale_fill_manual(values = c("#4582ec", "#b473e2", "#f860bd",
                                      "#ff6088", "#ff7d4e", "#ffa600"
                                      )
                             )
p <- p + scale_y_continuous(name = "")
p <- p + ggtitle("U.S. Government Action in  Conservation\n1860 - 1920")
p <- p + theme_gdocs()
p
filename <- "./figs/us_government_action_in_conservation_1860_1920.jpg"
ggsave(p, filename = filename, height = 5, width = 8, unit = "in")
