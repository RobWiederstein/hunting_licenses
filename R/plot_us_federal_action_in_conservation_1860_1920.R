plot_library_of_congress_bib_by_date_and_type <- function(){
library(ggthemes)
file <- "./data_tidy/loc_bibliography_conservation.csv"
df <- read.csv(file =  file, header = T, stringsAsFactors = F)
df.01 <- dplyr::select(df, date, action, amt)
df.01$date <- as.Date(df.01$date, format = "%Y-%m-%d")
df.01$action[grep("Senate", df.01$action)] <- "Senate Document"
df.02 <-    df.01 %>%
      mutate(year = format(date, "%Y")) %>%
      group_by(year, action) %>%
      summarise(total = sum(amt)) %>%
      arrange(year, total)
df.02$year <- as.Date(df.02$year, format = "%Y")
df.02$action <- factor(df.02$action, levels = c("Senate Document", "House Report", "Debates and Proceedings", 
                                            "Joint Resolution", "Law Passed", "Proclamation")
)
# set levels to: dput(dimnames(sort(table(df.01$action)))[[1]]) Ha!
#plot
p <- ggplot(df.02, aes(fill=action, y=total, x=year))
p <- p + geom_bar(position="stack", stat="identity")
p <- p + scale_x_date(name = "",
                      limits = as.Date(c("1860", "1921"), format = "%Y"),
                      breaks = as.Date(c("1860", "1880", "1900", "1920"), format = "%Y"),
                      #date_breaks = "10 years",
                      date_labels = "%Y"
)
# p <- p + scale_fill_manual(values = c("#f860bd", "#ff6088", "#ff7d4e", "#b473e2", "#ffa600", "#4582ec")
# )
p <- p + scale_fill_manual(values = c('#4daf4a','#984ea3','#ff7f00','#ffff33','#e41a1c','#377eb8'))
p <- p + scale_y_continuous(name = "")
p <- p + ggtitle("U.S. Government Action in  Conservation\n1860 - 1920")
p <- p + theme_gdocs()
filename <- "./figs/us_government_action_in_conservation_1860_1920.jpg"
ggsave(p, filename = filename, height = 5, width = 8, unit = "in")
p
}
