create_sankey_plot_for_pittman_robertson_funds_to_states <- function(){# A connection data frame is a list of flows with intensity for each flow
plot_revenue_to_apportionment <- function(){
   path <-  "./data_tidy/2020-12-7-pittman-roberson-fund-flow.xlsx"
   df  <- readxl::read_xlsx(path = path)
   }
plot_apportionment_to_states_top_bottom_5 <- function(){
      file <- "./data_pure/usfw/tabula-WRFinalApportionment2020.csv"
      df.00 <- read.csv(file = file, skip = 2, stringsAsFactors = F)
      #colnames
      colnames(df.00) <- c("state", "wildlife", "hunter ed", "enhanced", "total")
      #init caps
      df.00$state <- stringr::str_to_title(df.00$state)
      #omit total
      df.00 <- df.00[, 1:4]
      #omit territories and D.C.
      dc.terr <- c("American Samoa", "District Of Columbia", "Guam", 
                   "N. Mariana Islands", "Puerto Rico", "Virgin Islands")
      df.00 <- df.00[-unlist(sapply(dc.terr, grep, df.00$state)), ]
      #convert to values
      convert.cols <- grep("wildlife|hunter|enhanced", colnames(df.00))
      df.00[, convert.cols] <- apply(df.00[, convert.cols], 2, function(x) gsub("\\$|,", "", x))
      df.00[, convert.cols] <- apply(df.00[, convert.cols], 2, function(x) as.numeric(x))
      #fix connecticut
      df.00$state[grep("Conn", df.00$state)] <- "Connecticut"
      #convert to long
      df.01 <- gather(df.00, key=key, value = value, -state)
      #merge for abbs
      df.state <- data.frame(state = state.name, abb = state.abb, stringsAsFactors = F)
      df.02 <- merge(df.01, df.state)
      #select & rename
      df.02 <- dplyr::select(df.02, key, abb, value)
      names(df.02) <- c("source", "target", "value")
      #convert to millions
      df.02$value <- round((df.02$value / 1000000), 2)
      top.5.states <- 
         df.02 %>% 
         dplyr::filter(source == "wildlife") %>% 
         arrange(-value) %>% 
         slice_head(n = 5) %>%
         select(target) %>%
         unlist()
      bot.5.states <- 
         df.02 %>% 
         dplyr::filter(source == "wildlife") %>% 
         arrange(-value) %>% 
         slice_tail(n = 5) %>%
         select(target) %>%
         unlist()
      
      dplyr::filter(df.02, target %in% (c(bot.5.states, top.5.states)))
      
}
df <- rbind(plot_revenue_to_apportionment(),
            plot_apportionment_to_states_top_bottom_5())
# load libraries for Sankey
library(networkD3)
library(dplyr)
#create links between the source and target.  Include a value
links <- df 
# From these flows we need to create a node data frame: it lists 
#every entities involved in the flow
nodes <- data.frame(
       name=c(as.character(links$source), 
              as.character(links$target)) %>% unique()
 )
# Add a 'group' column to the nodes data frame:
nodes$group <- "a"
nodes$group[c(8, 13:22)] <- "b"
nodes$group <- as.factor(nodes$group)
 #With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
 links$IDsource <- match(links$source, nodes$name)-1 
 links$IDtarget <- match(links$target, nodes$name)-1
 # Give a color for each group:
 my_color <- 'd3.scaleOrdinal() .domain(["a", "b"]) .range(["#b9c8fb", "#ffa600"])'
 
 # Make the Network
 p <- sankeyNetwork(Links = links, 
                    Nodes = nodes,
                    Source = "IDsource",
                    Target = "IDtarget",
                    Value = "value", 
                    NodeID = "name", 
                    sinksRight=FALSE,
                    fontSize = 12,
                    nodeWidth = 20,
                    nodePadding = 40,
                    units = "m",
                    width = 800,
                    height = 500, 
                    colourScale=my_color,
                    NodeGroup = "group")
 p
}
create_sankey_plot_for_pittman_robertson_funds_to_states()
