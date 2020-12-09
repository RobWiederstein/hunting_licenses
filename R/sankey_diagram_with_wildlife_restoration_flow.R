 scrub_file <- function(){
      file <- "./data_pure/usfw/tabula-WRFinalApportionment2020.csv"
      df.00 <- read.csv(file = file, skip = 2, stringsAsFactors = F)
      #colnames
      colnames(df.00) <- c("state", "wildlife", "hunter", "enhanced", "total")
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
      #dplyr::filter(df.02, key == "wildlife") %>% arrange(value)
      
      #take df.02 and 
      #source column
      df.02$source <- "wildlife fund"
      df.02 <- dplyr::select(df.02, source, key, abb, value)
      #summarize
      df.03 <-    df.02 %>%
                  filter(key == "wildlife") %>%
                  arrange(value)
      df.04 <- rbind(head(df.03, 3), tail(df.03, 3))
      df.04 <- dplyr::select(df.04, key, abb, value)
      colnames(df.04)<- c("source", "target", "value")
      df.04
 }
 links <- scrub_file()
 links$value <- round((links$value / 10e5), 2)
 # From these flows we need to create a node data frame: it lists every entities involved in the flow
 nodes <- data.frame(
       name=c(as.character(links$source), 
              as.character(links$target)) %>% unique()
 )
 #With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
 links$IDsource <- match(links$source, nodes$name)-1 
 links$IDtarget <- match(links$target, nodes$name)-1
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
                    nodePadding = 10,
                    units = "m",
                    width = 800,
                    height = 500)
 p
 