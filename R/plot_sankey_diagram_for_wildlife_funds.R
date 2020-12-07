#pittman-robertson fund flow
path <-  "./data_tidy/2020-12-7-pittman-roberson-fund-flow.xlsx"

# Library
library(networkD3)
library(dplyr)

# A connection data frame is a list of flows with intensity for each flow
links <- readxl::read_xlsx(path = path)
#colnames(links)[4:5] <- gsub("Id", "ID", colnames(links[4:5]))

# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- data.frame(
      name=c(as.character(links$source), 
             as.character(links$target)) %>% unique()
)

# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
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
                   nodePadding = 100,
                   units = "m",
                   width = 800,
                   height = 500)
p
