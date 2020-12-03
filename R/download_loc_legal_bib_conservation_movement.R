# https://memory.loc.gov/ammem/amrvhtml/consarcl.html
# The Law Library of Congress is represented in this collection by a variety of materials documenting the 
# growth of Federal conservation policy between 1862 and 1920. The collection includes approximately 140 
# Federal statutes and Congressional resolutions on the full range of conservation-related topics that engaged
# Congress in this era, drawn from the Statutes at Large. The legislative history of eighteen of the most 
# important of these pieces of legislation is also more fully represented through the inclusion of all 
# relevant pages of procedure and debate from the Congressional Globe or Congressional Record and all 
# official Senate and House documents (Senate Reports, Senate Documents, Senate Executive Documents, 
# Senate Miscellaneous Documents, and House Reports) associated with the passage of each Act or Resolution 
# in the Congressional session in which it was enacted. The collection also includes approximately 360 
# Presidential proclamations on conservation-related topics from 1889 to 1920, also from the Statutes at Large;
# these provide an especially comprehensive record of the growth of the national forest and national monument 
# systems, and include numerous maps.

# Readers should also note that transcripts of Congressional hearings on the bills permitting the 
# damming of Hetch Hetch Valley (finally enacted as the Raker Act, 38 Stat. 242) and creating the National 
# Park Service (the National Park Service Act, 39 Stat. 535), as well as a 1909 House Report on what became 
# the Raker Act, are included in the portion of this collection drawn from the General Collection and Rare 
# Book and Special Collections Division of the Library of Congress.

library(rvest)
#build list of pages to scrape
lc_urls <- paste("https://memory.loc.gov/cgi-bin/query/D?consrvbib:",
                 1:555, 
                 ":./temp/~ammem_I8sX::",
                 sep = ""
)
#scrape the pages               
build_list <- function(lc_urls){
      my.l <- list()
      for(i in 1:length(lc_urls)){
            scrape_bib <- function(x){
                  list(
                        read_html(x) %>% 
                        html_nodes("br+ b , p:nth-child(12) , p:nth-child(13) , p:nth-child(14)") %>% 
                        html_text()
                  )
            }
            my.l[i] <- scrape_bib(lc_urls[i])
      }
      my.l
}
#apply the function over the list of urls
a <- sapply(lc_urls, build_list)

#convert to dataframe
df <- data.frame(do.call(rbind, a), stringsAsFactors = F)

#write to file
file <- "./data_pure/loc/library_of_congress_legal_directory_american_conservation_movement.csv"
write.csv(df, file = file, row.names = F)