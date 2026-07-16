library(tidyverse)
library(jsonlite)

file_names <- list.files(path = "html")%>%
  paste0("html/", .)
file_names
source("scrape_html.R")

beehive <- map_df(file_names, scrape_search_results)
beehive <- beehive %>% distinct()
saveRDS(beehive, "beehive.rds")
colnames(beehive)
minister_names <- beehive %>%
  separate_rows(ministers,sep=";")%>%
  distinct(ministers) %>%
  pull(ministers)

minister_names
source("get_wikipedia_infobox.R")
ministers <- map_df(minister_names, get_wikipedia_infobox)
sum(duplicated(ministers))
ministers<-distinct(ministers)
sum(duplicated(ministers))
saveRDS(ministers, "ministers.rds")

