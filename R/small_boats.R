# Turn off science
options(scipen = 999)
# Library load
#install.packages("xml2")
library(pacman)
p_load("httr", "jsonlite", "lubridate", "tidyverse", 
       "zoo", "DatawRappr", "dplyr", "googlesheets4","rvest", "xml2", "tidyr")

# Read in URL
boats <- read_html("https://www.gov.uk/government/statistical-data-sets/migrants-detected-crossing-the-english-channel-in-small-boats")

# CSS selector
tags <- html_nodes(boats, "h4")

# Bind rows to create a data frame
working_crossings <- bind_rows(lapply(xml_attrs(tags), function(x) data.frame(as.list(x), stringsAsFactors = FALSE))) %>%
  slice(1) %>%
  rename(method = "id")

# Splitting columns and adding date
final_crossings <- separate(working_crossings, method, into = c("method", "crossings"), sep = 43)

final_crossings$method <- gsub("number-of-migrants-detected-in-small-boats-", "migrants detected", final_crossings$method)

final_crossings$date[nrow(final_crossings)] <- format(Sys.time(), "%Y-%m-%d")

crossings <- final_crossings %>%
  rename(last_updated = "date")

sheet_write(crossings, "https://docs.google.com/spreadsheets/d/1UlzndCSZvoiJHLzCgrD-jOpVefBlOD7f57DovZCEDws/edit#gid=1203388094")

############################ Unused ############################


url <- "https://www.gov.uk/government/statistical-data-sets/migrants-detected-crossing-the-english-channel-in-small-boats"

raw <- read_html(url) %>%
  html_elements("ul") %>%
  html_elements("h4") %>%
  as.data.frame()

