library(httr)
library(jsonlite)
library(dplyr)
library(remotes)
library(zoo)
library(tidyverse)

# Get the latest case numbers for UK nations

#' Extracts paginated data by requesting all of the pages
#' and combining the results.
#'
#' @param filters    API filters. See the API documentations for 
#'                   additional information.
#'                   
#' @param structure  Structure parameter. See the API documentations 
#'                   for additional information.
#'                   
#' @return list      Comprehensive list of dictionaries containing all 
#'                   the data for the given ``filter`` and ``structure`.`
get_paginated_data <- function (filters, structure) {
  
  endpoint     <- "https://api.coronavirus.data.gov.uk/v1/data"
  results      <- list()
  current_page <- 1
  
  repeat {
    
    httr::GET(
      url   = endpoint,
      query = list(
        filters   = paste(filters, collapse = ";"),
        structure = jsonlite::toJSON(structure, auto_unbox = TRUE),
        page      = current_page
      ),
      timeout(10)
    ) -> response
    
    # Handle errors:
    if ( response$status_code >= 400 ) {
      err_msg = httr::http_status(response)
      stop(err_msg)
    } else if ( response$status_code == 204 ) {
      break
    }
    
    # Convert response from binary to JSON:
    json_text <- content(response, "text")
    dt        <- jsonlite::fromJSON(json_text)
    results   <- rbind(results, dt$data)
    
    if ( is.null( dt$pagination$`next` ) ){
      break
    }
    
    current_page <- current_page + 1;
    
  }
  
  return(results)
  
}


# Create filters:
query_filters <- c(
  "areaType=nation"
)

# Create the structure as a list or a list of lists:
query_structure <- list(
  date       = "date", 
  name       = "areaName", 
  code       = "areaCode", 
  daily_cases      = "newCasesByPublishDate"
)

cases_nations <- get_paginated_data(query_filters, query_structure)

list(
  "Shape"                = dim(cases_nations),
  "Data (first 3 items)" = cases_nations[0:3, 0:-1]
) -> report

print(report)




#Selecting nations
england <- filter(cases_nations, name =="England")
wales <- filter(cases_nations, name =="Wales")
scotland <- filter(cases_nations, name =="Scotland")
northern_ireland <- filter(cases_nations, name =="Northern Ireland")


#Calculating rates for England and Wales and then merging them together
england_average <- england %>%
  dplyr::mutate(england_seven_day_average = zoo::rollmean(daily_cases, k = 7, align="left", fill = NA))

england_average <- england_average %>%
  select(date, daily_cases, england_seven_day_average)

england_average <- england_average %>%
  rename(
    england_daily_cases = daily_cases
  )
 
wales_average <- wales %>%
  dplyr::mutate(wales_seven_day_average = zoo::rollmean(daily_cases, k = 7, align="left", fill = NA))

wales_average <- wales_average %>%
  select(date, daily_cases, wales_seven_day_average)

wales_average <- wales_average %>%
  rename(
    wales_daily_cases = daily_cases
  )

englandandwales <- merge(england_average, wales_average, by ="date")

scotland_average <- scotland %>%
  dplyr::mutate(scotland_seven_day_average = zoo::rollmean(daily_cases, k = 7, align="left", fill = NA))

scotland_average <- scotland_average %>%
  select(date, daily_cases, scotland_seven_day_average)

scotland_average <- scotland_average %>%
  rename(
    scotland_new_cases = daily_cases
  )

northern_ireland_average <- northern_ireland %>%
  dplyr::mutate(northern_ireland_seven_day_average = zoo::rollmean(daily_cases, k = 7, align="left", fill = NA))

northern_ireland_average <- northern_ireland_average %>%
  select(date, daily_cases, northern_ireland_seven_day_average)

northern_ireland_average <- northern_ireland_average %>%
  rename(
    northern_ireland_new_cases = daily_cases
  )

scotlandandnorthernireland <- merge(scotland_average, northern_ireland_average, by ="date")

all_nations <- merge(englandandwales, scotlandandnorthernireland, by="date")

write.csv(all_nations, file="raw-data/cases_nations.csv")
