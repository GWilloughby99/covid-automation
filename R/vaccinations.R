library(httr)
library(jsonlite)
library(dplyr)
library(zoo)
library(remotes)
library(tidyverse)

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
  "areaType=nation",
  "areaName=england"
)

# Create the structure as a list or a list of lists:
query_structure <- list(
  date       = "date", 
  name       = "areaName",
  new_daily_first_dose = "newPeopleVaccinatedFirstDoseByPublishDate",
  new_daily_second_dose = "newPeopleVaccinatedSecondDoseByPublishDate",
  new_daily_third_dose = "newPeopleVaccinatedThirdInjectionByPublishDate"
)

vaccinations <- get_paginated_data(query_filters, query_structure)

list(
  "Shape"                = dim(vaccinations),
  "Data (first 3 items)" = vaccinations[0:3, 0:-1]
) -> report

print(report)


vaccinations <- vaccinations %>%
  rename(
    first_dose = new_daily_first_dose,
    second_dose = new_daily_second_dose,
    third_dose = new_daily_third_dose
  )

write.csv(vaccinations, file="raw-data/vaccinations.csv")
