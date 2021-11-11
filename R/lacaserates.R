#Load the libraries used in the script
library(httr)
library(jsonlite)
library(dplyr)
library(zoo)
library(remotes)
library(lubridate)

#API query for lower tier authorities

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
  "areaType=ltla"
)

# Create the structure as a list or a list of lists:
query_structure <- list(
  date       = "date", 
  name       = "areaName", 
  code       = "areaCode", 
  daily_cases      = "newCasesByPublishDate"
)

latest_lower_authority <- get_paginated_data(query_filters, query_structure)

list(
  "Shape"                = dim(latest_lower_authority),
  "Data (first 3 items)" = latest_lower_authority[0:3, 0:-1]
) -> report

print(report)


#API query for upper tier authorities

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
  "areaType=utla"
)

# Create the structure as a list or a list of lists:
query_structure <- list(
  date       = "date", 
  name       = "areaName", 
  code       = "areaCode", 
  daily_cases      = "newCasesByPublishDate"
)

latest_upper_authority <- get_paginated_data(query_filters, query_structure)

list(
  "Shape"                = dim(latest_upper_authority),
  "Data (first 3 items)" = latest_upper_authority[0:3, 0:-1]
) -> report

print(report)

# Using regex to remove the local authorities from Scotland and Wales

#We can use regex to include all the data we need just for England by using [E]. 
#It asks if a word starts with the letter E and it will create another column for when it does. 
#In this case, it will get the cases for the authorities in England.

#Check for code names
unique(latest_lower_authority$code)
unique(latest_upper_authority$code)

#Selecting authority codes that begin with 'E'
latest_lower_authority$England <- grepl("[E]", latest_lower_authority$code)

#Upper authority
latest_upper_authority$England <- grepl("[E]", latest_upper_authority$code)


#Remove LAs in Wales and Scotland
latest_lower_authority <- subset(latest_lower_authority, latest_lower_authority$England == TRUE)
latest_upper_authority <- subset(latest_upper_authority, latest_upper_authority$England == TRUE)


#Combining the upper and lower authority dataframes and removing duplicates
#bind
cases_all_authorities <- rbind(latest_lower_authority, latest_upper_authority)

cases_all_authorities %>% distinct()

# Calculating the rate per 100k for the local authorities in England

#To do this, we will need to import the latest population figures for all of the authorities. 
#Data for this is supplied by the Office for National Statistics and we can import it.

##Importing the population figures

populationestimates <- read.csv("https://raw.githubusercontent.com/GWilloughby99/covid-automation/master/raw-data/pop_estimates_2020.csv", stringsAsFactors = FALSE)

populationestimates <- populationestimates %>%
  rename(
    name = Name
  )

#Merging cases and population data together
la_population <- merge(cases_all_authorities, populationestimates, by = "name")

#View column headers
colnames(la_population)


#Dropping unwanted columns

la_population <- la_population %>%
  select(name, date, code, daily_cases, All.ages)

#Changing all.ages heading
la_population <- la_population %>%
  rename(
    population = All.ages
  )

# Calculating the case rates per 100k

#To do this, we need to do divide cases by the population of the authority and then multiply by **100,000**.

#Calculating the rate per 100 thousand people
la_population$case_rate <- 100000 * (la_population$daily_cases/la_population$population)

# Figuring out the weekly sum 

#The next thing to calculate is weekly sums of case rate for each authority. 
#We can use lubridate and the today() function to select the week we want to look at. 
#By doing this, we can then calculate a summary of cases by day to get the weekly average.

#Previous week for cases
cases_previous_week <- la_population %>%
  select(name, date, code, daily_cases, population, case_rate) %>%
  filter(date >= today() - days(7))

#Calculate the sum
sum_cases_previous_week <- cases_previous_week %>%
  group_by(name) %>%
  summarise(previous_week_average = sum(case_rate))

start_of_penultimate_week = lubridate:: today() - days(14)
end_of_penultimate_week = lubridate:: today() - days(8)


#Previous week for cases
cases_penultimate_week <- subset(la_population, la_population$date >= start_of_penultimate_week & la_population$date <= end_of_penultimate_week)

#Create summary
sum_cases_penultimate_week <- cases_penultimate_week %>%
  group_by(name) %>%
  summarise(penultimate_week_average = sum(case_rate))


#Merge dataframes together
all_weeks <- merge(sum_cases_previous_week, sum_cases_penultimate_week, by = "name")

#Change in cases
all_weeks <- all_weeks %>%
  mutate(all_weeks, change_in_cases = all_weeks$previous_week_average - all_weeks$penultimate_week_average)

#Percentage change in cases
all_weeks <- all_weeks %>%
  mutate(percentage_change_in_cases = 100 * ((all_weeks$previous_week_average - (all_weeks$penultimate_week_average)) / (all_weeks$penultimate_week_average)))

#renaming columns
all_weeks <- all_weeks %>%
  rename(
    local_authority = name,
    previous_week = previous_week_average,
    penultimate_week_ = penultimate_week_average,
    pct_change = percentage_change_in_cases
  )

#Export
write.csv(all_weeks, file="raw-data/local_authority_case_rates.csv")

