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
  "areaType=overview"
)

# Create the structure as a list or a list of lists:
query_structure <- list(
  date       = "date",
  name       = "areaName",
  code = "areaCode",
  daily_cases      = "newCasesByPublishDate"
)

cases_uk <- get_paginated_data(query_filters, query_structure)

list(
  "Shape"                = dim(cases_uk),
  "Data (first 3 items)" = cases_uk[0:3, 0:-1]
) -> report

print(report)

cases_uk_average <- cases_uk %>%
  dplyr::mutate(cases_seven_day_average = zoo::rollmean(daily_cases, k = 7, align="left", fill = NA))

write.csv(cases_uk_average, file = "raw-data/cases-uk.csv")

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
  "areaType=nhsTrust"
)

# Create the structure as a list or a list of lists:
query_structure <- list(
  date       = "date", 
  name       = "areaName",
  hospital_cases = "hospitalCases",
  hospital_admissions = "newAdmissions"
)

hospitalisations <- get_paginated_data(query_filters, query_structure)

list(
  "Shape"                = dim(hospitalisations),
  "Data (first 3 items)" = hospitalisations[0:3, 0:-1]
) -> report

print(report)

#Replacing NA values with 0
hospitalisations <- hospitalisations %>%
  mutate_all(~replace(., is.na(.), 0))

#arranging the data
top_10_hospital_cases <- hospitalisations %>%
  arrange(desc(hospital_cases)) %>%
  arrange(desc(date)) %>%
  head(10) %>%
  group_by(name)

top_10_hospital_cases

write.csv(top_10_hospital_cases, file="raw-data/top_10_hospital_cases.csv")

#Calculating the rolling weekly average
avg_hospitalisations <- hospitalisations %>%
  dplyr::arrange(desc(date)) %>%
  dplyr::group_by(name) %>%
  dplyr::mutate(hospitalisations_seven_day_average = zoo::rollmean(hospital_admissions, k = 7, align="left", fill = NA)) %>%
  dplyr::ungroup()

write.csv(avg_hospitalisations, file="raw-data/average_hospital_admissions.csv")

# Cases by age

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
  age_group      = "newCasesBySpecimenDateAgeDemographics"
)

cases_age_group <- get_paginated_data(query_filters, query_structure)

list(
  "Shape"                = dim(cases_age_group),
  "Data (first 3 items)" = cases_age_group[0:3, 0:-1]
) -> report

print(report)

#Unnesting the age groups

age_groups <- cases_age_group %>%
  unnest(age_group)

#Dropping the pre-determined age group
age_groups <- subset(age_groups, age!="00_59" & age!="60+")

#Remove unassigned
age_groups <- subset (age_groups, age!="unassigned")

#Creating the 0-19 age range
ages_0_19 <- filter(age_groups, age =="00_04" | age =="05_09" | age == "10_14" | age == "15_19")

sum_ages_0_19 <- ages_0_19 %>%
  group_by(date) %>%
  summarise(sum_0_19_cases = sum(cases))

#Creating the 20-44 age range
ages_20_44 <- filter(age_groups, age =="20-24" | age == "25_29" | age == "30_34" | age == "40_44")

#Creating a summary for the age range

sum_ages_20_44 <- ages_20_44 %>%
  group_by(date) %>%
  arrange(desc(date)) %>%
  summarise(sum_20_24_cases = sum(cases))

#Creating the 45-69 age range
ages_45_69 <- filter(age_groups, age =="45_49" | age == "50_54" | age == "55_59" | age == "60_64")

#Creating a summary for the age range

sum_ages_45_69 <- ages_45_69 %>%
  group_by(date) %>%
  arrange(desc(date)) %>%
  summarise(sum_45_69_cases = sum(cases))


#Creating the 70-90+ age range
ages_70_90 <- filter(age_groups, age =="70_74" | age == "75_79" | age == "80_84" | age == "85_89" | age == "90+")

#Creating a summary for the age range

sum_ages_70_90 <- ages_70_90 %>%
  group_by(date) %>%
  arrange(desc(date)) %>%
  summarise(sum_70_90_cases = sum(cases))

#First merge
sum_ages_0_44 <- merge(sum_ages_0_19, sum_ages_20_44, by = "date")

#Second merge
sum_ages_45_90 <- merge(sum_ages_45_69, sum_ages_70_90, by = "date")

#Final merge
sum_all_ages <- merge(sum_ages_0_44, sum_ages_45_90, by ="date")

write.csv(sum_all_ages, file="raw-data/cases_by_age.csv")


install.packages("usethis")

usethis::create_package(path = "covid-automation")
