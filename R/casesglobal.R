library(httr)
library(jsonlite)
library(dplyr)
library(zoo)
library(remotes)
library(tidyverse)
library(readr)
library(lubridate)

urlfile="https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/jhu/new_cases.csv"

cases_global <- read.csv(url(urlfile))

colnames(cases_global)

south_africa <- cases_global %>%
  select(date, South.Africa)

south_africa <- south_africa %>%
  rename(
    cases = South.Africa
  )

south_africa <- south_africa %>%
  replace(is.na(.), 0)

#Removing these dates because of missing data or data that does not fit the trend

south_africa <- filter(south_africa, date!="2021-05-08" & date!="2021-05-25" & date!="2021-05-30" & date!="2021-09-29" & date!="2021-11-14" & date!="2021-11-18" & date!="2021-11-23")

south_africa <- south_africa %>%
  dplyr::mutate(seven_day_average = zoo::rollmean(cases, k = 7, align="left", fill = NA)) %>%
  arrange(desc(date)) 
  
  final_south_africa <- subset(south_africa, south_africa$date >= "2020-03-22" & south_africa$date <= today() - days(1))
  
  write.csv(final_south_africa, file="raw-data/cases_south_africa.csv")
  
  file="raw-data/cases_by_age.csv"
