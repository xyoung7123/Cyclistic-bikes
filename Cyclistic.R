#Load packages
library(tidyverse)
library(geosphere)
library(lubridate)
library(dplyr)

#MERGE SEPERATE DATASETS INTO SINGLE DATAFRAME
path <- "C:\\Users\\HP\\Documents\\Data Analytics Projects\\Cyclistic\\Raw Data"
files <- list.files(path, full.names = TRUE, pattern = "*.csv")
raw_df <- map_df(files, ~read_csv(.))

#TRANSFOMING THE DATA
#add necessary columns (ride duration, month of the year, day of the week, hour of day)
transformed_df <- raw_df %>%
  mutate(ride_duration = as.numeric(difftime(ended_at, started_at, unit="mins"))) %>% 
  mutate(ride_distance = distHaversine(cbind(start_lng, start_lat), cbind(end_lng, end_lat))) %>%
  mutate(ride_year = year(started_at)) %>%
  mutate(ride_month = month(started_at, label = TRUE)) %>%
  mutate(day_of_week = weekdays(started_at)) %>%
  mutate(hour_of_day = hour(started_at))

#CLEANING THE DATA
#No duplicate entries found in pre-checks!
#Remove rides not starting from a station
#Remove rides not ending at a station
#Remove rides with duration <= 0
#Remove rides with distance <= 0
cleaned_df <- transformed_df %>%
  filter(!is.na(start_station_name)) %>% 
  filter(!is.na(end_station_name)) %>% 
  filter(ride_duration > 0) %>%
  filter(ride_distance > 0) 

#ANALYSE DATA
#Membership type, month, day of week aggregated by average and max duration and distance
result_df <- cleaned_df %>%
  group_by(member_casual, rideable_type, ride_year, ride_month, day_of_week, hour_of_day) %>%
  summarise(number_of_rides = n(), avg_ride_duration = mean(ride_duration), avg_ride_distance = mean(ride_distance))

#WRITE RESULT DATAFRAME TO CSV
#assign filename + set path to wd
#write file to path
result_file <- paste(getwd(), "/analysis_results.csv", sep = "") 
write_csv(result_df, result_file)