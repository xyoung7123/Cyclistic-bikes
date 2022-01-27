#Load packages
library(tidyverse)
library(geosphere)
library(lubridate)
library(dplyr)

#MERGE SEPERATE DATASETS INTO SINGLE DATAFRAME
path <- "C:\\Users\\HP\\Documents\\Data Analytics Projects\\Cyclistic\\Raw Data"
files <- list.files(path, full.names = TRUE, pattern = "*.csv")
raw_df <- map_df(files, ~read_csv(.))

#TRANSFOM DATA
#Add columns for ride duration (mins), ride distance (m), day of week, month
transformed_df <- raw_df %>%
  mutate(ride_duration = as.numeric(difftime(ended_at, started_at, unit="mins"))) %>%
  mutate(ride_distance = distHaversine(cbind(start_lng, start_lat), cbind(end_lng, end_lat))) %>%
  mutate(day_of_week = weekdays(started_at)) %>%
  mutate(ride_month = month(started_at, label = TRUE))

#CLEAN DATA
#Remove rides not starting or ending at a station
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
  group_by(member_casual, ride_month, day_of_week) %>%
  summarise(number_of_rides = n(), avg_ride_duration = mean(ride_duration), avg_ride_distance = mean(ride_distance))

View(result_df)
str(result_df)

result_df %>%
  ggplot(mapping = aes(x = ride_month, y = avg_ride_distance, color = member_casual)) +
    geom_vline()


#EXPORT SUMMARY DATA INTO CSV
write_csv(summary_df, "C:\\Users\\HP\\Documents\\Data Analytics Projects\\Cyclistic\\Analysis_Results.csv")





