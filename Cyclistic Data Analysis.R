library(tidyverse) # wrangle data
library(lubridate) # wrangle date attributes
library(janitor) # for examining and cleaning dirty data
library(skimr) # get summary data
library(dplyr) # data frame manipulation
library(ggplot2) # for visualizations

# upload the data for the last 12 months
trip2207 <- read.csv("202207-divvy-tripdata.csv")
trip2206 <- read.csv("202206-divvy-tripdata.csv")
trip2205 <- read.csv("202205-divvy-tripdata.csv")
trip2204 <- read.csv("202204-divvy-tripdata.csv")
trip2203 <- read.csv("202203-divvy-tripdata.csv")
trip2202 <- read.csv("202202-divvy-tripdata.csv")
trip2201 <- read.csv("202201-divvy-tripdata.csv")
trip2112 <- read.csv("202112-divvy-tripdata.csv")
trip2111 <- read.csv("202111-divvy-tripdata.csv")
trip2110 <- read.csv("202110-divvy-tripdata.csv")
trip2109 <- read.csv("202109-divvy-tripdata.csv")
trip2108 <- read.csv("202108-divvy-tripdata.csv")

# inspect column names
colnames(trip2207)
colnames(trip2206)
colnames(trip2205)
colnames(trip2204)
colnames(trip2203)
colnames(trip2202)
colnames(trip2201)
colnames(trip2112)
colnames(trip2111)
colnames(trip2110) 
colnames(trip2109)
colnames(trip2108)
compare_df_cols(trip2108, trip2109, trip2110, trip2111, trip2112, trip2201, trip2202, trip2203, trip2204, trip2205, trip2206, trip2207, return = "mismatch")

# combine all the of the data
alltripdata <- rbind(trip2108, trip2109, trip2110, trip2111, trip2112, trip2201, trip2202,trip2203, trip2204, trip2205, trip2206, trip2207)

# add columns that list the date, month, day, and year for each ride

alltripdata$date <- as.Date(alltripdata$started_at) # default date format is yyyy-mm-dd
alltripdata$month <- format(as.Date(alltripdata$date), "%m")
alltripdata$day <- format(as.Date(alltripdata$date), "%d")
alltripdata$year <- format(as.Date(alltripdata$date), "%Y")
alltripdata$day_of_week <- format(as.Date(alltripdata$date), "%A")

# add ride length/ride (time) calculated to alltripdata as difftime

alltripdata$ride_length <- difftime(alltripdata$ended_at, alltripdata$started_at)

# check class of variables
sapply(alltripdata, class)


# ride_length needs to be numeric in order to calculate 
alltripdata$ride_length <- as.numeric(as.character(alltripdata$ride_length)) 
is.numeric(alltripdata$ride_length)

# A lot of ride lengths that are negative - Innaccurate data. Rides can't be negative
alltripdata[order(alltripdata$ride_length),]
alltripdata[order(alltripdata$start_station_name),]
alltripdata <- alltripdata[!(alltripdata$ride_length<=0),]

colnames(alltripdata)
str(alltripdata)
head(alltripdata)
summary(alltripdata)
skim(alltripdata)

summary(alltripdata$ride_length)

# exported data to csv file - now you can put it on tableau to make visualizations
write.csv(alltripdata, "data.csv")

# order days of week within the dataset so they appear in order
alltripdata$day_of_week <- ordered(alltripdata$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# group by usertype and day of week
alltripdata %>%
group_by(member_casual, day_of_week) %>%
summarize(number_of_rides = n())

# Visualization - number of rides / day of week

alltripdata %>%
  group_by(member_casual, day_of_week) %>%
  summarize(number_of_rides = n(), average_duration = mean(ride_length)) %>%
  arrange(member_casual, day_of_week) %>%
  ggplot(aes(x = day_of_week, y = number_of_rides, fill = member_casual)) + geom_col(position = "stack")

# Visualization - average duration / day of week
alltripdata %>%
  group_by(member_casual, day_of_week) %>%
  summarize(number_of_rides = n(), average_duration = mean(ride_length)) %>%
  arrange(member_casual, day_of_week) %>%
  ggplot(aes(x = day_of_week, y = average_duration, fill = member_casual)) + geom_col(position = "dodge")

# Visualization - average duration / day of week

alltripdata %>%
  group_by(member_casual, day_of_week) %>%
  summarize(number_of_rides = n(), average_ride_length = mean(ride_length)) %>%
  arrange(member_casual, day_of_week) %>%
  ggplot(aes(x = day_of_week, y = average_ride_length, fill = member_casual)) + geom_col(position= "stack")

# Visualization - number of rides / month

alltripdata %>%
  group_by(member_casual, month) %>%
  summarize(number_of_rides = n()) %>%
  arrange(member_casual, month) %>%
  ggplot(aes(x = month, y = number_of_rides, fill = member_casual)) + geom_col(position = "stack")
  
alltripdata %>%
  group_by(member_casual, month) %>%
  summarize(number_of_rides = n()) %>%
  arrange(member_casual, month) %>%
  ggplot(aes(x = month, y = number_of_rides, fill = member_casual)) + geom_col(position = "dodge")



