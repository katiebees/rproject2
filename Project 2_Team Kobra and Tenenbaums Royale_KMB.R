# # --- Steer Group: Data Analysis with R - Project 2 - Times Series analysis, forecasting and model optimization --- #
# --- Date: 12-April-2021
# --- Team: Kate Bridges, Sarah McMinimy, Derek Cheah, Armando Orta

# Part 1 - Feature Engineering and data visualization ---------------------------------

#load libraries 
library(readr)
library(zoo)
library(xts)
library(timetk)
library(tsibble)
library(dplyr)
library(forecast)
library(data.table)
library(lubridate)
library(ggplot2)
library(ggpubr)
library(dygraphs)
library(seasonal)
library(highcharter)
library(reshape2)

#set working directory
# setwd("C:/Users/KBridges/Desktop/TRANSFER/R Programming/project 2")
setwd("~/00 Steer R Training/Project 2/data")

#load the bikeshare dataset 
load('capitalbikeshare_11_17.RData')
str(bikes_20112017)
head(bikes_20112017)
names(bikes_20112017)
sapply(bikes_20112017, function (x) sum(is.na(x))) 

#load the weatherdataset
weather_201112017 <- read_csv("weather_20112017.csv")
str(weather_201112017)
head(weather_201112017)
sapply(weather_201112017, function (x) sum(is.na(x))) 

#extract date and time information using data.table and as.IDate() and as.ITime() 
bikes_20112017 <- bikes_20112017 %>% 
  mutate(Date = as.IDate(Start_date, format = "%d/%m/%Y"),
         STime = as.ITime(Start_date, format = "%H:%M:%S"),
         ETime = as.ITime(End_date, format = "%H:%M:%S"))

str(bikes_20112017)
head(bikes_20112017)

#extract date and time info using data.table for the weather dataset so they match
weather_201112017 <- weather_201112017 %>% 
  mutate(Date = as.IDate(Datetime, format = "%d/%m/%Y"),
         Time = as.ITime(Datetime, format = "%H:%M:%S"))

str(weather_201112017)
head(weather_201112017)

#create a subset for just 2017
bikes_2017 <- bikes_20112017 %>%
  dplyr::filter(between(Date, "2017-01-01", "2017-12-31")) 
min(bikes_2017$Date)
max(bikes_2017$Date)

#extract day of the month information using lubridate package
str(bikes_2017)
bikes_2017$DayOfWeek <- lubridate::wday(bikes_2017$Date, 
                                        label = TRUE, 
                                        abbr = FALSE, 
                                        week_start = 1) 

#extract month information using lubridate information
bikes_2017$MonthName <- lubridate::month(bikes_2017$Date, 
                                         label = TRUE, abbr = TRUE) 
bikes_2017$MonthInt <- lubridate::month(bikes_2017$Date, 
                                        label = FALSE) 

#extract day of the month using lubridate
bikes_2017$DayOfMonth <- lubridate::day(bikes_2017$Date) 

# Extract day of year using lubridate
bikes_2017$DayOfYear <- lubridate::yday(bikes_2017$Date)
tail(bikes_2017)

# Extract year information using lubridate
bikes_2017$Year <- lubridate::year(bikes_2017$Date)

# Extract hour information using lubridate
bikes_2017$Hour <- lubridate::hour(bikes_2017$STime)
str(bikes_2017)
head(bikes_2017)

# using base package extract the hour data for the 2015-2017 dataset
bikes_20112017$Hour <- data.table::hour(bikes_20112017$STime) 
str(bikes_20112017)
head(bikes_20112017)

#create new variable for trip duration in seconds/minutes using difftime()
bikes_2017 <- bikes_2017 %>% 
  mutate(Duration = difftime(bikes_2017$End_date,  bikes_2017$Start_date, units = "mins"))
str(bikes_2017)
head(bikes_2017)

bikes_20112017 <- bikes_20112017 %>%
  mutate(Duration = difftime(bikes_20112017$End_date, bikes_20112017$Start_date, units = "mins"))
str(bikes_20112017)
head(bikes_20112017)

#calculate the average trip duration per day for bike trips in 2017

stats_duration_2017 <- bikes_2017 %>% 
  group_by(Date) %>%
  summarise(N = n(), 
            Mean = mean(Duration, na.rm = TRUE), 
            SD = sd(Duration, na.rm = TRUE)) %>%
  as.data.frame()

head(stats_duration_2017)

# create TS and XTS of daily trips using all of the bike data for the last five years
trips_daily_ts <- bikes_20112017 %>% 
  group_by(Date) %>%
  summarise(N = n()) %>%
  tk_ts(select = N, start = c(2011, 1), frequency = 365.25)

str(trips_daily_ts)
tail(trips_daily_ts) 

trips_daily_xts <- bikes_20112017 %>% 
  group_by(Date) %>%
  summarise(N = n()) %>%
  tk_xts(select = N, start = c(2011, 1))

# create TS and XTS of average trip duration per day
duration_dailyts <- bikes_20112017 %>% 
  group_by(Date) %>% 
  summarise(N = n(), 
            trips_duration = as.numeric(mean(Duration, na.rm = TRUE)), 
            SD = sd(Duration, na.rm = TRUE)) %>%
  tk_ts(select = trips_duration, start = c(2011, 1), frequency = 365.25)

str(duration_dailyts)
head(duration_dailyts)

duration_daily_xts <- bikes_20112017 %>% 
  group_by(Date) %>%
  summarise(trips_duration = as.numeric(mean(Duration, na.rm = TRUE))) %>%
  tk_xts(select = trips_duration, start = c(2011, 1))

# Estimate the average trip duration by time of day 
duration_hourlyts <- bikes_20112017 %>% 
  group_by(Hour) %>% 
  summarise(N = n(), 
            trips_duration = mean(Duration, na.rm = TRUE), 
            SD = sd(Duration, na.rm = TRUE))

# create TS and XTS of monthly trips using all of the bike data for the last five years
trips_monthly_xts <- apply.monthly(trips_daily_xts, FUN = sum)
nmonths(trips_monthly_xts)

trips_monthly_ts <- trips_monthly_xts %>%
  tk_ts(start = c(2011, 1), frequency = 12)
trips_monthly_ts

#visualize the time series for daily trips using the forecast package
# 1) Total Daily Trips
trips_daily_ts %>% autoplot()

# 2) Duration of Daily Trips
duration_dailyts %>% autoplot()

# 3) Average Duration of Trips by Hour of Day
ggplot(duration_hourlyts, aes(Hour, trips_duration)) +
         geom_col() + 
  ggtitle("Average Trip Duration by Hour of Day") +
  xlab("Hour of Day") + 
  ylab("Average Trip Duration")

#zoom in on just the last 2 years
window(trips_daily_ts, start = c(2017, 1), end = c(2018, 31)) %>% 
  autoplot()
window(duration_dailyts, start = c(2017, 1), end = c(2018, 31)) %>% 
  autoplot()


# Join weather and trip datasets
daily_biketrips_all <- bikes_20112017 %>%
  group_by(Date) %>%
  summarise(alltrips = n(),
            trips_duration = mean(Duration, na.rm = T)) 

daily_biketrips_member <- bikes_20112017 %>%
  filter(Member == "Member") %>%
  group_by(Date) %>%
  summarise(membertrips = n(),
            membertrips_duration = mean(Duration, na.rm = T)) 

daily_biketrips_casual <- bikes_20112017 %>%
  filter(Member == "Casual") %>%
  group_by(Date) %>%
  summarise(casualtrips = n(),
            casualtrips_duration = mean(Duration, na.rm = T))

str(daily_biketrips_member)
str(daily_biketrips_casual)

weather_daily <- weather_201112017 %>% 
  mutate(Date = as.Date(Datetime, format = "%Y-%m-%d")) %>%
  group_by(Date) %>%
  summarise(avg_temp = mean(temp, na.rm = T),
            avg_wspd = mean(wspd, na.rm = T), 
            sum_precip = sum(precip_hrly, na.rm = T), 
            sum_snow = sum(snow_hrly, na.rm = T))

daily_biketrips_all
daily_biketrips_member
daily_biketrips_casual
weather_daily

join1 <- merge(daily_biketrips_all,  daily_biketrips_member, by.x = 'Date', by.y = 'Date')
join2 <- merge(join1, daily_biketrips_casual, by.x = 'Date', by.y = 'Date')
biketrip_weather <- merge(join2,  weather_daily, by.x = 'Date', by.y = 'Date')

str(biketrip_weather)

# Part 2 - Hypothesis testing ---------------------------------

#hypothesis 2: members usage will differ between weekdays versus weekends

# 1) extract day of the week 
biketrip_weather$DayOfWeek <- lubridate::wday(biketrip_weather$Date, 
                                        label = TRUE, 
                                        abbr = FALSE, 
                                        week_start = 1) 

# 2) create a subset with new variables for Weekday 
member_subset <- biketrip_weather %>%
  select (Date, membertrips, membertrips_duration, DayOfWeek) %>%
  mutate(Day_Group = if_else(DayOfWeek == "Saturday" | DayOfWeek == "Sunday", 'Weekend','Weekday'))

member_subset

# 3) calculate mean and sd for member trips by weekday

stats_members<- member_subset%>% 
  group_by(Day_Group) %>%
  summarise(N = n(), 
            Mean = mean(membertrips, na.rm = TRUE),
            SD = sd(membertrips, na.rm = TRUE)) %>%
  as.data.frame()

stats_members

summary(member_subset)
hist(member_subset)
plot(density(member_subset$membertrips))

# 4) use a Kolmogorov-Smirnov test because these are larger samples

set.seed(999)
sample_weekday <- round(rnorm(1823, mean = 6319.126, sd = 2748.938), 0)

set.seed(999)
sample_weekend <- round(rnorm(730, mean = 4562.685, sd = 1941.656), 0)

ks.test(member_subset[member_subset$Day_Group == 'Weekday', membertrips], 
        sample_weekday) 

ks.test(member_subset[member_subset$Day_Group == 'Weekend', membertrips], 
        sample_weekend) 

# 5) variance test
var.test( weekday ~ membertrips, data = member_subset) 

# 6) run independent (unpaired) t-test
t.test(membertrips ~ Day_Group, data=member_subset, alternative="two.sided")
plot(t.test(membertrips ~ Day_Group, data=member_subset, alternative="two.sided")) 

# 7) Plot simple boxplots with means to compare groups:
ggplot(member_subset, aes(x=Day_Group, y=membertrips, colour=Day_Group)) + 
  geom_boxplot(width=.5) + 
  stat_summary(fun=mean, geom="point", shape=23, 
               size=3, color="blue", fill="blue") +
  labs(title="Boxplots of member usage on weekdays and weekends with mean statistic", 
       x="Group", y="Member Trips") +
  theme_minimal()

# Part 3 - Optimize forecasting models ---------------------------------

# Join with employment data from .....

# Create training and testing datasets




auto.arima(trips_monthly_ts)




