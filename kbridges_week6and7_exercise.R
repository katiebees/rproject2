#import data and check structure of data
library(readr)
library(dplyr)
library(ggplot2)
library(lsr)
library(psych)
library(tidyverse)

hour <- read_csv("C:/Users/KBridges/Desktop/TRANSFER/R Programming/week6/data/hour.csv")
str(hour)
head(hour)

#hypothesis: average count of casual users will be higher on weekends than weekdays. 
#start by exploring patterns in data, number of casual users by weekday. 
summary(hour$casual)
summary(hour$registered)
hist(hour$casual)
hist(hour$registered)
plot(density(hour$casual))
plot(density(hour$registered))

stats_casual <- hour %>% 
  group_by(weekday) %>%
  summarise(N = n(), 
            Mean = mean(casual, na.rm = TRUE), 
            SD = sd(casual, na.rm = TRUE)) %>%
  as.data.frame()

stats_casual


#create 2 samples for weekdays and weekends
gp_weekday <- hour %>% filter(weekday > 0 & weekday < 6)
gp_weekend <- hour %>% filter (weekday == 0 | weekday == 6)

summary(gp_weekday$casual)
summary(gp_weekend$casual)
summary(gp_weekday$registered)
summary(gp_weekend$registered)

#create a subset with a new column to separate sample groups

subset1 <- hour %>%
  select (instant, weekday, registered, casual) %>%
  mutate(day_gp = if_else(weekday == 0 | weekday == 6, 'satsun','monfri'))

subset1

#delete this, just a check to make sure that the new column was created properly
check <- subset1 %>% filter(weekday == 4)
check

stats_subset1<- subset1 %>% 
  group_by(day_gp) %>%
  summarise(N = n(), 
            Mean = mean(casual, na.rm = TRUE), 
            SD = sd(casual, na.rm = TRUE)) %>%
  as.data.frame()

stats_subset1

#test normality of each sample and start by creating histograms 

with(subset1, hist(casual[day_gp == 'monfri']))
with(subset1, hist(casual[day_gp == 'satsun']))

#histogram with both variables together

ggplot(subset1, aes(x=casual, colour=day_gp, fill=day_gp)) +
  geom_histogram(alpha=0.5, position="identity") +
    labs(title="Histogram plot of casual users on weekends and weekday",
       x="Casual Users", y="Frequencies") + 
  theme_minimal()

#density plot - not sure why the line is curved
ggplot(subset1, aes(x=casual, colour=day_gp, fill=day_gp)) +
  geom_density(alpha=0.5) + 
  geom_vline(data=subset1, aes(xintercept=mean(casual), colour=day_gp),
             linetype="dashed") +
  labs(title="Density plot of casual users on weekends and weekdays",
       x="Casual Users", y="Density") +
  theme_minimal()

# QQ-plots:
ggplot(subset1, aes(sample = casual, colour = day_gp)) + 
  stat_qq() +
  stat_qq_line() + 
  theme_minimal()


# use a Kolmogorov-Smirnov test because these are larger samples
#neither sample appears to be have significant difference from normal distribution 

stats_subset1

set.seed(999)
sample_weekday <- round(rnorm(12365, mean = 26.33595, sd = 31.13191), 0)

set.seed(999)
sample_weekend <- round(rnorm(5014, mean = 58.71021, sd = 72.73894), 0)

ks.test(subset1[subset1$day_gp == 'monfri', 'casual'], 
        sample_weekday) # p-value > .05; not significant difference from normal distribution

ks.test(subset1[subset1$day_gp == 'satsun', 'casual'], 
        sample_weekend) # p-value > .05; not significant difference from normal distribution

#variance tests
var.test(casual ~ day_gp, data = subset1) 
#p-value is >0.5 so the variance in usage between casual users on weekdays 
#and weekends is not statistically different 

# run independent (unpaired) t-test
t.test(casual ~ day_gp, data=subset1, alternative="two.sided")
# The difference is not significant, p>.05 and confidence intervals include "zero". 

plot(t.test(casual ~ day_gp, data=subset1, alternative="two.sided"))

# Plot simple boxplots with means to compare groups:
ggplot(subset1, aes(x=day_gp, y=casual, colour=day_gp)) + 
  geom_boxplot(width=.5) + 
  stat_summary(fun=mean, geom="point", shape=23, 
               size=3, color="blue", fill="blue") +
  labs(title="Boxplots of casual usage on weekdays and weekends with mean statistic", 
       x="day_gp", y="casual") +
  theme_minimal()

