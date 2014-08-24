# Reproducible Research: Peer Assessment 1

#Load data
setwd("~/GitHub/RepData_PeerAssessment1")
activity <- read.csv("activity.csv", colClasses = c("integer","Date","integer"))

str(activity)
summary(activity)
tail(activity)

### What is mean total number of steps taken per day? ####

#For this part of the assignment, you can ignore the missing values in the dataset.

# 1. Make a histogram of the total number of steps taken each day 
library(dplyr)
library(ggvis)
library(ggplot2)

hist(activity$steps)
hist(activity_na$steps)

qplot(date, steps, data = activity %>% group_by(date), geom = "bar", stat = "identity", main = "Total number of steps per day (NAs removed)")

activity_na <- na.omit(activity)
activity_na %>% ggvis(~date, ~steps) %>% group_by(date) %>% layer_bars(stat="bin")

# 2. Calculate and report the **mean** and **median** total number of steps taken per day
tot_steps <- activity %>% group_by(date) %>% summarise(tot_steps = sum(steps, na.rm=TRUE))
avg_steps <- mean(tot_steps$tot_steps, na.rm=TRUE)
med_steps <- median(tot_steps$steps, na.rm=TRUE)

### What is the average daily activity pattern? ####

# 1. Make a time series plot (i.e. `type = "l"`) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

# qplot(interval, steps, data = activity %>% group_by(interval, date) %>% summarise(steps= mean(steps, na.rm=TRUE)), geom = "line", main = "Averge number of steps for 5 minute intervals" )

qplot(interval, steps, data = activity %>% group_by(interval) %>% summarise(steps= mean(steps, na.rm=TRUE)), geom = "line", main = "Averge number of steps for 5 minute intervals")

p <- activity_na %>% ggvis(~interval, ~steps) %>% group_by(interval) %>% summarise(steps=mean(steps, na.rm=TRUE)) %>% layer_lines()

get_data(p)

# 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
d1 <- activity %>% group_by(interval) %>% summarise(avg_steps = mean(steps, na.rm=TRUE))
with(d1,d1[order(-avg_steps),])
d1[104,]

arrange(d1, interval, desc(avg_steps)) #Not working, don't know why

### Imputing missing values

# Note that there are a number of days/intervals where there are missing
# values (coded as `NA`). The presence of missing days may introduce
# bias into some calculations or summaries of the data.

#1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with `NA`s)
summary(activity)

#2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
library(dplyr)

day_mean <- activity %>% group_by(date) %>% summarise(avg_steps = mean(steps, na.rm = TRUE), med_steps = median(steps, na.rm=TRUE))

act_nareplace <- activity

for (l in act_nareplace$steps[is.na(act_nareplace$steps)]) {
  for (r in day_mean) {
    if (act_nareplace$date==day_mean$date) {
      act_nareplace$steps=day_mean$avg_steps
    }
  }
}
#Replace `NaN` with 0
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))
act_nareplace[is.nan(act_nareplace)] <- 0



act_nareplace$steps[is.na(act_nareplace$steps)] <- mean(act_nareplace$steps, na.rm = TRUE)

#3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


#4. Make a histogram of the total number of steps taken each day and Calculate and report the **mean** and **median** total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

qplot(date, steps, data = act_nareplace %>% group_by(date), geom = "bar", stat = "identity", main = "Total number of steps per day (NAs replaced)")

tot_steps_narep <- act_nareplace %>% group_by(date) %>% summarise(tot_steps = sum(steps))
summary(tot_steps_narep)

