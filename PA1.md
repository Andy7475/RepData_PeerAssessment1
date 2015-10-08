Assignment 1
========================================================
## Loading and preprocessing the data

First, let's load the activity log data. And change the date variable to a date format

```r
library(lubridate)
library(dplyr)
library(ggplot2)
library(scales)
activity <- read.csv("~/4 Trainer/coursera/ReproducibleResearch/activity.csv")
activity$date <- ymd(activity$date)
```
## What is the mean total number of steps taken per day?
Ignoring missing values

### Total number of steps taken per day
Group the dataset by date (day), and then calculate the sum for the variable 'steps', ignoring NA values. Then plot a histogram of the data.


```r
df.steps <- activity %>% group_by(date) %>% summarise(total.steps=sum(steps,na.rm=TRUE))
qplot(data=df.steps,x=total.steps,geom="histogram",
      binwidth=1000,
      main="Total Number of Steps taken each Day",
      xlab="Total steps per day")
```

![plot of chunk histogram](figure/histogram-1.png) 

### Mean and Median of total steps per day
The mean steps per day is 9354.2295082

The median steps per day is 10395

## What is the average daily activity pattern?
Below is a time series plot of the 5 minute internal number, vs the average number of steps taken on each interval. First a dataframe is created grouped by interval with the mean number of steps for each interval calculated. Then the data are plotted in a line plot.


```r
df.time <- activity %>% group_by(interval) %>% summarise(steps=mean(steps,na.rm=TRUE))
plot(df.time$steps ~ df.time$interval, type="l",
     xlab="Interval Number",
     ylab="Mean number of Steps")
title("Average steps for each 5 minute interval")
```

![plot of chunk averageDailyPattern](figure/averageDailyPattern-1.png) 

## Input missing values

### Total Number of NAs in the dataset
How many missing values are there in the dataset? Most useful would be information broken down by each variable


```r
activity %>% summarise_each(funs(sum(is.na(.))))
```

```
##   steps date interval
## 1  2304    0        0
```

### Fill in missing values

Fill in missing values by assigning the mean number of steps for each interval to missing values for that interval.

Method is to use the df.time dataset from earlier (mean steps by interval) and merge this set with the original.


```r
#add a mean.steps column for each interval in the original dataframe
activity2 <- merge(activity,df.time,by="interval",all.x=TRUE)
names(activity2) <- c("interval","steps","date","mean.steps")
#For every NA value in steps replace that with the value in mean.steps from the df.time dataframe
missing.steps <- is.na(activity2$steps)
activity2$steps[missing.steps]<- activity2$mean.steps[missing.steps]
```

### Analyse the new missing values dataframe
Plot a histogram and calculate the mean and median value


```r
df.steps2 <- activity2 %>% group_by(date) %>% summarise(total.steps=sum(steps,na.rm=TRUE))
qplot(data=df.steps2,x=total.steps,geom="histogram",
      binwidth=1000,
      main="Total Number of Steps taken each Day",
      xlab="Total steps per day")
```

![plot of chunk histogramMissing](figure/histogramMissing-1.png) 

### Mean and Median of total steps per day (filled in missing values)
The mean steps per day is 10,766.19

Previously it was: 9,354.23

The median steps per day is 10,766.19

Previously it was: 10,395

### Impact of this
The effect is to reduce the number of days with fewer steps (we only ever added data to each interval) and therefore increase the mean and the median values.

## Weekdays and Weekends

Using data with filled-in values for NA (activity2)

### Create a factor variable for weekdays and weekend


```r
activity3 <- activity2 %>% mutate(day = wday(date))
#Note that Sunday will have a value of 1 and Saturday 7 with the wday function
#Now create a function to determine if a day is a weekend 
weekend <- function(vWeekday){
    #define a factor variable
    weekend.factor <- factor(c("weekend","weekday"))
    #if it's 1 or a 7 it's a weekend
    if(vWeekday == 1 | vWeekday == 7)  {
        return(weekend.factor[1])
    }
    #otherwise a weekday
    else return(weekend.factor[2])
}

#assign a weekday or weekend variable for each day
activity3$day.type <- sapply(activity3$day,weekend)
```

### Make a panel plot

Make two time series plots broken down by weekday and weekend. y axis will be average steps, x axis will be interval number.

```r
qplot(data=activity3,
      x=interval,
      y=steps,
      stat="summary",
      fun.y="mean",
      geom="line",
      facets=day.type~.,
      ylab="Mean number of steps",
      xlab="Interval")
```

![plot of chunk panelPlot](figure/panelPlot-1.png) 
