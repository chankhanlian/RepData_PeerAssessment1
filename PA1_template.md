---
title: 'Reproducible Research: Peer Assessment 1'
self_contained: no
output:
  html_document:
    keep_md: yes
  pdf_document: default
---


## Loading and preprocessing the data

```r
#Load the activity data set 
data<-read.csv("activity.csv")

#Replace date factors with date format
data$date<-as.Date(data$date)
```

## What is mean total number of steps taken per day?
Create a histogram of the daily steps.  Missing data ("NA") are ignored.

```r
#Calculate total steps for each date
library(ggplot2)
grouped_data<-aggregate(data$steps, by=list(data$date), FUN=sum, na.rm=TRUE)
colnames(grouped_data)<-c("date", "total_steps")
ggplot(data=grouped_data, aes(total_steps)) + geom_histogram(bins=20) + xlab("Total Steps") +
    ylab("Count") + ggtitle("Frequency Distribution of Total Daily Steps")
```

![](PA1_template_files/figure-html/figure1-1.png)<!-- -->


```r
#Calculate the mean across the dates that are not NA
mean_steps<-mean(grouped_data[!(is.na(grouped_data$total_steps)),2])
median_steps<-median(grouped_data[!(is.na(grouped_data$total_steps)),2])
```

The average number of steps taken per day is

```r
mean_steps
```

```
## [1] 9354.23
```
and the median number of steps per day is

```r
median_steps
```

```
## [1] 10395
```

## What is the average daily activity pattern?

```r
#Calculate the mean number of steps across dates by interval
grouped_interval<-aggregate(data$steps, by=list(data$interval), FUN=mean, na.rm=TRUE)
colnames(grouped_interval)<-c("interval", "avg_steps" )
ggplot(grouped_interval, aes(interval, avg_steps)) + geom_line() + ylab("Avg Number of  Steps") + xlab("Interval")
```

![](PA1_template_files/figure-html/figure2-1.png)<!-- -->

```r
#Get row of dataframe that has the maximum number of steps
max_row<-grouped_interval[which.max(grouped_interval$avg_steps),]
```

The interval with the maximum number of steps (on average across all the days)

```r
max_row[,1]
```

```
## [1] 835
```

## Imputing missing values
The number of missing values (coded as "NA") is

```r
sum(is.na(data$steps))
```

```
## [1] 2304
```
Missing data ("NA") will be replaced by the average number of steps (across all the days) for that interval.

```r
#The lookup function looks up the average number of steps for the given interval
#if the steps is NA.
lookup_function<-function(interval, steps) {
    if(is.na(steps)) grouped_interval[match(interval, grouped_interval$interval), 2]
    else steps }
    
#Create a copy of the data
imputed_data<-data
#Add a column to that has all the data filled in.
imputed_data$imputed_steps<-mapply(lookup_function, imputed_data$interval,     imputed_data$steps)

#Group the new data set by date and create a histogram of daily steps
grouped_imputed<-aggregate(imputed_data$imputed_steps, by=list(imputed_data$date), FUN=sum, na.rm=TRUE)
colnames(grouped_imputed)<-c("date", "total_steps")
ggplot(data=grouped_imputed, aes(total_steps)) + geom_histogram(bins=20) + 
xlab("Total Steps") + ylab("Count") + ggtitle("Frequency Distribution of Total Daily Steps")
```

![](PA1_template_files/figure-html/figure3-1.png)<!-- -->

## Are there differences in activity patterns between weekdays and weekends?
Create two time series plots (one for 'weekday' and one for 'weekend') of the average number of steps.

```r
#Add new column 'day' to the dataset that indicates whether the date is a 
#weekday or weekend

#day_function uses the weekdays() function to a date and determines whether it 
#is a weekend or weekday
day_function<-function(x) {
    if(weekdays(x)=='Sunday' || weekdays(x)=="Saturday") 
        'weekend'
    else 'weekday'}

#Use the day_function to add a new column to the dataset
imputed_data$day<-sapply(imputed_data$date, day_function)

#Sum the steps by interval and by day
grouped_day<-aggregate(imputed_data$imputed_steps, by=list(imputed_data$interval, imputed_data$day), FUN=mean)
grouped_day_names<-c("interval", "day", "avg_steps")
colnames(grouped_day)<-grouped_day_names
ggplot(grouped_day, aes(interval, avg_steps)) + geom_line() + ylab("Avg Number of Steps") + xlab("Interval") + facet_grid(day~.)
```

![](PA1_template_files/figure-html/figure4-1.png)<!-- -->
