---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

## Setting options to turn warnings off


```r
knitr::opts_chunk$set(warning = FALSE, echo = TRUE)
```
## Loading and preprocessing the data

At first, we load the "activityData" from file "activity.csv" and processed the data into a dataframe. The variable "date" was of character type. So, it's changed into a date type variable for the convenience in furthur works. Weekdays are determined using the weekdays() function. 


```r
library(ggplot2)

activityData <- read.csv(file = "activity.csv", sep = ",")
activityData$date <- as.POSIXct(activityData$date, "%Y-%m-%d")
activityData$weekday <- weekdays(activityData$date)
```
## What is mean total number of steps taken per day?


```r
totalSteps <- with(activityData, aggregate(steps, by =list(date), sum, na.rm = TRUE))
names(totalSteps) <- c("Date", "Total Steps") 
hist(totalSteps$`Total Steps`, main = "Total number of steps taken per day", xlab = "Total steps taken per day", col = "darkgreen", ylim = c(0,20), breaks = seq(0,25000, by=2500))
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->


+  The mean total number of steps taken per day is -


```r
mean(totalSteps$`Total Steps`)
```

```
## [1] 9354.23
```
+  And The median of total number of steps taken per day is -


```r
median(totalSteps$`Total Steps`)  
```

```
## [1] 10395
```
## What is the average daily activity pattern?
+  A time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis) is drawn to display the average daily activity pattern.


```r
stepsPerInterval <- with(activityData, aggregate(steps, by = list(interval), mean, na.rm = TRUE))

names(stepsPerInterval) <- c("interval", "average steps") 
with(stepsPerInterval, plot(interval, `average steps`, type = "l", main = "Average Steps taken Per Interval", xlab = "Interval", ylab = "Average Steps", col = "darkgreen"))
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

+  The 5-minute interval, which contains the maximum number of steps, on average across all the days in the dataset.


```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
maxStep <- filter(stepsPerInterval, `average steps` == max(stepsPerInterval$`average steps`)) 
maxStep$interval
```

```
## [1] 835
```
## Imputing missing values
There are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

The total number of missing values in the dataset (i.e. the total number of rows with NAs) is - 

```r
totalMissingValue <- sum(is.na(activityData$steps)) # Number of missing values
totalMissingValue
```

```
## [1] 2304
```
A strategy for filling in all of the missing values in the dataset -

```r
imputedSteps <- stepsPerInterval$`average steps`[match(activityData$interval, stepsPerInterval$interval)]
```
Now, a new dataset is created that is equal to the original dataset but with the missing data filled in.

```r
imputedData <- transform(activityData, steps = ifelse(is.na(activityData$steps), yes = imputedSteps, no = activityData$steps))
totalImputedSteps <- with(imputedData, aggregate(steps, by =list(date), sum))
names(totalImputedSteps) <- c("Date", "Total Steps") 
```
A histogram of the total number of steps taken each day -  

```r
hist(totalImputedSteps$`Total Steps`, main = "Total number of steps taken per day", xlab = "Total steps per day", col = "darkgreen", ylim = c(0,20), breaks = seq(0,25000, by=2500))
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

+ Calculation of the mean total number of steps taken per day

```r
mean(totalImputedSteps$`Total Steps`)
```

```
## [1] 10766.19
```

+ Calculation of the median of total number of steps taken per day


```r
median(totalImputedSteps$`Total Steps`)
```

```
## [1] 10766.19
```
So, These values differ greatly with the previous one. Both the mean and median has the same value unlike the earlier. The impact of imputing missing data on the estimates of the total daily number of steps is evident in the histogram drawn based on the imputed missing data. 

## Are there differences in activity patterns between weekdays and weekends?
Here, we created a new factor variable in the dataset with two levels – “weekday” and “weekend” indicates whether a given date is a weekday or weekend day.

```r
activityData$date <- as.Date(strptime(activityData$date, format="%Y-%m-%d"))
activityData$datetype <- sapply(activityData$date, function(x) {
  if (weekdays(x) == "Saturday" | weekdays(x) =="Sunday") 
  {y <- "Weekend"} else 
  {y <- "Weekday"}
  y
}) 
```

A panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis): 

```r
activity_by_date <- aggregate(steps~interval + datetype, activityData, mean, na.rm = TRUE)
plot<- ggplot(activity_by_date, aes(x = interval , y = steps, color = datetype)) +
  geom_line() +
  labs(title = "Average daily steps by type of days(weekday/Weekend)", x = "Interval", y = "Average number of steps") +theme(plot.title = element_text(hjust = 0.5))+
  facet_wrap(~datetype, ncol = 1, nrow=2)
print(plot)
```

![](PA1_template_files/figure-html/unnamed-chunk-15-1.png)<!-- -->






