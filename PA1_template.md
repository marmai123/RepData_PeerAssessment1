---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Read in needed R-packages

```r
library(dplyr)
library(ggplot2)
```



## Loading and preprocessing the data

Unzip and load the data to R. Change format on the date field to 'Date'.

```r
data <- read.csv(unzip("activity.zip"))
data$date <- as.Date(data$date, "%Y-%m-%d")
```



## What is mean total number of steps taken per day?

Make a histogram of the total number of steps taken each day.

```r
data_day <- aggregate(steps~date, data, sum, na.rm=TRUE)
hist(data_day$steps, main = "Histogram of number of steps per day", xlab="Number of steps per day", breaks = 10)
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

Calculate and report the mean and median total number of steps taken per day.

Mean

```r
mean(data_day$steps)
```

```
## [1] 10766.19
```

Median

```r
median(data_day$steps)
```

```
## [1] 10765
```



## What is the average daily activity pattern?

Create the time series and plot the data

```r
data_meanint <- aggregate(steps~interval, data, mean, na.rm=TRUE)
plot(data_meanint$interval, data_meanint$steps, type="l", xlab = "5-minute interval", ylab = "Average number of steps taken")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->


Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
data_meanint$interval[which.max(data_meanint$steps)]
```

```
## [1] 835
```




## Imputing missing values

Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
sum(is.na(data$steps))
```

```
## [1] 2304
```


Impute missing values with mean for that 5-minute interval and create a new data set.
The only missing values are in the 'steps' column.

```r
# Create a new data frame
data_new <- data

# Check data frame value by value and impute value if NA
for (i in 1:nrow(data)) {
     if (is.na(data$steps[i])) {
        interval <- data$interval[i]
        value_index <- which(data_meanint$interval == interval)
        value <- data_meanint$steps[value_index]
        data_new$steps[i] <- value   
        }
}
```



Make a histogram of the total number of steps taken each day using the imputed data.

```r
data_day_new <- aggregate(steps~date, data_new, sum, na.rm=TRUE)
hist(data_day_new$steps, main = "Histogram of number of steps per day", xlab="Number of steps per day", breaks = 10)
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

Calculate and report the mean and median total number of steps taken per day.

Mean

```r
mean(data_day_new$steps)
```

```
## [1] 10766.19
```

Median

```r
median(data_day_new$steps)
```

```
## [1] 10766.19
```

The mean value is the same as with the original data. Median value changes slightly, from 10765 to 10766.19.


## Are there differences in activity patterns between weekdays and weekends?

Include weekdays in data_new table.

```r
# Add weekdays to data table
data_new$day <- weekdays(data_new$date)

# Classify the days to either weekdays or weekends
data_new$daytype <- "day" #start with dummy value
for (i in 1:nrow(data_new)) {
        if (data_new$day[i] == "lördag") {
                data_new$daytype[i] <- "weekend" 
        }
        else if (data_new$day[i] == "söndag") {
                data_new$daytype[i] <- "weekend" 
        }
        else {
                data_new$daytype[i] <- "weekday"
        }
                
}

# Change daytype to factor variable                     
data_new$daytype <- as.factor(data_new$daytype)

# Split the data in two sets, weekdays and weekends
weekday_data <- filter(data_new, daytype == "weekday")
weekend_data <- filter(data_new, daytype == "weekend")

# Take mean over 5-minute intervals for the two data sets
# Weekdays
weekday_data_mean <- aggregate(steps~interval, weekday_data, mean)
weekday_data_mean$daytype <- "weekday"
# Weekends
weekend_data_mean <- aggregate(steps~interval, weekend_data, mean)
weekend_data_mean$daytype <- "weekend"

# Add the tables together to prepare for the plot
plot_data <- rbind(weekday_data_mean, weekend_data_mean)
plot_data$daytype <- as.factor(plot_data$daytype)
```

Plot the data

```r
ggplot(plot_data, aes(x=interval, y=steps)) +
  geom_line() + 
  facet_grid(daytype ~.) +
  labs(title = "Average number of steps taken", x = "5-minute interval",
             y = "Number of steps taken")
```

![](PA1_template_files/figure-html/unnamed-chunk-14-1.png)<!-- -->

The step pattern is different on weekdays and weekends.




