---
title: "Reproducible Research Project 1"
output: 
    html_document: 
        keep_md: true 
---


## Loading and preprocessing the data


```r
library(tidyverse)
```

```
## -- Attaching packages ------------------------------------------------------------------------------------------------- tidyverse 1.3.0 --
```

```
## v ggplot2 3.3.2     v purrr   0.3.4
## v tibble  3.0.3     v dplyr   1.0.2
## v tidyr   1.1.1     v stringr 1.4.0
## v readr   1.3.1     v forcats 0.5.0
```

```
## -- Conflicts ---------------------------------------------------------------------------------------------------- tidyverse_conflicts() --
## x dplyr::filter() masks stats::filter()
## x dplyr::lag()    masks stats::lag()
```

```r
library(ggplot2)
activity <- read.csv('activity.csv') # read in data
```


## What is mean total number of steps taken per day?

1. Calculate the total number of steps taken per day


```r
Total_Steps <- activity %>%
    drop_na() %>%
    group_by(date) %>% 
    summarize(steps = sum(steps))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
head(Total_Steps, 10)
```

```
## # A tibble: 10 x 2
##    date       steps
##    <chr>      <int>
##  1 2012-10-02   126
##  2 2012-10-03 11352
##  3 2012-10-04 12116
##  4 2012-10-05 13294
##  5 2012-10-06 15420
##  6 2012-10-07 11015
##  7 2012-10-09 12811
##  8 2012-10-10  9900
##  9 2012-10-11 10304
## 10 2012-10-12 17382
```

2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day. 


```r
ggplot(Total_Steps, aes(x = steps)) +
    geom_histogram(fill = "blue", binwidth = 1000) +
    labs(title = "Daily Steps", 
         x = "Steps", 
         y = "Frequency")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

3. Calculate and report the mean and median of the total number of steps taken per day

```r
Mean_Steps = mean(Total_Steps$steps, na.rm = TRUE)
Median_Steps = median(Total_Steps$steps, na.rm = TRUE)
```

## What is the average daily activity pattern?

1. Make a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
Interval <- activity %>% 
    drop_na() %>%
    group_by(interval) %>%
    summarize(steps = mean(steps))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
ggplot(Interval, aes(interval , steps)) + 
    geom_line(color="red", size=1) + 
    labs(title = "Avg. Daily Steps", 
         x = "Interval", 
         y = "Avg. Steps per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
max_step <- Interval %>%
    filter(steps == max(steps)) %>%
    select(interval)
```

## Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with 𝙽𝙰s)


```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

I will use the mean to replace the NA values

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
a <- activity$steps
sum(is.na(a))
```

```
## [1] 2304
```

```r
b <- lapply(a, function(x){ifelse(is.na(x), Mean_Steps, x)})
sum(is.na(b))
```

```
## [1] 0
```

```r
activity2 <- activity
activity2$steps <- b
```

4. Make a histogram of the total number of steps taken each day and calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
# mean and median total number of steps taken per day
Mean_TSteps = mean(Total_Steps$steps) 
Median_TSteps = median(Total_Steps$steps)

ggplot(Total_Steps, aes(x = steps)) + 
    geom_histogram(fill = "green", binwidth = 1000) + 
    labs(title = "Daily Steps", 
         x = "Steps",
         y = "Frequency")
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->
## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
activity2 <- activity2 %>%
    mutate(Date = as.POSIXct(date, format = "%Y-%m-%d"), 
           steps = as.numeric(steps), 
           day = weekdays(Date), 
           type_of_day = as.factor(ifelse(day == 'Saturday', 'weekend',
                                          ifelse(day == 'Sunday', 'weekend', 
                                                 'weekday'))))
```

2. Make a panel plot containing a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
Interval2 <- activity2 %>% 
    group_by(interval, type_of_day) %>%
    summarize(steps = mean(steps))
```

```
## `summarise()` regrouping output by 'interval' (override with `.groups` argument)
```

```r
ggplot(Interval2 , aes(interval, steps, color=type_of_day)) + 
    geom_line() + 
    labs(title = "Avg. Daily Steps by Week type", 
         x = "Interval", 
         y = "Number of Steps") + 
    facet_wrap(~type_of_day , ncol = 1, nrow=2)
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->


