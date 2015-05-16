---
title: "PA1_template"
author: "Alex Zukowsky"
date: "Saturday, May 16, 2015"
output: html_document
---

This first section of code will load the data and do some preprocessing of the
    data
It switches the date into a date format and also converts the interval into a
    factor format


```r
Activity <- read.csv(file="activity.csv")

Activity$dateformat <- as.Date(Activity$date, format = "%Y-%m-%d")
Activity$intervalformat <- as.factor(Activity$interval)
```


This next chunk of code will create a histogram reflecting the total daily steps
It will also save the histogram as a png file


```r
library(xts)
```

```
## Loading required package: zoo
## 
## Attaching package: 'zoo'
## 
## The following objects are masked from 'package:base':
## 
##     as.Date, as.Date.numeric
```

```r
ActivityDaily <- xts(Activity$steps, order.by=Activity$dateformat)
dailytotals <- apply.daily(ActivityDaily, sum)

hist(dailytotals)
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 

```r
dev.copy(png, file = "Histogram.png", width = 700, height = 480)
```

```
## png 
##   3
```

```r
dev.off()
```

```
## png 
##   2
```


Here we report on the mean and median of the total number of steps taken per day


```r
apply(dailytotals,2,mean, na.rm=TRUE)
```

```
## [1] 10766.19
```

```r
apply(dailytotals,2,median, na.rm=TRUE)
```

```
## [1] 10765
```


Next we want to plot what an average day looks like in terms of the number of
    steps per 5 minute interval
    

```r
library(data.table)
```

```
## data.table 1.9.4  For help type: ?data.table
## *** NB: by=.EACHI is now explicit. See README to restore previous behaviour.
## 
## Attaching package: 'data.table'
## 
## The following object is masked from 'package:xts':
## 
##     last
```

```r
ActivityShort <- data.frame(Activity$intervalformat, Activity$steps)
ActivityTable <- data.table(ActivityShort)
ActivityShortTable <- ActivityTable[,list(mean=mean(Activity.steps, 
    na.rm = TRUE)), by=Activity.intervalformat]

ActivityShortTable$intervalNumber <- 
    as.numeric(as.character(ActivityShortTable$Activity.intervalformat))
ActivityShortTable$intervalNumber <- 
    as.integer(ActivityShortTable$intervalNumber)

ActivitySummary <- as.data.frame.matrix(ActivityShortTable)

ActivitySummary$intervalTime <- 
    as.POSIXlt(strptime((ActivitySummary$intervalNumber %/% 100), 
    format = "%H")) + ((ActivitySummary$intervalNumber %% 100)*60)

plot(ActivitySummary$intervalTime, ActivitySummary$mean, type = "l")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

```r
dev.copy(png, file = "DailyActivity.png", width = 700, height = 480)
```

```
## png 
##   3
```

```r
dev.off()
```

```
## png 
##   2
```


We then need to answer the question of which 5 minute interval has the highest
    average steps taken
    

```r
maxrow <- which.max(ActivitySummary$mean)  
ActivitySummary[maxrow, 1]
```

```
## [1] "835"
```

From above we can see that 8:35am has the highest average steps taken


Next we needed to report on the number of NA's in the dataset


```r
sum(is.na(Activity$steps))
```

```
## [1] 2304
```

From above we see that number to be 2,304


Next we need to deal with the NA values located in the steps column
My approach was to use the 5 minute interval average and apply it to the NA's


```r
ActivityMerged <- merge(Activity, ActivitySummary, 
    by.x = "intervalformat", by.y = "intervalNumber")
ActivityMerged$NoNASteps <- ActivityMerged$steps
my.na <- is.na(ActivityMerged$steps)
ActivityMerged$NoNASteps[my.na] <- ActivityMerged$mean[my.na]
```

We then needed to create a new histogram and report this new dataset's mean
    and median
    

```r
ActivityDailyNEW <- xts(ActivityMerged$NoNASteps, 
    order.by=ActivityMerged$dateformat)
dailytotalsNEW <- apply.daily(ActivityDailyNEW, sum)

hist(dailytotalsNEW)
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png) 

```r
dev.copy(png, file = "Histogram_no_NA.png", width = 700, height = 480)
```

```
## png 
##   3
```

```r
dev.off()
```

```
## png 
##   2
```

```r
apply(dailytotalsNEW,2,mean, na.rm=TRUE)
```

```
## [1] 10766.19
```

```r
apply(dailytotalsNEW,2,median, na.rm=TRUE)
```

```
## [1] 10766.19
```

From above we see no difference in the mean and only a very small, insignificant
    difference in the median
    
    
We then need to divide the dataset into weekday (Mon-Fri) and weekend (Sat-Sun)
We need to create a panel plot showing both this weekday and weekend average
    steps
    

```r
library(data.table)

Activity$DOW <- as.POSIXlt(Activity$dateformat)$wday  ## add DOW to data frame
Activity$WeekdayStatus <- ifelse(Activity$DOW > 0 & 
    Activity$DOW < 6, 'Weekday', 'weekend')

ActivityShortDOW <- data.frame(Activity$intervalformat, Activity$steps, 
    Activity$WeekdayStatus)
ActivityTableDOW <- data.table(ActivityShortDOW)
ActivityTableDOW$DOWInterval = paste(ActivityTableDOW$Activity.WeekdayStatus, 
    ActivityTableDOW$Activity.intervalformat)
ActivityShortTableDOW <- ActivityTableDOW[,list(mean=mean(Activity.steps, 
    na.rm = TRUE)), by=DOWInterval]
ActivitySummaryDOW <- as.data.frame.matrix(ActivityShortTableDOW)

library(stringr)
Split <- as.data.frame(str_split_fixed(ActivityShortTableDOW$DOWInterval, 
    " ", 2))
ActivitySummaryDOW4 <- cbind(ActivitySummaryDOW, Split)

ActivitySummaryDOW4$intervalNumber <- 
    as.numeric(as.character(ActivitySummaryDOW4$V2))
ActivitySummaryDOW4$intervalNumber <- 
    as.integer(ActivitySummaryDOW4$intervalNumber)

ActivitySummaryDOW4$intervalTime <- 
    as.POSIXlt(strptime((ActivitySummaryDOW4$intervalNumber %/% 100), 
    format = "%H")) + ((ActivitySummaryDOW4$intervalNumber %% 100)*60)

library(ggplot2)
p <- ggplot(data=ActivitySummaryDOW4, aes(x=intervalTime, y=mean)) + geom_line()
p + facet_grid(V1 ~ .)
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png) 

```r
dev.copy(png, file = "Weekday_Weekend_Activity.png", width = 700, height = 480)
```

```
## png 
##   3
```

```r
dev.off()
```

```
## png 
##   2
```

From this graph we can see that the Weekdays start with a higher peak in average
    steps while the weekend has less valleys of inactive times
