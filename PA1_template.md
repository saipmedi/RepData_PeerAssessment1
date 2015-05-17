
# This markdown file contains R code to calculate the following: 
## Mean daily steps, Average weekly patterns, and determine if there are different patterns between Weekdays and weekends.


```r
library(ggplot2)
activity<-read.csv("./activity.csv")
```

## Calculating the mean daily steps,median, and histogram of frequency per step number:


```r
total<-tapply(activity$steps, activity$date, FUN=sum, na.rm=T)
qplot(data=total,binwidth=1000, xlab="Total number of daily steps",ylab="Frequency")
```

```
## Error: ggplot2 doesn't know how to deal with data of class array
```

```r
mean(total)
```

```
## [1] 9354.23
```

```r
median(total)
```

```
## [1] 10395
```

## What is the average daily activity pattern:

```r
meansteps <- aggregate(x=list(steps=activity$steps), by=list(interval=activity$interval), FUN=mean, na.rm=TRUE)
plot<-ggplot(data=meansteps, aes(x=interval, y=steps))

plot + geom_line() + xlab("5-Minute Interval") + ylab("Average Steps Taken")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

```r
meansteps[which.max(meansteps$steps),]
```

```
##     interval    steps
## 104      835 206.1698
```

## Labeling missing values:

```r
misval<-is.na(activity$steps)
table(misval)
```

```
## misval
## FALSE  TRUE 
## 15264  2304
```

```r
#Creating a function to replace missing values with corresponding mean values
replaceMean<-function(steps,interval) {
    toReplace<-NA
    if(!is.na(steps))
        toReplace<-c(steps)
    else
        toReplace<-(meansteps[meansteps$interval==interval, "steps"])
    return(toReplace)
}

#using defined function to replace the values in activity dataset
activity.replaced<-activity
activity.replaced$steps<-mapply(replaceMean, activity.replaced$steps,activity.replaced$interval)
```

## We can now take activity.replaced and produce a Histogram with 
## Mean and Median of total steps

```r
totalv2<-tapply(activity.replaced$steps,activity.replaced$date, FUN=sum)
qplot(totalv2, binwidth=1000, xlab="Daily Total Steps", ylab="Frequency")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 

```r
mean(totalv2)
```

```
## [1] 10766.19
```

```r
median(totalv2)
```

```
## [1] 10766.19
```

## Characterizing activity patterns between Weekdays and Weekends:

```r
whichday<-function(date) {
    day<-weekdays(date)
    
    if (day %in% c("Monday","Tuesday","Wednesday","Thursday","Friday"))
        return("weekday")
    
    else if(day %in% c("Saturday","Sunday"))
        return("weekend")
    
    else
        stop("Invalid Date")
}
#using above function, will append new column indicator of weekend or weekday
activity.replaced$date <-as.Date(activity.replaced$date)
activity.replaced$day<-sapply(activity.replaced$date, FUN=whichday)
head(activity.replaced)
```

```
##       steps       date interval     day
## 1 1.7169811 2012-10-01        0 weekday
## 2 0.3396226 2012-10-01        5 weekday
## 3 0.1320755 2012-10-01       10 weekday
## 4 0.1509434 2012-10-01       15 weekday
## 5 0.0754717 2012-10-01       20 weekday
## 6 2.0943396 2012-10-01       25 weekday
```
## Final step is to produce a panel plot differentiating weekdays and ##weekends on the mean daily steps 

```r
meansteps <- aggregate(steps~interval + day, data=activity.replaced, mean)

patternplot <- ggplot(meansteps, aes(interval,steps))
patternplot + geom_line() + facet_grid(day~.)+xlab("5-Minute Interval") + ylab("Number of Steps")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png) 

