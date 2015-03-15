# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


```r
fileurl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"

if(!file.exists("activity.zip")) {
        download.file(fileurl, destfile = "activity.zip", method = "curl")
        date_downloaded <- date()
        unzip("activity.zip")
}

activitydata <- read.csv("activity.csv")
```


## What is mean total number of steps taken per day?


```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
## 
## The following object is masked from 'package:stats':
## 
##     filter
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
# Create a data frame containing the date and number of steps for each day
dailysteps <- summarise(group_by(activitydata, date), sum(steps))
names(dailysteps) <- c("date","totalsteps")

# Plot histogram of number of steps across all days
with(dailysteps, plot(date, totalsteps, type = "h"))
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

```r
# Calculate mean and median total number of steps per day
meandailysteps <- round(mean(dailysteps$totalsteps, na.rm = TRUE))
mediandailysteps <- round(median(dailysteps$totalsteps, na.rm = TRUE))
```

Mean total daily steps: 1.0766\times 10^{4}  
Median total daily steps: 1.0765\times 10^{4}


## What is the average daily activity pattern?


```r
# Create a data frame containing the time interval and average number of steps for each time interval
intervalsteps <- summarise(group_by(activitydata, interval), mean(steps, na.rm = TRUE))
names(intervalsteps) <- c("interval","meansteps")

# Make a time series plot of average # of steps in each time interval vs the time interval
with(intervalsteps, plot(interval, meansteps, type = "l"))
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

```r
# Determine which time interval contains the largest average # of steps
maxmeansteps <- intervalsteps[which(intervalsteps$meansteps == max(intervalsteps$meansteps)),]
```

Time interval 835 contains the largest average number of steps (206)

## Imputing missing values


```r
# Calculate the total number of missing values in the dataset (i.e. the total number of rows with NAs)
totalNAs <- sum(is.na(activitydata$steps))

# Devise a strategy for filling in all of the missing values in the dataset. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
#activitydata <- mutate(activitydata, stepsinterpolated = steps)

#rows (dates and intervals) with NAs
activitydatacompleted <- activitydata

NArows <- which(is.na(activitydatacompleted$steps))

activitydatacompleted[NArows,"steps"]  <- round(intervalsteps[intervalsteps$interval %in% activitydatacompleted[NArows,"interval"], "meansteps"])

# Create a new dataset that is equal to the original dataset but with the missing data filled in.

# Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment?

# Create a data frame containing the date and number of steps for each day
dailysteps <- summarise(group_by(activitydatacompleted, date), sum(steps))
names(dailysteps) <- c("date","totalsteps")

# Plot histogram of number of steps across all days
with(dailysteps, plot(date, totalsteps, type = "l"))
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

```r
# Calculate mean and median total number of steps per day
meandailysteps <- round(mean(dailysteps$totalsteps, na.rm = TRUE))
mediandailysteps <- round(median(dailysteps$totalsteps, na.rm = TRUE))

# What is the impact of imputing missing data on the estimates of the total daily number of steps?
```

Total number of missing values: 2304  
Mean total daily steps with missing values filled in: 1.0766\times 10^{4}  
Median total daily steps with missing values filled in: 1.0762\times 10^{4}  
  
Imputing missing data has no effect on the total daily number of steps.  

## Are there differences in activity patterns between weekdays and weekends?


```r
# Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

activitydatacompleted <- mutate(activitydatacompleted, weekday = weekdays(as.Date(activitydatacompleted$date)))

activitydatacompleted[which(activitydatacompleted$weekday %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")), "weekday"] <- "weekday" 
activitydatacompleted[which(activitydatacompleted$weekday %in% c("Saturday", "Sunday")), "weekday"] <- "weekend"

activitydatacompleted$weekday <- as.factor(activitydatacompleted$weekday)


# Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


# Create a data frame containing the time interval and average number of steps for each time interval
intervalsteps <- summarise(group_by(activitydatacompleted, interval, weekday), mean(steps, na.rm = TRUE))
names(intervalsteps) <- c("interval", "weekday", "meansteps")

# Make a time series plot of average # of steps in each time interval vs the time interval

library(lattice)
attach(intervalsteps)
xyplot(meansteps~interval|weekday,
   main="Number of Steps per time interval on Weekday and Weekend",
   xlab="Interval",
   ylab="Average Number of Steps",
   type = "l")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 
