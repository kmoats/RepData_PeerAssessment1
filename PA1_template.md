# Reproducible Research: Peer Assessment 1


```r
knitr::opts_chunk$set(fig.width=12, fig.height=8, fig.path='Figures/',
                      echo=TRUE, warning=FALSE, message=FALSE)
```

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

# Create a data frame containing the date and number of steps for each day
dailysteps <- summarise(group_by(activitydata, date), sum(steps))
names(dailysteps) <- c("date","totalsteps")

# Calculate mean and median total number of steps per day
meandailysteps <- round(mean(dailysteps$totalsteps, na.rm = TRUE))
mediandailysteps <- round(median(dailysteps$totalsteps, na.rm = TRUE))

# Plot histogram of number of steps across all days
hist(dailysteps$totalsteps, breaks = 10, main = "Total Number of Steps across all days", xlab = "Number of Steps", ylab = "Frequency")
abline(v = meandailysteps, col = "red")
abline(v = mediandailysteps, col = "blue")
legend("topright", legend = c("Mean", "Median"), col=c("red","blue"), lty = 1)
```

![](Figures/unnamed-chunk-2-1.png) 

Mean total daily steps: 1.0766\times 10^{4}  
Median total daily steps: 1.0765\times 10^{4}


## What is the average daily activity pattern?


```r
# Create a data frame containing the time interval and average number of steps for each time interval
intervalsteps <- summarise(group_by(activitydata, interval), mean(steps, na.rm = TRUE))
names(intervalsteps) <- c("interval","meansteps")

# Determine which time interval contains the largest average # of steps
maxmeansteps <- intervalsteps[which(intervalsteps$meansteps == max(intervalsteps$meansteps)),]

# Make a time series plot of average # of steps in each time interval vs the time interval
with(intervalsteps, plot(interval, meansteps, main = "Average steps for each time interval", xlab = "Time Interval", ylab = "Steps", type = "l"))
```

![](Figures/unnamed-chunk-3-1.png) 

Time interval 835 contains the largest average number of steps (206)


## Imputing missing values


```r
# Calculate the total number of missing values in the dataset (i.e. the total number of rows with NAs)
totalNAs <- sum(is.na(activitydata$steps))

# Create a new dataset that is equal to the original dataset but with the missing data filled in.
activitydatacompleted <- activitydata

#rows (dates and intervals) with NAs
NArows <- which(is.na(activitydatacompleted$steps))

# Fill in all of the missing values in the dataset with the mean for that 5-minute interval
activitydatacompleted[NArows,"steps"]  <- round(intervalsteps[intervalsteps$interval %in% activitydatacompleted[NArows,"interval"], "meansteps"])

# Create a data frame containing the date and number of steps for each day
dailysteps <- summarise(group_by(activitydatacompleted, date), sum(steps))
names(dailysteps) <- c("date","totalsteps")

# Calculate mean and median total number of steps per day
meandailysteps <- round(mean(dailysteps$totalsteps, na.rm = TRUE))
mediandailysteps <- round(median(dailysteps$totalsteps, na.rm = TRUE))

# Plot histogram of number of steps across all days
hist(dailysteps$totalsteps, breaks = 10, main = "Total Number of Steps across all days with missing values filled in", xlab = "Number of Steps", ylab = "Frequency")
abline(v = meandailysteps, col = "red")
abline(v = mediandailysteps, col = "blue")
legend("topright", legend = c("Mean", "Median"), col=c("red","blue"), lty = 1)
```

![](Figures/unnamed-chunk-4-1.png) 

Total number of missing values: 2304  

The missing values in the dataset were filled in with the mean for that 5-minute interval.  

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
       main="Number of Steps per time interval on Weekdays and Weekends",
       xlab="Time Interval",
       ylab="Steps",
       type = "l")
```

![](Figures/unnamed-chunk-5-1.png) 
