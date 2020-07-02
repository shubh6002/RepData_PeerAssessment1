# Loading and preprocessing the data

```{r, echo=TRUE}

activitydata <- read.csv("./temp/activity.csv")
summary(activitydata)
names(activitydata)
head(activitydata)
```

# What is mean total number of steps taken per day?

  1.Calculate the total number of steps taken per day
```{r, echo=TRUE} 
stepsperday <- aggregate(steps ~ date, activitydata, sum, na.rm = TRUE)
```

  2.If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day
```{r}
hist(stepsperday$steps)
```
![](figure/Rplot1.png)

   3.Calculate and report the mean and median of the total number of steps taken per day
   # mean
```{r}
meanstepsperday <- mean(stepsperday$steps)
meanstepsperday
```
   # median
```{r}
medianstepsperday <- median(stepsperday$steps)
medianstepsperday
```

# What is the average daily activity pattern?
  1.Make a time series plot (i.e.,type = “𝚕”) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
  
```{r, echo=TRUE}
library(ggplot2)
stepsPerInterval<-aggregate(steps~interval, data=activitydata, mean, na.rm=TRUE)
plot(steps~interval, data=stepsPerInterval, type="l")
```
![](figure/Rplot2.png)
  
  2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
```{r, echo=TRUE}
intervalWithMaxNbSteps <- stepsPerInterval[which.max(stepsPerInterval$steps),]$interval
intervalWithMaxNbSteps
```

# Imputing missing values

    1.Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with 𝙽𝙰s)
```{r, echo=TRUE}
totalValuesMissings <- sum(is.na(activitydata$steps))
totalValuesMissings
```

   2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be  sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
```{r, echo=TRUE}
getMeanStepsPerInterval<-function(interval){
    stepsPerInterval[stepsPerInterval$interval==interval,]$steps
}
```


    3.Create a new dataset that is equal to the original dataset but with the missing data filled in.
```{r, echo=TRUE}
activityDataNoNA<-activitydata
for(i in 1:nrow(activityDataNoNA)){
    if(is.na(activityDataNoNA[i,]$steps)){
        activityDataNoNA[i,]$steps <- getMeanStepsPerInterval(activityDataNoNA[i,]$interval)
    }
}
```
    

   4.Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
```{r, echo=TRUE}
totalStepsPerDayNoNA <- aggregate(steps ~ date, data=activityDataNoNA, sum)
hist(totalStepsPerDayNoNA$steps)
```
![](figure/Rplot3.png)

To calculate the mean and median total number of steps per day, we first find total number of steps per day
```{r, echo = TRUE}
meanStepsPerDayNoNA <- mean(totalStepsPerDayNoNA$steps)
meanStepsPerDayNoNA

medianStepsPerDayNoNA <- median(totalStepsPerDayNoNA$steps)
medianStepsPerDayNoNA
```


# Are there differences in activity patterns between weekdays and weekends?

    1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
```{r, echo=TRUE}
activityDataNoNA$date <- as.Date(strptime(activityDataNoNA$date, format="%Y-%m-%d"))
activityDataNoNA$day <- weekdays(activityDataNoNA$date)
for (i in 1:nrow(activityDataNoNA)) {
    if (activityDataNoNA[i,]$day %in% c("Saturday","Sunday")) {
        activityDataNoNA[i,]$day<-"weekend"
    }
    else{
        activityDataNoNA[i,]$day<-"weekday"
    }
}
stepsByDay <- aggregate(activityDataNoNA$steps ~ activityDataNoNA$interval + activityDataNoNA$day, activityDataNoNA, mean)
```

    2. Make a panel plot containing a time series plot (i.e. 𝚝𝚢𝚙𝚎 = “𝚕”) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.
```{r, echo=TRUE}
names(stepsByDay) <- c("interval", "day", "steps")
library(lattice)
xyplot(steps ~ interval | day, stepsByDay, type = "l", layout = c(1, 2), 
    xlab = "Interval", ylab = "Number of steps")
```
![](figure/Rplot4.png)

