# Reproducible Research: Peer Assessment 1

**Load and preprocess the data**
----------------------------------

1. Download,Unzip & Read the file


```r
if(!file.exists("./data")){dir.create("./data")}
        fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
        zipfile <- "./data/activity.zip"
        csvfile <- "./data/activity.csv"
download.file(fileUrl,destfile="./data/activity.zip",method="curl")

unzip(zipfile="./data/activity.zip",exdir="./data")

raw_data <- read.csv(csvfile, header=TRUE)
head(raw_data)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

2. Process/transform the data by setting the 5 minute intervals to FACTOR class & date to DATE class


```r
library(plyr)
myData <- mutate(raw_data, interval = as.factor(interval), date = as.Date(date))
```

*a summary of the data:*

```r
summary(myData)
```

```
##      steps             date               interval    
##  Min.   :  0.00   Min.   :2012-10-01   0      :   61  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   5      :   61  
##  Median :  0.00   Median :2012-10-31   10     :   61  
##  Mean   : 37.38   Mean   :2012-10-31   15     :   61  
##  3rd Qu.: 12.00   3rd Qu.:2012-11-15   20     :   61  
##  Max.   :806.00   Max.   :2012-11-30   25     :   61  
##  NA's   :2304                          (Other):17202
```

**What is the mean total number of steps taken per day?**
-------------------------------------------------
1. Calculate total stps taken per day, ignoring the missing values in the dataset.


```r
dailySummary <- ddply(myData, .(date), summarise,
      total_steps = sum(steps, na.rm = TRUE)
      )
```

2. Make a histogram of the total number of steps taken each day


```r
steps <- dailySummary$total_steps
hist(steps)
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

3. Calculate and report the mean and median total number of steps taken per day 


```r
summarise(dailySummary,
          mean_steps = mean(total_steps, na.rm = TRUE),
          median_steps = median(total_steps, na.rm = TRUE))
```

```
##   mean_steps median_steps
## 1    9354.23        10395
```

4. Calculate average steps per interval across all days


```r
        intervalSummary <- ddply(myData, .(interval), summarise,
                      mean_steps = mean(steps, na.rm = TRUE))
        head(intervalSummary)
```

```
##   interval mean_steps
## 1        0  1.7169811
## 2        5  0.3396226
## 3       10  0.1320755
## 4       15  0.1509434
## 5       20  0.0754717
## 6       25  2.0943396
```

**What is the average daily activity pattern?**
-------------------------------------------
1. Make a time series plot of the 5-minute interval (x-axis) 
and the average number of steps taken, averaged across all days (y-axis)


```r
        x <- intervalSummary$interval
        y <- intervalSummary$mean_steps
    plot(x, y, type = "l"
         , main = "Average Daily Activity Pattern"
         , xlab = "5-minute interval", ylab = "average steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png) 

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
    max_steps <- max(y)
    max_interval <- subset(intervalSummary, mean_steps == max_steps, select = interval)
        paste("The 5-minute interval ",max_interval$interval,"contains the maximum number of average steps across all days. The average steps during this interval is ",max_steps)
```

```
## [1] "The 5-minute interval  835 contains the maximum number of average steps across all days. The average steps during this interval is  206.169811320755"
```

**Imputing missing values**
-----------------------
Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
    nas <- subset(myData, is.na(steps) == TRUE, select = date)
    missingvalues <- nrow(nas)
        print(paste("There exist ",missingvalues," missing values in the dataset"))
```

```
## [1] "There exist  2304  missing values in the dataset"
```

2. Devise a strategy for filling in all of the missing values in the dataset, such as the mean for that 5-minute interval

- Fill in the missing values in the dataset with the mean for the indicated interval.
- Create a new data set with the NAs filled in.  


```r
    x <- myData
    y <- intervalSummary
    myData2 <- join(x, y, type = "left", match = "all")
```

```
## Joining by: interval
```

```r
          steps <- myData2$steps
          meansteps <- myData2$mean_steps
        steps <- ifelse(is.na(steps) == TRUE,
             meansteps,
             steps)
        myData2 <- cbind(steps,myData[,2:3])
```

3. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. 


```r
    dailySummary2 <- ddply(myData2, .(date), summarise,
                      total_steps = sum(steps, na.rm = TRUE)
                    )
    steps2 <- dailySummary2$total_steps
    hist(steps2)
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png) 

4. Calculate and report the mean and median total number of steps taken per day. 


```r
        summarise(dailySummary2,
                mean_steps2 = mean(total_steps, na.rm = TRUE),
                median_steps2 = median(total_steps, na.rm = TRUE))
```

```
##   mean_steps2 median_steps2
## 1    10766.19      10766.19
```

5. Do these values differ from the estimates from the first part of the assignment? 
These values are different from the estimates where NAs were removed, which had a mean and median as follows:


```r
        summarise(dailySummary,
                  mean_steps = mean(total_steps, na.rm = TRUE),
                  median_steps = median(total_steps, na.rm = TRUE))
```

```
##   mean_steps median_steps
## 1    9354.23        10395
```

The is due to the fact that imputing missing data on the estimates of the total daily number of steps normalized the daily totals

**Are there differences in activity patterns between weekdays and weekends?**
-------------------------------------------------------------------------

1. Create a new factor variable, day_type, that label each day as a Weekend or a Weekday


```r
        day_of_week <- weekdays(myData2$date)
        weekend <- c("Saturday","Sunday")
        day_type <- ifelse(day_of_week == weekend,
            "weekend",
            "weekday")
        day_type <- as.factor(day_type)
        myData2 <- cbind(myData2[,1:3], day_of_week, day_type)
```

2. Calculate average steps per interval across all days


```r
        intervalSummary2 <- ddply(myData2, .(interval,day_type), summarise,
                         mean_steps = mean(steps, na.rm = TRUE)
                      )
```

3.   Make a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekdays (y-axis).


```r
weekday_set <- subset(intervalSummary2, day_type == "weekday")

plot(weekday_set$interval, weekday_set$mean_steps, type = "l",
     main = "Average Daily Activity Pattern",
     sub = "Weekdays",
     xlab = "5-minute interval", 
     ylab = "number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-17-1.png) 

4.   Make a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all Weekend days (y-axis).


```r
weekend_set <- subset(intervalSummary2, day_type == "weekend")

plot(weekend_set$interval, weekend_set$mean_steps, type = "l",
     main = "Average Daily Activity Pattern",
     sub = "Weekends",
     xlab = "5-minute interval", 
     ylab = "number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-18-1.png) 
