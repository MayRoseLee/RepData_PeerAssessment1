---
title: "PA1_template"
author: "May Robinson"
date: '2018-03-08'
output:
  html_document:
    keep_md: yes
---


List of Variables in activity dataset:

Variable name     Type          Description   

 steps            integer       Number of steps taking in a 5-minute interval (missing values are                                                                             coded as NA)  
 
 date             factor        The date on which the measurement was taken in YYYY-MM-DD format  
 
 interval         integer       Identifier for the 5-minute interval in which measurement was taken  
#=============================================================================================================
Each specific date had 288 measurements.
# Load and preprocess the data. Data was downloaded to the working directory.  Read into R

```r
activity <-read.csv("activity.csv")
```
For this part of the assignment, observations with missing values were deleted.

```r
full <-complete.cases(activity)
###Create a dataset with all complete cases
complete<-activity[full, ]
```

# What is mean total number of steps taken per day?
Calculate the total number of steps taken per day.
Install packages for analysis

```r
library(dplyr)
```

```
## Warning: package 'dplyr' was built under R version 3.4.3
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
library(lattice)
library(lubridate)
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following object is masked from 'package:base':
## 
##     date
```

```r
daily_steps <- group_by(complete, date)
steps <- summarize(daily_steps, total_steps=sum(steps))
```


# 1. Make a histogram of the total number of steps taken each day.  

```r
hist(steps$total_steps, col="green", breaks = 50, xlab="Total number of steps", main="Missing Values deleted")
rug(steps$total_steps)
```

![](PA1_template_files/figure-html/histogram-1.png)<!-- -->

# 2. Calculate and report the mean and median of the total number of steps taken per day

```r
stepmean <-summarize(steps, mean=mean(total_steps))
stepmedian <- summarize(steps, median=median(total_steps))
```

```
## Warning: package 'bindrcpp' was built under R version 3.4.3
```
The mean number of total steps per day is 1.0766189\times 10^{4}.
The median number of total steps per day is 10765.



# What is the average daily activity pattern?

Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken,averaged across all days (y-axis)

```r
        int_steps <- group_by(complete, interval)
        meansteps <- summarize(int_steps, avg_steps = mean(steps))
        meansteps
```

```
## # A tibble: 288 x 2
##    interval avg_steps
##       <int>     <dbl>
##  1        0 1.7169811
##  2        5 0.3396226
##  3       10 0.1320755
##  4       15 0.1509434
##  5       20 0.0754717
##  6       25 2.0943396
##  7       30 0.5283019
##  8       35 0.8679245
##  9       40 0.0000000
## 10       45 1.4716981
## # ... with 278 more rows
```
Here is a time-series plot of the time interval vs average number of steps taken per day.
 
 ```r
        with(meansteps, plot (interval, avg_steps, type = "l", ylab ="Number of Steps",
                              main= "Missing values deleted"))
 ```
 
 ![](PA1_template_files/figure-html/tsplot-1.png)<!-- -->
# 2. Which 5-minute interval, on average across all the days in the dataset,
contains the maximum number of steps?

```r
         max <- max(meansteps$avg_steps)
             interval<- filter(meansteps, avg_steps >= max )
         answer <-interval[1]
```
The maximum number of steps occurs in the interval 835

# Imputing missing values
Note that there are a number of days/intervals where there are missing values
(coded as NA). 
   The presence of missing days may introduce bias into some calculations or summaries 
   of the data.
        
1.Calculate and report the total number of missing values in the dataset (i.e. the total number
                                                                        of rows with NAs)

```r
       missing <- is.na(activity$steps)
       
       blank <- activity[missing, ]
```
blank is a dataset with the missing values.
What is the total number of missing values in the dataset?

```r
count <- dim(blank)
nummissing <- count[1]
```
The total number of missing values in the dataset is 2304
       
2.Fill in all of the missing values in the blank. Use the mean/median for that day.

```r
mean_for_day <- mean(meansteps$avg_steps)
       mean_for_day
```

```
## [1] 37.3826
```
3.Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
       blank<-mutate(blank, steps =mean_for_day)
 ###Create a new data set with the missing values filled in      
       newdata <- rbind(blank, complete)
```
       
Create data for histogram using the filled dataset.

```r
       newdaily_steps <- group_by(newdata, date)
       newsteps <- summarize(newdaily_steps, total_steps=sum(steps))
```
4.Here is a histogram for the Imputed data.

```r
       hist(newsteps$total_steps, col="green", breaks = 50, main="Missing Values imputed")
       rug(newsteps$total_steps)
```

![](PA1_template_files/figure-html/plothist-1.png)<!-- -->
 Calculate and report the mean and median total number of steps taken per day. 

```r
    newmean <- summarize(newsteps, mean= mean(total_steps))
    newmedian<- summarize(newsteps, median= median(total_steps))
```
For the imputed data, the mean is 1.0766189\times 10^{4}.
                      median = 1.0766189\times 10^{4}.
                      
Do these values differ from the estimates from the first part of the assignment?
                        
When missing data were imputed, the mean stayed the same, while the median changed.  

What is the impact of imputing missing data on the estimates of the total daily number of steps?
                       
In the imputed data, the median equaled the mean, while in the data with missing values, the 
median was slightly greater than the mean.

# Are there differences in activity patterns between weekdays and weekends?

Create a dataset for analysis, using the filled-in data.

```r
    weekdata<-  mutate(newdata, weekday=wday(date, label=TRUE))
          newdata$date <-ymd(as.character(newdata$date))
           weekday <-filter(weekdata, weekday=="Mon"|weekday=="Tue"
                       |weekday=="Wed"|weekday=="Thu"|weekday=="Fri")
      
            steps_weekday  <- group_by(weekday, interval)
      avg_weekday <- summarize(steps_weekday, avg_steps = mean(steps))
           avg_weekday<- mutate(avg_weekday, daytype="weekday")
      
      weekend <- filter(weekdata, weekday=="Sat"|weekday=="Sun")
      steps_weekend  <- group_by(weekend, interval)
      avg_weekend <- summarize(steps_weekend, avg_steps = mean(steps))
            avg_weekend<- mutate(avg_weekend, daytype="weekend")
      ### Create data set for plot
      plotdata <- rbind (avg_weekend, avg_weekday)
```
      Here is a panel plot containing a time series plot of the 5-minute interval (x-axis)
      and the average number of steps taken across all weekday days or weekend days (y-axis). 
       

```r
      xyplot(avg_steps ~ interval | as.factor(daytype), data= plotdata, type ="l",
             ylab="Number of steps",layout = c(1,2))
```

![](PA1_template_files/figure-html/plotdaytypes-1.png)<!-- -->
   
dev.copy(png, file="PA1.png")      
dev.off       
