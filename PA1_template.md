---
title: "ReproducibleResearch_CourseProject1"
author: "Jixia Liu"
date: "July 28, 2017"
output:
  html_document: default
  pdf_document: default
---




# Task 1: Loading and preprocessing the data

## Load the data (i.e. read.csv())



```r
setwd("C:/Users/liujb/Desktop/Coursera_DataScienceSpecializationSeriers/Coursera_ReproducibleResearch/CourseProject1")
tmp0 <- unzip("./data/ActivityData.zip") # "./activity.csv"
tmp1 <- read.csv("activity.csv",header=TRUE)
head(tmp1)
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




# Task 2: What is mean total number of steps taken per day?

## 1. Calculate the total number of steps taken per day


```r
tmp2 <- aggregate(steps~date,tmp1,sum,na.rm=TRUE)
colnames(tmp2) = c("date","total_steps")
print(tmp2)
```

```
##          date total_steps
## 1  2012-10-02         126
## 2  2012-10-03       11352
## 3  2012-10-04       12116
## 4  2012-10-05       13294
## 5  2012-10-06       15420
## 6  2012-10-07       11015
## 7  2012-10-09       12811
## 8  2012-10-10        9900
## 9  2012-10-11       10304
## 10 2012-10-12       17382
## 11 2012-10-13       12426
## 12 2012-10-14       15098
## 13 2012-10-15       10139
## 14 2012-10-16       15084
## 15 2012-10-17       13452
## 16 2012-10-18       10056
## 17 2012-10-19       11829
## 18 2012-10-20       10395
## 19 2012-10-21        8821
## 20 2012-10-22       13460
## 21 2012-10-23        8918
## 22 2012-10-24        8355
## 23 2012-10-25        2492
## 24 2012-10-26        6778
## 25 2012-10-27       10119
## 26 2012-10-28       11458
## 27 2012-10-29        5018
## 28 2012-10-30        9819
## 29 2012-10-31       15414
## 30 2012-11-02       10600
## 31 2012-11-03       10571
## 32 2012-11-05       10439
## 33 2012-11-06        8334
## 34 2012-11-07       12883
## 35 2012-11-08        3219
## 36 2012-11-11       12608
## 37 2012-11-12       10765
## 38 2012-11-13        7336
## 39 2012-11-15          41
## 40 2012-11-16        5441
## 41 2012-11-17       14339
## 42 2012-11-18       15110
## 43 2012-11-19        8841
## 44 2012-11-20        4472
## 45 2012-11-21       12787
## 46 2012-11-22       20427
## 47 2012-11-23       21194
## 48 2012-11-24       14478
## 49 2012-11-25       11834
## 50 2012-11-26       11162
## 51 2012-11-27       13646
## 52 2012-11-28       10183
## 53 2012-11-29        7047
```

## 2. Make a histogram of the total number of steps taken each day


```r
hist(tmp2$total_steps,main="Total number of steps per day in 2012",xlab="Total steps per day",col="grey")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)

## 3. Calculate and report the mean and median of the total number of steps taken per day


```r
mean(tmp2$total_steps)
```

```
## [1] 10766.19
```

```r
median(tmp2$total_steps)
```

```
## [1] 10765
```




# Task 3: What is the average daily activity pattern?

## 1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
tmp3 <- aggregate(steps~interval,tmp1,mean,na.rm=TRUE)
colnames(tmp3) = c("interval","ave_steps")
plot(tmp3$interval,tmp3$ave_steps,type="l",xlab="interval",ylab="Average steps",main="Daily activity",col="blue")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)

## 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
tmp3[which.max(tmp3$ave_steps),]
```

```
##     interval ave_steps
## 104      835  206.1698
```




# Task 4:Imputing missing values

## 1. calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
length(which(is.na(tmp1)))
```

```
## [1] 2304
```

## 2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.


```r
### mean number for steps per day
tmp4 <- aggregate(steps~date,tmp1,mean,na.rm=TRUE)
colnames(tmp4) = c("date","ave_steps")
```

## 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
### fill in missing values with mean number for steps per day
tmp1_new = tmp1

for (i in 1:dim(tmp1_new)[1]) {

    if (is.na(tmp1_new$steps[i])) {	
	
	tmp4_sub = tmp4[which(tmp4$date==tmp1$date[i]),]
	
	if (length(tmp4_sub$ave_steps)==0){
		tmp1_new$steps[i] = 0 }
	
	if (length(tmp4_sub$ave_steps)!=0){
		tmp1_new$steps[i] = tmp4_sub$ave_steps }
	
	}
	}
```

## 4-I. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. 
   

```r
tmp5 <- aggregate(steps~date,tmp1_new,sum,na.rm=TRUE)
colnames(tmp5) = c("date","total_steps")
head(tmp5)
```

```
##         date total_steps
## 1 2012-10-01           0
## 2 2012-10-02         126
## 3 2012-10-03       11352
## 4 2012-10-04       12116
## 5 2012-10-05       13294
## 6 2012-10-06       15420
```

```r
hist(tmp5$total_steps,main="Total number of steps per day in 2012",xlab="Total steps per day",col="blue")
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png)

```r
mean(tmp5$total_steps)
```

```
## [1] 9354.23
```

```r
median(tmp5$total_steps)
```

```
## [1] 10395
```

## 4-II. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
print(mean(tmp5$total_steps) - mean(tmp2$total_steps))
```

```
## [1] -1411.959
```

```r
print(median(tmp5$total_steps) - median(tmp2$total_steps))
```

```
## [1] -370
```

### Conclusion: Both the mean and median from the imputed dataset are deceased.




# Task 5: Are there differences in activity patterns between weekdays and weekends?

## 1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
#### Ref blog: Creating factor variables 'weekend' and 'weekday' from date


```r
tmp1_new2 = tmp1_new
weekday1 = c("Monday","Tuesday","Wednesday","Thursday","Friday")
tmp1_new2$wDays <- factor((weekdays(as.Date(tmp1_new2$date)) %in% weekday1),levels=c(FALSE,TRUE),labels=c("weekend","weekday"))
```

## 2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
library(lattice)

tmp6 <- aggregate(tmp1_new2$steps,list(tmp1_new2$interval,tmp1_new2$wDays),FUN="mean")

colnames(tmp6) = c("interval","wDays","ave_steps")
head(tmp6)
```

```
##   interval   wDays ave_steps
## 1        0 weekend      0.00
## 2        5 weekend      0.00
## 3       10 weekend      0.00
## 4       15 weekend      0.00
## 5       20 weekend      0.00
## 6       25 weekend      3.25
```

```r
xyplot(tmp6$ave_steps ~ tmp6$interval | tmp6$wDays, layout = c(1,2), type ="l", xlab = "Interval", ylab = "Number of steps")
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13-1.png)

