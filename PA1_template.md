# Reproducible Research: Peer Assessment 1



##1:  Loading and preprocessing the data

#### 1.1:Check for a folder called data, if the folder does not exist it creates a folder called data


```r
if(!file.exists("./data")){dir.create("./data")}
```

####  1.2: Downloading the data zip file


```r
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileUrl,destfile="./data/Factivity.zip")
```
#### 1.3: Unzipping the data zip file & defining the directory for the data.


```r
unzip(zipfile="./data/Factivity.zip")
```

#### 1.4: Load the data

```r
Data <- read.csv("activity.csv", header=TRUE)
```

#### 1.5: Process/transform the data (if necessary) into a format suitable for your analysis

#### Converting date vector into dates format


```r
Data$date <- as.Date(Data$date)
```






#### 1.6 Removing NA values from the data

##### 1.6.1 Total number of NA Values:


```r
sum(is.na(Data))
```

```
## [1] 2304
```

##### 1.6.2 Removing NA values


```r
Cleanedata <- na.exclude(Data)
```

##### 1.6.3 Confirming NA values have been resolved.


```r
sum(is.na(Cleanedata))
```

```
## [1] 0
```

#### 1.7 Aggregate the cleaned Data for 2 Histogram.

```r
Aggregatedata <- aggregate(steps ~ date, data=Cleanedata, sum)
```

##2: Histogram of the total number of steps taken each day


```r
hist(Aggregatedata$steps,main="Total Steps per Day", xlab="Steps", ylab="Frequency")
```

![](PA1_template_files/figure-html/Histogram -1.png)<!-- -->


##3: Mean and median number of steps taken each day

### Mean

```r
mean(Aggregatedata$steps)
```

```
## [1] 10766.19
```

### Median

```r
median(Aggregatedata$steps)
```

```
## [1] 10765
```

##4: Time series plot of the average number of steps taken

#### 4.1 ## Aggregate the cleaned Data using steps & interval data

```r
Aggregatedata <- aggregate(steps~interval, data=Cleanedata, mean)
```

#### What is the average daily activity pattern?


```r
plot(Aggregatedata$interval,Aggregatedata$steps, type="l", xlab="Interval(minutes)", ylab="Number of Steps",main="Average Number of Steps per Day by Interval")
```

![](PA1_template_files/figure-html/Timeseries-1.png)<!-- -->

##5: The 5-minute interval that, on average, contains the maximum number of steps


```r
max_interval.factor <- Aggregatedata[which.max(  
        Aggregatedata$steps),]
print(round(max_interval.factor,1))
```

```
##     interval steps
## 104      835 206.2
```

##6: Code to describe and show a strategy for imputing missing data

##### 6.1 Obtain data with Na values


```r
CopiedData <- Data
```


##### 6.2 Total NA in the data set:

```r
sum(is.na(CopiedData))
```

```
## [1] 2304
```




### 6.3 strategy: replacing the NA values with the Column means in the uncleaned date set.

##### 6.3.1  The code below replaces the NA values with the column means:


```r
CopiedData$steps[which(is.na(CopiedData$steps))] <- mean(CopiedData$steps,na.rm = TRUE)
```

##### 6.3.2 Total NA in the data set:

```r
sum(is.na(CopiedData))
```

```
## [1] 0
```

##### 6.3.3: Aggregate the cleaned Data for 7 Histogram


```r
Aggregatedata3  <- with(CopiedData, aggregate(steps ~ date,FUN=sum))
```


##7: Histogram of the total number of steps taken each day after missing values are imputed

```r
hist(Aggregatedata3$steps,main="The distribution of daily total (with missing data imputed)", xlab="Steps", ylab="Frequency")
```

![](PA1_template_files/figure-html/unnamed-chunk-19-1.png)<!-- -->


#####  Do these values differ from the estimates from the first part of the assignment?

#### Calculate and report the mean and median total number of steps taken per day. 

#####: Means & Median of part 3: 

#### Mean: 10766.19

#### Median: 10765

##### New means & Median: 

### Mean

```r
mean(Aggregatedata3$steps)
```

```
## [1] 10766.19
```

### Median

```r
median(Aggregatedata3$steps)
```

```
## [1] 10766.19
```

#### Do these values differ from the estimates from the first part of the assignment? 

#####: Comparing the calculations done in the first part of this document and the calculations done in the second  part, we can see the mean value remains unchanged,however the median value has increased &now matches the mean

#### What is the impact of imputing missing data on the estimates of the total daily number of steps?
The impact of aggregating missing values does not have any negative impact on our predictions. 

##8: Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

#### 8.1 Converting date vector into dates format

```r
Data$date <- as.Date(Data$date)
```

#### 8.1 create a vector of weekdays

```r
weekdays1 <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
```

#### 8.2 Use `%in%` and `weekdays` to create a logical vector
#### convert to `factor` and specify the `levels/labels`

```r
Data$wDay <- factor((weekdays(Data$date) %in% weekdays1), 
                   levels=c(FALSE, TRUE), labels=c('weekend', 'weekday')) 
                   Data$wDay <- c('weekend', 'weekday')[(weekdays(Data$date) %in% weekdays1)+1L] 
```


```r
Step2 <- with(Data, aggregate(steps ~ interval+wDay,FUN=mean))
```


```r
library(lattice)
xyplot(steps ~ interval/100 | factor(wDay),
       layout = c(1, 2),
       xlab="Interval(minutes)",
       ylab="Number of steps",
       type="l",
       lty=1,
       data=Step2)
```

![](PA1_template_files/figure-html/Q8-1.png)<!-- -->


#### Are there differences in activity patterns between weekdays and weekends?

There is a clear difference between weekend and weekday patterns.  The data shows that the subject waktes up earlier
on the week day with morning spike, than the weekend. However there is an increase in activity on the weekends, compared to the weekday.



