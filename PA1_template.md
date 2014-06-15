# Reproducible Research: Peer Assessment 1
# This is my first assignment for the course Reproducible Research.

## Loading and preprocessing the data

```r
data <- read.csv("activity.csv", header = TRUE)
```

```
## Warning: cannot open file 'activity.csv': No such file or directory
```

```
## Error: cannot open the connection
```

```r
clean_data <- data[which(data$steps != "NA"), ]
```

```
## Error: object of type 'closure' is not subsettable
```


## What is mean total number of steps taken per day?

```r
library(plyr)
total_by_day <- ddply(clean_data, .(date), summarize, steps = sum(steps))
```

```
## Error: object 'clean_data' not found
```

```r

# Making the histogram

hist(total_by_day$steps, main = "Number of steps", xlab = "Total number of steps taken each day", 
    col = "red")
```

```
## Error: object 'total_by_day' not found
```

```r

# Calculating mean and median
mean(total_by_day$steps)
```

```
## Error: object 'total_by_day' not found
```

```r
median(total_by_day$steps)
```

```
## Error: object 'total_by_day' not found
```


## What is the average daily activity pattern?

```r
average_by_interval <- ddply(clean_data, .(interval), summarize, steps = mean(steps))
```

```
## Error: object 'clean_data' not found
```

```r

# Making the time series plot
plot(average_by_interval$interval, average_by_interval$steps, type = "l", col = "blue", 
    xlab = "5-minute interval", ylab = "Average number of steps taken", main = "Average daily activity pattern")
```

```
## Error: object 'average_by_interval' not found
```

```r

# Which 5-minute interval, on average across all the days in the dataset,
# contains the maximum number of steps?
average_by_interval[average_by_interval$steps == max(average_by_interval$steps), 
    ]
```

```
## Error: object 'average_by_interval' not found
```

```r
colnames(average_by_interval)[2] <- "intervalAvg"
```

```
## Error: object 'average_by_interval' not found
```


## Imputing missing values

```r
# Total number of missing values in the dataset
sum(is.na(data$steps))
```

```
## Error: object of type 'closure' is not subsettable
```

```r

# Filling NA's with average for that 5-min interval
merged <- arrange(join(data, average_by_interval), interval)
```

```
## Error: object 'average_by_interval' not found
```

```r

# Creating a new dataset with the missing data filled in.
merged$steps[is.na(merged$steps)] <- merged$intervalAvg[is.na(merged$steps)]
```

```
## Error: object 'merged' not found
```

```r

# Making the histogram
new_total_by_day <- ddply(merged, .(date), summarize, steps = sum(steps))
```

```
## Error: object 'merged' not found
```

```r
hist(new_total_by_day$steps, main = "Number of Steps", xlab = "Total number of steps taken each day", 
    col = "red", )
```

```
## Error: object 'new_total_by_day' not found
```

```r

# Calculating mean and median total number of steps taken per day
mean(new_total_by_day$steps)
```

```
## Error: object 'new_total_by_day' not found
```

```r
median(new_total_by_day$steps)
```

```
## Error: object 'new_total_by_day' not found
```

```r
total_steps1 <- sum(clean_data$steps)
```

```
## Error: object 'clean_data' not found
```

```r
total_steps2 <- sum(merged$steps)
```

```
## Error: object 'merged' not found
```

```r
total_diff <- total_steps2 - total_steps1[]
```

```
## Error: object 'total_steps2' not found
```

### Mean values did not change but the median changed.  Imputing missing data on the estimates added to the total daily number of steps

## Are there differences in activity patterns between weekdays and weekends?


```r
library(lattice)
weekdays <- weekdays(as.Date(merged$date))
```

```
## Error: object 'merged' not found
```

```r
data_with_weekdays <- transform(merged, day = weekdays)
```

```
## Error: object 'merged' not found
```

```r
data_with_weekdays$wk <- ifelse(data_with_weekdays$day %in% c("Saturday", "Sunday"), 
    "weekend", "weekday")
```

```
## Error: object 'data_with_weekdays' not found
```

```r
average_by_interval_wk <- ddply(data_with_weekdays, .(interval, wk), summarize, 
    steps = mean(steps))
```

```
## Error: object 'data_with_weekdays' not found
```

```r

xyplot(steps ~ interval | wk, data = average_by_interval_wk, layout = c(1, 2), 
    type = "l")
```

```
## Error: object 'average_by_interval_wk' not found
```

