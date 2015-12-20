# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


```r
da<-read.csv("activity.csv",header=TRUE)
da[,2]<-as.Date(da[,2],format="%Y-%m-%d")
```

## What is mean total number of steps taken per day?



## What is the average daily activity pattern?



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
