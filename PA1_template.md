# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


```r
da<-read.csv("activity.csv",header=TRUE)
da[,2]<-as.Date(da[,2],format="%Y-%m-%d")
```

## What is mean total number of steps taken per day?

1.Calculate the total number of steps taken per day

```r
da_1<-da[complete.cases(da),]
temp_date<-split(da_1,da_1[,c("date")],drop=TRUE) 
result1<-lapply(temp_date,FUN=function(x) sum(x$steps)) 
result2<-lapply(temp_date,FUN=function(x) max(x$steps)) 
numofstep<-cbind(result1,result2)
```

2.Make a histogram of the total number of steps taken each day

```r
hist(as.numeric(numofstep[,1]),xlab="number of steps",main ="The histogram of the total number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

3.Calculate and report the mean and median of the total number of steps taken per day

```r
result_mean<-lapply(temp_date,FUN=function(x) mean(x$steps)) 
result_median<-lapply(temp_date,FUN=function(x) median(x$steps)) 
numofstep_report<-cbind(result_mean,result_median)
numofstep_report
```

```
##            result_mean result_median
## 2012-10-02 0.4375      0            
## 2012-10-03 39.41667    0            
## 2012-10-04 42.06944    0            
## 2012-10-05 46.15972    0            
## 2012-10-06 53.54167    0            
## 2012-10-07 38.24653    0            
## 2012-10-09 44.48264    0            
## 2012-10-10 34.375      0            
## 2012-10-11 35.77778    0            
## 2012-10-12 60.35417    0            
## 2012-10-13 43.14583    0            
## 2012-10-14 52.42361    0            
## 2012-10-15 35.20486    0            
## 2012-10-16 52.375      0            
## 2012-10-17 46.70833    0            
## 2012-10-18 34.91667    0            
## 2012-10-19 41.07292    0            
## 2012-10-20 36.09375    0            
## 2012-10-21 30.62847    0            
## 2012-10-22 46.73611    0            
## 2012-10-23 30.96528    0            
## 2012-10-24 29.01042    0            
## 2012-10-25 8.652778    0            
## 2012-10-26 23.53472    0            
## 2012-10-27 35.13542    0            
## 2012-10-28 39.78472    0            
## 2012-10-29 17.42361    0            
## 2012-10-30 34.09375    0            
## 2012-10-31 53.52083    0            
## 2012-11-02 36.80556    0            
## 2012-11-03 36.70486    0            
## 2012-11-05 36.24653    0            
## 2012-11-06 28.9375     0            
## 2012-11-07 44.73264    0            
## 2012-11-08 11.17708    0            
## 2012-11-11 43.77778    0            
## 2012-11-12 37.37847    0            
## 2012-11-13 25.47222    0            
## 2012-11-15 0.1423611   0            
## 2012-11-16 18.89236    0            
## 2012-11-17 49.78819    0            
## 2012-11-18 52.46528    0            
## 2012-11-19 30.69792    0            
## 2012-11-20 15.52778    0            
## 2012-11-21 44.39931    0            
## 2012-11-22 70.92708    0            
## 2012-11-23 73.59028    0            
## 2012-11-24 50.27083    0            
## 2012-11-25 41.09028    0            
## 2012-11-26 38.75694    0            
## 2012-11-27 47.38194    0            
## 2012-11-28 35.35764    0            
## 2012-11-29 24.46875    0
```

## What is the average daily activity pattern?



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
