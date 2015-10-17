COURSERA-Reproducible Research Course Project 1
===============================================

Name: Christoph Thommen

Date: 17.10.2015

Data: The dataset contains information about the number of steps taken by 
an individual collected during October and November 2012 in a 5 minute 
interval. The following variables are collected:
steps: Number of steps taking in a 5-minute interval (missing values are coded
as NA)
date: The date on which the measurement was taken in YYYY-MM-DD format
interval: Identifier for the 5-minute interval in which measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there are a 
total of 17,568 observations in this dataset.

##Loading and preprocessing the data

First of all the data is loaded.


```r
data = read.csv("activity.csv")
```

```
## Warning: kann Datei 'activity.csv' nicht öffnen: No such file or directory
```

```
## Error: kann Verbindung nicht öffnen
```


The data is then preprocessed such that the variable date appears in the 
date-format.


```r
data$date = as.Date(data$date)
```


##What is mean total number of steps taken per day?

The sum of steps taken by the individual is aggregated on a daily base, 
with the aggregate function.
The variables of the resulting table are then renamed.


```r
result1 = aggregate(data$steps, by = list(data$date), sum, na.rm = T)
dimnames(result1)[[2]] = c("date", "steps")
```

The result is as follows:

```r
print(result1)
```

```
##          date steps
## 1  2012-10-01     0
## 2  2012-10-02   126
## 3  2012-10-03 11352
## 4  2012-10-04 12116
## 5  2012-10-05 13294
## 6  2012-10-06 15420
## 7  2012-10-07 11015
## 8  2012-10-08     0
## 9  2012-10-09 12811
## 10 2012-10-10  9900
## 11 2012-10-11 10304
## 12 2012-10-12 17382
## 13 2012-10-13 12426
## 14 2012-10-14 15098
## 15 2012-10-15 10139
## 16 2012-10-16 15084
## 17 2012-10-17 13452
## 18 2012-10-18 10056
## 19 2012-10-19 11829
## 20 2012-10-20 10395
## 21 2012-10-21  8821
## 22 2012-10-22 13460
## 23 2012-10-23  8918
## 24 2012-10-24  8355
## 25 2012-10-25  2492
## 26 2012-10-26  6778
## 27 2012-10-27 10119
## 28 2012-10-28 11458
## 29 2012-10-29  5018
## 30 2012-10-30  9819
## 31 2012-10-31 15414
## 32 2012-11-01     0
## 33 2012-11-02 10600
## 34 2012-11-03 10571
## 35 2012-11-04     0
## 36 2012-11-05 10439
## 37 2012-11-06  8334
## 38 2012-11-07 12883
## 39 2012-11-08  3219
## 40 2012-11-09     0
## 41 2012-11-10     0
## 42 2012-11-11 12608
## 43 2012-11-12 10765
## 44 2012-11-13  7336
## 45 2012-11-14     0
## 46 2012-11-15    41
## 47 2012-11-16  5441
## 48 2012-11-17 14339
## 49 2012-11-18 15110
## 50 2012-11-19  8841
## 51 2012-11-20  4472
## 52 2012-11-21 12787
## 53 2012-11-22 20427
## 54 2012-11-23 21194
## 55 2012-11-24 14478
## 56 2012-11-25 11834
## 57 2012-11-26 11162
## 58 2012-11-27 13646
## 59 2012-11-28 10183
## 60 2012-11-29  7047
## 61 2012-11-30     0
```


Visualised in a histogram:


```r
hist(result1$steps, ylab = "Frequency", xlab = "Total number of steps per day", 
    main = "Total number of steps per day")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 


The mean of the total number of steps taken is equal to ```{r} mean(result1$steps,na.rm=T)```
,the median is equal to ```{r} median(result1$steps,na.rm=T)```

##What is the average daily activity pattern?

The mean of steps taken by the individual is calculated for each interval with the aggregate function.
The variables of the resulting table are then renamed.


```r
result2 = aggregate(data$steps, by = list(data$interval), mean, na.rm = T)
dimnames(result2)[[2]] = c("interval", "steps")
```

The following plot shows the average daily activity pattern in terms of steps.
Each interval determines a 5-minute period in a day.


```r
plot(result2$interval, result2$steps, type = "l", ylab = "Average steps", xlab = "Interval", 
    main = "Average steps \ntaken per interval across all days")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7.png) 

The 5-minute interval ```{r} result2[result2$steps==max(result2$steps,na.rm=T),]$interval``` on average across all 
the days in the dataset, contains the maximum number of steps. 

##Imputing missing values

The number of missing values is equal to ```{r} table(is.na(data$steps)==T)[2]```,
which is ```{r} mean(is.na(data$steps)*100)``` % of all observations.


The missing values are replaced or imputed by the rounded average interval values across all days.
therefore the already calculated table of average steps per interval across all days ist taken.


```r
result3 = result2
result3$steps = round(result2$steps, 0)
```


In a next step the new dataset is created in a slightly cumbersome way:

1. The part of the data containing missing values is taken.

```r
data_imp = data[is.na(data$steps) == T, ]
```


2. The part of the data for which values are imputed is then merged with the previously created table
containing the average number of steps per interval across all days.

```r
data_imp2 = merge(data_imp, result2, by = "interval", all.x = T)
```


3. The values of the average number of steps per interval across all days are then saved in a new object
while the missing values are skipped.

```r
data_imp3 = data_imp2[, c(1, 3, 4)]
dimnames(data_imp3)[[2]] = c("interval", "date", "steps")
```


4. The rows with the imputed values are then bound together with the rows without missing values.

```r
data2 = rbind(data[is.na(data$steps) == F, ], data_imp3)
```



```r
result4 = aggregate(data2$steps, by = list(data$date), sum, na.rm = T)
dimnames(result4)[[2]] = c("date", "steps")
```


Visualised in a histogram:


```r
hist(result4$steps, ylab = "Frequency", xlab = "Total number of steps per day", 
    main = "Total number of steps per day with imputed values")
```

![plot of chunk unnamed-chunk-14](figure/unnamed-chunk-14.png) 


The mean of the total number of steps taken is equal to ```{r} mean(result4$steps,na.rm=T)```,the median
is equal to ```{r} median(result4$steps,na.rm=T)```. 

Mean and median do increase with the applied method of imputation. A possible explanation is 
the skewed distribution of the values.

##Are there differences in activity patterns between weekdays and weekends?

The function weekdays() is used to create a new variable indicating weather a date is on a weekday
or weekend.


```r
data2$weekday = ifelse(weekdays(data2$date) %in% c("Samstag", "Sonntag"), "weekend", 
    "weekday")
```


The mean of steps taken by the individual is aggregated on a per interval and weekday-category.
The variables of the resulting table are then renamed.


```r
result5 = aggregate(data2$steps, by = list(data2$weekday, data2$interval), mean, 
    na.rm = T)
dimnames(result5)[[2]] = c("weekday", "interval", "steps")
```


The library ggplot2 is loaded to create the following plot.

```r
library(ggplot2)
```


The plot is then created for each interval and the two weekday categories.

```r
qplot(interval, steps, data = result5, geom = "line", facets = weekday ~ .)
```

![plot of chunk unnamed-chunk-18](figure/unnamed-chunk-18.png) 

The weekdays activity pattern differs from the weekend pattern, as there is a morning peak, while
on weekends the activity seems to be distributed more equally across the day.

The library knitr is loaded to create the markdown document.

```r
library(knitr)
```

The markdown document is created.


```r
knit2html("C:/Users/Christoph Thommen/RepData_PeerAssessment1/PA1.Rmd")
```

```
## 
## 
## processing file: C:/Users/Christoph Thommen/RepData_PeerAssessment1/PA1.Rmd
```

```
##   |                                                                         |                                                                 |   0%  |                                                                         |..                                                               |   2%
##   ordinary text without R code
## 
##   |                                                                         |...                                                              |   5%
## label: unnamed-chunk-21
##   |                                                                         |.....                                                            |   7%
##   ordinary text without R code
## 
##   |                                                                         |......                                                           |  10%
## label: unnamed-chunk-22
##   |                                                                         |........                                                         |  12%
##   ordinary text without R code
## 
##   |                                                                         |..........                                                       |  15%
## label: unnamed-chunk-23
##   |                                                                         |...........                                                      |  17%
##   ordinary text without R code
## 
##   |                                                                         |.............                                                    |  20%
## label: unnamed-chunk-24
##   |                                                                         |..............                                                   |  22%
##   ordinary text without R code
## 
##   |                                                                         |................                                                 |  24%
## label: unnamed-chunk-25
```

```
##   |                                                                         |.................                                                |  27%
##   ordinary text without R code
## 
##   |                                                                         |...................                                              |  29%
## label: unnamed-chunk-26
##   |                                                                         |.....................                                            |  32%
##   ordinary text without R code
## 
##   |                                                                         |......................                                           |  34%
## label: unnamed-chunk-27
```

```
##   |                                                                         |........................                                         |  37%
##   ordinary text without R code
## 
##   |                                                                         |.........................                                        |  39%
## label: unnamed-chunk-28
##   |                                                                         |...........................                                      |  41%
##   ordinary text without R code
## 
##   |                                                                         |.............................                                    |  44%
## label: unnamed-chunk-29
##   |                                                                         |..............................                                   |  46%
##   ordinary text without R code
## 
##   |                                                                         |................................                                 |  49%
## label: unnamed-chunk-30
##   |                                                                         |.................................                                |  51%
##   ordinary text without R code
## 
##   |                                                                         |...................................                              |  54%
## label: unnamed-chunk-31
##   |                                                                         |....................................                             |  56%
##   ordinary text without R code
## 
##   |                                                                         |......................................                           |  59%
## label: unnamed-chunk-32
##   |                                                                         |........................................                         |  61%
##   ordinary text without R code
## 
##   |                                                                         |.........................................                        |  63%
## label: unnamed-chunk-33
##   |                                                                         |...........................................                      |  66%
##   ordinary text without R code
## 
##   |                                                                         |............................................                     |  68%
## label: unnamed-chunk-34
```

```
##   |                                                                         |..............................................                   |  71%
##   ordinary text without R code
## 
##   |                                                                         |................................................                 |  73%
## label: unnamed-chunk-35
##   |                                                                         |.................................................                |  76%
##   ordinary text without R code
## 
##   |                                                                         |...................................................              |  78%
## label: unnamed-chunk-36
##   |                                                                         |....................................................             |  80%
##   ordinary text without R code
## 
##   |                                                                         |......................................................           |  83%
## label: unnamed-chunk-37
##   |                                                                         |.......................................................          |  85%
##   ordinary text without R code
## 
##   |                                                                         |.........................................................        |  88%
## label: unnamed-chunk-38
```

```
##   |                                                                         |...........................................................      |  90%
##   ordinary text without R code
## 
##   |                                                                         |............................................................     |  93%
## label: unnamed-chunk-39
##   |                                                                         |..............................................................   |  95%
##   ordinary text without R code
## 
##   |                                                                         |...............................................................  |  98%
## label: unnamed-chunk-40
```

```
##   |                                                                         |.................................................................| 100%
##   ordinary text without R code
```

```
## output file: PA1.md
```

