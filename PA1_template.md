# PA1_template
Anuj Jain  
April 10, 2016  



## Loading and preprocessing the Data


```r
# Anuj Jain
# Reproducible Research Project 1

library(reshape2)
library(dplyr)
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
library(ggplot2)

setwd("C:/DSWork/Reproducible_Data")

# Reading Data

filename <- "activity.csv"
df <- read.csv(filename)

# Omitting rows with missing values

df2 <- na.omit(df)
df2 <- df2[,-3]

# Calculate the total number of steps taken each day

test <- group_by(df2, date)
ans1 <- summarise(test, sum(steps))

# Histogram for Total Number of Steps taken each day

hist(ans1$`sum(steps)`, xlab = "Number of Steps Each Day", main = "Total Number of Steps Taken Each Day")
```

![](PA1_template_files/figure-html/unnamed-chunk-1-1.png)

```r
# Mean and Meadian Steps for Each Day

test2 <- group_by(df2, date)
ans2 <- summarise(test2, sum(steps))
Step_Mean <- mean(ans2$`sum(steps)`)
Step_Median <- median(ans2$`sum(steps)`)

# Time series plot of the 5 minute interval across all days

df2 <- na.omit(df)
test3 <- group_by(df2, interval)
ans3 <- summarise(test3, mean(steps))
plot(ans3$interval, ans3$`mean(steps)`, type = "l", xlab = "Intervals (Every 5 min)", ylab = "Average Steps", main = "Time Series Plot for Average Steps for each interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-1-2.png)

```r
# Missing Data

# Number of Rows with missing data
nrow(df) - nrow(df2)
```

```
## [1] 2304
```

```r
df3 <- df

for( i in 1:nrow(df3)){
  if (is.na(df3[i,1])){
    for (j in 1:nrow(ans3)){
      if(ans3[j, 1] == df3[i,3]){
        avg_step <- ans3[j, 2]
      }
    }
    df3[i,1] <- avg_step
  }
}

# Create a Histogram Using DF3 and create mean and median

test <- group_by(df3, date)
ans4 <- summarise(test, sum(steps))

hist(ans4$`sum(steps)`, xlab = "Number of Steps Each Day", main = "Total Number of Steps Taken Each Day (Plot 2)")
```

![](PA1_template_files/figure-html/unnamed-chunk-1-3.png)

```r
Step_Mean2 <- mean(ans4$`sum(steps)`)
Step_Median2 <- median(ans4$`sum(steps)`)


# Creating a new variable to DF3
df3$date <- as.Date(df3$date)
df3$var <- weekdays(df3$date)

for(i in 1:nrow(df3)){
  if((df3[i,4] == "Sunday") | (df3[i,4] == "Saturday")){
    df3[i,4] <- "Weekend"
  }
  else{
    df3[i,4] <- "Weekday"
  }
}


# Creating new graph

df_weekend <- filter(df3, df3$var == "Weekend")
df_weekday <- filter(df3, df3$var == "Weekday")

test_weekend <- group_by(df_weekend, interval)
ans_weekend <- summarise(test_weekend, mean(steps))

test_weekday <- group_by(df_weekday, interval)
ans_weekday <- summarise(test_weekday, mean(steps))

ans4 <- ans_weekday
ans4 <- cbind(ans4, ans_weekend$`mean(steps)`)
colnames(ans4) <- c("interval", "Weekday(mean)", "Weekend(mean)")


par(mfcol = c(2,1))
with(ans4, {
  plot(ans4$interval, ans4$`Weekday(mean)`, type = "l", xlab = "Intervals (Every 5 min)", ylab = "Average Steps Weekday" )
  plot(ans4$interval, ans4$`Weekend(mean)`, type = "l", xlab = "Intervals (Every 5 min)", ylab = "Average Steps Weekend")
})
```

![](PA1_template_files/figure-html/unnamed-chunk-1-4.png)



## Calculating the number of steps taken each day


## Histogram of Steps Taken Each Day


## Mean and Median of the Total Number of Steps taken per Day


## Create a Time Series PLot for Average Steps for Each interval

## Filling in Missing Vales


## This does a comparision betwen the Weekday and weekend average steps for all intervals.
