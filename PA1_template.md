Using RMarkdown to report an analysis
-------------------------------------

We will analize two months of data from an anonymous individual
collected during the months of October and November, 2012 and include
the number of steps taken in 5 minute intervals each day. The first step
is to obtain the data, unzip it, and read it into R. Afterwards, the
appropriate class will be asigned to dates.

    download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", destfile = "data.zip")
    unzip("data.zip")
    data <- read.csv("activity.csv")
    data$date <- as.Date(data$date)

Next, we'll install some packages necessary for the analysis:

    library(ggplot2)
    library(dplyr)
    library(simputation)
    library(lubridate)

Now, we're ready to answer some questions:

### What is mean total number of steps taken per day?

First, a histogram with the total of steps per day:

    ggplot(data, aes(data$date, data$steps)) + geom_histogram(stat = "identity")  + scale_x_date(date_breaks = "days", date_labels = "%b %d") + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + labs(x = "Day", y = "Total steps") 

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-3-1.png)

Now, a table with the mean and median number of steps each day:

    Mean <- tapply(data$steps, data$date, mean, na.rm = TRUE)
    Median <- tapply(data$steps, data$date, median, na.rm = TRUE)
    table1 <- cbind(Mean, Median)
    print(table1, type = "html")

    ##                  Mean Median
    ## 2012-10-01        NaN     NA
    ## 2012-10-02  0.4375000      0
    ## 2012-10-03 39.4166667      0
    ## 2012-10-04 42.0694444      0
    ## 2012-10-05 46.1597222      0
    ## 2012-10-06 53.5416667      0
    ## 2012-10-07 38.2465278      0
    ## 2012-10-08        NaN     NA
    ## 2012-10-09 44.4826389      0
    ## 2012-10-10 34.3750000      0
    ## 2012-10-11 35.7777778      0
    ## 2012-10-12 60.3541667      0
    ## 2012-10-13 43.1458333      0
    ## 2012-10-14 52.4236111      0
    ## 2012-10-15 35.2048611      0
    ## 2012-10-16 52.3750000      0
    ## 2012-10-17 46.7083333      0
    ## 2012-10-18 34.9166667      0
    ## 2012-10-19 41.0729167      0
    ## 2012-10-20 36.0937500      0
    ## 2012-10-21 30.6284722      0
    ## 2012-10-22 46.7361111      0
    ## 2012-10-23 30.9652778      0
    ## 2012-10-24 29.0104167      0
    ## 2012-10-25  8.6527778      0
    ## 2012-10-26 23.5347222      0
    ## 2012-10-27 35.1354167      0
    ## 2012-10-28 39.7847222      0
    ## 2012-10-29 17.4236111      0
    ## 2012-10-30 34.0937500      0
    ## 2012-10-31 53.5208333      0
    ## 2012-11-01        NaN     NA
    ## 2012-11-02 36.8055556      0
    ## 2012-11-03 36.7048611      0
    ## 2012-11-04        NaN     NA
    ## 2012-11-05 36.2465278      0
    ## 2012-11-06 28.9375000      0
    ## 2012-11-07 44.7326389      0
    ## 2012-11-08 11.1770833      0
    ## 2012-11-09        NaN     NA
    ## 2012-11-10        NaN     NA
    ## 2012-11-11 43.7777778      0
    ## 2012-11-12 37.3784722      0
    ## 2012-11-13 25.4722222      0
    ## 2012-11-14        NaN     NA
    ## 2012-11-15  0.1423611      0
    ## 2012-11-16 18.8923611      0
    ## 2012-11-17 49.7881944      0
    ## 2012-11-18 52.4652778      0
    ## 2012-11-19 30.6979167      0
    ## 2012-11-20 15.5277778      0
    ## 2012-11-21 44.3993056      0
    ## 2012-11-22 70.9270833      0
    ## 2012-11-23 73.5902778      0
    ## 2012-11-24 50.2708333      0
    ## 2012-11-25 41.0902778      0
    ## 2012-11-26 38.7569444      0
    ## 2012-11-27 47.3819444      0
    ## 2012-11-28 35.3576389      0
    ## 2012-11-29 24.4687500      0
    ## 2012-11-30        NaN     NA

### What is the average daily activity pattern?

We'll make a time-series plot with the average number of steps taken per
5-minute interval across all days:

    ggplot(data, aes(data$interval, data$steps)) + geom_line(stat= "summary", fun.y = "mean") + labs(x = "5-minute interval", y = "Average number of steps") + theme_light()

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-4-1.png)

Now, lets find the interval with the highest average steps.

    averages <- aggregate(data$steps, list(data$interval), mean, na.rm = TRUE)
    max_average <- round(max(averages$x))
    max_interval <- averages$Group.1[averages$x == max(averages$x)]

The maximum average of steps per interval is **206**. The corresponding
5-minute interval is **835**.

### Imputing missing values

    missing <- is.na(data$steps)
    missing_sum <- sum(missing)

The amount of missing values is **2304**.

Using the package *simputation*, we'll impute the mean value of the
corresponding interval for each NA in steps, and create a dataset with
the imputed values.

    m_data <- data
    m_data <- impute_proxy(m_data, steps ~ mean(steps,na.rm=TRUE) | interval)

The histogram for the data with imputed missing values is:

    ggplot(m_data, aes(m_data$date, m_data$steps)) + geom_histogram(stat = "identity")  + scale_x_date(date_breaks = "days", date_labels = "%b %d") + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + labs(x = "Day", y = "Total steps")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-8-1.png)

Next, a table of the mean and median step per day accounting for the
imputed missing values:

    m_Mean <- tapply(m_data$steps, m_data$date, mean, na.rm = T)
    m_Median <- tapply(m_data$steps, m_data$date, median, na.rm = T)
    table2 <- cbind(m_Mean, m_Median)
    print(table2, type = "html")

    ##                m_Mean m_Median
    ## 2012-10-01 37.3825996 34.11321
    ## 2012-10-02  0.4375000  0.00000
    ## 2012-10-03 39.4166667  0.00000
    ## 2012-10-04 42.0694444  0.00000
    ## 2012-10-05 46.1597222  0.00000
    ## 2012-10-06 53.5416667  0.00000
    ## 2012-10-07 38.2465278  0.00000
    ## 2012-10-08 37.3825996 34.11321
    ## 2012-10-09 44.4826389  0.00000
    ## 2012-10-10 34.3750000  0.00000
    ## 2012-10-11 35.7777778  0.00000
    ## 2012-10-12 60.3541667  0.00000
    ## 2012-10-13 43.1458333  0.00000
    ## 2012-10-14 52.4236111  0.00000
    ## 2012-10-15 35.2048611  0.00000
    ## 2012-10-16 52.3750000  0.00000
    ## 2012-10-17 46.7083333  0.00000
    ## 2012-10-18 34.9166667  0.00000
    ## 2012-10-19 41.0729167  0.00000
    ## 2012-10-20 36.0937500  0.00000
    ## 2012-10-21 30.6284722  0.00000
    ## 2012-10-22 46.7361111  0.00000
    ## 2012-10-23 30.9652778  0.00000
    ## 2012-10-24 29.0104167  0.00000
    ## 2012-10-25  8.6527778  0.00000
    ## 2012-10-26 23.5347222  0.00000
    ## 2012-10-27 35.1354167  0.00000
    ## 2012-10-28 39.7847222  0.00000
    ## 2012-10-29 17.4236111  0.00000
    ## 2012-10-30 34.0937500  0.00000
    ## 2012-10-31 53.5208333  0.00000
    ## 2012-11-01 37.3825996 34.11321
    ## 2012-11-02 36.8055556  0.00000
    ## 2012-11-03 36.7048611  0.00000
    ## 2012-11-04 37.3825996 34.11321
    ## 2012-11-05 36.2465278  0.00000
    ## 2012-11-06 28.9375000  0.00000
    ## 2012-11-07 44.7326389  0.00000
    ## 2012-11-08 11.1770833  0.00000
    ## 2012-11-09 37.3825996 34.11321
    ## 2012-11-10 37.3825996 34.11321
    ## 2012-11-11 43.7777778  0.00000
    ## 2012-11-12 37.3784722  0.00000
    ## 2012-11-13 25.4722222  0.00000
    ## 2012-11-14 37.3825996 34.11321
    ## 2012-11-15  0.1423611  0.00000
    ## 2012-11-16 18.8923611  0.00000
    ## 2012-11-17 49.7881944  0.00000
    ## 2012-11-18 52.4652778  0.00000
    ## 2012-11-19 30.6979167  0.00000
    ## 2012-11-20 15.5277778  0.00000
    ## 2012-11-21 44.3993056  0.00000
    ## 2012-11-22 70.9270833  0.00000
    ## 2012-11-23 73.5902778  0.00000
    ## 2012-11-24 50.2708333  0.00000
    ## 2012-11-25 41.0902778  0.00000
    ## 2012-11-26 38.7569444  0.00000
    ## 2012-11-27 47.3819444  0.00000
    ## 2012-11-28 35.3576389  0.00000
    ## 2012-11-29 24.4687500  0.00000
    ## 2012-11-30 37.3825996 34.11321

### Are there differences in activity patterns between weekdays and weekends?

First. we'll creat a variable that tell's us the weekday, and then
divide it in "Weekdays" and "Weekend".

    m_data$wd <- wday(m_data$date)
    m_data$wd <- ifelse(m_data$wd == 2 | m_data$wd == 3 | m_data$wd == 4 | m_data$wd == 5 | m_data$wd == 6, "1", "0")
    m_data$wd <- factor(m_data$wd, levels = c(0, 1), labels = c("Weekends", "Weekdays"))

Now, a time-series plot with the average number of steps taken per
5-minute interval comparing weekdays and weekends.

    ggplot(m_data, aes(m_data$interval, m_data$steps)) + geom_line(stat= "summary", fun.y = "mean") + labs(x = "5-minute interval", y = "Average number of steps") + theme_light() + facet_grid(m_data$wd)

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-11-1.png)

There seems to be more steps taken during weekdays than during weekends.

The end.
========

------------------------------------------------------------------------
