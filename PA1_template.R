##----Load Data----

activity_raw <- read.csv("activity.csv", stringsAsFactors=FALSE)

##----Process Data----

activity_raw$date <- as.POSIXct(activity_raw$date, format="%Y-%m-%d")

activity_raw <- data.frame(date=activity_raw$date, 
                           weekday=tolower(weekdays(activity_raw$date)), 
                           steps=activity_raw$steps, 
                           interval=activity_raw$interval)

activity_raw <- cbind(activity_raw, 
                      daytype=ifelse(activity_raw$weekday == "saturday" | 
                                       activity_raw$weekday == "sunday", "weekend", 
                                     "weekday"))

activity <- data.frame(date=activity_raw$date, 
                       weekday=activity_raw$weekday, 
                       daytype=activity_raw$daytype, 
                       interval=activity_raw$interval,
                       steps=activity_raw$steps)

##----Steps Histogram----

sum_data <- aggregate(activity$steps, by=list(activity$date), FUN=sum, na.rm=TRUE)

names(sum_data) <- c("date", "total")

hist(sum_data$total, 
     breaks=seq(from=0, to=25000, by=2500),
     col="blue", 
     xlab="Total number of steps", 
     ylim=c(0, 20), 
     main="Histogram of the total number of steps taken each day\n(NA removed)")

##----Mean and Median Steps---

mean(sum_data$total)

median(sum_data$total)

##----Time Plot----

mean_data <- aggregate(activity$steps, 
                       by=list(activity$interval), 
                       FUN=mean, 
                       na.rm=TRUE)

names(mean_data) <- c("interval", "mean")

plot(mean_data$interval, 
     mean_data$mean, 
     type="l", 
     col="blue", 
     lwd=2, 
     xlab="Interval [minutes]", 
     ylab="Average number of steps", 
     main="Time-series of the average number of steps per intervals\n(NA removed)")

##----Highest Interval----

max_pos <- which(mean_data$mean == max(mean_data$mean))

max_interval <- mean_data[max_pos, 1]

max_interval

##----Missing Values----

NA_count <- sum(is.na(activity$steps))

##----Replace Values----

na_pos <- which(is.na(activity$steps))

mean_vec <- rep(mean(activity$steps, na.rm=TRUE), times=length(na_pos))

activity[na_pos, "steps"] <- mean_vec

##----Revised Histogram----

sum_data <- aggregate(activity$steps, by=list(activity$date), FUN=sum)

names(sum_data) <- c("date", "total")

hist(sum_data$total, 
     breaks=seq(from=0, to=25000, by=2500),
     col="blue", 
     xlab="Total number of steps", 
     ylim=c(0, 30), 
     main="Histogram of the total number of steps taken each day\n(NA replaced by mean value)")

##----Revised Mean and Median----

mean(sum_data$total)

median(sum_data$total)

##----Weekday/Weekend Differences----

library(lattice)

mean_data <- aggregate(activity$steps, 
                       by=list(activity$daytype, 
                               activity$weekday, activity$interval), mean)

names(mean_data) <- c("daytype", "weekday", "interval", "mean")

xyplot(mean ~ interval | daytype, mean_data, 
       type="l", 
       lwd=1, 
       xlab="Interval", 
       ylab="Number of steps", 
       layout=c(1,2))