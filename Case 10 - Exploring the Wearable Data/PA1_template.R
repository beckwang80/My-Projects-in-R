### Reproducible Research: Peer Assessment 1 (data from a wearable device) ###
### Created by XIANG WANG on July 19, 2015 ###

### Setting global options
echo = TRUE  # Always make code readable

### Loading and processing the data
data <- read.csv("activity.csv", colClasses = c("integer", "Date", "factor"))
head(data)
str(data)
data$month <- as.numeric(format(data$date, format="%m"))
noNA <- na.omit(data)
rownames(noNA) <- 1:nrow(noNA)
head(noNA)
str(noNA)

### What is mean total number of steps taken per day?
# For this part of the assignment, you can ignore the missing values in the dataset.
# Make a histogram of the total number of steps taken each day
library(ggplot2)
ggplot(noNA, aes(date, steps)) + 
  geom_bar(stat = "identity", colour = "blue", fill = "blue", width = 0.9) + 
  facet_grid(. ~ month, scales = "free") + 
  labs(title = "Histogram of Total Number of Steps Taken Each Day", x = "Date", y = "Total number of steps")

### Calculate and report the mean and median total number of steps taken per day

# Mean and Median total number of steps taken per day:
totalSteps <- aggregate(noNA$steps, list(Date = noNA$date), FUN = "sum")$x
mean(totalSteps)
median(totalSteps)

### What is the average daily activity pattern?
# Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
avgSteps <- aggregate(noNA$steps, list(interval = as.numeric(as.character(noNA$interval))), FUN = "mean")
str(avgSteps)
names(avgSteps)[2] <- "meanOfSteps"
ggplot(avgSteps, aes(interval, meanOfSteps)) + 
  geom_line(color = "blue", size = 0.9) + 
  labs(title = "Time Series Plot", x = "5-minute intervals", y = "Average Number of Steps")

# Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
avgSteps[avgSteps$meanOfSteps == max(avgSteps$meanOfSteps), ]

### Imputing missing values
# The total number of rows with NAs:
sum(is.na(data))

# Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
# My strategy is to use the mean for that 5-minute interval to fill each NA value in the steps column.
# Create a new dataset that is equal to the original dataset but with the missing data filled in.

newData <- data 
for (i in 1:nrow(newData)) {
  if (is.na(newData$steps[i])) {
    newData$steps[i] <- avgSteps[which(newData$interval[i] == avgSteps$interval), ]$meanOfSteps
  }
}

str(newData)
head(newData)
sum(is.na(newData))

# Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. 
ggplot(newData, aes(date, steps)) + 
    geom_bar(stat = "identity", colour = "blue", fill = "blue", width = 0.9) + 
    facet_grid(. ~ month, scales = "free") + 
    labs(title = "Histogram of Total Number of Steps Taken Each Day (NAs replaced by mean)", 
         x = "Date", y = "Total number of steps")

### Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

# Mean and median total number of steps taken per day
newTotalSteps <- aggregate(newData$steps, list(Date = newData$date), FUN = "sum")$x
mean(newTotalSteps)
median(newTotalSteps)

# So, after imputing the missing data, the new mean of total steps taken per day is the same as that of the old mean; the new median of total steps taken per day is greater than that of the old median.

### Are there differences in activity patterns between weekdays and weekends?
# Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
newData$weekdays <- factor(format(newData$date, "%A")) # %A for Full weekday
levels(newData$weekdays)
levels(newData$weekdays) <- list(weekday = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"),
                                 weekend = c("Saturday", "Sunday"))
levels(newData$weekdays)
table(newData$weekdays)
str(newData)

# Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).
library(lattice)
avgSteps <- aggregate(newData$steps, list(interval = as.numeric(as.character(newData$interval)), weekdays = newData$weekdays), FUN = "mean")
names(avgSteps)[3] <- "meanOfSteps"
xyplot(avgSteps$meanOfSteps ~ avgSteps$interval | avgSteps$weekdays, layout = c(1, 2), type = "l", xlab = "5-minute interval", ylab = "Average number of steps")
