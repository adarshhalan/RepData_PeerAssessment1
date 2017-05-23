## set working directory to the folder containing the raw data
##setwd("~/activity data")

## load relevant packages
library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)

## read file

data <- read.csv("activity.csv", header=TRUE, sep=",")

## Convert date 
data$date <- as.Date(data$date)


steps <- aggregate(steps~date, data, sum, na.rm = TRUE)
p <- plot(steps$date, steps$steps, type = "h", lwd = 8, 
          xlab = "Date", ylab = "Steps per Day", col = "darkgray", 
          main = "Total Steps per Day") + abline(h = median(steps$steps, na.rm = TRUE), col="red", lwd=2)

meansteps <- mean(steps$steps,na.rm = TRUE)
mediansteps <- median(steps$steps, na.rm = TRUE)
 
paste("Mean Steps = ", meansteps)
paste("Median Steps =", mediansteps)

summary(steps)


steps1 <- aggregate(steps~interval, data, mean, na.rm = TRUE)

##plotgraph
q <- plot(steps~interval, data = steps1, col="blue", type = "l", 
          main = "Average Daily Activity Pattern")
abline(h=mean(steps1$steps, na.rm=TRUE), col="red", lwd=2)

maxinterval <- steps1[which.max(steps1$steps),]
paste("5-Minute interval with Max no of Steps = ", maxinterval$interval, 
      "with no of steps = ", round(maxinterval$steps))

# Calculate number of rows in activity data set with NA rows
stepsna <- sum(is.na(data$steps))

data2 <- data
nadata <- is.na(data2$steps)
avg_interval <- tapply(data2$steps, data2$interval, mean, na.rm=TRUE, simplify = TRUE)
data2$steps[nadata] <- avg_interval[as.character(data2$interval[nadata])]
names(data2)
## Check for no-NA
sum(is.na(data2))

steps2 <- aggregate(steps~date, data2, sum, na.rm = TRUE)

stepsdata1 <- group_by(data, date) %>% summarise(sum_steps = sum(steps),
                                                  mean_steps = mean(steps), median_steps = median(steps))

stepsdata2 <- group_by(data2, date) %>% summarise(sum_steps = sum(steps),
              mean_steps = mean(steps), median_steps = median(steps))


q1 <- plot(stepsdata2$date, stepsdata2$sum_steps, type = "h", lwd = 8, 
           xlab = "Date", ylab = "Steps per Day", col = ifelse(stepsdata2$median_steps>0,'red','darkgrey'), 
           main = "Total Steps per Day (w/o NA's)") + 
        abline(h = mean(stepsdata2$sum_steps, na.rm = TRUE), col="blue", lwd=2)

meansteps1 <- mean(data2$steps,na.rm = TRUE)
mediansteps1 <- median(data2$steps, na.rm = TRUE)

paste("Mean Steps = ", meansteps1)
paste("Median Steps =", mediansteps1)
data2<- data2 %>% 
    mutate(day= ifelse(weekdays(data2$date)=="Saturday" | weekdays(data2$date)=="Sunday", "Weekend", "Weekday"))

data2$day <- as.factor(data2$day)

summary(stepsdata1)
summary(stepsdata2)

## Mean and median values are almost identical, but the quantiles are significantly different.

## add a column to new data with day as weekday or weekend

plotdata <- data2 %>% group_by(interval, day) %>% 
    summarise(steps = mean(steps))
s <- ggplot(plotdata, aes(x=interval, y=steps, color = day)) +
    geom_line() + facet_wrap(~day, ncol = 1, nrow=2) + 
    labs(title = "Avg. Daily Steps by Day type", x = "Interval", y = "No. of Steps") 

print(s)


## Ans: During the weekday, the test object is more active earlier in the day 
## but the object is more active throughout the weekends probably because the oject is 
## working during the weekdays, hence moving less during the day.