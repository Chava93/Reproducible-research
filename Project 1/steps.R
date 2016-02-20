setwd("C:/Users/adrian/Documents/Coursera/Reproducible")
library(lubridate)
library(ggplot2)
## Loading data
activity <- read.csv("activity.csv")

##Histogram of steps
#summing by day
Tot_data <- aggregate(steps ~ date, activity, sum, na.rm = T)

png(filename = "stepsByDay.png", width = 495,height = 357)

 ggplot(Tot_data, aes(day(date),steps, fill = factor(month(date), labels = c("oct", "nov"))))+
        geom_bar(stat= "identity")+ labs(x = "Day", y = "steps")+
        labs(title = "Steps by day") + scale_fill_discrete(name = "Month")
dev.off()


##Plotting mean and median
Mean_data <- aggregate(steps ~ date, activity, mean, na.rm = T)
## we note that all median are 0
Median_data <- aggregate(steps ~ date, activity, median, na.rm = T)

mean_and_median <- merge(Mean_data,Median_data, by = "date")
mean_and_median$month <- month(mean_and_median$date, label = T)

png(filename = "steps mean and median.png")
ggplot(mean_and_median, aes(day(date), steps.x ))+ 
        geom_contour(stat = "identity") + geom_line(aes(y = steps.y, color = "Median" ))+
         labs(x = "Day", y = "steps mean")+ labs(title = "Steps Mean and Median by day")+
        facet_grid(.~month)
dev.off()

## Time series
## creating new columns so its easier to separate by factor
Mean_data$month <- month(Mean_data$date,label = T)
Mean_data$month <- as.factor(Mean_data$month)
Mean_data$day <- day(Mean_data$date)
Mean_data$day <- as.factor(Mean_data$day)


mean_interval <- aggregate( steps ~interval,activity, mean)
mean_interval[mean_interval$steps == max(mean_interval$steps),]
## we can know the coordinates of the highest average-step interval
## 104      835 206.1698 

png(filename = "TimeSeries.png",width = 615, height = 357 )
ggplot(mean_interval, aes( interval, steps ))+ 
        geom_point(stat = "identity")+geom_line()+labs(y = "average steps")+
        labs(title = "Average steps by interval")
        
dev.off()

## Total number of NA´s
sum(is.na(activity$steps))  ## 2304

## we take the median of the day´s steps as the new NA´s
filled_activity <- activity[,]
filled_activity$steps[is.na(filled_activity$steps)] <- 0

mean_filled <- aggregate( steps ~date,filled_activity, mean)
max(mean_filled$steps) ##179.1311
## getting the highest coordinates
mean_filled[mean_filled$steps == max(mean_filled$steps),]
##835 179.1311
ggplot(filled_activity, aes(day(date),steps, fill = factor(month(date), labels = c("oct", "nov"))))+
        geom_bar(stat= "identity")+ labs(x = "Day", y = "steps")+
        labs(title = "Steps by da") + scale_fill_discrete(name = "Month")



boxplot(Mean_data$steps, mean_filled$steps)


## Weekdays

## creating a variable factor that says if the day is weekday or weekend
filled_activity$day <- weekdays(as.Date(filled_activity$date), abbreviate = T)

filled_activity$day <- ifelse(filled_activity$day == "sáb."  |
                                      filled_activity$day == "dom.", "weekend", "weekday")

##filled_activity$day <- as.factor(filled_activity$day)
wend <- filled_activity[filled_activity$day == "weekend",]

wday <- filled_activity[filled_activity$day != "weekend",]

wend_mean <- aggregate(steps~interval , wend, mean)
wday_mean <- aggregate(steps~interval , wday, mean)
#W_mean <- merge(wend_mean,wday_mean, by = "interval")
#names(W_mean) <- c("interval", "wend.steps", "wday.steps")

library(gridExtra)
png(filename = "week_days&ends.png", width = 664, height = 366)
g <- ggplot(wend_mean, aes(interval, steps ))+ geom_point(stat = "identity")+
        geom_line()+labs(y = "average steps")+
        labs(title = "Weekends average steps")
k <- ggplot(wday_mean, aes(interval, steps ))+ geom_point(stat = "identity")+
        geom_line()+labs(y = "average steps")+
        labs(title = "Weekdays average steps")
grid.arrange(k,g, ncol=2)
dev.off()

