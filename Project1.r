#########1. Code for reading in the dataset and/or processing the data

activity <- read.csv("activity.csv")

Sys.setlocale("LC_TIME", "English")

library(ggplot2)

library(dplyr)

str(activity)

############2. Histogram of the total number of steps taken each day

StepsPerDay <- aggregate(activity$steps, list(activity$date), FUN=sum)
colnames(StepsPerDay) <- c("Date", "Steps")
StepsPerDay

# draw the histogram

png("Plot1.png")

g <- ggplot(StepsPerDay, aes(Steps))
g+geom_histogram(boundary=0, binwidth=2500, col="yellowgreen", fill="lightgreen")+ggtitle("Total Number of STeps Taken")+xlab("Steps")+ylab("Frequency")+theme(plot.title = element_text(face="bold", size=12))+scale_x_continuous(breaks=seq(0,25000,2500))+scale_y_continuous(breaks=seq(0,18,2))


dev.off()

#########3. Mean and median number of steps taken each day

mean(StepsPerDay$Steps, na.rm=TRUE)
median(StepsPerDay$Steps, na.rm=TRUE)

########4. Time series plot of the average number of steps taken 


StepsPerTime <- aggregate(steps~interval,data=activity,FUN=mean,na.action=na.omit)
# variable time (more comprensible for the graph axis)
StepsPerTime$time <- StepsPerTime$interval/100
# draw the line plot
png("Plot2.png")

h <- ggplot(StepsPerTime, aes(time, steps))
h+geom_line(col="brown")+ggtitle("Average steps per time interval")+xlab("Time")+ylab("Steps")+theme(plot.title = element_text(face="bold", size=12))

dev.off()

# table for dplyr
ST <- tbl_df(StepsPerTime)
# find the column
ST %>% select(time, steps) %>% filter(steps==max(ST$steps))

## Source: local data frame [1 x 2]
## 
##    time    steps
##   (dbl)    (dbl)
## 1  8.35 206.1698




##################6. Code to describe and show a strategy for imputing missing data

###Total Missing Values

ACT <- tbl_df(activity)

ACT %>% filter(is.na(steps)) %>% summarize(missing_values = n())

######Replace missing values

activity$CompleteSteps <- ifelse(is.na(activity$steps), round(StepsPerTime$steps[match(activity$interval, StepsPerTime$interval)],0), activity$steps)

####New dataset that is equal to the original dataset but with the missing data filled in

activityFull <- data.frame(steps=activity$CompleteSteps, interval=activity$interval, date=activity$date)

head(activityFull, n=10)


###########7. Histogram of the total number of steps taken each day after missing values are imputed

StepsPerDayFull <- aggregate(activityFull$steps, list(activityFull$date), FUN=sum)
colnames(StepsPerDayFull) <- c("Date", "Steps")


png('Plot3.png)

g <- ggplot(StepsPerDayFull, aes(Steps))
g+geom_histogram(boundary=0, binwidth=2500, col="brown", fill="lightblue")+ggtitle("Steps per Day")+xlab("Steps")+ylab("Frequency")+theme(plot.title = element_text(face="bold", size=12))+scale_x_continuous(breaks=seq(0,25000,2500))+scale_y_continuous(breaks=seq(0,26,2))

dev.off()

mean(StepsPerDayFull$Steps)

median(StepsPerDayFull$Steps)


################8. Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends


activityFull$RealDate <- as.Date(activityFull$date, format = "%Y-%m-%d")

activityFull$weekday <- weekdays(activityFull$RealDate)

activityFull$DayType <- ifelse(activityFull$weekday=='Saturday' | activityFull$weekday=='Sunday', 'weekend','weekday')

head(activityFull, n=10)

####

StepsPerTimeDT <- aggregate(steps~interval+DayType,data=activityFull,FUN=mean,na.action=na.omit)

StepsPerTimeDT$time <- StepsPerTime$interval/100


png("Plot4.png")
j <- ggplot(StepsPerTimeDT, aes(time, steps))
dev.off()
j+geom_line(col="darkred")+ggtitle("Average steps per time interval: weekdays vs. weekends")+xlab("Time")+ylab("Steps")+theme(plot.title = element_text(face="bold", size=12))+facet_grid(DayType ~ .)





