library(ggplot2)
library(plyr)
library(lattice)
activity_data <- read.csv("activity.csv")
head(activity_data)
activity_data$DateTime<- as.POSIXct(activity_data$date, format="%Y-%m-%d")
activity_weekday<- weekdays(as.Date(activity_data$date))
activity <- cbind(activity_data,activity_weekday)
head(activity)
summary(activity)
##Histogram of the Total Number of Steps
sum_activity <- with(activity,aggregate(steps,by = list(date), FUN=sum,na.rm = TRUE))
colnames(sum_activity)<- c("Date", "Steps")
hist(sum_activity$Steps, xlab="Steps Taken Per Day", ylab="Frequency", main = "Histogram:Total Number of Steps Taken per Day",col="blue",ylim = c(0,25),breaks = seq(0,25000, by=2500))
##Getting the Mean and Median of Steps taken per Day
mean(sum_activity$Steps)
median(sum_activity$Steps)
##Getting the Average Number of Steps taken per Day
clean <- activity[!is.na(activity$steps),]
interval_aveg <- ddply(clean, .(interval), summarize, Aveg = mean(steps))
head(interval_aveg)
q <- ggplot(interval_aveg, aes(x=interval, y=Aveg))
q + geom_line()+ggtitle("Average Number of Steps per Interval")+xlab("Interval")+ylab("Average Number of Steps")
##Getting the interval that contains the maximun # of Steps
max_Steps <- max(interval_aveg$Aveg)
interval_aveg[interval_aveg$Aveg==max_Steps,1]
##Imputing Missing Data
imputed_steps <- interval_aveg$Aveg[match(activity$interval, interval_aveg$interval)]
imputed_activity <- transform(activity, steps = ifelse(is.na(activity$steps), yes = imputed_steps, no = activity$steps))
imputed_total <- aggregate(steps ~ date, imputed_activity, sum)
colnames(imputed_total)<- c("Date", "Steps")
##Histogram of the Total Number of Steps after Imputing Missing Values
mean(imputed_total$Steps)
median(imputed_total$Steps)
hist(imputed_total$Steps, xlab="Steps", main = "Total Steps per Day with NAs Fixed", col="blue",breaks = seq(0,25000,by=2500))
##Average # of Steps Across Weekdays and Weekends
activity$date <- as.Date(strptime(activity$date, format="%Y-%m-%d"))
activity$dateCategory <- ifelse(activity$activity_weekday %in% c("Saturday", "Sunday"), "Weekend", "Weekday")
activity_by_date <- aggregate(steps~interval + dateCategory, data=activity, FUN=mean, na.rm = TRUE)
plot<- ggplot(activity_by_date, aes(x = interval , y = steps, color = dateCategory)) +
  geom_line() +
  labs(title = "Average Daily Steps Across Weekdays and Weekends", x = "Interval", y = "Average number of steps") +
  facet_wrap(~dateCategory, ncol = 1, nrow=2)
print(plot)