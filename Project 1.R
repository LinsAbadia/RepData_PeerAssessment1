#global parameters for knitr so you don't need to keep doing it locally   
knitr::opts_chunk$set(echo = TRUE, warning = FALSE)

#dev.off()

#1.  Loading and preprocessing the data
#Show any code that is needed to

#`1.1  Load the data (i.e. \color{red}{\verb|read.csv()|}read.csv())
activity <- read.csv("activity.csv")
#head(activity)
#dim(activity) #17568 x 3


# 1.2.  Process/transform the data (if necessary) into a format suitable for your analysis
# datw column is processed in Step 8 for lattice plotting


#What is mean total number of steps taken per day?
#  For this part of the assignment, you can ignore the missing values in the dataset.

#2.  Calculate the total number of steps taken per day

#12 * 24 #288 intervals per day

library(dplyr)

#'Partition' data into sum of steps per day
stepsPerDay <- activity %>%
  group_by(date) %>%
  summarise(steps= sum(steps))

#dim(stepsPerDay) #61 (ayys) x 2
#head(stepsPerDay) #cols: date steps (sumO

hist(stepsPerDay$steps, xlab = "Number of Steps per day", 
     ylab="Number of Days", 
     main="Steps taken each day (NAs excluded)")

#make png
dev.copy(png, filename = "plotQ1(Steps Per Day).png", height = 480, width = 480)
dev.off()

 
# If you do not understand the difference between a histogram and a barplot, 
#research the difference between them. Make a histogram of the total number of steps taken each day

#3.  Calculate and report the mean and median of the total number of steps taken per day
sPDMean <- mean(stepsPerDay$steps, na.rm = TRUE) # mean of steps 
#class(sPDMean) #numeric
#length(sPDMean)#1


sPDMean #10,766.19
tempStr <- "The daily step mean is "
print(paste(tempStr, sPDMean))

sPDMedian <- median(stepsPerDay$steps, na.rm = TRUE)# median 

sPDMedian #10,765
tempStr <- "The daily step median is "
print(paste(tempStr, sPDMedian))


#What is the average daily activity pattern?
#4.  Make a time series plot (i.e.  of the 5-minute interval (x-axis) and
#the average number of steps taken, averaged across all days (y-axis)

#'Partition' data into means of steps per interval
stepsPerInterval <- activity %>%
  group_by(interval) %>%
  summarise(steps = mean(steps, na.rm = TRUE))
#head(stepsPerInterval) #col:  interval steps(mean)
#dim(stepsPerInterval) #288 x 2

plot(steps~interval, data=stepsPerInterval, type="l")


#make png
dev.copy(png, filename = "plotQ4(Steps Per Interval).png", height = 480, width = 480)
dev.off()


#5.  Which 5-minute interval, on average across all the days in the dataset, 
#    contains the maximum number of steps?

#get interval with largest mean of steps
sPIMax <- stepsPerInterval[which.max(stepsPerInterval$steps),]$interval

sPIMax #835
tempStr <- "Interval with the maximum step average is "
print(paste(tempStr, sPIMax))


#6.  Imputing missing values
#Note that there are a number of days/intervals where there are missing values (coded as NA). 
#The presence of missing days may introduce bias into some calculations or summaries of the data

#6.1.  Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with \color{red}{\verb|NA|}NAs)
NACount <- sum(is.na(activity$steps))

NACount#2304
tempStr <- "The numer of NAs are "
print(paste(tempStr, NACount))


#6.2. Devise a strategy for filling in all of the missing values in the dataset. 
#The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

#get the mean (previously compted and used)
#head(stepsPerInterval)


#6.3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
activityImputed <-activity

lastRow <- nrow(activityImputed)

#head(activityImputed)
NACount <- sum(is.na(activityImputed$steps))

NACount#2304
tempStr <- "The number of NAsin activityImputed are (before)"
print(paste(tempStr, NACount))

for (DFRow in 1:lastRow) {
  if(is.na(activityImputed[DFRow, ]$steps))
  {
    activityImputed[DFRow, ]$steps <- sPDMean/288#getSPIMean(activityImputed[DFRow, ]$interval)
  }  
}
    
#head(activityImputed)
NACount <- sum(is.na(activityImputed$steps))

NACount#
tempStr <- "The number of NAs in activityImputed are (after)"
print(paste(tempStr, NACount))


#7.  Make a histogram of the total number of steps taken each day 
#   and Calculate and report the mean and median total number of steps taken per day. 
#   Do these values differ from the estimates from the first part of the assignment? 
#   What is the impact of imputing missing data on the estimates of the total daily number of steps?  

#'Partition' data into sum of steps per day (with no NA)
stepsPerDayImputed <- activityImputed %>%
  group_by(date) %>%
  summarise(steps = sum(steps))

#head(stepsPerDayImputed)

dev.off()
hist(stepsPerDayImputed$steps, xlab = "Number of Steps per day (NAs removed)", 
     ylab="Number of Days", 
     main="Steps taken each day (No NA)")

#make png
dev.copy(png, filename = "plotQ7(Steps Per Day with NAs removed).png", height = 480, width = 480)
dev.off()

#8.  Are there differences in activity patterns between weekdays and weekends?
#  For this part the weekdays() function may be of some help here. 
#  Use the dataset with the filled-in missing values for this part.

#create a copy of original dataframe
#activityImputed <- activity


#8.1.`Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating 
#whether a given date is a weekday or weekend day.

#head(activityImputed)
#summary(activityImputed)

#'reformat' ehe date and transform it's data type from factor to dat
library(lubridate)
activityImputed$date<-dmy(activityImputed$date)
#head(activityImputed)

activityImputed$day <- weekdays(activityImputed$date)

#head(activityImputed)

lastRow <- nrow(activityImputed)

for (dfRow in 1:lastRow) {
  if (activityImputed[dfRow, ]$day %in% c("Saturday","Sunday")) {
    activityImputed[dfRow, ]$day <-"weekend"
  }
  else{
    activityImputed[dfRow,]$day <-"weekday"
  }
}


#8.2  Make a panel plot containing a time series plot (i.e.type = "l") 
#    of the 5-minute interval (x-axis) and the average number of steps taken, 
#    averaged across all weekday days or weekend days (y-axis). 
#    See the README file in the GitHub repository to see an example of what this plot should look like
#   using simulated data.
#names(stepsByDay) <- c("interval", "day", "steps")

#plot((steps~interval, data=aIWeekday, type="l")
#
#par(mfrow=c(2, 1))    # set the plotting area into a 1*2 array

dev.off


#head(activityImputed)



#make sure steps are 'imputed.'
lastRow <- nrow(activityImputed)
for (DFRow in 1:lastRow) {
  if(is.na(activityImputed[DFRow, ]$steps))
  {
    activityImputed[DFRow, ]$steps <- sPDMean/288#getSPIMean(activityImputed[DFRow, ]$interval)
  }  
}
#head(activityImputed)
activityImputedPlot <-activityImputed %>%
  group_by(day, interval) %>%
  summarize(steps = mean(steps))

#head(activityImputedPlot)
  

dev.off()
library(lattice)

dev.off()
#head(activityImputedPlot)
xyplot(steps ~ interval | day, activityImputedPlot, type = "l", layout = c(1, 2), 
       xlab = "Interval", ylab = "Number of steps")


#make png
dev.copy(png, filename = "plotQ8(Steps Per Day (with NAs removed and by type of day).png")
dev.off()

