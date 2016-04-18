#Package loading
require(dplyr)
require(stats)
require(plyr)

#Data Loading
y1 <- read.csv('2011.csv', header = T)
y1$Year <- rep(2011,times=nrow(y1))
y2 <- read.csv('2012.csv', header = T)
y2$Year <- rep(2012,times=nrow(y2))
y3 <- read.csv('2013.csv', header = T)
y3$Year <- rep(2013,times=nrow(y3))
y4 <- read.csv('2014.csv', header = T)
y4$Year <- rep(2014,times=nrow(y4))
y5 <- read.csv('2015.csv', header = T)
y5$Year <- rep(2015,times=nrow(y5))

#One big dataframe is created and grouped by year. Running time ~ 4 mins
df <- rbind(y1,y2,y3,y4,y5)
df <- group_by(df, Year)
head(df)

#Question 1: Fraction of calls of the most common type
calls <- count(df$TypeText)
total <- sum(calls$freq)    #Total number of calls
top <- max(calls$freq)      #Most common call
ratio <- top/total          #calculation of fraction
filter(calls, freq == max(freq))  #'Complaint Other' is the most common call
sprintf("%.10f",ratio)           #printing of the ratio with 10 significant digits

#Time data conversion into POSIX format. Allows to operate on times directly. Runing time ~ 3 mins
df$TimeCreate <- as.POSIXlt(df$TimeCreate, format = "%d/%m/%Y %H:%M:%S %p")
df$TimeDispatch <- as.POSIXlt(df$TimeDispatch, format = "%d/%m/%Y %H:%M:%S %p")
df$TimeArrive <- as.POSIXlt(df$TimeArrive, format = "%d/%m/%Y %H:%M:%S %p")
df$TimeClosed <- as.POSIXlt(df$TimeClosed, format = "%d/%m/%Y %H:%M:%S %p")


#Question 2: Time response variable created to find the median
df$TimeResponse <- df$TimeArrive - df$TimeDispatch
dt <- na.omit(df$TimeResponse)       #Cleaning of unreported or noisy values
dt <- subset(dt, dt>=0)              #Positive times filtered  
sprintf('%.10f', median(dt))         #Print the median

#Question 3: Difference between mean and, max and min reponse times.
#Assuming the maximum value lies in the top quantile of the time response
#distribution and the minimum in the bottom quantile, I calculated the standard deviation, which returns the distance from the mean.
sprintf('%.10f', sd(dt))

#Question 4: Percentage decrease of call type from 2011 to 2015.
#The difference between the frequency of each call in each year is calculated, 
#then it is divided by the value from 2011 to find the ration between 0 and 1.
callType2011 <- select(y1, TypeText)               
callType2015 <- select(y5, TypeText) 
callT2011 <- count(callType2011) 
callT2015 <- count(callType2015) 

Dcall <- merge(callT2011, callT2015, by = 'TypeText')
Dcall$difference <- Dcall$freq.y - Dcall$freq.x
Dcall$ratio <- Dcall$difference/Dcall$freq.x
sprintf('%.10f', min(Dcall$ratio))
filter(Dcall, ratio == min(ratio))                       

#The final answer is a negative value, which accounts for the largest decrease in calls. The call is Crime Against Nature.



