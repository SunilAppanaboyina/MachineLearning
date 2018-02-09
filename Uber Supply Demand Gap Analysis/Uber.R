

# Loading required libraries
library(stringr)
library(ggplot2)

# Reading data file into a data frame
uber <- read.csv("Uber Request Data.csv",stringsAsFactors = FALSE)

# DATA CHECKING
# Checking number of NA's in each column
sapply(uber,function(y) sum(is.na(y)))
# From the columns where NA values are present we can safely conclude they dont have to be imputed and the data can be used for analysis. 

# Checking number of unique values in each column
sapply(uber,function(y) length(unique(y)))

# Checking for lowercase/uppercase/symbol issues. This will show if the same categorical value is represented in different format.
summary(factor(uber$Pickup.point))
summary(factor(uber$Status))
# Conclusion: no issue with lowercase/uppercase/symbol


# DATA CLEANING & PREPARATION
# On visually examining the Request.timestamp & Drop.timestamp column we can see that some values use "/" whereas other use "-"
# Converting "/" in date to "-" in Request.timestamp. Not doing the same for Drop.timestamp as it is not required for analysis
uber$Request.timestamp <- str_replace_all(uber$Request.timestamp,"/","-")

# Deriving new variable "date" by extracting the date from Request.timestamp 
uber$Request.date <- sapply(uber$Request.timestamp, function(y) str_split_fixed(y," ",2)[1])

# Converting the extracted date to R format
uber$Request.date <- as.Date(uber$Request.date,format = "%d-%m-%Y")

# Deriving new variable "hour" by extracting the hour from Request.timestamp
uber$Request.hour <- sapply(uber$Request.timestamp, function(y) str_split_fixed(y," ",2)[2])
uber$Request.hour <- sapply(uber$Request.hour, function(y) str_split_fixed(y,":",2)[1])
uber$Request.hour <- as.numeric(uber$Request.hour)

# Deriving new variable "weekday" to show the day of the week for a particular date
uber$Request.weekday <- weekdays(uber$Request.date)

# DATA ANALYSIS
# Bar plot to visualise the frequency of requests that get cancelled or show 'no cars available'
ggplot(uber,aes(x=Status,fill=Pickup.point)) + geom_bar() + labs(y="Frequency")
# Conclusion: from the plot we understand that a significant number of requests by customers are not being met due to "No Cars Available" & "Cancelled" by drivers

# Segmented Analysis
# Histogram plot to visualize the "Cancelled" requests during course of the day
# Centers the title of the plot for all the plots
theme_update(plot.title = element_text(hjust = 0.5))
ggplot(data=subset(uber, Status == "Cancelled"),aes(x=Request.hour, fill=Pickup.point)) + geom_histogram(bins = 24,position = "Dodge") + labs(x="Hour of the Day",y="Frequency",title="Distribution of \"Cancelled\" requests during the day")
# Conclusion: from the plot we understand that a very high number of City-Airport request are not being met in the Mornings (5AM - 10AM) 
# compared to other times during the day and compared to Airport-City requests 

# Segmented Analysis
# Histogram plot to visualize the "No Cars Available" requests during course of the day
ggplot(data=subset(uber, Status == "No Cars Available"),aes(x=Request.hour, fill=Pickup.point)) + geom_histogram(bins = 24,position = "Dodge") + labs(x="Hour of the Day",y="Frequency",title="Distribution of \"No Cars Available\" requests during the day")
# Conclusion: from the plot we understand that a very high number of Airport-City request are not being met in the Evenings (4PM - 9PM) 
# compared to other times during the day and compared to City-Airport requests 

# Demand-Supply gap analysis
# Demand = requests (Cancelled + No Cars Available + Trip Completed)
# Supply = cabs available at a particular pickup point at a particular hour during the day (Trip Completed)
uber_supply <- subset(uber, Status == "Trip Completed")
uber_supply$Drop.hour <- sapply(uber_supply$Drop.timestamp, function(y) str_split_fixed(y," ",2)[2])
uber_supply$Drop.hour <- sapply(uber_supply$Drop.hour, function(y) str_split_fixed(y,":",2)[1])
uber_supply$Drop.hour <- as.numeric(uber_supply$Drop.hour)
uber_supply$Drop.point <- ifelse(uber_supply$Pickup.point=="Airport","City","Airport")

# Superimposed bar plots to understand the Demand-Supply gap for the City-Airport requests
ggplot() + geom_bar(data=subset(uber, Pickup.point=="City"),aes(x=Request.hour,fill="Demand")) + geom_bar(data=subset(uber_supply, Drop.point=="City"),aes(x=Drop.hour,fill="Supply")) + scale_fill_manual(name="",values = c("red","green")) + theme(legend.position = "top") + labs(x="Hour of the Day", y="Frequency",title="City-Airport demand-supply gap during the day")
# Conclusion: from the plot we understand that there is a huge gap in Demand-Supply from City-Airport during Morning hours between 5AM-10AM

# Superimposed bar plots to understand the Demand-Supply gap for the Airport-City requests
ggplot() + geom_bar(data=subset(uber, Pickup.point=="Airport"),aes(x=Request.hour,fill="Demand")) + geom_bar(data=subset(uber_supply, Drop.point=="Airport"),aes(x=Drop.hour,fill="Supply")) + scale_fill_manual(name="",values = c("red","green")) + theme(legend.position = "top") + labs(x="Hour of the Day", y="Frequency",title="Airport-City demand-supply gap during the day")
# Conclusion: from the plot we understand that there is a huge gap in Demand-Supply from Airport-City during Evening hours between 4PM-9PM

# Weekday demand analysis
uber$Request.weekday <- factor(uber$Request.weekday,levels = c("Monday","Tuesday","Wednesday","Thursday","Friday"))
ggplot(uber,aes(x=Request.weekday,fill=Status)) + geom_bar() + labs(x="Day of the Week",y="Frequency",title="Weekday demand")
# Conclusion: the demand is nearly same for all the days of the week. The frequency of "Cancelled", "No Cars Available" & "Trip Completed"
# are also nearly the same for all the days of the week.


