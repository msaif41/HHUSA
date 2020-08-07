#install.packages("tidyverse")
library(caret)
library(pROC)
library(readxl)
library(lubridate)
library(dplyr)
library(ggplot2)

# Read data
df <- read_excel('Downloads/Group Project Option 2 and 3 HHUSA/EPO_Teradata_Employer_Profile_Creation_Report.xlsx')
dftime <- subset(df, select = c(created_at))
str(dftime)
summary(dftime)

dftime$created_at <- as.POSIXct(dftime$created_at,
                                format = "%Y-%m-%d %H:%M:%S",
                                tz = "UTC")
str(dftime$created_at)
summary(dftime$created_at)
head(dftime$created_at)
tz(dftime$created_at)

dftime$created_in_year <- year(dftime$created_at)
dftime.grp.year <- group_by(dftime,created_in_year)
tally(dftime.grp.year)
year_created <- factor(dftime$created_in_year)
ggplot(dftime,aes(x=year_created))+geom_bar()

dftime2018 <- subset(dftime,created_in_year == 2018)
summary(dftime2018)

dftime2018$created_in_month <- month(dftime2018$created_at)
dftime2018.grp.month <- group_by(dftime2018,created_in_month)
tally(dftime2018.grp.month)
ggplot(dftime2018,aes(x=created_in_month))+geom_bar()+scale_x_discrete(limits=c("1","2","3","4","5","6","7","8","9","10","11","12"))

dftime2018$created_on_day <- day(dftime2018$created_at)
dftime2018.grp.day <- group_by(dftime2018,created_on_day)
tally(dftime2018.grp.day)
ggplot(dftime2018,aes(x=created_on_day))+geom_bar()+scale_x_continuous(breaks = seq(1,31, by = 1))

dftime2018$created_on_dayofweek <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")[as.POSIXlt(dftime2018$created_at)$wday + 1]
#dftime2018$created_at_dayofweek <- weekdays(as.Date(dftime2018$created_at)) # this works too
#df
dftime2018.grp.dayofweek <- group_by(dftime2018,created_on_dayofweek)
tally(dftime2018.grp.dayofweek)
ggplot(dftime2018,aes(x=created_on_dayofweek))+geom_bar()+scale_x_discrete(limits=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

dftime2018$created_at_hour <- hour(dftime2018$created_at)
dftime2018.grp.hour <- group_by(dftime2018,created_at_hour)
tally(dftime2018.grp.hour)
ggplot(dftime2018,aes(x=created_at_hour))+geom_histogram(bins=24, binwidth=.5)+scale_x_continuous(breaks = seq(0,23, by=1))
