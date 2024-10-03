#install.packages(c("lubridate", "dplyr", "ggplot2"))
library(lubridate)
library(dplyr)
library(ggplot2)

weather <- read.csv("/cloud/project/campus_weather.csv", 
                    na.strings="#N/A")
weather$dateF <- mdy_hm(weather$Date)

interval_ <- weather$dateF[-length(weather$dateF)] %--% weather$dateF[-1]
interval_

#set up time intervals in a vector of dates
timeInterval <- function(x) {
  x[-length(x)] %--% x[-1]
}

interval <- timeInterval(weather$dateF)
interval

seqEx <- c(1,4,6)
for(i in 1:6) {
  print(i)
}

for(i in seqEx) {
  print(paste("example", i))
}

#create empty vector w/ character type data
chEx <- character()
for(i in 1:6) {
  chEx[i] <- paste("example", i) 
}
numEx <- numeric()
numEx[1]
num <- 5

#In-Class

#P1

weather$dateF

weather$day <- yday(weather$dateF)
weather$year <- year(weather$dateF)
weather$month <- month(weather$dateF)

jan22 <- weather %>%
  filter(month == 1) %>%
  filter(year == 2022)

janInterval <- timeInterval(jan22$dateF)
rollAveTemp <- numeric()

for(i in 8:nrow(jan22)) {
  rollAveTemp[i] <- mean(jan22$AirTemp[(i-7):i])
}
rollAveTemp
