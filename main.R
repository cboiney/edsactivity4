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

#gathering the rolling averages
for(i in 8:nrow(jan22)) {
  rollAveTemp[i] <- mean(jan22$AirTemp[(i-7):i])
}
rollAveTemp
start_times

#plot of all times
ggplot(jan22, aes(x = dateF, y = AirTemp)) + geom_line()

starting_times <- jan22$dateF[8:nrow(jan22)]

rolling_df <- data.frame(Average = rollAveTemp[8:nrow(jan22)], StartTime = starting_times)

#plot of averages
ggplot(rolling_df, aes(x = StartTime, y = Average)) + geom_line()

#P2


#P3



#HOMEWORK PROBLEMS

#Q1

clinton_data <- weather %>%
  filter((abs(XLevel) < 2) & (abs(YLevel) < 2)) %>%
  filter(AirTemp > 0)

#number of data points we removed
num_data <- nrow(weather) - nrow(clinton_data)

num_data

count <- 0

for(i in 1:nrow(clinton_data)) {
  if(is.na(clinton_data$Precip[i])) {
    count = count + 1
  }
}


#Q2

weather$batteryFlag <- ifelse(weather$BatVolt < 8.5, 
                              1, 
                              0)
#Q3

#function that checks generally if values are extreme outliers (number
# 4 is somewhat arbitrary but also chosen w/ size of dataset in mind)
check_for_unusual_values <- function(x) {
  avg = mean(x)
  dev = sd(x)
  for (i in 1:nrow(x)) {
    if (((abs(x[i]-avg))/sd) > 4) {
      return(TRUE)
    }
  }
  return(FALSE)
}

#checking for unusual air temperature values across variable
check_for_unusual_airtemp <- function(x) {
  for (i in 1:nrow(x)) {
    if (x[i] > 50 | x[i] < -40) {
      #could also increment a count here
      return(TRUE)
    }
  }
  return(FALSE)
}

#checking for unusual air temperature values in one cell
check_for_unusual_airtemp <- function(x) {
  if (x > 50 | x < -40) {
    return(TRUE)
  }
  return(FALSE)
}

#checking for unusual radiation values across variable
check_for_unusual_solrad <- function(x) {
  for (i in 1:nrow(x)) {
    if (x[i] > 1000 | x[i] < 0) {
      #could also increment a count here
      return(TRUE)
    }
  }
  return(FALSE)
}

#checking for unusual radiation values in one cell
check_for_unusual_solrad <- function(x) {
  if (x > 1000 | x < 0) {
    return(TRUE)
  }
  return(FALSE)
}

#similarly, we could do this with ifelse and flags
weather$unusualtemp <- ifelse(weather$AirTemp > 50 | weather$AirTemp < -40, 
                              1, 
                              0)

weather$unusualradiation <- ifelse(weather$SolRad > 1000 | weather$SolRad < 0, 
                              1, 
                              0)


#Q4

janmar <- weather %>%
  filter(month < 4) %>%
  filter(year == 2021)

ggplot(janmar, aes(x = dateF, y = AirTemp)) + geom_line() + labs(x = "Date", 
          title = "Air Temperatures in Clinton, Early 2021")


#Q5

total_precip <- 0

weather$AirTempFahren <- weather$AirTemp*(9/5)+32

marapr <- weather %>%
  filter(month == 3 | month == 4) %>%
  filter(year == 2021)

max(marapr$day)
min(marapr$day)

#going through each day
for(i in min(marapr$day):max(marapr$day)) {
  #take sub-array for the current day
  current_day <- marapr %>%
    filter(day == i)
  #if we aren't dealing with the last day
  if(i != max(marapr$day)) {
    day_valid = TRUE
    #go through each measurement of the current day
    for(j in 1:nrow(current_day)) {
      #if temps are too low at any measurement
      if(current_day$AirTempFahren[j] < 35) {
        day_valid = FALSE
      }
    }
    #if the current day has a temperature that's too low, we add NAs to its temp
    #entries, as well as those of the previous day
    if(day_valid == FALSE) {
      for(k in 1:nrow(marapr)) {
        #if we are in a the current day or the previous 
        if(marapr$day[k] == i | marapr$day[k] == i+1) {
          #change precipitation to NA
          marapr$Precip[k] = NA
        }
      }
    }
  }
  #if we are dealing with the last day
  else {
    day_valid = TRUE
    for(j in 1:nrow(current_day)) {
      if(current_day$AirTempFahren[j] < 35) {
        day_valid = FALSE
      }
    }
    if (day_valid == FALSE) {
      for(k in 1:nrow(marapr)) {
        if(marapr$day[k] == i) {
          marapr$Precip[k] = NA
        }
      }
    }
  }
}

count <- 0

for(i in 1:nrow(marapr)) {
  if(is.na(marapr$Precip[i])) {
    count = count + 1
  }
}

count
