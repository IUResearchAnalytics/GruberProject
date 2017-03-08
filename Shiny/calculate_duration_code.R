library(shiny)
library(shinyTime)
library(ggplot2)
library(plyr)
library(data.table)
library(lubridate)
library(scales)
library(RMySQL)

setwd("~/Fitbit_project/Code")

config <- read.table('rshiny.cnf.txt')
# 
mydb <- dbConnect(MySQL(), user=as.character(config[1,2]), password=as.character(config[2,2]),
                  dbname=as.character(config[3,2]), host=as.character(config[4,2]),
                  port = as.integer(as.character(config[5,2])))


alldata <- dbGetQuery(conn = mydb, statement = "SELECT * FROM intens;")

alldata$in_time <-as.POSIXct(alldata$in_time)


# If Intensity large than or equal to 2, then Intensity2 = 1, else Intensity2 = 0
alldata$intensity2 <- ifelse(alldata$intensity >= 2, 1, 0)

# Apply rle() to find Intensity2 >= 1

selectRow <- rle(alldata$intensity2 >= 1)

# Find indices of the selectRow with length of at least 1
index <-which(selectRow$values == TRUE & selectRow$lengths >= 1)

# Check if selectRow has any value in it
any(index)

# Do a communitive sum of the selectRow lengths and extract the end positions of the selectRow with length of at least 1
# using the above found indices
selectRow_lengths_cumsum <- cumsum(selectRow$lengths)

ends <- selectRow_lengths_cumsum[index]

# Find the start position of these selectRow
newindex <- ifelse(index > 1, index -  1, 0)
starts <- selectRow_lengths_cumsum[newindex] + 1
if (0 %in% newindex) starts = c(1,starts)

duration <- as.numeric(difftime(alldata$in_time[ends], alldata$in_time[starts])) / 60

# Calculate average intensity per activiity
avg_intens <- rep(0, length(starts))

for (i in 1:length(starts)){
  avg_intens[i] <- sum(alldata$intensity[starts[i]:ends[i]])/(ends[i]-starts[i]+1)
}

# Extrac data with the number of starts
StartTime <- alldata[starts, ]

EndTime <- alldata$in_time[ends]

DurationData <- cbind(StartTime, EndTime, duration, avg_intens)


write.csv(DurationData, file="DurationData.csv", row.names=FALSE)
