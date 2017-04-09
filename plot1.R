##Download data from Internet
temp <- tempfile()
download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip",temp)
data <- read.csv(unz(temp, "household_power_consumption.txt"), sep=";")
unlink(temp)

## Load Packages
library(ggplot2)
library(scales)
library(dplyr)
library(tidyr)

## Check Data
str(data)
dim(data)

## Calculate Memory

2075259*9*8

149418648 / (2^20)
# = 142.4967 MB

## Data Wrangling
# Format Date and subset data
Ver <- data
NewDate <- Ver %>% 
  mutate( Date = as.character(Date),
                OK = gsub("/", "-" , Date),
                sample_date = as.Date(OK, format("%d-%m-%Y")),
                Date_Time = paste(sample_date,Time, sep=" ")) %>%
 filter(sample_date == '2007-02-02' | sample_date == '2007-02-01')

NewDate$Date_Time_New <- strptime(NewDate$Date_Time,format = "%Y-%m-%d %H:%M:%S")

#Check how many observations
table(NewDate$sample_date)

#Check class for time and date
class(NewDate$Date_Time_New)

# Write data out and read again to transfer data type
write.csv(NewDate,"/Users/hhj/Documents/Lori/coursera/Check/CourseData.csv")

# read data from work directory 
mydata <- read.csv("/Users/hhj/Documents/Lori/coursera/Check/CourseData.csv")
mydata$Date_Time_New <- strptime(mydata$Date_Time,format = "%Y-%m-%d %H:%M:%S")

# Check missing value
sum(is.na(mydata))

## Plot 1
png(filename="plot1.png", width=480, height=480)
print(hist(mydata$Global_active_power,
           col = "red",
           xlab = "Global Active Power (kilowatts)",
           main = "Global Active Power"))
dev.off()

