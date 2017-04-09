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

## Plot 3

# "gather" function to combine three columns to one column.
pre <- mydata %>%
  select(Sub_metering_1,Sub_metering_2,Sub_metering_3,Date_Time_New) %>%
  data.frame()

pre$Date_Time_New <- as.character(pre$Date_Time_New)
combind <- gather(pre,Sub_metering,Values,-Date_Time_New)
combind$Date_Time_New <- strptime(combind$Date_Time,format = "%Y-%m-%d %H:%M:%S")


# Check row number
sapply(combind,length)

png("plot3.png",width=480, height=480)
with(combind, plot(Date_Time_New, Values,type = "l", ylab = "Energy sub metering", xlab = " "))
with(subset(combind, Sub_metering == "Sub_metering_1"), lines(Date_Time_New, Values, col = "black"))
with(subset(combind, Sub_metering == "Sub_metering_2"), lines(Date_Time_New, Values, col = "red"))
with(subset(combind, Sub_metering == "Sub_metering_3"), lines(Date_Time_New, Values, col = "blue"))
legend("topright",lty = 1,merge = TRUE,col = c("red","blue", "blue"), legend = c("Sub_metering_1", "Sub_metering_2","Sub_metering_3"))

dev.off()


