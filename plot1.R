#Data munging 
households = read.csv("household_power_consumption.txt", header = TRUE, sep = ";", stringsAsFactors = FALSE)
households$Date = as.Date(households$Date , "%d/%m/%Y")
households$Time = format(households$Time, format="%H:%M:%S")
households$Global_active_power = as.double(households$Global_active_power)
households$Global_reactive_power = as.double(households$Global_reactive_power)
#using data from the dates 2007-02-01 and 2007-02-02
startDate = as.Date("01/02/2007", "%d/%m/%Y")
endDate = as.Date("02/02/2007", "%d/%m/%Y")
subsetData = subset(households, Date >= startDate & Date <= endDate)
#add column of cimbined Time + Date
subsetData = transform(subsetData, 
                       TotalTime = format(paste(subsetData$Date, subsetData$Time,sep = " "),
                                          format="%d/%m/%Y %H:%M:%S"))
#create plots and save them localy
#1 histogram
png("plot1.png", width = 480, height = 480)
with(subsetData, 
     hist(Global_active_power, 
          col = "red", 
          xlab = "Global Active Power (kilowatts)",
          main = "Global Active Power"))
dev.off()
