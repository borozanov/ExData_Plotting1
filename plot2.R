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
#2  plot
#Adjust the X axis, we only want three distinct values
xAxis = c("Thu", "Fri", "Sat")
#Place each tick on correspond place i.e. the first/last value of each date
thuVal = subset(subsetData$TotalTime,subsetData$Date == startDate)[1]
friVal = subset(subsetData$TotalTime,subsetData$Date == endDate)[1]
satVal = tail(subset(subsetData$TotalTime,subsetData$Date == endDate),n=1)
xVals = c(thuVal, friVal, satVal)
#plot first the points without showing them and the x axis, 
#then plot the lines between them and add the custom axe
png("plot2.png", width = 480, height = 480)
plot2(subsetData)
dev.off()

plot2 = function(subsetData) {
    with(subsetData,
         plot(TotalTime,Global_active_power, ylab = "Global Active Power (kilowatts)", type="n", xaxt="n"))
    with(subsetData,
         lines(TotalTime,Global_active_power, type = "l"))
    axis(1, at=xVals, labels=xAxis, las=1)    
}
