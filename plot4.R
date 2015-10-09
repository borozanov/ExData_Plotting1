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
#4 combined plots
png("plot4.png", width = 480, height = 480)
par(mfcol = c(2, 2), mar = c(4, 4, 2, 1), oma = c(0, 0, 2, 0))
plot2(subsetData)
plot3(subsetData)
plot4.1(subsetData)
plot4.2(subsetData)
dev.off()

plot2 = function(subsetData) {
    with(subsetData,
         plot(TotalTime,Global_active_power, ylab = "Global Active Power (kilowatts)", type="n", xaxt="n"))
    with(subsetData,
         lines(TotalTime,Global_active_power, type = "l"))
    axis(1, at=xVals, labels=xAxis, las=1)    
}

plot3 = function(subsetData) {
    #submeeting 1
    with(subsetData,
         plot(TotalTime, Sub_metering_1, xaxt="n", type="n",xlab="", ylab = "Energy sub meeting"))
    with(subsetData,
         lines(TotalTime,Sub_metering_1, type = "l"))
    #submeeting 2
    with(subsetData,
         lines(TotalTime,Sub_metering_2, type = "l", col="red"))
    #submeeting 3
    with(subsetData,
         lines(TotalTime,Sub_metering_3, type = "l", col="blue"))
    axis(1, at=xVals, labels=xAxis, las=1)
    #wheather to print OuterBox or not
    legend("topright", col=c("black","red", "blue"), lwd = 1, bty = "n",
           legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3")) 
    }
}

plot4.1 = function(subsetData) {
    with(subsetData,
         plot(TotalTime,Voltage, xlab = "datetime", ylab = "Voltage", type="n", xaxt = "n"))
    with(subsetData,
         lines(TotalTime,Voltage, type = "l"))
    axis(1, at=xVals, labels=xAxis, las=1)   
}

plot4.2 = function(subsetData) {
    with(subsetData,
         plot(TotalTime,Global_reactive_power,xlab = "datetime", ylab = "Global_reactive_power", type="n", xaxt="n"))
    with(subsetData,
         lines(TotalTime,Global_reactive_power, type = "l"))
    axis(1, at=xVals, labels=xAxis, las=1)    
}