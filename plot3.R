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
#3 plot
png("plot3.png", width = 480, height = 480)
plot3(subsetData)
dev.off()

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
    legend("topright", col=c("black","red", "blue"), lwd = 1, 
           legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))   
}