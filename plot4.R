## Function to generate four plots using Household Power Concumption data
## from 2/1/2007 to 2/2/2007 from a zip file or URL

plot4 <- function(zipFile, url = NULL) {
        
        if(!is.null(url)) {
                download.file(url, zipFile, method = "curl")      
        }
        
        allData <- read.table(unz(zipFile, "household_power_consumption.txt"), header = TRUE, sep = ";")
        plotData <- subset(allData, allData$Date == "1/2/2007" | allData$Date == "2/2/2007")
        
        ## Combine Date & Time columns into single set "Plot Time"
        plotTime <- as.POSIXct(paste(plotData$Date, plotData$Time), format = "%d/%m/%Y %H:%M:%S")
        
        ## Change classes of values from factor to numeric
        gapNumbers <- as.numeric(as.character(plotData$Global_active_power))
        voltNumbers <- as.numeric(as.character(plotData$Voltage))
        sub1numbers <- as.numeric(as.character(plotData$Sub_metering_1))
        sub2numbers <- as.numeric(as.character(plotData$Sub_metering_2))
        sub3numbers <- as.numeric(as.character(plotData$Sub_metering_3))
        grpNumbers <- as.numeric(as.character(plotData$Global_reactive_power))
        
        png("plot4.png", width=480, height=480)
        
        ## Create a 2x2 matrix to arrange four plot graphs
        par(mfrow=c(2, 2))
        
        ## Line plot of Global Active Power along "Plot Time" (1, 1)
        plot(plotTime, gapNumbers, type="l", xlab = "", ylab = "Global Active Power (kilowatts)")
        
        ## Line plot of Voltage along "Plot Time" (1, 2)
        plot(plotTime, voltNumbers, type="l", xlab = "datetime", ylab = "Voltage")
        
        ## Plot three line graphs with legend, one for each Sub_metering column (2, 1)
        plot(plotTime, sub1numbers, type = "n", xlab = "", ylab="Energy sub metering")
                lines(plotTime, sub1numbers, col = "black")
                lines(plotTime, sub2numbers, col = "red")
                lines(plotTime, sub3numbers, col = "blue")
                legend("topright", c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
                       lty = c(1, 1, 1), col = c("black", "red", "blue"))
                
        ## Line plot of Global Reactive Power along "Plot Time" (2, 2)
        plot(plotTime, grpNumbers, type="l", xlab = "datetime", ylab = "Global_Reactive_Power")
 
        dev.off()
        
}
        
        