## Function to generate Plot 3 using Household Power Concumption data
## from 2/1/2007 to 2/2/2007 from a zip file or URL

plot3 <- function(zipFile, url = NULL) {
        
        if(!is.null(url)) {
                download.file(url, zipFile, method = "curl")      
        }
        
        allData <- read.table(unz(zipFile, "household_power_consumption.txt"), header = TRUE, sep = ";")
        plotData <- subset(allData, allData$Date == "1/2/2007" | allData$Date == "2/2/2007")
        
        ## Combine Date & Time columns into single set "Plot Time"
        plotTime <- as.POSIXct(paste(plotData$Date, plotData$Time), format = "%d/%m/%Y %H:%M:%S")
        
        ## Change class of Sub_metering values from factor to numeric
        sub1numbers <- as.numeric(as.character(plotData$Sub_metering_1))
        sub2numbers <- as.numeric(as.character(plotData$Sub_metering_2))
        sub3numbers <- as.numeric(as.character(plotData$Sub_metering_3))
        
        png("plot3.png", width=480, height=480)
        
        ## Plot three line graphs with legend, one for each Sub_metering column
        plot(plotTime, sub1numbers, type = "n", xlab = "", ylab="Energy sub metering")
                lines(plotTime, sub1numbers, col = "black")
                lines(plotTime, sub2numbers, col = "red")
                lines(plotTime, sub3numbers, col = "blue")
                legend("topright", c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
                       lty = c(1, 1, 1), col = c("black", "red", "blue"))
        
        dev.off()
                
}