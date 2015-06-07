## Function to generate Plot 2 using Household Power Concumption data
## from 2/1/2007 to 2/2/2007 from a zip file or URL

plot2 <- function(zipFile, url = NULL) {
        
        if(!is.null(url)) {
                download.file(url, zipFile, method = "curl")      
        }
        
        allData <- read.table(unz(zipFile, "household_power_consumption.txt"), header = TRUE, sep = ";")
        plotData <- subset(allData, allData$Date == "1/2/2007" | allData$Date == "2/2/2007")
        
        ## Combine Date & Time columns into single set "Plot Time"
        plotTime <- as.POSIXct(paste(plotData$Date, plotData$Time), format = "%d/%m/%Y %H:%M:%S")
        
        png("plot2.png", width=480, height=480)
        
        ## Line plot of Global Active Power along "Plot Time"
        plot(plotTime, as.numeric(as.character(plotData$Global_active_power)), type="l",
             xlab = "", ylab = "Global Active Power (kilowatts)")
        
        dev.off()
        
}