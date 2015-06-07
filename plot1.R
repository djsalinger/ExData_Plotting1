## Function to generate Plot 1 using Household Power Concumption data
## from 2/1/2007 to 2/2/2007 from a zip file or URL

plot1 <- function(zipFile, url = NULL) {
        
        if(!is.null(url)) {
                download.file(url, zipFile, method = "curl")      
        }
        
        allData <- read.table(unz(zipFile, "household_power_consumption.txt"), header = TRUE, sep = ";")
        plotData <- subset(allData, allData$Date == "1/2/2007" | allData$Date == "2/2/2007")
        
        ## Change class of Global Active Power values from factor to numeric
        gapNumbers <- as.numeric(as.character(plotData$Global_active_power))
        
        png("plot1.png", width=480, height=480)
        
        ## Create histogram of color red of Global Active Power
        hist(gapNumbers, col = "red", main = "Global Active Power", xlab = "Global Active Power (kilowatts)")
        
        dev.off()
        
}