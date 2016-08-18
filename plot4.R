## Plot4.R - Four plots
## Access, read and plot the data following the assignment.


# Default path. If the file is not found, you wil be asked if you want to download the file.
dataPath <- "household_power_consumption.txt"


## Step 1: access the data

# Small check to see if the file exists. If the file does not exists, ask if the user wants to download it
print(paste("Looking for", dataPath))

if (!file.exists(dataPath)) {
    repeat {
        inp <- readline("File has not been found. Do you want to download the file in the folder? Type Y for yes, N for No. ")
        
        if (inp == "Y") {
            
            # Download the zip through a temp file
            print ("Downloading file ...")
            temp <- tempfile()
            fileUrl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
            download.file(fileUrl, temp)
            unzip(zipfile=temp)
            
            # Remove the temp file
            unlink(temp)
            
            break
        }
        else if (inp == "N") {
            print("Please change the dataPath in the code.")
            stop(paste(dataPath, "not found. Exiting."))
        }
    }
}


## Step 2 - Read the data and keep only what is necessary

# If the file exists, proceed with reading only the dates
print(paste("Reading", dataPath))
householdData <- read.delim(file = dataPath, header = TRUE, sep = ";")

# Include only the data from the dates 2007-02-01 and 2007-02-02
dateRange <- (householdData$Date == "1/2/2007" |  householdData$Date == "2/2/2007")

# We have to coerce factors into numeric using a custom function
as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}
householdGAP <- as.numeric.factor(householdData$Global_active_power[dateRange])
householdVoltage <- as.numeric.factor(householdData$Voltage[dateRange])
householdGRP <- as.numeric.factor(householdData$Global_reactive_power[dateRange])
householdSub1 <- as.numeric.factor(householdData$Sub_metering_1[dateRange])
householdSub2 <- as.numeric.factor(householdData$Sub_metering_2[dateRange])
householdSub3 <- householdData$Sub_metering_3[dateRange]        # For some reason this one is already in numeric


# Coerce the data into time
as.POSIXct.factor <- function(time, date) {
    
    # Coerce the time factors and date factors into characters
    time_char <- as.character(levels(time))[time]
    date_char <- as.character(levels(date))[date]
    
    # Vector of empty POSIXct
    time_date_vector <- as.POSIXct(rep(NA, length(time_char)))
    
    for (x in seq(length(time_char))) {
        
        # Concatenate the data and convert it into POSIXct
        date_time <- paste(date_char[x],time_char[x])
        date_time_POSIXct <- as.POSIXct(date_time, format="%d/%m/%Y %H:%M:%S")
        
        # Add current iteration to the vector of POSIXct
        time_date_vector[x] <- date_time_POSIXct
    }
    time_date_vector
}

householdPOSIXct <- as.POSIXct.factor(householdData$Time[dateRange], householdData$Date[dateRange])



## Step 3 - Plot

# Open a connection with the .png
png(file = "plot4.png", bg = "transparent")


# Prepare the four plots
par(mfrow = c(2,2))

# Plot 1 - Global Active Power
plot(householdPOSIXct, householdGAP, type = "l", xlab = "", ylab = "Global Active Power (kilowatts)")

# Plot 2 - Voltage
plot(householdPOSIXct, householdVoltage, type = "l", xlab = "datetime", ylab = "Voltage")

# Plot 3 - Energy sub metering
plot(householdPOSIXct, householdSub1, type = "l", xlab = "", ylab = "Energy sub metering")
lines(householdPOSIXct, householdSub2, type = "l", col = "red")
lines(householdPOSIXct, householdSub3, type = "l", col = "blue")
legend("topright", legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), lty = c(1,1,1), col = c("black", "red", "blue"))

# Plot 4 - Global Reactive Power
plot(householdPOSIXct, householdGRP, type = "l", xlab = "datetime", ylab = "Global_reactive_poower (kilowatts)")


# Close the connection
dev.off()

print("plot4.png has been created. Celebrate with a rabbit.")
cat("  /)_/) \n ( . .) \nC(\")(\") \n")