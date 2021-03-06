## Plot1.R - Global Active Power ~ Frequency
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


## Step 3 - Plot

# Open a connection with the .png
png(file = "plot1.png", bg = "transparent")

# Plot the histogram
hist(householdGAP, col = "red", main = "Global Active Power", xlab = "Global Active Power (kilowatts)", ylab = "Frequency")

# Close the connection
dev.off()

print("plot1.png has been created.")