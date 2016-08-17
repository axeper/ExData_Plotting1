## Plot1.R - Global Active Power ~ Frequency
## Access, read and plot the data following the assignment.


# Default path. If the file is not found, you wil be asked if you want to download the file.
filePath <- "household_power_consumption.txt"



# Small check to see if the file exists. If the file does not exists, ask if the user wants to download it
print(paste("Looking for", filePath))

if (!file.exists(filePath)) {
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
            print("Please change the filePath in the code.")
            stop(paste(filePath, "not found. Exiting."))
        }
    }
}

# If the file exists, proceed with reading only the dates
print(paste("Reading", filePath))
householdData <- read.delim(file = filePath, header = TRUE, sep = ";")

# Include only the data from the dates 2007-02-01 and 2007-02-02
dateRange <- (householdData$Date == "1/2/2007" |  householdData$Date == "2/2/2007")
householdGAP <-  householdData$Global_active_power[dateRange]

# Remove the incomplete data (i.e. data labelled as "?")
good <- !(householdGAP == "?")
householdGAP_cleaned <- householdGAP[good]

# Coerce the data into numeric
householdGAP_cleaned <- as.numeric(householdGAP_cleaned)

# Divide the data by 1000 to get kiloWatts
householdGAP_kW <- householdGAP_cleaned / 1000


# Open a connection with the .png

# Plot the histogram
hist(householdGAP_kW, col = "red", main = "Global Active Power", xlab = "Global Active Power (kiloWatts)", ylab = "Frequency")
# Close the connection

# DONE! Celebrate with a rabbit!
#
#   /)_/)
#  ( . .)
# C(")(")