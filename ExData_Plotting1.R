
#1.Download the file using the URL and store it in a zip file in the directory
download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip", dest= "Household_Power_Consumption.zip")

#2. Unzip the downloaded file
unzip("Household_Power_Consumption.zip")

#3.Parse the file and read it as csv, limit the number of rows as per the requirement
file<-read.csv("Household_Power_Consumption.txt",header=T, sep=';', na.strings="?", 
               nrows=2075259, check.names=F, stringsAsFactors=F, comment.char="", quote='\"')

# Can be used to check the info of the file read above, and commented for now
#head(file)

#4. A collection of the names of the files that will have the code for all the plots
RFileNames<- c("Plot1.R","Plot2.R","Plot3.R","Plot4.R")

#4. A collection of the names of the png files that will have all the plots
PNGFileNames<- c("Plot1.png","Plot2.png","Plot3.png","Plot4.png")


#5. Take the range of the data within the given dates
rangeofData<- subset(file, Date %in% c("1/2/2007","2/2/2007"))

#6. Format the Date column usinf as.Date in the desired format
rangeofData$Date <- as.Date(rangeofData$Date, format="%d/%m/%Y")

#7. create a variable with Data and Time columns together
datetime <- paste(as.Date(rangeofData$Date), rangeofData$Time)

#8. Convert the dates according to POSXct notation
rangeofData$Datetime <- as.POSIXct(datetime)


#9. A function that will take the file name as input and will call the corresponding function for that file
DrawAllPlots<- function(FileName)
{
        #(i) Check if the file is the first in the list provided above
        if(FileName == RFileNames[1])
        {
                #a. Call the Plot function for the corresponding file
                DrawPlot1(FileName)
               
        }
        #(ii) Check if the file is the second in the list provided above
        else if(FileName == RFileNames[2])
        {
                #b. Call the Plot function for the corresponding file
                DrawPlot2(FileName)
        }
        #(iii) Check if the file is the third in the list provided above
        else if(FileName == RFileNames[3])
        {
                #c. Call the Plot function for the corresponding file
                DrawPlot3(FileName)
        }
        #(iv) Check if the file is the fourth in the list provided above
        else if(FileName == RFileNames[4])
        {
                #d. Call the Plot function for the corresponding file
                DrawPlot4(FileName)
        }
        
        #(v) If we try to call the DrawAllPlots function with wrong file name
        else
        {
                #e. Give the error to the user
             stop("Please provide a valid file name")   
        }
        
}

#10. Funtion to draw plot for File plot1.R
DrawPlot1<- function(fileName)
{
        # Check if the file exists       
        if(!file.exists(fileName))
        {
                # Create the file if it does not exist
                file.create(fileName)
        }

#11. Connection object for the file 
        fileConn<-file(fileName)
        # Variable to plot the graph according to the question
        plot1<- deparse(substitute(hist(rangeofData$Global_active_power, main="Global Active Power", 
                                xlab="Global Active Power (kilowatts)", ylab="Frequency", col="Red")))

        # Write the above variable in the file plot1.R 
        writeLines(plot1, fileConn)
        # Close the connection
        close(fileConn)

# Read the file from the memory using the source function
        source(fileName)
# Plot the graph using the plot function written in the file to the PNG file
        dev.copy(png, PNGFileNames[1],width=480, height=480)
# Turn off the device
        dev.off()
}

#12. Funtion to draw plot for File plot2.R
DrawPlot2<- function(fileName)
{
        # Check if the file exists    
        if(!file.exists(fileName))
        {
                # Create the file if it does not exist
                file.create(fileName)
        }
#13. Connection object for the file 
        
        fileConn<-file(fileName)
        # Variable to plot the graph according to the question
        plot2<- deparse(substitute(with(rangeofData, {
                plot(Global_active_power~Datetime, type="l",
                     ylab="Global Active Power (kilowatts)", xlab="")
        })))
        
        # Write the above variable in the file plot2.R     
        writeLines(plot2, fileConn)
        # Close the connection
        close(fileConn)
        
# Read the file from the memory using the source function
        source(fileName)
# Plot the graph using the plot function written in the file to the PNG file
        dev.copy(png, PNGFileNames[2],width=480, height=480)
# Turn off the device
        dev.off()
}

#14. Funtion to draw plot for File plot3.R
DrawPlot3<- function(fileName)
{ 
        # Check if the file exists   
        if(!file.exists(fileName))
        {
                # Create the file if it does not exist
                file.create(fileName)
        }
        
#15. Connection object for the file         
        fileConn<-file(fileName)
# Variable to plot the graph according to the question
        plot3<- deparse(substitute(with(rangeofData, {
                plot(Sub_metering_1~Datetime, type="l",
                     ylab="Global Active Power (kilowatts)", xlab="")
                lines(Sub_metering_2~Datetime,col='Red')
                lines(Sub_metering_3~Datetime,col='Blue')
        })))
        # Legends for the plot     
      plot3legend <- deparse(substitute(legend("topright", col=c("black", "red", "blue"), lty=1, lwd=2, 
               legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))))
      
      #unused code, so commented
    #  plot3<- paste(plot3,plot3legend, sep=" ")
        
      # Write the above variable in the file plot3.R  
        writeLines(plot3,fileConn)
#Concatenate the legend variable with the file, starting on the new line
        cat(plot3legend, file= fileName, append=TRUE,sep="\n")
#Close the connection
        close(fileConn)
        
# Read the file from the memory using the source function
        source(fileName)
# Plot the graph using the plot function written in the file to the PNG file
        dev.copy(png, PNGFileNames[3],width=480, height=480)
# Turn off the device
        dev.off()
}

#16. Funtion to draw plot for File plot4.R
DrawPlot4<- function(fileName)
{
        # Check if the file exists   
        if(!file.exists(fileName))
        {
                # Create the file if it does not exist
                file.create(fileName)
        }
        
#17. Connection object for the file  
        fileConn<-file(fileName)
# Variable to set the location parameters for the plot
        plot4par<- deparse(substitute(par(mfrow=c(2,2), mar=c(4,4,2,1), oma=c(0,0,2,0))))
# Variable to plot the graph according to the question
        plot4<- deparse(substitute(with(rangeofData, {
                plot(Global_active_power~Datetime, type="l", 
                     ylab="Global Active Power (kilowatts)", xlab="")
                plot(Voltage~Datetime, type="l", 
                     ylab="Voltage (volt)", xlab="")
                plot(Sub_metering_1~Datetime, type="l", 
                     ylab="Global Active Power (kilowatts)", xlab="")
                lines(Sub_metering_2~Datetime,col='Red')
                lines(Sub_metering_3~Datetime,col='Blue')
                legend("topright", col=c("black", "red", "blue"), lty=1, lwd=2, bty="n",
                       legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
                plot(Global_reactive_power~Datetime, type="l", 
                     ylab="Global Rective Power (kilowatts)",xlab="")
        })))
        # Write the par in the file plot4.R  
        writeLines(plot4par, fileConn)
#Concatenate the legend variable with the file, starting on the new line
        cat(plot4, file= fileName, append=TRUE,sep="\n")
        #Close the connection
        close(fileConn)
        
   # Read the file from the memory using the source function
        source(fileName)
        # Plot the graph using the plot function written in the file to the PNG file
        dev.copy(png, PNGFileNames[4],width=480, height=480)
        # Turn off the device
        dev.off()
}

# Loop through the collection of file names
for(i in RFileNames)
{
        # Call the DrawAllPlots function for each file name in the collection
        DrawAllPlots(i)
}
