# Preparatory in making the plots
  ## Unzipping the file, reading it, converting the Date variable into Date class using the as.Date() function
  ## Sub setting the data from the dates 2007-02-01 and 2007-02-02, and storing it to the data frame, df.
data_file <- unzip("household_power_consumption_data.zip", exdir = ".")
data <- read.table(data_file, header=T, sep=";")
data$Date <- as.Date(data$Date, format="%d/%m/%Y")
df <- data[(data$Date=="2007-02-01") | (data$Date=="2007-02-02"),]
  ## Since an error occur ("x must be numeric") when we directly subset the variables from df, we need to use the as.numeric() function to convert them into numeric value
  ## The transform() function is use to transform the data frame , df
  ## By adding a column, Full_Date, which is formed through as.POSIXCT() function where pasting the Date and Time variables as the argument
df$Global_active_power <- as.numeric(df$Global_active_power)
df$Global_reactive_power <- as.numeric(df$Global_reactive_power)
df$Voltage <- as.numeric(df$Voltage)
df$Sub_metering_1 <- as.numeric(df$Sub_metering_1)
df$Sub_metering_2 <- as.numeric(df$Sub_metering_2)
df$Sub_metering_3 <- as.numeric(df$Sub_metering_3)
df <- transform(df, Full_Date=as.POSIXct(paste(Date, Time)), "%d/%m/%Y %H:%M:%S")

## Generating the plot1.png
  ### Plot1() function
  ### Creating a histogram on Global Active Power using the hist() function with the specific arguments to be included
  ### Note: The "red" color was being changed to "dark pink" using the hexadecimal color code (just for personal preference and exploration)
  ### The dev.copy() function copies the graphics contents of the plot to the device/file (PNG) being specified and with specific dimensions
  ### The dev.off() function closes the PNG file device
plot1<-function(){
  hist(df$Global_active_power, main = paste("Global Active Power"), col = "#FF3366", xlab="Global Active Power (kilowatts)")
  dev.copy(png, file="plot1.png", width=480, height=480)
  dev.off()
}
plot1()

## Generating the plot2.png
  ### Plot2() function
  ### Creating a plot on Global Active Power using the generic X-Y plotting function, plot, and specifying the needed arguments  
  ### The dev.copy() function copies the graphics contents of the plot to the device/file (PNG) being specified and with specific dimensions
  ### The dev.off() function closes the PNG file device
plot2<-function(){
  plot(df$Full_Date, df$Global_active_power,  type="l", xlab="", ylab="Global Active Power (kilowatts)")
  dev.copy(png, file="plot2.png", width=480, height=480)
  dev.off()
}
plot2()

## Generating the plot3.png
### Plot3() function
  ### Creating a plot on Sub-metering 1 and inserting line graphs of Sub-metering 1 and 2 with the specified arguments
  ### Inserting a legend using the legend() function
  ### The dev.copy() function copies the graphics contents of the plot to the device/file (PNG) being specified and with specific dimensions
  ### The dev.off() function closes the PNG file device.
plot3<-function(){
  plot(df$Full_Date, df$Sub_metering_1, type="l", xlab="", ylab="Energy sub metering")
  lines(df$Full_Date, df$Sub_metering_2,col="red")
  lines(df$Full_Date, df$Sub_metering_3,col="blue")
  legend("topright", col=c("black","red","blue"), c("Sub_metering_1  ","Sub_metering_2  ", "Sub_metering_3  "),lty=1, lwd=1, cex= 0.5)
  dev.copy(png, file="plot3.png", width=480, height=480)
  dev.off()
}
plot3()

## Generating the plot4.png
  ### Plot4() function
  ### Creating 1 plot (PNG file) containing 4 graphs/plots with the use of par function being the mfrow, or number of plots per row, column (plots are filled row-wise), as the argument
plot4<- function(){
  par(mfrow=c(2,2), mar= c(5,5,1,2), oma=c(0,0,0,0)) ### Setting the plot area into 2 by 2
  ### Graph 1
  plot(df$Full_Date,df$Global_active_power, type="l", xlab="", ylab="Global Active Power")
  ### Graph 2
  plot(df$Full_Date,df$Voltage, type="l", xlab="datetime", ylab="Voltage")
  ### Graph 3
  plot(df$Full_Date,df$Sub_metering_1, type="l", xlab="", ylab="Energy sub metering")
  lines(df$Full_Date,df$Sub_metering_2,col="red")
  lines(df$Full_Date,df$Sub_metering_3,col="blue")
  legend("topright", col=c("black","red","blue"), c("Sub_metering_1  ","Sub_metering_2  ", "Sub_metering_3  "),lty=c(1,1), bty="n", cex= 0.5) # bty removes the box, cex adjusts the size of the text
  ### Graph 4
  plot(df$Full_Date,df$Global_reactive_power, type="l", xlab="datetime", ylab="Global_reactive_power")
  ### Copying the graphics contents of the plot to the device/file (PNG) and closing the PNG file device, using the dev.copy() and dev.off functions, respectively.
  dev.copy(png, file="plot4.png", width=480, height=480)
  dev.off()
}
plot4()
