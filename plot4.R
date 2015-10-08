#This file plots four graphs.

#This functions reads data from a file in range [fromString, beforeString) where fromString and beforeString are patterns
# in file "fileName" that are matched at the beginning of the line.
#colClasses: Specifies which colums to read and with what classes. see read.csv for documentation.
#sep: Specifies the field separator, defaults to ";"
#keepHeader: If true, the returned dataframe has headers that are in the first line of "fileName". If false. default
#synthetic headers are used.
#na.strings: Specified NA string, defaults to "?"
readData <- function(fileName, fromString,beforeString,colClasses,keepHeader = TRUE, sep = ';', na.strings = "?") {
  if (keepHeader) {
    names = colnames(read.csv(fileName,sep=sep, nrows =1))
    #print(names)
  }
  con = file(fileName, open = "r")
  #user readLines, currently do not wory about copying overhead when R recopies vectors
  # when it grows from 10,000 to 20,000 etc.
  startPos = -1
  endPos = -1
  startPattern = paste0("^",fromString)
  endPattern = paste0("^",beforeString)
  pos = 1
  while (length (line <- readLines(con, n=1, warn = FALSE)) > 0) {
    if(startPos == -1) {
      if(length(grep(startPattern,line,value = FALSE)) > 0) {
        startPos = pos
      }
    }
    if(length(grep(endPattern,line,value = FALSE)) > 0) {
      endPos = pos
      break
    }
    pos = pos +1
  }
  close(con)
  if(startPos == -1 || endPos == -1) {
    println("Error, cannot locate the date range")
    return
  }
  if(keepHeader) {
    read.csv(fileName, header = FALSE, sep = sep, colClasses = colClasses, col.names = names, 
             skip = startPos-1, nrows = (endPos - startPos), na.strings = na.strings)
  }
  else {
    read.csv(fileName, header = FALSE, sep = sep, colClasses = colClasses, 
             skip = startPos-1, nrows = (endPos - startPos), na.strings = na.strings)
  }
}


#Read in everything except the intensity column
colClasses = c("character","character",NA,NA,NA,"NULL",NA,NA,NA)
twoDayData = readData("household_power_consumption.txt", fromString = "1/2/2007", beforeString = "3/2/2007",colClasses = colClasses)

#Get a POSIX compliant datetime composite from date and time
#This can be directly used as the abscissa.
getDateTime <- function(date,time){
  datetime = paste(date,"T",time, sep="")
  as.POSIXct(strptime(datetime, format='%d/%m/%YT%H:%M:%S'))
}
datetime = getDateTime(twoDayData$Date, twoDayData$Time)
#Plots the graphs
plotActivePower <- function() {
  with(twoDayData, plot(datetime,Global_active_power, xlab ='', ylab="Global Active Power", type="n"))
  with(twoDayData, lines(datetime, Global_active_power))
}


plotVoltage <- function() {
  with(twoDayData, plot(datetime,Voltage, type="n"))
  with(twoDayData, lines(datetime, Voltage))
}

plotReactivePower <- function() {
  with(twoDayData, plot(datetime,Global_reactive_power, type="n"))
  with(twoDayData, lines(datetime, Global_reactive_power))
}


plotCompositeGraph <- function() {
  with(twoDayData, plot(datetime,Sub_metering_1, xlab ='', ylab="Energy sub metering", type="n"))
  with(twoDayData, lines(datetime, Sub_metering_1, col="black"))
  with(twoDayData, lines(datetime, Sub_metering_2, col="red"))
  with(twoDayData, lines(datetime, Sub_metering_3, col="blue"))
  legend("topright", pch = "-", lwd = 1, col = c("black","blue", "red"), legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
}

plotFourGraphs <- function() {
  par(mfrow = c(2,2))
  plotActivePower()
  plotVoltage()
  plotCompositeGraph()
  plotReactivePower()
}
plotFourGraphs() #Do quartz() first, not in the script since may run on windows etc. and this is not the main point of the assignment.
png("plot4.png",width=480, height=480,units="px")
plotFourGraphs()
dev.off()

