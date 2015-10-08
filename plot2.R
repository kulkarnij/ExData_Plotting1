#This file plots the Global active power line graph.

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



#Read in, date, time and active power columns. Cast date and time to "character" to avoid factors
colClasses = c("character","character",NA,"NULL","NULL","NULL","NULL","NULL","NULL")
twoDayData = readData("household_power_consumption.txt", fromString = "1/2/2007", beforeString = "3/2/2007",colClasses = colClasses)

#Get a POSIX compliant datetime composite from date and time
#This can be directly used as the abscissa.
getDateTime <- function(date,time){
  datetime = paste(date,"T",time, sep="")
  as.POSIXct(strptime(datetime, format='%d/%m/%YT%H:%M:%S'))
}
#Plots the graph
plotLineGraph <- function() {
  with(twoDayData, plot(getDateTime(Date,Time),Global_active_power, xlab ='', ylab="Global Active Power (kilowatts)", type="n"))
  with(twoDayData, lines(getDateTime(Date,Time), Global_active_power))
}
plotLineGraph()  #Do quartz() before this. Not put in the script since script may run on windows/linux.
png("plot2.png",width=480, height=480,units="px")
plotLineGraph()
dev.off()