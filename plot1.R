loadData <- function() {
  oldDir <- getwd()
  setwd("~/DataScienceCoursera/exdata-032-project1")
  a <- read.table("household_power_consumption.txt", sep=";", header = TRUE, na.strings="?")
  setwd(oldDir)
  a <- a[a$Date %in% c("1/2/2007", "2/2/2007"),]
  dt_tm <- strptime(paste(a$Date, a$Time), "%d/%m/%Y %H:%M:%S")
  a <- cbind(dt_tm, a)
  names(a[,1]) <- "dt_tm"
  a
}

graph1 <- function() {
  a <- loadData()
  hist(a$Global_active_power, breaks = 12, main = "Global Active Power", xlab = "Global Active Power (kilowatts)", col = "red")
  dev.copy(png, file = "plot1.png", width = 480, height = 480)
  dev.off()
}