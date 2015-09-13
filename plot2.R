require(ggplot2)
require(scales)

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

graph2 <- function() {
  a <- loadData()
  myplot <- ggplot(data = a, aes(dt_tm, Global_active_power)) + geom_line() + scale_x_datetime(breaks="1 day", labels=date_format("%a"))
  myplot <- myplot + theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank())
  myplot <- myplot + labs(x=NULL, y="Global Active Power (kilowatts)")
  dev.copy(png, file = "plot2.png", width = 480, height = 480)
  dev.off()
}