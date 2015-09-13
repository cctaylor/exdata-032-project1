require(ggplot2)
require(scales)
require(grid)

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

graph3 <- function() {
  a <- loadData()
  myplot <- ggplot(data = a, aes(dt_tm)) +
            geom_line(aes(y = Sub_metering_1, colour="Sub_metering_1")) +
            geom_line(aes(y = Sub_metering_2, colour="Sub_metering_2")) +
            geom_line(aes(y = Sub_metering_3, color="Sub_metering_3")) +
            scale_x_datetime(breaks="1 day", labels=date_format("%a")) +
            scale_color_manual(values=c("black", "red", "blue"))
  myplot <- myplot + theme_bw()
  myplot <- myplot  + theme(panel.grid.major = element_blank(),
                          panel.grid.minor = element_blank(),
                          panel.background=element_rect(color="black"),
                          panel.margin=unit(0.0, "cm"),
                          legend.justification=c(1,1),
                          legend.position=c(1,1),
                          legend.title=element_blank(),
                          legend.background=element_rect(color="black"),
                          legend.key=element_blank(),
                          legend.key.size=unit(0.5, "cm"),
                          legend.margin=unit(0.0, "cm"),
                          legend.text=element_text(size=12),
                          axis.text=element_text(size=12),
                          axis.title=element_text(size=12))
  myplot <- myplot + labs(x=NULL, y="Energy sub metering")
  print(myplot, width=480, height=480)
  dev.copy(png, filename="plot3.png")
  dev.off()
}