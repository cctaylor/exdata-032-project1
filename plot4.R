require(ggplot2)
require(scales)
require(grid)
require(gridExtra)

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

graph_a <- function(datafram) {
  a <- datafram
  myplot <- ggplot(data = a, aes(dt_tm, Global_active_power)) + geom_line() + scale_x_datetime(breaks="1 day", labels=date_format("%a"))
  myplot <- myplot + theme_bw()
  myplot <- myplot + theme(panel.grid.major=element_blank(),
                           panel.grid.minor=element_blank(),
                           panel.border=element_blank(),
                           panel.background=element_rect(color="black"))
  myplot <- myplot + labs(x=" ", y="Global Active Power")
}

graph_b <- function(datafram) {
  a <- datafram
  myplot <- ggplot(data = a, aes(dt_tm, Voltage)) + geom_line() + scale_x_datetime(breaks="1 day", labels=date_format("%a")) + scale_y_continuous(breaks=c(234,238,242,246))
  myplot <- myplot + theme_bw()
  myplot <- myplot + theme(panel.grid.major=element_blank(),
                           panel.grid.minor=element_blank(),
                           panel.border = element_blank(),
                           panel.background=element_rect(color="black"))
  myplot <- myplot + labs(x="datetime", y="Voltage")
}


graph_c <- function(datafram) {
  a <- datafram
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
                            legend.background=element_blank(),
                            legend.key=element_blank(),
                            legend.key.size=unit(0.35, "cm"),
                            legend.margin=unit(0.0, "cm"),
                            legend.text=element_text(size=10))
  myplot <- myplot + labs(x=" ", y="Energy sub metering")
}

graph_d <- function(datafram) {
  a <- datafram
  myplot <- ggplot(data = a, aes(dt_tm, Global_reactive_power)) + geom_line() + scale_x_datetime(breaks="1 day", labels=date_format("%a"))
  myplot <- myplot + theme_bw()
  myplot <- myplot + theme(panel.grid.major=element_blank(),
                           panel.grid.minor=element_blank(),
                           panel.border = element_blank(),
                           panel.background=element_rect(color="black"))
  myplot <- myplot + labs(x="datetime", y="Global_reactive_power")
}

graph4 <- function() {
  datafram <- loadData()
  gra <- graph_a(datafram)
  grb <- graph_b(datafram)
  grc <- graph_c(datafram)
  grd <- graph_d(datafram)
  myplot <- grid.arrange(gra, grb, grc, grd, ncol=2)
  print(myplot, width=480, height=480)
  dev.copy(png, filename="plot4.png")
  dev.off()
}