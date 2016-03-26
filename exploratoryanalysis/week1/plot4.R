
library(data.table)
setwd('/Users/HOME/GitHub/datasciencecoursera/exploratoryanalysis/week1/')

dat <- fread('household_power_consumption.txt')
dat$Date <- as.Date(dat$Date, "%d/%m/%Y")

png(filename="plot4.png", width = 480, height = 480, units = "px")
par(mfrow=c(2, 2))
# (1, 1)
with(subset(dat, Date %in% c(as.Date(c("2007-02-01", "2007-02-02")))),
	plot(
		strptime(paste(Date, Time), format="%Y-%m-%d %H:%M:%S"),
		as.numeric(Global_active_power),
		ylab = "Global Active Power",
		xlab = "",
		type = 'l'
	)
)
# (1, 2)
with(subset(dat, Date %in% c(as.Date(c("2007-02-01", "2007-02-02")))),
	plot(
		strptime(paste(Date, Time), format="%Y-%m-%d %H:%M:%S"),
		as.numeric(Voltage),
		ylab = "Voltage",
		xlab = "datetime",
		type = 'l'
	)
)
# (2, 1)
with(subset(dat, Date %in% c(as.Date(c("2007-02-01", "2007-02-02")))),
	plot(
		strptime(paste(Date, Time), format="%Y-%m-%d %H:%M:%S"),
		as.numeric(Sub_metering_1),
		col = 'black',
		type = 'l',
		xlab = "",
		ylab = "Energy sub metering"
	)
)
with(subset(dat, Date %in% c(as.Date(c("2007-02-01", "2007-02-02")))),
	lines(
		strptime(paste(Date, Time), format="%Y-%m-%d %H:%M:%S"),
		as.numeric(Sub_metering_2),
		col = 'red'
	)
)
with(subset(dat, Date %in% c(as.Date(c("2007-02-01", "2007-02-02")))),
	lines(
		strptime(paste(Date, Time), format="%Y-%m-%d %H:%M:%S"),
		as.numeric(Sub_metering_3),
		col = 'blue'
	)
)
legend("topright", legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), col=c('black', 'red', 'blue'), lty=c(1,1,1)) # add legend at topleft of graph
# (2, 2)
with(subset(dat, Date %in% c(as.Date(c("2007-02-01", "2007-02-02")))),
	plot(
		strptime(paste(Date, Time), format="%Y-%m-%d %H:%M:%S"),
		as.numeric(Global_reactive_power),
		ylab = "Global_reactive_power",
		xlab = "datetime",
		type = 'l'
	)
)
dev.off()
