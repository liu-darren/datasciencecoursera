
library(data.table)
setwd('/Users/HOME/GitHub/datasciencecoursera/exploratoryanalysis/week1/')

dat <- fread('household_power_consumption.txt')
dat$Date <- as.Date(dat$Date, "%d/%m/%Y")

png(filename="plot2.png", width = 480, height = 480, units = "px")
with(subset(dat, Date %in% c(as.Date(c("2007-02-01", "2007-02-02")))),
	plot(
		strptime(paste(Date, Time), format="%Y-%m-%d %H:%M:%S"),
		as.numeric(Global_active_power),
		ylab = "Global Active Power (kilowatts)",
		xlab = "",
		type = 'l'
	)
)
dev.off()
