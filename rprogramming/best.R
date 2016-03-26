
# assignment 3
# data 	: Outcome of Care Measures.csv
# 			: Hospital Data.csv

best <- function(state, outcome) {
	## Read outcome data
	dat <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
	## Check that state and outcome are valid
	if (!state %in% dat$State) stop ('invalid state')
	
	if (outcome == 'heart attack') {
		b.col <- 'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack' 
	} else if (outcome == 'heart failure') {
		b.col <- 'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure'
	} else if (outcome == 'pneumonia') {
		b.col <- 'Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia'
	} else stop ('invalid outcome')
	## Return hospital name in that state with lowest 30-day death
	b <- subset(dat, State == state) [, c('Hospital.Name', b.col)]
	b1 <- b[b[, b.col] != "Not Available", ]
	b.min <- min( as.numeric(b1[, b.col]) )
	b.best <- b1[as.numeric(b1[, b.col]) == b.min, ]$Hospital.Name
	## rate
	b.answer <- b.best[order(b.best)] [1]
	return(b.answer)
}