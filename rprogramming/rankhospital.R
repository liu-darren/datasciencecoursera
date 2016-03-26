
# assignment 3
# data 	: Outcome of Care Measures.csv
# 			: Hospital Data.csv

rankhospital <- function(state, outcome, num = "best") {
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

	b <- subset(dat, State == state) [, c('Hospital.Name', b.col)]
	b1 <- b[b[, b.col] != "Not Available", ]
	
	b.rank <- b1[order(as.numeric(b1[, b.col]), b1[, 'Hospital.Name']), ] $ Hospital.Name

	if (num == "best") {
		num <- 1
	} else if (num == "worst") {
		num <- length(b.rank)
	} else if (num > length(b.rank)) {
		return (NA)
	}
	## Return hospital name in that state with the given rank
	b.answer <- b.rank [num]
	return(b.answer)
	## 30-day death rate
}