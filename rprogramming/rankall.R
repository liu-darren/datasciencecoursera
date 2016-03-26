

# assignment 3
# data 	: Outcome of Care Measures.csv
# 			: Hospital Data.csv

rankall <- function(outcome, num = "best") {
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

	b <- data.frame(state = dat$State, hospital = dat$Hospital.Name, measure = dat[, b.col])
	b1 <- b[b$measure != "Not Available", ]
	b1$measure <- as.numeric(as.character(b1$measure))
	b2 <- b1[order(b1$state, b1$measure, b1$hospital), ]

	b.rank <- data.frame(hospital = NA, state = unique(dat$State[order(dat$State)]), row.names = unique(dat$State[order(dat$State)]))

	if (num == "best") {
		for (st in b.rank$state) {
			b.rank[st, 'hospital'] <- ifelse(
				nrow(subset(b2, state == st)) > 0,
				head(as.character(subset(b2, state == st)$hospital), 1),
				NA
			)
		}
	} else if (num == "worst") {
		for (st in b.rank$state) {
			b.rank[st, 'hospital'] <- ifelse(
				nrow(subset(b2, state == st)) > 0,
				tail(as.character(subset(b2, state == st)$hospital), 1),
				NA
			)
		}
	} else {
		for (st in b.rank$state) {
			b.rank[st, 'hospital'] <- ifelse(
				nrow(subset(b2, state == st)) > num,
				as.character(subset(b2, state == st)$hospital) [num],
				NA
			)
		}
	}
	return(b.rank)
}
