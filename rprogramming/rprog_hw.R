setwd('/Users/HOME/Github/datasciencecoursera/rprog/')

# assignment 1
pollutantmean <- function(directory = "specdata", pollutant = "sulfate", id = 1:332) {
	base_dir <- "/users/HOME/downloads/"
	readings <- vector()
	for (i in id) {
		if (i < 10) {
			read_in <- read.csv(paste(base_dir, directory, "/00", i, ".csv", sep=""))[, pollutant]
		} else if (i < 100) {
			read_in <- read.csv(paste(base_dir, directory, "/0", i, ".csv", sep=""))[, pollutant]
		} else {
			read_in <- read.csv(paste(base_dir, directory, "/", i, ".csv", sep=""))[, pollutant]
		}
		readings <- append(readings, read_in)
	}
	round(mean(readings, na.rm=TRUE), 3)
}

complete <- function(directory = "specdata", id = 1:332) {
	base_dir <- "/users/HOME/downloads/"
	observes <- data.frame(id=vector(), nobs=vector())
	for (i in id) {
		if (i < 10) {
			read_in <- read.csv(paste(base_dir, directory, "/00", i, ".csv", sep=""))
		} else if (i < 100) {
			read_in <- read.csv(paste(base_dir, directory, "/0", i, ".csv", sep=""))
		} else {
			read_in <- read.csv(paste(base_dir, directory, "/", i, ".csv", sep=""))
		}
		obs_comp <- nrow(read_in[!is.na(read_in$sulfate) & ! is.na(read_in$nitrate), ])
		obs_new <- data.frame(id=i, nobs=obs_comp)
		observes <- rbind(observes, obs_new)
	}
	observes
}

corr <- function(directory = "specdata", threshold = 0) {
	base_dir <- "/users/HOME/downloads/"
	id <- 1:332
	correlations <- vector()
	for (i in id) {
		if (i < 10) {
			read_in <- read.csv(paste(base_dir, directory, "/00", i, ".csv", sep=""))
		} else if (i < 100) {
			read_in <- read.csv(paste(base_dir, directory, "/0", i, ".csv", sep=""))
		} else {
			read_in <- read.csv(paste(base_dir, directory, "/", i, ".csv", sep=""))
		}
		read_comp <- read_in[!is.na(read_in$sulfate) & ! is.na(read_in$nitrate), ]
		if (nrow(read_comp) > threshold) {
			correlations <- append(correlations, round(cor(read_comp$nitrate, read_comp$sulfate), 5))
		}
	}
	correlations
}

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

# HW submission code 
ghID <- "97512ed5d66ddee1a5ff"
ghSecret <- "78dcb35fc642f1f7c400f86482a9c6be7b24db8f"

library(httr)

# 1. Find OAuth settings for github:
#    http://developer.github.com/v3/oauth/
oauth_endpoints("github")

# 2. Register an application at https://github.com/settings/applications;
#    Use any URL you would like for the homepage URL (http://github.com is fine)
#    and http://localhost:1410 as the callback url
#
#    Insert your client ID and secret below - if secret is omitted, it will
#    look it up in the GITHUB_CONSUMER_SECRET environmental variable.
myapp <- oauth_app("github", ghID, ghSecret)

# 3. Get OAuth credentials
github_token <- oauth2.0_token(oauth_endpoints("github"), myapp)

# 4. Use API
gtoken <- config(token = github_token)
req <- GET("https://api.github.com/rate_limit", gtoken)
stop_for_status(req)
content(req)

req <- GET("https://api.github.com/users/jtleek/repos", gtoken)
json1 <-content(req)
json2 <- jsonlite::fromJSON(toJSON(json1))

# json2 is now in data frame form
# found by eye ball of description column
json2[5, 'created_at']


temp<-url("http://biostat.jhsph.edu/~jleek/contact.html")
htmlCode<-readLines(temp)
close(temp)

nchar(htmlCode[10])
nchar(htmlCode[20])
nchar(htmlCode[30])
nchar(htmlCode[100])

x <- read.fwf(
  file=url("http://www.cpc.ncep.noaa.gov/data/indices/wksst8110.for"),
  skip=4,
  widths=c(12, 7,4, 9,4, 9,4, 9,4)
  )


