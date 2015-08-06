
library(ggplot2)
library(plyr)

# first read in the data file and do some minor formatting
setwd("C:/Users/David/workspace/lyft/")
lyftData <- read.csv('lyft_data_1.csv')
colnames(lyftData) <- c('id', 'donation', 'timestamp')
lyftData$id <- factor(lyftData$id)

# convert the UNIX timestamps (will assume PST because Lyft is in CA)
lyftData$timestamp <- as.POSIXct(lyftData$timestamp, 
	origin='1970-01-01', tz='US/Pacific')
lyftData$weekday <- factor(weekdays(lyftData$timestamp))

# order each driver's data by time
driverSplits <- split(lyftData, lyftData$id)
lyftDataList <- lapply(driverSplits, function(x) {
	x[order(x$timestamp),]
})
lyftData <- ldply(lyftDataList)
lyftData$.id <- NULL


# ~~~~~~~~~~~~~~~ EXPLORATORY ANALYSIS ~~~~~~~~~~~~~~~ #

# one boxplot of donation values for each unique driver
ggplot(lyftData, aes(x = id, y = donation)) +
	geom_boxplot()
# we see that the donation values are almost identical between drivers
# so we can focus on number / frequency of rides given
avgDonation <- mean(lyftData$donation)

# one scatterplot of donation vs time for each unique driver
ggplot(lyftData, aes(x = timestamp)) +
	geom_histogram() +
	facet_wrap(~ id)


# ~~~~~~~ CALCULATING EXPECTED DRIVER LIFETIME ~~~~~~~ #

# First get some summary statistics related to how long drivers
# keep driving
lastRideRecorded <- max(lyftData$timestamp)

driverSummary <- ddply(lyftData, .(id), summarize,
	numRides = length(timestamp),
	duration = as.numeric(timestamp[numRides] - timestamp[1]),
	lastRide = timestamp[numRides],
	stillActive = as.numeric(lastRideRecorded - lastRide) < 7
)
numStillActive <- sum(driverSummary$stillActive)
numDrivers <- nrow(driverSummary)

# Now we attempt to fit an exponential distribution to the survival
# probabilities observed in the data
t <- 1:83
activeAtT <- sapply(t, function(x) {
	(sum(driverSummary[!driverSummary$stillActive, "duration"] > x) + numStillActive) / 
		numDrivers
})

ggplot(survivalData, aes(x = t, y = log(activeAtT))) + 
	geom_point() + 
	stat_smooth(method = "lm", formula = y ~ x - 1) + 
	ylim(-1, 0)

mod <- lm(log(activeAtT) ~ t - 1, data = survivalData)
summary(mod)

lambda <- -mod$coef[[1]]
expectedLife <- 1 / lambda


# ~~~~~~~ CALCULATING  ~~~~~~~ #