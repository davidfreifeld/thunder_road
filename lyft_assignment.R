
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

# order each driver's data by timestamp
driverSplits <- split(lyftData, lyftData$id)
lyftDataList <- lapply(driverSplits, function(x) {
	x[order(x$timestamp),]
})
lyftData <- ldply(lyftDataList)
lyftData$.id <- NULL

lyftData$date <- as.Date(lyftData$timestamp, tz = 'US/Pacific')


# ~~~~~~~~~~~~~~~ EXPLORATORY ANALYSIS ~~~~~~~~~~~~~~~ #

# one boxplot of donation values for each unique driver
ggplot(lyftData[lyftData$id %in% as.character(1:25),], aes(x = id, y = donation)) +
	geom_boxplot()
# we see that the donation values are almost identical between drivers
# so we can focus on number / frequency of rides given
avgDonation <- mean(lyftData$donation)

# one scatterplot of donation vs time for each unique driver
ggplot(lyftData[lyftData$id %in% as.character(26:50),], aes(x = timestamp)) +
	geom_histogram() +
	facet_wrap(~ id)

# First get some summary statistics related to how long drivers
# keep driving
lastRideRecorded <- max(lyftData$date)

driverSummary <- ddply(lyftData, .(id), summarize,
	numRides = length(date),
	firstRide = date[1],
	lastRide = date[numRides],
	duration = as.numeric(lastRide - firstRide) + 1,
	stillActive = as.numeric(lastRideRecorded - lastRide) <= 7,
	uniqueDays = length(unique(date)),
	fractionWorked = uniqueDays / duration,
	ridesPerActiveDay = numRides/uniqueDays,
	maxBreak = as.numeric(max(date[2:numRides] - date[1:(numRides-1)])) - 1
)
rownames(driverSummary) <- driverSummary$id
activeDrivers <- driverSummary$stillActive
numStillActive <- sum(activeDrivers)
numDrivers <- nrow(driverSummary)

ggplot(driverSummary, aes(x = duration, y = fractionWorked)) + 
	geom_point(aes(fill = stillActive, size = ridesPerActiveDay), 
		position = 'jitter', shape = 21) +
	scale_fill_brewer(palette="Set1")


# ~~~~~~~ CALCULATING EXPECTED DRIVER LIFETIME ~~~~~~~ #

# Now we attempt to fit an exponential distribution to the survival
# probabilities observed in the data
t <- 1:83
activeAtT <- sapply(t, function(x) {
	(sum(driverSummary[!activeDrivers, "duration"] > x) + numStillActive) / 
		numDrivers
})
survivalData <- data.frame(t = t, activeAtT = activeAtT)

ggplot(survivalData, aes(x = t, y = log(activeAtT))) + 
	geom_point() + 
	stat_smooth(method = "lm", formula = y ~ x - 1) + 
	ylim(-1, 0)

mod <- lm(log(activeAtT) ~ t - 1, data = survivalData)
summary(mod)

lambda <- -mod$coef[[1]]
expectedLife <- 1 / lambda


# ~~~~~~~~~~~~~~ FINAL DLV CALCULATION ~~~~~~~~~~~~~~~ #

recordedRides <- nrow(lyftData)
recordedRides

aRidesPerActiveDay <- mean(driverSummary[activeDrivers, "ridesPerActiveDay"])
aFractionWorked <- mean(driverSummary[activeDrivers, "fractionWorked"])
futureRides <- aRidesPerActiveDay * aFractionWorked * expectedLife * numStillActive
futureRides

lyftRate <- 0.2

avgRides <- (recordedRides + futureRides) / numDrivers
avgRides

DLV <- avgRides * avgDonation * lyftRate
DLV