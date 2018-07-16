## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----preRequisites, echo=FALSE, message=FALSE, warning=FALSE-------------
# installing packages if needed
if("ggplot2" %in% rownames(installed.packages()) == FALSE) {install.packages("ggplot2")}
if("grid" %in% rownames(installed.packages()) == FALSE) {install.packages("grid")}
if("gridExtra" %in% rownames(installed.packages()) == FALSE) {install.packages("gridExtra")}
if("gtools" %in% rownames(installed.packages()) == FALSE) {install.packages("gtools")}
if("reshape" %in% rownames(installed.packages()) == FALSE) {install.packages("reshape")}
if("purrr" %in% rownames(installed.packages()) == FALSE) {install.packages("purrr")}
# loading libraries
rm(list = ls())
library(ggplot2)
library(grid)
library(gridExtra)
library(gtools)
library(reshape)
library(purrr)

## ----loadData-7.2, echo=TRUE, message=FALSE, warning=FALSE---------------

dataFileName <- "temps.txt"
if (!file.exists(dataFileName)) {
  url1 <- paste0(c("https://prod-edxapp.edx-cdn.org/assets/courseware/v1/592f3be3e90d2bdfe6a69f62374a1250/asset-v1:GTx+ISYE6501x+2T2018+type@asset+block/7.2tempsSummer2018.txt"))
  download.file(url1, dataFileName) }


AtlantaTemperatureData <- read.table(dataFileName, stringsAsFactors = TRUE, header = TRUE )
AtlantaTemperatureData$DAY <- as.Date(AtlantaTemperatureData$DAY, '%e-%b')

## ----exploratoryAnalysis-7.2, echo=TRUE, message=FALSE, warning=FALSE----
meltedtemps <- melt(AtlantaTemperatureData, id = 'DAY')
ggplot(meltedtemps, aes(x = DAY, y = value, colour = variable, group=variable)) +
  geom_line() +
  ylab("Temperature") +
  xlab("Day")

## ----applyCusumModel-CreateCUSUMAndFindingMeanAndSD-7.2, echo=TRUE-------

cusumLocalModel <- function(data, CCoefficient, TCoefficient){
  ans <- data.frame(S=double(), alarm=integer())
  ans[nrow(ans)+1,] <- c(0,0)
  mu <- mean(data)
  std <- sd(data)
  C <- CCoefficient * std
  thresh <- TCoefficient * std
  for (i in 2:length(data)){
    S <- max(0, ans[[i-1,1]] + (mu - data[i] - C))
    alarm <- S > thresh
    ans[nrow(ans)+1,] <- c(S, alarm)
  }
  ans
}
# map_dbl from the purr library is similar to the lapply, sapply class of functions
# but a lot easier to use! Here I use it to iterate on each of the columns (years) and create the mean and sd for the first 30 days.
# no need to run this below anymore since cusumLocalModel calculates the AVE and SD real time for every years data passed, but keeping for search/grep reasons since its a cool fn!
averageForEachYear <- map_dbl(AtlantaTemperatureData[1:30,2:21], mean)
standardDeviationForEachYear <- map_dbl(AtlantaTemperatureData[1:30,2:21], sd)

## ----applyCusumModelRunningTheModelFindingThePredictedDaysOfWinter-7.2, echo=TRUE----

CCoefficient <- 0.7
TCoefficient <- 4

firstPredictedDayOfWinter <- c()

# iterate through each year
for (cnum in 1:(ncol(AtlantaTemperatureData) - 1)){
  tempdat <- AtlantaTemperatureData[,cnum + 1]
  # pass the temperature for each of the 123 days into the cusum model
  # along with the C and T coefficient
  
  ans <- cusumLocalModel(tempdat, CCoefficient, TCoefficient)
  firstDayOfDropRaw <- which(ans$alarm == 1)[1]
  firstPredictedDayOfWinter <- c(firstPredictedDayOfWinter,firstDayOfDropRaw)
}

## ----medianFirstDayCalcAndPrint-7.2, echo=TRUE---------------------------

# taking the median of the first day of coldness:
MedianFirsColdtDay <- floor(median(firstPredictedDayOfWinter, na.rm = TRUE))
MeanFirstColdDay <- floor(mean(firstPredictedDayOfWinter))
# comes out as 91
veryFirstDay <- as.Date("1996-07-01") #first of july is the first day in the atlanta data set

firstPredictedDayOfWinterHumanReadable <- veryFirstDay + MeanFirstColdDay

cat(c("The first day of winter roughly and unofficially is: ",
      format(firstPredictedDayOfWinterHumanReadable, format = "%m/%d"), "\n"))


## ----plottingTheDaysOfWinter-7.2, echo=FALSE-----------------------------
firstPredictedDayOfWinterDataFrame <- data.frame(firstPredictedDayOfWinter)

qplot(seq_along(firstPredictedDayOfWinterDataFrame$firstPredictedDayOfWinter), firstPredictedDayOfWinterDataFrame$firstPredictedDayOfWinter, xlab="Years after 1996", ylab="First cold day of winter - # of Days > 7/1 ", main="CUSUM First trip date versus year")


## ----usingHoltWintersToSeeIfTheresAPattern-7.2, echo=TRUE----------------
unlistTempData <- as.vector(unlist(AtlantaTemperatureData[,2:ncol(AtlantaTemperatureData)]))
# convert to time series 
time_series <- ts(unlistTempData, start=1996, frequency = 123)

 

esModel <- HoltWinters(time_series)

plot(esModel)

cat(c("Alpha: ", as.numeric(esModel$alpha), "\n"))
cat(c("Beta: ", as.numeric(esModel$beta), "\n"))
cat(c("Gamma: ", as.numeric(esModel$gamma), "\n"))
 

## ----fittedPlot-7.2a, echo=TRUE------------------------------------------
plot(fitted(esModel))

## ----usingHoltWintersToSeeIfTheresAPattern-7.2a, echo=TRUE---------------
head(esModel$fitted)

## ----usingHoltWintersToSeeIfTheresAPattern-7.2b, echo=TRUE---------------
# we will use the 4th column and run it through CUSUM..
seasonalFactors <- as.data.frame(matrix(esModel$fitted[,4], ncol = 123))
didSummerComeLater <- c()

for (cnum in 1:(ncol(seasonalFactors))) {
  #seasonal <- seasonalFactors[,cnum]
  # pass the temperature for each of the 123 days into the cusum model
  # along with the C and T coefficient
  cusumOutputForTheSpecificDay <-  cusumLocalModel(seasonalFactors[,cnum], 0.5, 4)
  
  if (any(cusumOutputForTheSpecificDay$alarm == 1)) {
  whichYearDidTheSeasonalComponentChange <- which(grepl(1, cusumOutputForTheSpecificDay$alarm))[1] # [1] since there can be multiple changes on that day across the years 1996:2015 and we just want the first one.
  #whatTheChangeWas <- cusumOutputForTheSpecificDay$S[]
  } else {
    whichYearDidTheSeasonalComponentChange <- 0
  }
  #ans2 <- cusumLocalModel(seasonal, CCoefficient, TCoefficient)
  #whichYearDidTheSeasonalComponentChange <- which(ans2$alarm == 1)
  #cat(c("Count: ", cnum, "\n"))
  didSummerComeLater <- c(didSummerComeLater,whichYearDidTheSeasonalComponentChange)
}
print("the following matrix shows which years each day (July through Oct) there were changes in the seasonal coefficients")
didSummerComeLater

## ----usingHoltWintersToSeeIfTheresAPattern-7.2c, echo=TRUE---------------
scatter.smooth(didSummerComeLater)

## ----load-data-8.2, echo=FALSE, message=FALSE, warning=FALSE-------------

dataFile <- "uscrime.txt"
if (!file.exists(dataFile)) {
  crimeDataURL <- paste0(c("http://www.statsci.org/data/general/uscrime.txt"))
  download.file(crimeDataURL, dataFile) }

crimeDataTable <- read.table(dataFile, header = TRUE )


## ----runGeneralLinearRegressionModel-8.2, echo=TRUE----------------------
model <- lm (Crime ~ . , data = crimeDataTable)
summary(model)
as.data.frame(model$coefficients)


## ----predictingCrime-8.2, echo=TRUE--------------------------------------

# data:
M = 14.0
So = 0
Ed = 10.0
Po1 = 12.0 
Po2 = 15.5
LF = 0.640
M.F = 94.0 
Pop = 150
NW = 1.1
U1 = 0.120
U2 = 3.6 
Wealth = 3200 
Ineq = 20.1 
Prob = 0.04 
Time = 39.0

CrimePredictorData = c(1, M, So, Ed, Po1, Po2, LF, M.F, Pop, NW, U1, U2, Wealth, Ineq, Prob, Time) # pre-pending a "1" to multiply with a0 intercept) 
CrimePredictorCoefficients = array(data = model$coefficients) # need to fix this
PredictedValue = crossprod(CrimePredictorCoefficients, CrimePredictorData)

#this answer ,155, is confirmed by the predict function too:
# predict(model, data.frame(CrimePredictorData))

cat(c("The crime rate (number of offenses per 100,000 population): ",
      as.numeric(round(PredictedValue)), "\n"))


## ----rSquared-8.2, echo=TRUE---------------------------------------------


cat(c("This model explains ",
      round(summary(model)$r.squared, 4), " of the variability in the data\n"))


