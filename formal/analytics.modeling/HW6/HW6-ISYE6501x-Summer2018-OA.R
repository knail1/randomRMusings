## ----preRequisites, echo=FALSE, message=FALSE, warning=FALSE-------------
# installing packages if needed
if("ggplot2" %in% rownames(installed.packages()) == FALSE) {install.packages("ggplot2")}
#if("grid" %in% rownames(installed.packages()) == FALSE) {install.packages("grid")}
#if("gridExtra" %in% rownames(installed.packages()) == FALSE) {install.packages("gridExtra")}
#if("gtools" %in% rownames(installed.packages()) == FALSE) {install.packages("gtools")}
#if("reshape" %in% rownames(installed.packages()) == FALSE) {install.packages("reshape")}
if("purrr" %in% rownames(installed.packages()) == FALSE) {install.packages("purrr")}
if("kknn" %in% rownames(installed.packages()) == FALSE) {install.packages("kknn")}

# loading libraries
rm(list = ls())
library(ggplot2)
#library(grid)
#library(gridExtra)
#library(gtools)
#library(reshape)
library(purrr)
library(kknn)

## ----loadData-13.2, echo=TRUE, message=FALSE, warning=FALSE--------------

dataFile <- "breast.cancer.data.txt"
if (!file.exists(dataFile)) {
  #crimeDataURL <- paste0(c("http://www.statsci.org/data/general/uscrime.txt"))
  bcURL <- paste0(c("http://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/breast-cancer-wisconsin.data"))
  
  download.file(bcURL, dataFile) }

breastCancerTable <- read.csv(dataFile, header = FALSE, na.strings = "\\?" )


## ----nameCols-13.1, echo=TRUE, message=FALSE, warning=FALSE--------------
colnames(breastCancerTable) <- c("sampleCodeNumber",
                                 "clumpThickness",
                                 "cellSizeUniformity",
                                 "cellShapeUniformity",
                                 "marginalAdhesion",
                                 "singleEpithelialCellSize",
                                 "bareNuclei",
                                 "blandChromatin",
                                 "normalNucleoli",
                                 "Mitoses",
                                 "Class")

  head(breastCancerTable, 3)

## ----fillingMeanModeInMissing-13.1, echo=TRUE, message=FALSE, warning=FALSE----
getMode <- function(v) {
   uniqv <- unique(v)
   uniqv[which.max(tabulate(match(v, uniqv)))]
}

breastCancerTable[which(breastCancerTable$bareNuclei == "?"),]

errors <- breastCancerTable[which(breastCancerTable$bareNuclei == "?"), ]
percentNoData <- length(errors)/length(breastCancerTable$bareNuclei)

#storing the indices of missing data:
missingDataIndices <- which(breastCancerTable$bareNuclei == "?", arr.ind = TRUE)

#find mode of clean data:
mode_bareNuclei <- as.numeric(getMode(breastCancerTable[-missingDataIndices, "bareNuclei"]))
#find mean:
mean_bareNuclei <- round(mean(as.numeric(breastCancerTable[-missingDataIndices, "bareNuclei"])))

mean_bareNuclei

#gsub("\\?","22",breastCancerTable[322,])
#breastCancerTable[322,]


## ----fillingMeanModeInMissing-13.1b, echo=TRUE, message=FALSE, warning=FALSE----
breastCancerTableImputedByMean <- breastCancerTable
breastCancerTableImputedByMean[missingDataIndices, ]$bareNuclei <- as.integer(mean_bareNuclei)


## ----compareCurrentWithImputedMean-13.1c, echo=TRUE, message=FALSE, warning=FALSE----
head(breastCancerTable[which(breastCancerTable$bareNuclei == "?"), ], 3)


## ----compareCurrentWithImputedMean-13.1, echo=TRUE, message=FALSE, warning=FALSE----
#head(breastCancerTable[which(breastCancerTable$bareNuclei == "?"), ], 3)
breastCancerTableImputedByMean[24,]
breastCancerTableImputedByMean[41,]
breastCancerTableImputedByMean[140,]


## ----regressionImputation-13.1a, echo=TRUE, message=FALSE, warning=FALSE----
bcPredictors <- breastCancerTable[-missingDataIndices, 2:10]
# converting to integer so the lm can write back into this column without error
bcPredictors$bareNuclei <- as.integer(bcPredictors$bareNuclei)

# running the linear regression, to predict the bareNuclei  values (note its predicting ALL the values , including non missing ones, so we'll have to pick out the indices of the observations with the missing ones out of this )
bareNucleiLRM <- lm(bareNuclei~., data = bcPredictors)
summary(bareNucleiLRM)

## ----regressionImputation-13.1b, echo=TRUE, message=FALSE, warning=FALSE----
bareNucleiLRM <- lm(bareNuclei ~ clumpThickness + cellShapeUniformity + marginalAdhesion + normalNucleoli, data = bcPredictors)
summary(bareNucleiLRM)

## ----regressionImputation-13.1c, echo=TRUE, message=FALSE, warning=FALSE----
bareNucleiPredicted <- predict(bareNucleiLRM, newdata = breastCancerTable[missingDataIndices, ])
breastCancerTableImputedByRegression <- breastCancerTable
breastCancerTableImputedByRegression[missingDataIndices, ]$bareNuclei <- round(bareNucleiPredicted)
breastCancerTableImputedByRegression$bareNuclei <- as.integer(breastCancerTableImputedByRegression$bareNuclei)
breastCancerTableImputedByRegression[missingDataIndices, ]$bareNuclei

## ----regressionImputation-13.1d, echo=TRUE, message=FALSE, warning=FALSE----
#make sure no bareNuclei values are outside of original range:
breastCancerTableImputedByRegression$bareNuclei[breastCancerTableImputedByRegression$bareNuclei > 10 ] <-10
breastCancerTableImputedByRegression$bareNuclei[breastCancerTableImputedByRegression$bareNuclei < 1 ] <-1



## ----regressionImputation-13.1.3a, echo=TRUE, message=FALSE, warning=FALSE----
# first, creating a normal distribution
bareNucleiPerturbed <- rnorm(nrow(breastCancerTable[missingDataIndices,]), bareNucleiPredicted, sd(bareNucleiPredicted))

bareNucleiPerturbed

## ----regressionImputation-13.1.3b, echo=TRUE, message=FALSE, warning=FALSE----
breastCancerTablePerturbed <- breastCancerTable
breastCancerTablePerturbed[missingDataIndices, ]$bareNuclei <- round(bareNucleiPerturbed)
breastCancerTablePerturbed$bareNuclei <- as.integer(breastCancerTablePerturbed$bareNuclei)

# round to integers


breastCancerTablePerturbed$bareNuclei[breastCancerTablePerturbed$bareNuclei > 10] <- 10
breastCancerTablePerturbed$bareNuclei[breastCancerTablePerturbed$bareNuclei < 1] <- 1


## ----compareCurrentWithImputedPerturbed-a, echo=TRUE, message=FALSE, warning=FALSE----
head(breastCancerTable[which(breastCancerTable$bareNuclei == "?"), ], 3)


## ----compareCurrentWithImputedPerturbed-b, echo=TRUE, message=FALSE, warning=FALSE----
#head(breastCancerTable[which(breastCancerTable$bareNuclei == "?"), ], 3)
breastCancerTableImputedByMean[24,]
breastCancerTableImputedByMean[41,]
breastCancerTableImputedByMean[140,]


## ----compareCurrentWithImputedPerturbed-c, echo=TRUE, message=FALSE, warning=FALSE----
#head(breastCancerTable[which(breastCancerTable$bareNuclei == "?"), ], 3)
breastCancerTablePerturbed[24,]
breastCancerTablePerturbed[41,]
breastCancerTablePerturbed[140,]


