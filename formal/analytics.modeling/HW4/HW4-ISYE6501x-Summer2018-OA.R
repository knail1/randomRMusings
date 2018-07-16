## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----preRequisites, echo=FALSE, message=FALSE, warning=FALSE-------------
# installing packages if needed
if("ggplot2" %in% rownames(installed.packages()) == FALSE) {install.packages("ggplot2")}
#if("grid" %in% rownames(installed.packages()) == FALSE) {install.packages("grid")}
#if("gridExtra" %in% rownames(installed.packages()) == FALSE) {install.packages("gridExtra")}
#if("gtools" %in% rownames(installed.packages()) == FALSE) {install.packages("gtools")}
#if("reshape" %in% rownames(installed.packages()) == FALSE) {install.packages("reshape")}
if("purrr" %in% rownames(installed.packages()) == FALSE) {install.packages("purrr")}
if("factoextra" %in% rownames(installed.packages()) == FALSE) {install.packages("factoextra")}
if("tree" %in% rownames(installed.packages()) == FALSE) {install.packages("tree")}
if("rpart" %in% rownames(installed.packages()) == FALSE) {install.packages("rpart")}
if("randomForest" %in% rownames(installed.packages()) == FALSE) {install.packages("randomForest")}
if("rms" %in% rownames(installed.packages()) == FALSE) {install.packages("rms")}
if("caret" %in% rownames(installed.packages()) == FALSE) {install.packages("caret")}
if("e1071" %in% rownames(installed.packages()) == FALSE) {install.packages("e1071")}
if("treeClust" %in% rownames(installed.packages()) == FALSE) {install.packages("treeClust")}
if("ROCR" %in% rownames(installed.packages()) == FALSE) {install.packages("ROCR")}

# loading libraries
rm(list = ls())
library(ggplot2)
#library(grid)
#library(gridExtra)
#library(gtools)
#library(reshape)
library(purrr)
library(factoextra)
library(tree)
library(rpart)
library(rpart.plot)
library(randomForest)
library(rms)
library(caret)
library(e1071)
library(treeClust)
library(ROCR)


## ----loadData-9.1, echo=TRUE, message=FALSE, warning=FALSE---------------

dataFile <- "uscrime.txt"
if (!file.exists(dataFile)) {
  crimeDataURL <- paste0(c("http://www.statsci.org/data/general/uscrime.txt"))
  download.file(crimeDataURL, dataFile) }

crimeDataTable <- read.table(dataFile, header = TRUE )


## ----apply-prconf-9.1, echo=TRUE-----------------------------------------

#pcaModel <- prcomp( ~ crimeDataTable[,1:15] , scale. = TRUE)
# note, the tilda didn't work, so you shove the numeric data (x per man page directly, like so:

pcaModel2 <- prcomp(crimeDataTable[,1:15] ,  scale. = TRUE)

summary(pcaModel2)

## ----createNewDFandRunRegressionOnIt-9.1, echo=TRUE----------------------

CrimeDataWithPrincipalComponents <- as.data.frame(cbind(pcaModel2$x, crimeDataTable[,16]))
cn <- colnames(CrimeDataWithPrincipalComponents)
cn[16] <- "Crime"
colnames(CrimeDataWithPrincipalComponents)  <- cn

# running reg:
RegressionModelBasedonPCs <- lm(Crime ~ . , data = CrimeDataWithPrincipalComponents)
# TA used the command below, but I checked. both yield same output, and mine is cleaner
#RegressionModelBasedonPCs2 <- lm(CrimeDataWithPrincipalComponents[,16]~. , data = CrimeDataWithPrincipalComponents[,1:15])                                          



## ----printSummaryRegression-9.1, echo=TRUE-------------------------------

summary(RegressionModelBasedonPCs)


## ----cleaningUPThePCAModel-9.1, echo=TRUE--------------------------------
RegressionModelBasedonPCs_SHARPER <- lm(Crime ~ PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC12+PC14+PC15, data = CrimeDataWithPrincipalComponents)
summary(RegressionModelBasedonPCs_SHARPER)


## ----AndConvertingBackToOriginalModel-9.1, echo=TRUE---------------------

fviz_eig(pcaModel2)


## ----graphOfVarianceMattCode-9.1, echo=TRUE------------------------------

pcaVariance <- as.data.frame(summary(pcaModel2)$importance[3,])
PC <- 1:15
ggplot(pcaVariance, aes(x = factor(PC), y = pcaVariance, fill = factor(PC))) + geom_col() +
  labs(title = 'Cumulative Variance Accounted',
       subtitle = 'Additive Sum of Prior PCs',
       x = 'Principal components',
       y = 'Variance Accounted',
       fill = 'PC')


## ----graphOfVars-9.1, echo=TRUE------------------------------------------

fviz_pca_var(pcaModel2, col.var = "contrib", repel = "TRUE")


## ----linModel-9.1, echo=TRUE---------------------------------------------

RegressionModelBasedonPCs_FINAL <- lm(Crime ~ PC1+PC2+PC3+PC4+PC5, data = CrimeDataWithPrincipalComponents)
summary(RegressionModelBasedonPCs_FINAL)

## ----rotateBack-9.1, echo=TRUE-------------------------------------------
RegCoefficientsForTop5PCs <- RegressionModelBasedonPCs_FINAL$coefficients[2:length(RegressionModelBasedonPCs_FINAL$coefficients)]

ScaledFinalCoefficients <- RegCoefficientsForTop5PCs %*%head(t(pcaModel2$rotation), 5)

ScaledFinalCoefficients


## ----unscaleData-9.1, echo=TRUE------------------------------------------

RegCoefficientsForTop5PCs <- RegressionModelBasedonPCs_FINAL$coefficients[2:length(RegressionModelBasedonPCs_FINAL$coefficients)]

ScaledFinalCoefficients <- RegCoefficientsForTop5PCs %*%head(t(pcaModel2$rotation), 5)

#ScaledFinalCoefficients


meanForEachPredictor <- map_dbl(crimeDataTable[,1:15], mean)
standardDeviationForEachPredictor <- map_dbl(crimeDataTable[, 1:15], sd)

FinalCoefficients_Unscaled <- ScaledFinalCoefficients*standardDeviationForEachPredictor + meanForEachPredictor

print("Final, Unscaled coefficients")
FinalCoefficients_Unscaled

## ----splitData-10.1, echo=TRUE, message=FALSE, warning=FALSE-------------
set.seed(1) 
crimeDataTrainingIndices <- sample(nrow(crimeDataTable), size = floor(nrow(crimeDataTable)*0.7))
crimeDataTraining <- crimeDataTable[crimeDataTrainingIndices,]

# will split the remaining 30% into validation and testing data sets
restOfData <- crimeDataTable[-crimeDataTrainingIndices,]
crimeDataTesting <- restOfData[1:floor(nrow(restOfData)),]
#crimeDataTesting <- restOfData[ceiling(nrow(restOfData)*0.5):nrow(restOfData),]

## ----treeAndForest-10.1, echo=TRUE, message=FALSE, warning=FALSE---------
#treeData <- tree(Crime~., data = crimeDataTraining)
#summary(treeData)

treeDataFromRPart_JustTrainingData <- rpart(Crime~., data = crimeDataTraining)
rpart.plot(treeDataFromRPart_JustTrainingData)


## ----treeInterpret-10.1, echo=TRUE, message=FALSE, warning=FALSE---------
treeDataFromRPart_JustTrainingData$frame

## ----splitLeaves-10.1, echo=TRUE, message=FALSE, warning=FALSE-----------

leaves <- rpart.predict.leaves(treeDataFromRPart_JustTrainingData, crimeDataTraining, type = "where" )

leaf4 <- vector()
leaf5 <- vector()
leaf3 <- vector()

for (valueWithinLeaf in 1:length(leaves)) {
  
  # figuring this out took really long!!!
  if(leaves[[valueWithinLeaf]] == 4) { leaf4 <- c(leaf4, valueWithinLeaf) }
  if(leaves[[valueWithinLeaf]] == 5) { leaf5 <- c(leaf5, valueWithinLeaf) }
  if(leaves[[valueWithinLeaf]] == 3) { leaf3 <- c(leaf3, valueWithinLeaf) }

}


## ----runLMonLeaves-10.1, echo=TRUE, message=FALSE, warning=FALSE---------
leaf3LinearModel <- lm(Crime ~ ., data = crimeDataTraining[leaf3,])
leaf4LinearModel <- lm(Crime ~ ., data = crimeDataTraining[leaf4,])
leaf5LinearModel <- lm(Crime ~ ., data = crimeDataTraining[leaf5,])

## ----predictedResp-10.1, echo=TRUE, message=FALSE, warning=FALSE---------

predictedCrimeRatesLeaf3 <- predict(leaf3LinearModel, crimeDataTesting[,1:15], type = "response")
predictedCrimeRatesLeaf4 <- predict(leaf4LinearModel, crimeDataTesting[,1:15], type = "response")
predictedCrimeRatesLeaf5 <- predict(leaf4LinearModel, crimeDataTesting[,1:15], type = "response")


#predict(treeDataFromRPart_JustTrainingData, crimeDataTraining[,1:15], type = "vector")
#predictedCrimeRates

## ----comparePredAndReal-10.1, echo=TRUE, message=FALSE, warning=FALSE----

#meanForCrime <- mean(crimeDataTable$Crime)
#sdForCrime <- sd(crimeDataTable$Crime)
#normalizedPredictedCrimeRates <- (predictedCrimeRates - meanForCrime)/ sdForCrime

differenceInPredictedAndActualInPercentage_leaf3 <- (predictedCrimeRatesLeaf3 - crimeDataTesting$Crime)/crimeDataTesting$Crime

errorPercentageForLeaf3 <- mean(abs(differenceInPredictedAndActualInPercentage_leaf3))
cat("The error % between predicted and real (testing) data for leaf3 is", errorPercentageForLeaf3, "\n")

differenceInPredictedAndActualInPercentage_leaf4 <- (predictedCrimeRatesLeaf4 - crimeDataTesting$Crime)/crimeDataTesting$Crime

errorPercentageForLeaf4 <- mean(abs(differenceInPredictedAndActualInPercentage_leaf4))

cat("The error % between predicted and real (testing) data for leaf4 is", errorPercentageForLeaf4, "\n")


differenceInPredictedAndActualInPercentage_leaf5 <- (predictedCrimeRatesLeaf5 - crimeDataTesting$Crime)/crimeDataTesting$Crime

errorPercentageForLeaf5 <- mean(abs(differenceInPredictedAndActualInPercentage_leaf5))

cat("The error % between predicted and real (testing) data for leaf5 is", errorPercentageForLeaf5, "\n")



## ----Forest-10.1, echo=TRUE, message=FALSE, warning=FALSE----------------

numberOfPredictorsToConsider <- 4
randomForestData <- randomForest(Crime ~. , data = crimeDataTable, mtry = numberOfPredictorsToConsider, importance = TRUE)

predictedCrimeRatesUsingForest <- predict(randomForestData, crimeDataTable[,1:15], type = "response")

predictedCrimeRatesUsingForest

## ----comparePredAndRealForest-10.1, echo=TRUE, message=FALSE, warning=FALSE----

# meanForCrime <- mean(crimeDataTable$Crime)
#sdForCrime <- sd(crimeDataTable$Crime)

#normalizedPredictedCrimeRates <- (predictedCrimeRates - meanForCrime)/ sdForCrime

differenceInPredictedAndActualInPercentage_FOREST <- (predictedCrimeRatesUsingForest - crimeDataTable$Crime)/crimeDataTable$Crime

mean(abs(differenceInPredictedAndActualInPercentage_FOREST))


## ----germanData-10.3, echo=TRUE, message=FALSE, warning=FALSE------------

dataFile <- "germancredit.txt"
if (!file.exists(dataFile)) {
  germanCreditURL <- paste0(c("https://prod-edxapp.edx-cdn.org/assets/courseware/v1/a145a478beb6f64b59ec1de082b84235/asset-v1:GTx+ISYE6501x+2T2018+type@asset+block/10.3germancreditSummer2018.txt"))
  download.file(germanCreditURL, dataFile) }

germanCreditTable <- read.table(dataFile, header = FALSE )

# changing the response variabl to 0 or 1 since glm binomial will be expecting that...
germanCreditTable$V21[germanCreditTable$V21 == 1] <- 0
germanCreditTable$V21[germanCreditTable$V21 == 2] <- 1


## ----runningGLM-10.3, echo=TRUE, message=FALSE, warning=FALSE------------

logisticRegModel <- glm(V21 ~ . , family = binomial(link = "logit"), data = germanCreditTable)

summary(logisticRegModel)

## ----createLogitModel-10.3, echo=TRUE, message=FALSE, warning=FALSE------

set.seed(1) 
germanDataTrainingIndices <- sample(nrow(germanCreditTable), size = floor(nrow(germanCreditTable)*0.7))
germanDataTraining <- germanCreditTable[germanDataTrainingIndices,]

# will split the remaining 30% into validation and testing data sets
restOfData <- germanCreditTable[-germanDataTrainingIndices,]
germanDataTesting <- restOfData[1:floor(nrow(restOfData)),]
#crimeDataTesting <- restOfData[ceiling(nrow(restOfData)*0.5):nrow(restOfData),]

fitlogisticRegModel_RMS <- lrm(V21 ~ . ,  data = germanDataTraining)

print(fitlogisticRegModel_RMS)

## ----confusionMatrixCalc-10.3, echo=TRUE, message=FALSE, warning=FALSE----

predictedGermanCreditResult_TESTDATA <- predict(fitlogisticRegModel_RMS, germanDataTesting[, 1:20])

probs <- round(exp(predictedGermanCreditResult_TESTDATA)/(1+exp(predictedGermanCreditResult_TESTDATA)))

cMatrix <- confusionMatrix(
  factor(probs),
  factor(germanDataTesting[, 21])
)
cMatrix$table
TP <- cMatrix$table[1,1]
FP <- cMatrix$table[1,2]
FN <- cMatrix$table[2,1]
TN <- cMatrix$table[2,2]

fourfoldplot(cMatrix$table)

## ----calcCost-10.3, echo=TRUE, message=FALSE, warning=FALSE--------------

Cost <- 0*TP + 1*TN + 0*FN + 5*FP

Cost

## ----usingROCR-10.3, echo=TRUE, message=FALSE, warning=FALSE-------------



pred <- prediction(probs,germanDataTesting[, 21])
cost.perf = performance(pred, "cost")
#pred@cutoffs[[1]][which.min(cost.perf@y.values[[1]])]

cost.perf@y.values


