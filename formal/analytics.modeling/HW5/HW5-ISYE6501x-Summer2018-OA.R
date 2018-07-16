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
if("FrF2" %in% rownames(installed.packages()) == FALSE) {install.packages("FrF2")}
if("glmnet" %in% rownames(installed.packages()) == FALSE) {install.packages("glmnet")}

# loading libraries
rm(list = ls())
library(ggplot2)
#library(grid)
#library(gridExtra)
#library(gtools)
#library(reshape)
library(purrr)
library(FrF2)
library(glmnet)


## ----loadData-11.1, echo=TRUE, message=FALSE, warning=FALSE--------------

dataFile <- "uscrime.txt"
if (!file.exists(dataFile)) {
  #crimeDataURL <- paste0(c("http://www.statsci.org/data/general/uscrime.txt"))
  crimeDataURL <- paste0(c("https://prod-edxapp.edx-cdn.org/assets/courseware/v1/17b85cea5d0e613bf08025ca2907b62f/asset-v1:GTx+ISYE6501x+2T2018+type@asset+block/11.1uscrimeSummer2018.txt"))
  
  download.file(crimeDataURL, dataFile) }

crimeDataTable <- read.table(dataFile, header = TRUE )


## ----applyStepWise-11.1, echo=TRUE, message=FALSE, warning=FALSE---------
stepWiseModel <- lm(Crime~1, data = crimeDataTable)
step(stepWiseModel, scope = formula(lm(Crime~., data = crimeDataTable)), direction = "forward")


## ----swFB-11.1, echo=TRUE, message=FALSE, warning=FALSE------------------
swModelForwardBackward <- lm(Crime~1 , data = crimeDataTable)
step(swModelForwardBackward, scope = list(lower = formula(lm(Crime~1, data=crimeDataTable)),
                                          upper = formula(lm(Crime~., data=crimeDataTable))),
                                          direction = "both")

## ----createModel-11.1, echo=TRUE, message=FALSE, warning=FALSE-----------
swModel2 <- lm (formula = Crime ~ Po1 + Ineq + Ed + M + Prob + U2, data = crimeDataTable)
summary(swModel2)

## ----scaleData-11.1, echo=TRUE, message=FALSE, warning=FALSE-------------
scaledData <- scale(crimeDataTable)

## ----lassomodelrun-11.1, echo=TRUE, message=FALSE, warning=FALSE---------

lassoModel <- cv.glmnet(x = as.matrix(scaledData[,-16]),
                        y = as.matrix(scaledData[,16]),
                        alpha = 1,
                        nfolds = 5,
                        type.measure = "mse",
                        family = "gaussian")


plot(lassoModel)#
#cbind(lassoModel$lambda, lassoModel$cvm)

## ----plotOtherGraphs-11.1, echo=TRUE, message=FALSE, warning=FALSE-------
plot(lassoModel$lambda, lassoModel$cvm)

## ----anotherplotOtherGraphs-11.1, echo=TRUE, message=FALSE, warning=FALSE----
plot(lassoModel$lambda, lassoModel$nzero)

## ----coeff-11.1, echo=TRUE, message=FALSE, warning=FALSE-----------------
coef(lassoModel, s = lassoModel$lambda.min)

## ----createModelFinal-11.1, echo=TRUE, message=FALSE, warning=FALSE------
lassoModelWithTightenedCoefficients <- lm (formula =  Crime ~ M + So + Ed + Po1 + LF + M.F + NW + U1 + U2 + Ineq + Prob, data = crimeDataTable)

summary(lassoModelWithTightenedCoefficients)

## ----finalLasso-11.1, echo=TRUE, message=FALSE, warning=FALSE------------
lassoModelWithTightenedCoefficientsFINAL <- lm (formula =  Crime ~ M + Ed + Po1 +  M.F + U1 + U2 + Ineq + Prob, data = crimeDataTable)

summary(lassoModelWithTightenedCoefficientsFINAL)

## ----2plotOtherGraphs-11.1, echo=TRUE, message=FALSE, warning=FALSE------
plot(lassoModel$lambda, lassoModel$cvm)

## ----2anotherplotOtherGraphs-11.1, echo=TRUE, message=FALSE, warning=FALSE----
plot(lassoModel$lambda, lassoModel$nzero)

## ----2coeff-11.1, echo=TRUE, message=FALSE, warning=FALSE----------------
coef(lassoModel, s = lassoModel$lambda.min)

## ----lassocreateModelFinal-11.1, echo=TRUE, message=FALSE, warning=FALSE----
lassoModelWithTightenedCoefficients <- lm (formula =  Crime ~ M + So + Ed + Po1 + LF + M.F + NW + U1 + U2 + Ineq + Prob, data = crimeDataTable)

summary(lassoModelWithTightenedCoefficients)

## ----2finalLasso-11.1, echo=TRUE, message=FALSE, warning=FALSE-----------
lassoModelWithTightenedCoefficientsFINAL <- lm (formula =  Crime ~ M + Ed + Po1 +  M.F + U1 + U2 + Ineq + Prob, data = crimeDataTable)

summary(lassoModelWithTightenedCoefficientsFINAL)

## ----elasticLoopy-11.1, echo=TRUE, message=FALSE, warning=FALSE----------
set.seed(1)
alphaBase <- 0.1 ;
alphaWhichGivesMinimumLambdas <- c()
for (i in 1:9) {

elasticModel <- cv.glmnet(x = as.matrix(scaledData[,-16]),
                        y = as.matrix(scaledData[,16]),
                        alpha = i*alphaBase,
                        nfolds = 5,
                        type.measure = "mse",
                        family = "gaussian")


alphaWhichGivesMinimumLambdas[i] <- elasticModel$lambda.min

}
print("the alpha which gives the minimum lambda is :")
which.min(alphaWhichGivesMinimumLambdas)*alphaBase


plot(lassoModel)#
#cbind(lassoModel$lambda, lassoModel$cvm)

## ----coeffElastic-11.1, echo=TRUE, message=FALSE, warning=FALSE----------
elasticModelWithBestAlpha <- cv.glmnet(x = as.matrix(scaledData[,-16]),
                        y = as.matrix(scaledData[,16]),
                        alpha = which.min(alphaWhichGivesMinimumLambdas)*alphaBase,
                        nfolds = 5,
                        type.measure = "mse",
                        family = "gaussian")

coef(elasticModelWithBestAlpha, s = elasticModelWithBestAlpha$lambda.min)

## ----finalEquation-11.1, echo=TRUE, message=FALSE, warning=FALSE---------

elasticModelWithTightenedCoefficientsFINAL <- lm (formula =  Crime ~ M + Ed + Po1 +  M.F + U1 + U2 + Ineq + Prob, data = crimeDataTable)

summary(elasticModelWithTightenedCoefficientsFINAL)


## ----FrF2-run-.12.1, echo=TRUE-------------------------------------------
set.seed(1)
#numberOfRuns <- 50*16 #each buyer sees 16 houses so total number of runs is 50x16
numberOfRuns <- 16
numberOfFactors <- 10

frf2Output <- FrF2(numberOfRuns, numberOfFactors)

head(frf2Output)


## ----2FrF2-run-.12.1, echo=TRUE------------------------------------------
set.seed(1)
#numberOfRuns <- 50*16 #each buyer sees 16 houses so total number of runs is 50x16
numberOfRuns <- 16
numberOfFactors <- 10

frf2Output <- FrF2(numberOfRuns, numberOfFactors)

head(frf2Output)


