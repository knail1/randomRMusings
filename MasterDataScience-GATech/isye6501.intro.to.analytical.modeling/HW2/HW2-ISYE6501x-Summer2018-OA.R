## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----preRequisites, echo=FALSE, message=FALSE, warning=FALSE-------------
# installing packages if needed
if("kernlab" %in% rownames(installed.packages()) == FALSE) {install.packages("kernlab")}
if("kknn" %in% rownames(installed.packages()) == FALSE) {install.packages("kknn")}
if("ggplot2" %in% rownames(installed.packages()) == FALSE) {install.packages("ggplot2")}
if("grid" %in% rownames(installed.packages()) == FALSE) {install.packages("grid")}
if("gridExtra" %in% rownames(installed.packages()) == FALSE) {install.packages("gridExtra")}
if("gtools" %in% rownames(installed.packages()) == FALSE) {install.packages("gtools")}
if("outliers" %in% rownames(installed.packages()) == FALSE) {install.packages("outliers")}
# loading libraries
rm(list = ls())
library(kernlab)
library(kknn)
library(ggplot2)
library(grid)
library(gridExtra)
library(gtools)
library(outliers)

## ----hideThis, echo=FALSE, message=FALSE, warning=FALSE------------------
# setosa | versicolor | virginica
# ------------- | ------------- | -------------
# ![setosa](https://upload.wikimedia.org/wikipedia/commons/thumb/5/56/Kosaciec_szczecinkowaty_Iris_setosa.jpg/440px-Kosaciec_szczecinkowaty_Iris_setosa.jpg) | ![versicolor](https://upload.wikimedia.org/wikipedia/commons/thumb/4/41/Iris_versicolor_3.jpg/440px-Iris_versicolor_3.jpg) | ![virginica](https://upload.wikimedia.org/wikipedia/commons/thumb/9/9f/Iris_virginica.jpg/440px-Iris_virginica.jpg)


# This image illustrates what a sepal and petal is:

# ![image of a flower](https://extension.illinois.edu/gpe/images/flower12.gif)


## ----q4.2.01a, echo=TRUE-------------------------------------------------
head(iris)

## ----q4.2.01b, echo=FALSE------------------------------------------------
# First, let's pull down this data and read it into a table, even though iris dataset comes with the R application.
dataFile <- "4.2irisSummer2018.txt"
if (!file.exists(dataFile)) {
  irisURl <- paste0(c("https://prod-edxapp.edx-cdn.org/assets/courseware/v1/26886db51f665dbde534f8c6326694b5/asset-v1:GTx+ISYE6501x+2T2018+type@asset+block/4.2irisSummer2018.txt"))
  download.file(irisUrl, dataFile) }

irisTable <- read.table(dataFile)


## ----q4.2.01c, echo=TRUE-------------------------------------------------
colnames(irisTable)


## ----hideThisToo, , echo=FALSE-------------------------------------------
# ![combinations](https://wikimedia.org/api/rest_v1/media/math/render/svg/813f7124a61dac205542db3f8491b36cb306453a)

## ----q4.2.01d, echo=FALSE------------------------------------------------
# predictors <- colnames(iris)[1:4] # ignoring the 5th column, which is the actual classification "Species"
# permsOfPredictors <- permutations(n = 4 , r = 2, v = predictors)
TotalWithinClusterSumOfSquares <- rep(0, 10)
# 1. Sepal.Length plotted against Sepal.Width
for (clusterCount in 1:10) {
  TotalWithinClusterSumOfSquares[clusterCount] <- kmeans(c(iris$Sepal.Length, iris$Sepal.Width),clusterCount)$tot.withinss
}
plotData1 <- as.data.frame(cbind(numberOfClusters = 1:10, TotalWithinClusterSumOfSquares))

#plotData1 <- as.data.frame(cbind(numberOfClusters = 1:10, TotalWithinClusterSumOfSquares))

# 2. Sepal.Length plotted against Petal.Length
for (clusterCount in 1:10) {
  TotalWithinClusterSumOfSquares[clusterCount] <- kmeans(c(iris$Sepal.Length, iris$Petal.Length),clusterCount)$tot.withinss
}
plotData2 <- as.data.frame(cbind(numberOfClusters = 1:10, TotalWithinClusterSumOfSquares))


# 3. Sepal.Length plotted against Petal.Width
for (clusterCount in 1:10) {
  TotalWithinClusterSumOfSquares[clusterCount] <- kmeans(c(iris$Sepal.Length, iris$Petal.Width),clusterCount)$tot.withinss
}
plotData3 <- as.data.frame(cbind(numberOfClusters = 1:10, TotalWithinClusterSumOfSquares))

# 4. Sepal.Width plotted against Petal.Length
for (clusterCount in 1:10) {
  TotalWithinClusterSumOfSquares[clusterCount] <- kmeans(c(iris$Sepal.Width, iris$Petal.Length),clusterCount)$tot.withinss
}
plotData4 <- as.data.frame(cbind(numberOfClusters = 1:10, TotalWithinClusterSumOfSquares))

# 5. Sepal.Width plotted against Petal.Width
for (clusterCount in 1:10) {
  TotalWithinClusterSumOfSquares[clusterCount] <- kmeans(c(iris$Sepal.Width, iris$Petal.Width),clusterCount)$tot.withinss
}
plotData5 <- as.data.frame(cbind(numberOfClusters = 1:10, TotalWithinClusterSumOfSquares))


# 6. Petal.Length plotted against Petal.Width
for (clusterCount in 1:10) {
  TotalWithinClusterSumOfSquares[clusterCount] <- kmeans(c(iris$Petal.Length, iris$Petal.Width),clusterCount)$tot.withinss
}
plotData6 <- as.data.frame(cbind(numberOfClusters = 1:10, TotalWithinClusterSumOfSquares))



cplot1 <- ggplot(data = plotData1) +
  geom_point(mapping = aes(x = numberOfClusters, y = TotalWithinClusterSumOfSquares)) +
  xlab("# of clusters") +
  ylab("TotalSSWithinClusters")
  
cplot2 <- ggplot(data = plotData2) +
  geom_point(mapping = aes(x = numberOfClusters, y = TotalWithinClusterSumOfSquares)) +
  xlab("# of clusters") +
  ylab("TotalSSWithinClusters")

cplot3 <- ggplot(data = plotData3) +
  geom_point(mapping = aes(x = numberOfClusters, y = TotalWithinClusterSumOfSquares)) +
  xlab("# of clusters") +
  ylab("TotalSSWithinClusters")

cplot4 <- ggplot(data = plotData4) +
  geom_point(mapping = aes(x = numberOfClusters, y = TotalWithinClusterSumOfSquares)) +
  xlab("# of clusters") +
  ylab("TotalSSWithinClusters")

cplot5 <- ggplot(data = plotData5) +
  geom_point(mapping = aes(x = numberOfClusters, y = TotalWithinClusterSumOfSquares)) +
  xlab("# of clusters") +
  ylab("TotalSSWithinClusters")

cplot6 <- ggplot(data = plotData6) +
  geom_point(mapping = aes(x = numberOfClusters, y = TotalWithinClusterSumOfSquares)) +
  xlab("# of clusters") +
  ylab("TotalSSWithinClusters")

grid.arrange(cplot1, cplot2, cplot3, cplot4, cplot5, cplot6, nrow = 3, ncol = 2)


## ----q4.2.01e, echo=FALSE------------------------------------------------
#1
plot1 <- ggplot(data = irisTable) +
  geom_point(mapping = aes(Sepal.Length, Sepal.Width, color = Species))
#2
plot2 <- ggplot(data = irisTable) +
  geom_point(mapping = aes(Sepal.Length, Petal.Length, color = Species))
#3
plot3 <- ggplot(data = irisTable) +
  geom_point(mapping = aes(Sepal.Length, Petal.Width, color = Species))
#4
plot4 <- ggplot(data = irisTable) +
  geom_point(mapping = aes(Sepal.Width, Petal.Length, color = Species))
#5
plot5 <- ggplot(data = irisTable) +
  geom_point(mapping = aes(Sepal.Width, Petal.Width, color = Species))
#6
plot6 <- ggplot(data = irisTable) +
  geom_point(mapping = aes(Petal.Length, Petal.Width, color = Species))

grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, nrow = 3, ncol = 2)

# ggplot(data = irisTable) +
#  geom_point(mapping = aes(Petal.Length, Petal.Width, color = Species)) +
#  facet_wrap(~ Species, nrow = 2)

## ----5.1a, echo=TRUE-----------------------------------------------------

dataFile5_1 <- "uscrime.txt"
if (!file.exists(dataFile5_1)) {
  crimeDataURL <- paste0(c("http://www.statsci.org/data/general/uscrime.txt"))
  download.file(crimeDataURL, dataFile5_1) }

crimeDataTable <- read.table(dataFile5_1, header = TRUE )

## ----5.1b, echo=TRUE-----------------------------------------------------
boxplot(crimeDataTable$Crime)

## ----5.1c1, echo=TRUE----------------------------------------------------
grubbs.test(crimeDataTable$Crime, type = 10, opposite = FALSE)


## ----5.1c, echo=TRUE-----------------------------------------------------

ggplot(data = crimeDataTable) + 
  geom_point(mapping = aes(Ed, Crime, color = Po1))


## ----5.1d, echo=TRUE-----------------------------------------------------

ggplot(data = crimeDataTable) + 
  geom_point(mapping = aes(LF, Crime, color = M))


## ----5.1e, echo=TRUE-----------------------------------------------------

ggplot(data = crimeDataTable) + 
  geom_point(mapping = aes(U2, Crime, color = Wealth))


