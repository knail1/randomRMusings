library(ggplot2)
library(gridExtra)
library(outliers)

set.seed(42)

crime <- read.table("5.1uscrimeSummer2018.txt", header=TRUE)

crime[,'Crime']

ggplot(data = crime, aes(x = "", y = Crime)) + 
  geom_boxplot()

gtest <- grubbs.test(crime[,'Crime'])

gtest

