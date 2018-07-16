library(ggplot2)
library(reshape)

temps <- read.table("6.2tempsSummer2018.txt", header=TRUE)

temps$DAY <- as.Date(temps$DAY, '%e-%b')

meltedtemps <- melt(temps, id = 'DAY')
ggplot(meltedtemps, aes(x = DAY, y = value, colour = variable, group=variable)) +
  geom_line() +
  ylab("Temperature") +
  xlab("Day")

cusum <- function(data, P, Q){
  ans <- data.frame(S=double(), alarm=integer())
  ans[nrow(ans)+1,] <- c(0,0)
  mu <- mean(data)
  std <- sd(data)
  C <- P * std
  thresh <- Q * std
  for (i in 2:length(data)){
    S <- max(0, ans[[i-1,1]] + (mu - data[i] - C))
    alarm <- S > thresh
    ans[nrow(ans)+1,] <- c(S, alarm)
  }
  ans
}

P <- 1
Q <- 5
firsttriplist <- c()
for (cnum in 1:(ncol(temps) - 1)){
  tempdat <- temps[,cnum + 1]
  ans <- cusum(tempdat, P, Q)
  firsttrip <- which(ans$alarm == 1)[1]
  firsttriplist <- c(firsttriplist,firsttrip)
}

hist(firsttriplist, xlim=c(40,140), xlab="First CUSUM trip - # of Days after July 1", main=sprintf("P: %s, Q: %s, min: %s, max: %s, mean: %s\n", P, Q, min(firsttriplist), max(firsttriplist), mean(firsttriplist)))
firsttripdf <- data.frame(firsttriplist)
qplot(seq_along(firsttripdf$firsttriplist), firsttripdf$firsttriplist, xlab="Years after 1996", ylab="First CUSUM trip - # of Days after July 1", main="CUSUM First trip date versus year")
