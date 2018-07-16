library(ggplot2)
library(gridExtra)

set.seed(42)

iris <- read.table("4.2irisSummer2018.txt", header=TRUE)

numcolumns <- ncol(iris) - 1
colsetn <- 1
colsetlist <- c()
results <- data.frame("colset"=integer(), "k"=integer(), "totss"=double(), "withinss"=double())
clusters <- c()
for (colchoose in 1:numcolumns){
  combvec <- combn(numcolumns,colchoose)
  for (colsetnum in 1:ncol(combvec)){
    colset <- combvec[,colsetnum]
    data_chunk <- iris[,colset]
    for (k in c(1,2,3,4,5)){
      model <- kmeans(data_chunk, k)
      results[nrow(results)+1,] <- c(colsetn, k, model$totss, model$withinss)
      clusters <- c(clusters, list(model$cluster))
    }
    colsetlist <- c(colsetlist, list(colset))
    colsetn <- colsetn + 1
  }
}


p1 <- ggplot(data = results, aes(x = k, y = withinss))+
xlab("k")+
ylab("Within Cluster SS") +
geom_point(aes(color = as.factor(colset)))+
labs(color='Column Combination')+
ggtitle("Within cluster SS versus k for 15 field combos")

sdlist <- c()
for (kval in unique(results[,'k'])){
  sdlist <- c(sdlist, sd(results[results$k == kval,'withinss']))
}

p2 <- ggplot(data = data.frame("k"=unique(results[,"k"]), "sd"=sdlist), aes(x = k, y = sd))+
  xlab("k")+
  ylab("Std Dev") +
  geom_point()+
  ggtitle("Std Dev of within cluster SS across 15 field combos versus k")

grid.arrange(p1, p2)

model <- kmeans(iris[,1:4], 3)

p1 <- ggplot(data = iris, aes(x = Petal.Length, y = Petal.Width))+
  xlab("Petal Length")+
  ylab("Petal Width") +
  geom_point(aes(color = Species,shape=Species))+
  ggtitle("Petal Length vs Width")

p2 <- ggplot(data = iris, aes(x = Petal.Length, y = Petal.Width))+
  xlab("Petal Length")+
  ylab("Petal Width") +
  geom_point(aes(color=as.factor(model$cluster),shape=as.factor(model$cluster)))+
  ggtitle("Petal Length vs Width")

grid.arrange(p1, p2)

comparedf <- data.frame("actual"=iris[,'Species'], "predicted"=model$cluster)
comparedf[comparedf$predicted == 1,'predicted'] <- 'setosa'
comparedf[comparedf$predicted == 2,'predicted'] <- 'versicolor'
comparedf[comparedf$predicted == 3,'predicted'] <- 'virginica'

accuracy <- sum(comparedf[,'actual'] == comparedf[,'predicted'])/nrow(comparedf)

sprintf("Model accuracy with k=3 is %s", accuracy)


