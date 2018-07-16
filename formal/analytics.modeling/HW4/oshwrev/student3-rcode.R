
### Load the data and the test values.

crimdata=read.delim("http://www.statsci.org/data/general/uscrime.txt")

### remove the dependent data for PCA analysis

crimdata2 = crimdata[,1:15]
head(crimdata2,2)
new.values = data.frame(
    M = 14.0,
    So = 0,
    Ed = 10.0,
    Po1 = 12.0,
    Po2 = 15.5,
    LF = 0.640,
    M.F = 94.0,
    Pop = 150,
    NW = 1.1,
    U1 = 0.120,
    U2 = 3.6,
    Wealth = 3200,
    Ineq = 20.1,
    Prob = 0.04,
    Time = 39.0)

### Run the PCA model and plot out the variances
pca1 = prcomp(crimdata2,scale=TRUE)

options(repr.plot.width=6, repr.plot.height=4)
plot(pca1,type="l") ## looks like 5 or 6 Principal components will be ideal.
summary(pca1)
pca1 ## Inferring from the rotation matrix, PC1 is mostly influenced by Wealth and Ineq, while PC2 is M.F and Population

### load new values into the principle component tranforms

pca.predvals = predict(pca1,newdata = new.values)

### build a table with the principle components and add the dependedn variable to the mix

pcatable = as.data.frame(pca1$x)
pcatable$Crime = crimdata$Crime

## build a linear regression model against 5 principal components

lm.fit = lm(Crime~PC1+PC2+PC3+PC4+PC5,data=pcatable)

## determine quality of fit against the original model in question 8.2

predict(lm.fit,newdata=as.data.frame(pca.predvals), interval = "prediction") ## new model predicts 1388.93

BIC (lm.fit) ## with PC5, the BIC was 670.65 - BIC on original model was 681 


L=5 ##Set the number of PC's in the model
factor.count = length(crimdata2) ## Original Factor Count

##Initialize the lists used
coeffs = c()
scl = c()
ctr = c()

## Calculate the sum of b*v for each factor/PC combination used.
## We will also capture the scale and center values for each factor used in the model
for(i in 1:factor.count){
    sum.sub.factors = 0
    for(x in 1:L){
        b=lm.fit$coefficients[x+1]
        v=pca1$rotation[i,x]
        sub.factor = b*v
        sum.sub.factors = sum.sub.factors + sub.factor 
        }
    fac.ctr = pca1$center[i]
    fac.scl = pca1$scale[i]
    names(sum.sub.factors) = row.names(pca1$rotation)[i]
    coeffs = round(append(coeffs,sum.sub.factors),2)
    scl = append(scl,fac.scl)
    ctr = append(ctr,fac.ctr)   
}

## Build the final table
Coefficient = coeffs
Coeff.Table = (as.data.frame(Coefficient))
Coeff.Table$Scale = round(scl,2)
Coeff.Table$Center = round(ctr,2)
Coeff.Table

### Load the Libraries
library(tree)
library(randomForest)
library(rpart)

### Load and analyze the 'tree' model
tree.fit=tree(Crime~.,data=crimdata)

summary(tree.fit)
predict(tree.fit,newdata=new.values)
options(repr.plot.width=6, repr.plot.height=6)
plot(tree.fit)
text(tree.fit)


### Load and analyze the 'rpart' model
rpt.tree = rpart(Crime~.,data=crimdata)
predict(rpt.tree,newdata = new.values)

summary(rpt.tree)

### Load and Analyze the 'randomForest' model
set.seed(1)
rf.fit = randomForest(Crime~.,crimdata)

round(mean(predict(rf.fit,data=new.values)),2)

rf.fit

### Load the data and take a look
germcred = read.table("https://prod-edxapp.edx-cdn.org/assets/courseware/v1/a145a478beb6f64b59ec1de082b84235/asset-v1:GTx+ISYE6501x+2T2018+type@asset+block/10.3germancreditSummer2018.txt")
head(germcred,2)
dim(germcred)
germcred$V1=as.factor(germcred$V1)
germcred$V3=as.factor(germcred$V3)
germcred$V4=as.factor(germcred$V4)
germcred$V6=as.factor(germcred$V6)
germcred$V7=as.factor(germcred$V7)
germcred$V9=as.factor(germcred$V9)
germcred$V10=as.factor(germcred$V10)
germcred$V12=as.factor(germcred$V12)
germcred$V14=as.factor(germcred$V14)
germcred$V15=as.factor(germcred$V15)
germcred$V17=as.factor(germcred$V17)
germcred$V19=as.factor(germcred$V19)
germcred$V20=as.factor(germcred$V20)

### Add Column names for Context
GCColumns = c("CA_Status","Duration","Credhist","Purpose","Credit_Amt","Savings","Employment_Status","Installment_Rate","Personal_Stat","Other_Debtors","Resident_Since","Property","Age","Other_Plans","Housing","Number_Credit","Job","People","Telephone","Foreign","GB")
colnames(germcred) = GCColumns

### transform response variable to 1-0
germcred$GB10 = germcred[,21]-1

set.seed(1)
testindx = sample(1:nrow(germcred),round(.2*nrow(germcred)))
testdata = germcred[testindx,]

## take every other row (remaining 80%) and call it traindata.

traindata = germcred[-testindx,]


lr=glm(GB10~.-GB,data=traindata,family=binomial(link='logit'))

summary(lr)

anova(lr)

###Plot and measure quality of fit
library(ROCR)
p <- predict(lr, newdata=testdata, type="response")
pr <- prediction(p, testdata$GB10)
prf <- performance(pr, measure = "sens", x.measure = "fpr")
plot(prf, xlab="1-Specificity") ## 1-Specificity = FPR.  Changing label for consistency


auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
round(auc*100,2)

Accuracy = c()
Threshold=seq(.1, 1, by=.01 )
for (i in Threshold){
    fitted.results <- predict(lr,newdata=testdata,type='response')
    fitted.results <- ifelse(fitted.results > i,1,0)
    misClasificError <- mean(fitted.results != testdata$GB10)
    Accuracy = append(Accuracy,1-misClasificError)
    print(paste('Accuracyfor Threshold',i,"is",1-misClasificError))}

plot(Threshold,Accuracy,type="l")
