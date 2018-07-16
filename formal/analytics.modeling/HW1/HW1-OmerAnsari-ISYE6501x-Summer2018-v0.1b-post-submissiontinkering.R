# This is my HW1 submission for ISYE 6501X Micromasters course , May 2018
# Omer Ansari

# these two packages need to be installed first.
# install.packages("kernlab")
# install.packages("kknn")

rm(list = ls())
library(kernlab)
library(kknn)


CreateSVM <- function(CValue, creditData2) {
  
  
  creditApprovalModel <- ksvm(R1~., data = creditData2, 
                              type = "C-svc",
                              kernel = "vanilladot",
                              C = CValue,
                              scaled = TRUE)
  
  return(creditApprovalModel)
}

Finda0 <- function(creditApprovalModel) { 
  return(creditApprovalModel@b)
}

FindCoefficients <- function(creditApprovalModel) {
  return(colSums(creditApprovalModel@xmatrix[[1]]*creditApprovalModel@coef[[1]]))
}

FindModelAccuracy_SVM <- function(creditApprovalModel, creditData2) {

  # using predict function to measure accuracy
  prediction <- predict(creditApprovalModel,creditData2[,1:10])
  
  percentSuccess <- sum(prediction == creditData2$R1 )/nrow(creditData2)
  # the above line sees which values in prediction and creditData2$R1 are equal
  # then it adds them up
  # then divides by total number of values 
  
  return(percentSuccess)
  
}


### MAIN CODE STARTS HERE:


# Question 2.1.1 : good classifier for this data. Show the equation of your classifier, 
# and how well it classifies the data points in the full data set.


creditData2 <- read.table("2.2credit_card_data-headersSummer2018.txt", header = TRUE);
CValues = c(0.001, 0.1, 10, 20, 50, 100);
print("====Question 2.2.1 : create a good classifier for this data. Show the equation of your classifier,and how well it classifies  ====")
      
for (i in CValues) {
  
  print("====starting iterationdir ====")
  print(paste0("C Value:", i))
  print("------")
  SVmodull <- CreateSVM(i,creditData2)
  print(paste0("co-efficients:", FindCoefficients(SVmodull)))
  print(paste0("a0 co-efficient:", Finda0(SVmodull)))
  print(paste0("model accuracy %:", FindModelAccuracy_SVM(SVmodull,creditData2)))
  print("====ending iteration====")
  
}

print("")
print("")

print("====Question 2.2.2 : try other (nonlinear) kernels as well====")
print("trying the polynomial kernel and value of C=100")

CreateSVMNonLinear <- function(CValue, creditData2) {
  nonLinearCreditApprovalModel <- ksvm(R1~., data = creditData2, 
                                       type = "C-svc",
                                       kernel = "polydot", ## you can change this to other kernels for full assessment)
                                       C = 100,
                                       scaled = TRUE)
  
return(nonLinearCreditApprovalModel)
}


for (i in CValues) {
  
  print("====starting iteration====")
  print(paste0("C Value:", i))
  print("------")
  SVmodull2 <- CreateSVMNonLinear(i,creditData2)
  print(paste0("co-efficients:", FindCoefficients(SVmodull2)))
  print(paste0("a0 co-efficient:", Finda0(SVmodull2)))
  print(paste0("model accuracy %:", FindModelAccuracy_SVM(SVmodull2,creditData2)))
  print("====ending iteration====")
  
}


print("")
print("")

print("====Question 2.2.3 : knn model to find an optimum k value====")


FindModelAccuracy_KNN <- function(kay, creditData2) {
  
  prediction_knn <- rep(0,(nrow(creditData2)))
  for (i in 1:nrow(creditData2)) {
    # during each iteration, each row is predicted by using the rest of the data as predictors
    # R1~A1+A2+A3+A9+A10+A11+A12+A14+A15 is same as R1~.
    
    creditModelKNN <- kknn(R1~.,
                           creditData2[-i, ],
                           creditData2[i, ],
                           k = kay,
                           scale = TRUE)
    prediction_knn[i] <- as.integer(fitted(creditModelKNN)+0.5)
    #as.integer helps round the value
    
  }
  
  
  percentSuccess_knn <- sum(prediction_knn == creditData2$R1)/nrow(creditData2);
  # the above line sees which values in prediction and creditData2$R1 are equal
  # then it adds them up
  # then divides by total number of values 
  
  return(percentSuccess_knn)
}


kayValues = c(0.1, 1, 2, 5, 10, 100,300);

for (kay in kayValues) {
  
  print(paste0("knn model accuracy for k-value: ",kay,": ", FindModelAccuracy_KNN(kay, creditData2)))
  
}

#[1] "knn model accuracy for 3: 0.814984709480122"
#[1] "knn model accuracy for 5: 0.851681957186544"
#[1] "knn model accuracy for 10: 0.850152905198777"
#[1] "knn model accuracy for 100: 0.836391437308868"
#[1] "knn model accuracy for 300: 0.828746177370031"

print("")
print("")
print("====Question 3.1 : use the ksvm or kknn function to find a good classifier")
#(a) usingcross-validation(dothisforthek-nearest-neighborsmodel;SVMisoptional);and

FindModelAccuracy_CVKNN <- function(kayFold, creditData2) {
  
  prediction_cvknn <- rep(0,(nrow(creditData2)))
  
  #### v0.1b post submission tinkering here, to play with the K value
  # OLD : crossValidationOutput <- cv.kknn(R1 ~ .,creditData2, kcv = kayFold) ##### STOPPING HERE
  crossValidationOutput <- cv.kknn(R1 ~ .,creditData2, kcv = kayFold, k = 12) # as K=1 by default and thats why the crappy accuracy 
  cvRawPredictions = as.data.frame(crossValidationOutput)[2] # these are continuous in nature so have decimal points
  
  for (i in nrow(crossValidationOutput)[2]) {
    prediction_cvknn[i] <- as.integer(fitted(cvRawPredictions[i,])+0.5)
  }
  
   # stopping point: goal   here is to create a function which , when passed k-fold # of ks, and creditData
   # which results in the 
  percentSuccess_cvknn <- sum(prediction_cvknn == creditData2$R1)/nrow(creditData2);
  # the above line sees which values in prediction and creditData2$R1 are equal
  # then it adds them up
  # then divides by total number of values 
  
  return(percentSuccess_cvknn)
  
}


# running the actual function, with 10 kay-fold.
kFold = 10
print(paste0("Model accuracy based on K-fold nearest neighbor with kFold = ",kFold, " is ", FindModelAccuracy_CVKNN(10, creditData2)))




#(b) splittingthedataintotraining,validation,andtestdatasets(pickeitherKNNorSVM;theother  is optional).

set.seed(1) # for Question 3
creditDataTrainingIndices <- sample(nrow(creditData2), size = floor(nrow(creditData2)*0.7))
creditDataTraining <- creditData2[creditDataTrainingIndices,]

# will split the remaining 30% into validation and testing data sets
restOfData <- creditData2[-creditDataTrainingIndices,]
creditDataValidation <- restOfData[1:floor(nrow(restOfData)*0.5),]
creditDataTesting <- restOfData[ceiling(nrow(restOfData)*0.5):nrow(restOfData),]


CValues = c(0.0001, 0.001, 0.01, 0.1, 1, 1, 10, 20, 25, 75)
accuracy <- rep (0,10)
countt=1;

# using SVM instead of KKNN here..
for (c in CValues) {
  modelOutput <- ksvm (as.matrix(creditDataTraining[,1:10]),
                     as.factor(creditDataTraining[,11]),
                     type = 'C-svc',
                     kernel = "vanilladot",
                     C = c,
                     scaled = TRUE)

  guess <- predict(modelOutput, creditDataValidation[ ,1:10])
  accuracy[countt] = sum(guess == creditDataValidation[ ,11]) / nrow(creditDataValidation)
  countt = countt +1
}



print(paste0("### best accuracy measured in validation round: ", accuracy[which.max(accuracy[1:10])]))
print(paste0("### Finding the best C value: ", CValues[which.max(accuracy[1:10])]))

# find the best performing model...
best_model <- ksvm (as.matrix(creditDataTraining[, 1:10]),
                    as.factor(creditDataTraining[, 11]),
                    type = "C-svc",
                    kernel = "vanilladot", 
                    C = CValues[which.max(accuracy[1:10])],
                    scaled = TRUE)

finalModelAccuracy <- sum(predict(best_model, creditDataTesting[, 1:10]) == creditDataTesting$R1) / nrow(creditDataTesting)
print(paste0("the performance of best model on test data = ", finalModelAccuracy))

