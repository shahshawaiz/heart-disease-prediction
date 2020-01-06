#
## Welcome to naive bayes solution for heart disease dataset from kaggle
# https://www.kaggle.com/johnsmith88/heart-disease-dataset
#
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# Set working directory to 'current' in R-Studio:
# Click:: Session -> Set Working Directory -> To Source File Location
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

library(lattice)
library(ggplot2)

# split data
#install.packages("caret")
library(caret)

# naive bayes
#install.packages("e1071")
library(e1071)

print(' ***Make sure you set working dir to current, see start of script! ***')
mydata <- read.table(file = "heart.csv",
           header = TRUE,
           sep = ",",
           stringsAsFactors = FALSE)

print(names(mydata))
print('Variable to predict: target')
#dim(mydata)
#summary(mydata)
#data.cor = cor(mydata)
#pairs(mydata)

print('Random split data to 70% train and 30% test')
set.seed(7267166)
trainIndex=createDataPartition(mydata$target, p=0.7)$Resample1
train=mydata[trainIndex, ]
test=mydata[-trainIndex, ]

#message("info in red")
print('Check the balance')
print(table(mydata$target))
print(table(train$target))

NB_disease<-naiveBayes(as.factor(target)~., data=train)
NB_disease
 
# With changes: from https://rpubs.com/riazakhan94/naive_bayes_classifier_e1071
printALL=function(model){
  trainPred=predict(model, newdata = train, type = "class")
  trainTable=table(train$target, trainPred)
  testPred=predict(model, newdata=test, type="class")
  testTable=table(test$target, testPred)
  trainAcc=(trainTable[1,1]+trainTable[2,2])/sum(trainTable)
  testAcc=(testTable[1,1]+testTable[2,2])/sum(testTable)
  message("Contingency Table for Training Data")
  print(trainTable)
  message("Contingency Table for Test Data")
  print(testTable)
  message("Accuracy")
  print(round(cbind(trainAccuracy=trainAcc, testAccuracy=testAcc),3))
}
printALL(NB_disease)

print('End of line')
