# dependency prep
  # install.packages("caret", dependencies=c("Depends", "Suggests"))
  library(caret)

# data import
  dataset <- read.csv("heart.csv")
  dataset$target <- as.factor(dataset$target)

# data splitting
  validation_index <- createDataPartition(dataset$target, p=0.60, list=FALSE)
  #test_index <- createDataPartition(dataset$Species, p=0.60, list=FALSE)
  
  # 
  validation <- dataset[-validation_index,]
  #test <- test_index[-test_index,]

  # 
  dataset <- dataset[validation_index,]

# pre-data analysis
  
  # dimensins
  dim(dataset)
  
  # var types
  sapply(dataset, class)
  
  # head
  head(dataset)
  
  # class distribution
  percentage <- prop.table(table(dataset$target)) * 100
  cbind(freq=table(dataset$target), percentage=percentage)
  
  # summary
  summary(dataset)
  
  # univariate plot
  x <- dataset[,1:13]
  y <- dataset[,14]
  
  
  # boxplot
  par(mfrow=c(1,14))
  for(i in 1:14) {
    boxplot(x[,i], main=names(dataset)[i])
  }
  
  # barplot - class dist.
  plot(y)
  
  # scatterplot
  featurePlot(x=x, y=y, plot="ellipse")
  
  # box and whisker plots
  featurePlot(x=x, y=y, plot="box")
  
  # density plot
  scales <- list(x=list(relation="free"), y=list(relation="free"))
  featurePlot(x=x, y=y, plot="density", scales=scales)
  
  
  # cross val.
  control <- trainControl(method="cv", number=10)
  metric <- "Accuracy"

# build  
  # init model
  set.seed(7)
  glm.fit <- train(target ~ age + sex + cp + trestbps + chol + fbs + restecg + thalach + exang + oldpeak + slope + ca + thal, data = dataset, method="glm", metric=metric, trControl=control)
  
  set.seed(7)
  nb.fit <- train(target ~ age + sex + cp + trestbps + chol + fbs + restecg + thalach + exang + oldpeak + slope + ca + thal, data=dataset, method="nb", metric=metric, trControl=control)
  
  # summary
  summary(glm.fit)
  
  # summarize accuracy
  results <- resamples(list(glm=glm.fit, nb=nb.fit))
  summary(results)
  dotplot(results)
  
  # predict
  predictions <- predict(glm.fit, validation)
  confusionMatrix(predictions, validation$target)
  
  print(glm.fit)