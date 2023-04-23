#' Stacking Ensemble Learning Function
#' 
#' This function takes 1) dataset, 2) Split ratio, 3) Response variable as arguments 
#' It uses SVM, Decision tree and Logistic Regression as base learners for stacking 
#' For meta model training it uses Random Forest 
#' This function returns confusion matrix 
#' please not that response variable should have factor levels or else SVM will not work 
#'
#' @param dataset,Split ratio, Response variable 
#' @return Confusion Matrix
#' @export
enstack <- function(iris, split, response) {
  library(caret)
  library(e1071)
  library(rpart)
  library(randomForest)
  library(nnet)
  library(tidyverse)
  # set a seed for reproducibility
  set.seed(123)
  
  # split the data into training and testing sets
  train_idx <- createDataPartition(iris[[resp_var]], p = split, list = FALSE)
  train <- iris[train_idx, ]
  test <- iris[-train_idx, ]
  
  # define the base models
  formula<-as.formula(paste(resp_var, "~ ."))
  svm_model <- svm(formula, data = train, kernel = "linear", probability = FALSE)
  tree_model <- rpart(formula, data = train, minsplit = 10)
  logistic_model <- nnet::multinom(formula, data = train)
  #logistic_model <- glm(Species ~ ., data = train, family = "multinom")
  
  # make predictions with the base models
  svm_preds <- predict(svm_model, train, probability = FALSE)
  svm_preds
  confusionMatrix(svm_preds, train[[resp_var]])
  tree_preds <- predict(tree_model, train, type = "class")
  tree_preds
  confusionMatrix(tree_preds, train[[resp_var]])
  logistic_preds <- predict(logistic_model, train, type = "class")
  logistic_preds
  confusionMatrix(logistic_preds, train[[resp_var]])
  
  
  # stack the predictions
  stacked_data <- data.frame(Species = train[[resp_var]], svm_preds, tree_preds, logistic_preds)
  # train the meta-model using random forest
  meta_model <- train(formula, data = stacked_data, method = "rf", ntree = 500)
  
  # make predictions on the test data with the base models
  svm_test_preds <- predict(svm_model, test)
  confusionMatrix(svm_test_preds, test[[resp_var]])
  tree_test_preds <- predict(tree_model, test, type = "class")
  confusionMatrix(tree_test_preds, test[[resp_var]])
  logistic_test_preds <- predict(logistic_model, test, type = "class")
  confusionMatrix(logistic_test_preds, test[[resp_var]])
  
  # create a stacked test dataset
  test_stacked <- data.frame(Species = test[[resp_var]], svm_preds=svm_test_preds, tree_preds=tree_test_preds, logistic_preds=logistic_test_preds)
  
  # make predictions with the meta-model
  ensemble_preds <- predict(meta_model, test_stacked)
  levels(ensemble_preds)
  
  return(confusionMatrix(ensemble_preds, test$Species))
}


