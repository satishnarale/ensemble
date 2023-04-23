#' Bagging Ensemble Learning Function 
#' 
#' This function takes 1) dataset, 2) Split ratio,3) Number of samples on which models to be train, 4) Response variable as arguments, 5) Numbers of trees to train 
#' It functions takes out samples from the dataset provided as per the Learner number passed in the argument  
#' Each sample will be train using Random Forest with number of trees passed in argument 
#' This function returns Average Accuracy 
#'
#' @param dataset, Split ratio, Number of Learners, Response variable, Number of Trees
#' @return Average Accuracy
#' @export
enbagg <- function(data, split, learn, resp_var,tree) {
  # Load required packages
  library(randomForest)
  
  # Train multiple models with different settings
  formula <- as.formula(paste(resp_var, "~ ."))
  
  acc <- numeric(learn)
  for(i in 1:learn) {
    trainIndex <- sample(1:nrow(data), split * nrow(data))
    trainData <- data[trainIndex, ]
    testData <- data[-trainIndex, ]
    mod <- randomForest(formula, data = trainData, ntree = tree)
    pred <- as.character(predict(mod, testData))
    acc[i] <- sum(pred == testData[, resp_var]) / length(testData[, resp_var])
  }
  a <- mean(acc, na.rm = TRUE)
  return(a)
}

