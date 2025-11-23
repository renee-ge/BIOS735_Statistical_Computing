#' Run RandomForest
#'
#' This function train and return the random forest using the package "randomForest"
#'
#' @param data data.frame the dataset including features and outcomes
#' @param features character the column names of features (X) in data
#' @param outcome character the columns names of outcome (y) in data
#'
#' @return the list of feature score (rf_scores) and models (rf_model)
#'
#' @import randomForest
#' @import data.table
#'
#' @export
do_rf <- function(data, features, outcome) {

  library(randomForest)
  library(data.table)

  # Separate features and outcome
  X <- data[, ..features]
  Y <- data[[outcome]]

  # Run random forest mod
  rf_model <- randomForest(x = X, y = Y)

  # Obtain feature importance score
  rf_scores <- importance(rf_model)

  # Return: both the rf model and importance score
  return(list(rf_scores = rf_scores, rf_model = rf_model))
}

#' Run RandomForest
#'
#' This function train and return the random forest using the package "caret"
#'
#' @param data data.frame the dataset including features and outcomes
#' @param features character the column names of features (X) in data
#' @param outcome character the columns names of outcome (y) in data
#'
#' @return the list of feature score (rf_scores) and models (rf_model)
#'
#' @import caret
#' @import data.table
#'
#' @export
do_rf_2 <- function(data, features, outcome) {
  # Separate features and outcome
  X <- data[, ..features]
  Y <- data[[outcome]]

  # Run random forest model
  rf_model <- caret::train(x = X, y = Y, method="rf")

  # Obtain feature importance score
  rf_scores <- caret::varImp(rf_model)

  # Return: both the rf model and importance score
  return(list(rf_scores = rf_scores, rf_model = rf_model))
}

#' Run Gradient Boosting Model
#'
#' This function train and return the gradient boosting model using the package "caret"
#'
#' @param data data.frame the dataset including features and outcomes
#' @param features character the column names of features (X) in data
#' @param outcome character the columns names of outcome (y) in data
#' @param tg tuning grid for caret
#'
#' @return the list of feature score (gbm_scores) and models (gbm_model)
#'
#' @import caret
#' @import data.table
#'
#' @export
do_gbm <- function(data, features, outcome, tg) {

  # Separate features and outcome
  X <- data[, ..features]
  Y <- data[[outcome]]

  # Run GBM model
  gbm_model <- caret::train(x = X, y = Y, method = "gbm", tuneGrid = tg, verbose=F)

  # Obtain feature importance score
  gbm_scores <- caret::varImp(gbm_model)

  # Return: both the gbm model and importance score
  return(list(gbm_scores = gbm_scores, gbm_model = gbm_model))
}
