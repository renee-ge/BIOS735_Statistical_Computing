#' Run RandomForest
#'
#' This function train and return the random forest using "caret" and "randomForest"
#'
#' @param data data.frame the dataset including features and outcomes
#' @param features character the column names of features (X) in data
#' @param outcome character the columns names of outcome (y) in data
#'
#' @return the list of model score (metric), feature score (scores) and models (model)
#'
#' @import caret
#' @import randomForest
#' @import data.table
#'
#' @export
do_rf_v2 <- function(data, features, outcome) {
  # Separate features and outcome
  X <- data[, ..features]
  Y <- data[[outcome]]

  # Run random forest model
  rf_model <- caret::train(x = X, y = Y, method="rf")

  # Obtain feature importance score
  rf_scores <- caret::varImp(rf_model)

  # Make the vector of mse, mae, rsquare
  rf_results_df <- rf_model[["results"]]
  rf_output_1 <- rf_results_df[which.min(rf_results_df$RMSE), c("RMSE", "Rsquared", "MAE")]

  # Return: both the rf model and importance score
  return(list(metric=rf_output_1, scores = rf_scores, model = rf_model))


}

#' Run the gradient boosting model
#'
#' This function train and return the gradient boosting model using "caret" and "gbm"
#'
#' @param data data.frame the dataset including features and outcomes
#' @param features character the column names of features (X) in data
#' @param outcome character the columns names of outcome (y) in data
#' @param tg tuning grid for caret
#'
#' @return the list of model score (metric), feature score (scores) and models (model)
#'
#' @import caret
#' @import gbm
#' @import data.table
#'
#' @export
do_gbm_v2 <- function(data, features, outcome, tg) {

  library(gbm)
  library(caret)
  library(data.table)

  # Separate features and outcome
  X <- data[, ..features]
  Y <- data[[outcome]]

  # Run GBM model

  gbm_model <- caret::train(x = X, y = Y, method = "gbm", tuneGrid = tg, verbose=F)

  # Obtain feature importance score
  gbm_scores <- caret::varImp(gbm_model)

  # Make the vector of mse, mae, rsquare
  gbm_results_df <- gbm_model[["results"]]
  gbm_output_1 <- gbm_results_df[which.min(gbm_results_df$RMSE), c("RMSE", "Rsquared", "MAE")]

  # Return: both the gbm model and importance score
  return(list(metric=gbm_output_1, scores = gbm_scores, model=gbm_model))
}

#' Making DataFrame for scores
#'
#' This function will return the tran and test functions
#'
#' @param ml_result_score_list list list of importance scores of models
#' @param method vector vector of character with methods name
#'
#' @return the list of model score (metric), feature score (scores) and models (model)
#'
#' @import caret
#' @import randomForest
#' @import data.table
#'
#' @export
get_scores <- function(ml_result_score_list, method) {

  input_list <- ml_result_score_list
  score_col_name <- as.character(paste0("Importance_", method))

  variable_names <- rownames(input_list$importance)
  importance_scores <- input_list$importance[ ,1]

  #create the output data frame:
  var_imp_df <- data.frame(Variable = variable_names, Importance = importance_scores)
  colnames(var_imp_df) <- c("Variable", score_col_name)

  return(var_imp_df)
}
