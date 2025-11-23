RegressionNegLogLikelihood <- function(beta, X, y) {
  predictions <- X %*% beta
  residuals <- y - predictions
  sigma_squared <- sum(residuals^2) / length(residuals)
  log_likelihood <- -0.5 * length(y) * log(2 * pi * sigma_squared) - 0.5 * sum(residuals^2) / sigma_squared
  return(-log_likelihood)
}


L1_penalty = function(beta_temp){
  return(sum(abs(beta_temp)))
}

Lasso_regression <- function(X, y, lambda = 1, max_iter = 10000, eps = 10^(-8)) {
  X <- as.matrix(X)
  y <- as.matrix(y)

  X_scaled <- scale(X)
  mean_values <- attr(X_scaled, "scaled:center")
  std_dev_values <- attr(X_scaled, "scaled:scale")
  n <- nrow(X_scaled)
  p <- ncol(X_scaled)

  beta_temp <- rnorm(p + 1)
  X_scaled <- cbind(1, X_scaled)
  beta_list <- matrix(numeric(0), ncol = p + 1)

  for (iter in 1:max_iter) {
    beta_prev <- beta_temp
    for (j in 1:(p + 1)) {
      r_j <- y - X_scaled %*% beta_temp + X_scaled[, j] * beta_temp[j]
      rho_j <- t(X_scaled[, j]) %*% r_j
      beta_temp[j] <- soft_thresholding(rho_j / n, lambda / n)
    }
    beta_list <- rbind(beta_list, t(beta_temp))
    if (sqrt(sum((beta_prev - beta_temp)^2)) < eps) {
      break
    }
  }
  beta0 <- beta_temp[1] - sum(beta_temp[2:(p + 1)] * mean_values / std_dev_values)
  beta_optimal <- c(beta0, beta_temp[2:(p + 1)] / std_dev_values)
  return(list(Beta = beta_optimal, Optim_Trajectory = beta_list))
}


lasso_optimal = function(X,y,step_size=0.01,max_iter=10000,eps = 10^(-10),cv_fold=5){
  lambda_max <- max(abs(crossprod(X, y))) / nrow(X)
  lambda_min <- lambda_max * 0.001
  lambda_list <- exp(seq(log(lambda_max), log(lambda_min), length.out = 50))
  lambda_table = data.frame(cbind(lambda=lambda_list,score=rep(0,length(lambda_list))))

  for (i in 1:length(lambda_list)){
    n <- nrow(X)
    k <- cv_fold
    folds <- sample(cut(seq(1, n), breaks=k, labels=FALSE))
    nll_list <- numeric(k)

    for (j in 1:k) {
      test_indices <- which(folds == j)
      train_indices <- setdiff(1:n, test_indices)
      X_train = X[train_indices, ]
      y_train = y[train_indices]
      X_test = X[test_indices, ]
      y_test = y[test_indices]
      beta_coef = Lasso_regression(X_train,y_train,lambda=lambda_list[i],max_iter=5000)$Beta
      nll_list[j] = RegressionNegLogLikelihood(beta_coef, cbind(1,X_test), y_test)
    }
    lambda_table[i,2] = mean(nll_list)
  }
  min_lambda = lambda_table[lambda_table["score"]==min(lambda_table["score"]),][1,1]
  se1_lambda = lambda_table[abs((lambda_table["score"]-min(lambda_table["score"]))/min(lambda_table["score"])) < 0.03,][1,1]

  min_beta = Lasso_regression(X,y,lambda=min_lambda,max_iter=10000)$Beta
  se1_beta = Lasso_regression(X,y,lambda=se1_lambda,max_iter=10000)$Beta
  return(list("min_lambda" = min_lambda,"set1_lambda" = se1_lambda,"min_beta" = min_beta,"se1_beta" = se1_beta))
}

lasso_bootstrap_inference <- function(X, y, B = 100, prob_thresholds = c(0.5, 0.6, 0.7, 0.8, 0.9)) {
  base_optima = lasso_optimal(X,y)

  coef_1se <- base_optima["se1_beta"]

  n <- length(y)
  boot_coef_1se <- matrix(NA, nrow = B, ncol = ncol(X) + 1)
  colnames(boot_coef_1se) <- c("intercept", colnames(X))
  boot_coef_min <- boot_coef_1se
  boot_lambda <- matrix(NA, nrow = B, ncol = 2)
  colnames(boot_lambda) <- c("1se", "min")

  boot_fit_stats_1se <- matrix(NA, nrow = B, ncol = 3)
  colnames(boot_fit_stats_1se) <- c("mse", "mae", "r_squared")
  boot_fit_stats_min <- matrix(NA, nrow = B, ncol = 3)
  colnames(boot_fit_stats_min) <- c("mse", "mae", "r_squared")

  for (ii in 1:B) {
    boot_ind <- sample(1:n, size = n, replace = TRUE)
    XX <- X[boot_ind, ]
    yy <- y[boot_ind]


    optima = lasso_optimal(XX,yy)

    boot_coef_1se[ii, ] <- optima[["se1_beta"]]
    boot_coef_min[ii, ] <- optima[["min_beta"]]

    boot_lambda[ii, ] <- c(optima[["se1_lambda"]], optima[["min_lambda"]])

    XX = cbind(1,XX)
    boot_fit_stats_1se[ii, "mse"] <- mean((yy - XX%*%optima[["se1_beta"]])^2)
    boot_fit_stats_1se[ii, "mae"] <- mean(abs(yy - XX%*%optima[["se1_beta"]]))
    boot_fit_stats_1se[ii, "r_squared"] <- cor(yy, XX%*%optima[["se1_beta"]])^2

    boot_fit_stats_min[ii, "mse"] <- mean((yy - XX%*%optima[["min_beta"]])^2)
    boot_fit_stats_min[ii, "mae"] <- mean(abs(yy - XX%*%optima[["min_beta"]]))
    boot_fit_stats_min[ii, "r_squared"] <- cor(yy, XX%*%optima[["min_beta"]])^2
  }

  prob_nonzero_1se <- apply(abs(boot_coef_1se) > 1e-6, 2, mean)
  prob_nonzero_min <- apply(abs(boot_coef_min) > 1e-6, 2, mean)
  selected_vars_list <- list()

  for (prob in prob_thresholds) {
    selected_vars <- names(prob_nonzero_1se)[-1][prob_nonzero_1se[-1] > prob]
    selected_vars_df <- data.frame(variable = selected_vars, stringsAsFactors = FALSE)
    selected_vars_list[[paste0("prob_", prob)]] <- selected_vars_df
  }
  results <- list(
    coef_1se = coef_1se,                   # Coefficient estimates at lambda.1se
    boot_coef_1se = boot_coef_1se,         # Bootstrap coefficient estimates at lambda.1se
    boot_coef_min = boot_coef_min,         # Bootstrap coefficient estimates at lambda.min
    boot_lambda = boot_lambda,             # Bootstrap lambda values
    boot_fit_stats_1se = boot_fit_stats_1se,   # Bootstrap fit statistics at lambda.1se
    boot_fit_stats_min = boot_fit_stats_min,   # Bootstrap fit statistics at lambda.min
    prob_nonzero_1se = prob_nonzero_1se,   # Probability of nonzero coefficients at lambda.1se
    prob_nonzero_min = prob_nonzero_min,   # Probability of nonzero coefficients at lambda.min
    selected_vars_list = selected_vars_list    # List of selected variables for each probability threshold
  )
  return(results)
}
soft_thresholding <- function(z, gamma) {
  sign(z) * pmax(abs(z) - gamma, 0)
}

data = read.csv("DiabetesClassification_withTrainIndex.csv")
head(data)
data["Gender"] = as.numeric(data["Gender"] == "M")
data_train = data[data$train_index == 1,]
data_test = data[data$train_index == 0,]
y_train = as.matrix(data_train["Age"])
X_train = as.matrix(data_train[,-1][,1:9])
y_test = as.matrix(data_test["Age"])
X_test = as.matrix(data_test[,-1][,1:9])
head(X_test)

result = lasso_bootstrap_inference(X_train,y_train,B = 500)

result$boot_coef_1se
result$prob_nonzero_1se


final_result = list()
final_result$feature_score = result$prob_nonzero_1se

y_target = y_test
y_pred = cbind(1,X_test)%*%result[["coef_1se"]]$se1_beta
final_result$MSE_test = mean((y_target - y_pred)^2)
final_result$MAE_test = mean(abs(y_target - y_pred))
final_result$R2_test = (cor(y_target, y_pred)^2)[[1]]

y_target = y_train
y_pred = cbind(1,X_train)%*%result[["coef_1se"]]$se1_beta
final_result$MSE_train = mean((y_target - y_pred)^2)
final_result$MAE_train = mean(abs(y_target - y_pred))
final_result$R2_train = (cor(y_target, y_pred)^2)[[1]]


saveRDS(final_result,"/Result/Lasso_Result.RDS")


final_result = readRDS("Result/Lasso_Result.RDS")
#final_result$feature_score
# Top 5 Diagnosis,BUN,CHOL,LDL,BMI
X_top5 = X_train[,c("Diagnosis","BUN","Chol","LDL","BMI")]
X_test_top5 = X_test[,c("Diagnosis","BUN","Chol","LDL","BMI")]
model_top5 = lasso_optimal(X_top5,y_train)
model_full = lasso_optimal(X_train,y_train)

model = model_top5
y_target = y_test
X_target = X_test_top5
y_pred = cbind(1,X_target)%*%model$se1_beta
mse_top5_test = mean((y_target - y_pred)^2)
mae_top5_test = mean(abs(y_target - y_pred))
r2_top5_test = (cor(y_target, y_pred)^2)[[1]]

y_target = y_train
X_target = X_top5
y_pred = cbind(1,X_target)%*%model$se1_beta
mse_top5_train = mean((y_target - y_pred)^2)
mae_top5_train = mean(abs(y_target - y_pred))
r2_top5_train = (cor(y_target, y_pred)^2)[[1]]

top5_model = c(mse_top5_test,mae_top5_test,r2_top5_test,
  mse_top5_train,mae_top5_train,r2_top5_train)

model = model_full
y_target = y_test
X_target = X_test
y_pred = cbind(1,X_target)%*%model$se1_beta
mse_top5_test = mean((y_target - y_pred)^2)
mae_top5_test = mean(abs(y_target - y_pred))
r2_top5_test = (cor(y_target, y_pred)^2)[[1]]

y_target = y_train
X_target = X_train
y_pred = cbind(1,X_target)%*%model$se1_beta
mse_top5_train = mean((y_target - y_pred)^2)
mae_top5_train = mean(abs(y_target - y_pred))
r2_top5_train = (cor(y_target, y_pred)^2)[[1]]

fullmodel = c(mse_top5_test,mae_top5_test,r2_top5_test,
  mse_top5_train,mae_top5_train,r2_top5_train)

result = rbind(top5_model,fullmodel)
colnames(result) = c("MSE_test","MAE_test","R2_test","MSE_train","MAE_train","R2_train")
write.csv(result,"Result/Lasso_Prediction_Score.csv")
write.csv(final_result$feature_score,"Result/Lasso_Feature_Score.csv")
