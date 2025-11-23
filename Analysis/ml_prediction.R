library(caret)
library(data.table)
library(randomForest)
library(gbm)
library(tidyr)

# input data
input.dir <- "/Users/ling/Documents/OneDrive - University of North Carolina at Chapel Hill/MacBook/2024Spring/Bios735/FinalProject/DiabetesClassification.csv"
input_data <- fread(input.dir, header=T, sep=",")

# set train and test set
set.seed(73520244)
train.index <- rbinom(nrow(data), size = 1, prob = 0.75)
input_data$train_index <- train.index

train_data <- input_data[input_data$train_index == 1,]
test_data <- input_data[input_data$train_index == 0,]
train_data[which(train_data$Gender == "f"),"Gender"] <- "F"
table(train_data$Gender)
train_data$Gender <- factor(train_data$Gender)
train_data$Diagnosis <- factor(train_data$Diagnosis)

test_data$Gender <- factor(test_data$Gender)
test_data$Diagnosis <- factor(test_data$Diagnosis)

# gbm:
gbm_tg <- expand.grid(n.trees=c(70, 75),
                      interaction.depth=c(5,6,7),
                      shrinkage=c(0.04,0.05,0.06,0.07),
                      n.minobsinnode=c(40, 45, 50))

fit_gbm_v2 <- do_gbm_v2(data=train_data, outcome="Age", tg=gbm_tg,
                        features=c("Gender", "BMI", "Chol", "TG", "HDL", "LDL", "Cr", "BUN", "Diagnosis"))

gbm_scores <- fit_gbm_v2$scores
gbm_scores <- get_scores(gbm_scores, "gbm")

# rf:
fit_rf <- do_rf_v2(data=train_data, outcome="Age", 
                  features=c("Gender", "BMI", "Chol", "TG", "HDL", "LDL", "Cr", "BUN", "Diagnosis"))
rf_scores <- fit_rf$scores
rf_scores <- get_scores(rf_scores, "rf")

# merge two scores:
ml_scores <- merge(rf_scores, gbm_scores, by="Variable")

## Plot for importance score
data_long <- tidyr::pivot_longer(ml_scores, cols = starts_with("Importance"), names_to = "Model", values_to = "Importance")
# rf
rf_polt <- ggplot(data_long[data_long$Model == "Importance_rf", ], aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Variable Importance - Random Forest", x = "Variable", y = "Importance") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(filename = "D:/OneDrive - University of North Carolina at Chapel Hill/MacBook/2024Spring/Bios735/FinalProject/rf_plot.png",
       plot = rf_polt,height = 4, width = 6, units = "in")

#gbm
gbm_plot <- ggplot(data_long[data_long$Model == "Importance_gbm", ], aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "lightgreen") +
  labs(title = "Variable Importance - Gradient Boosting Machine", x = "Variable", y = "Importance") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(filename = "D:/OneDrive - University of North Carolina at Chapel Hill/MacBook/2024Spring/Bios735/FinalProject/gbm_plot.png",
       plot = gbm_plot,height = 4, width = 6, units = "in")

## Train the model again with the top 5 features:
#gbm
top5_features_gbm <- ml_scores[order(ml_scores$Importance_gbm, decreasing = T),1][1:5]
fit_gbm_top5 <- do_gbm_v2(data=train_data, outcome="Age", tg=gbm_tg,
                          features=top5_features_gbm)
test_data_gbm <- test_data[, ..top5_features_gbm]
predictions_gbm <- predict(fit_gbm_top5$model, newdata = test_data_gbm)
R_2_gbm <- (cor(predictions_gbm, test_data$Age))^2

#rf
top5_features_rf <- ml_scores[order(ml_scores$Importance_rf, decreasing = T),1][1:5]
fit_rf_top5 <- do_rf_v2(data=train_data, outcome="Age", 
                        features=top5_features_rf)
test_data_rf <- test_data[, ..top5_features_rf]
predictions_rf <- predict(fit_rf_top5$model, newdata = test_data_rf)
R_2_rf <- (cor(predictions_rf, test_data$Age))^2


## Prediction using all features
# gbm
test_data_all <- test_data[,-c("Age", "train_index")]
test_data_all$Gender <- factor(test_data_2$Gender)
test_data_all$Diagnosis <- factor(test_data_2$Diagnosis)
predictions_gbm_all <- predict(fit_gbm_v2$model, newdata = test_data_all)
R_2_gbm_all <- (cor(predictions_gbm_all, test_data$Age))^2

#rf
predictions_rf_all <- predict(fit_rf$model, newdata = test_data_all)
R_2_rf_all <- (cor(predictions_rf_all, test_data$Age))^2


# make a data frame to store the model metrics:
ml_model_metrics <- rbind(fit_gbm_v2$metric, fit_gbm_top5$metric, fit_rf$metric, fit_rf_top5$metric)
rownames(ml_model_metrics) <- c("gbm_all", "gbm_top5", "rf_all", "rf_top5")
# and the prediction R2
pred_r2 <- data.frame(matrix(NA, nrow=4, ncol=1))
colnames(pred_r2) <- "Rsuqared"
pred_r2$Rsuqared <- c(R_2_gbm_all, R_2_gbm, R_2_rf_all, R_2_rf)
rownames(pred_r2) <- c("gbm_all", "gbm_top5", "rf_all", "rf_top5")

#save the ml results
fwrite(pred_r2, file="~/Documents/OneDrive - University of North Carolina at Chapel Hill/MacBook/2024Spring/Bios735/FinalProject/pred_scores_ml.csv",
       quote = F, col.names = T, row.names = T, sep = ",")
fwrite(ml_model_metrics, file="~/Documents/OneDrive - University of North Carolina at Chapel Hill/MacBook/2024Spring/Bios735/FinalProject/model_metric.csv",
       quote = F, col.names = T, row.names = T, sep = ",")


## plot with LASSO:
Lasso_scores <- fread("~/Documents/OneDrive - University of North Carolina at Chapel Hill/MacBook/2024Spring/Bios735/FinalProject/Lasso_Feature_Score.csv")
colnames(Lasso_scores) <- c("Variables", "Importance")
Lasso_plot_data <- Lasso_scores[order(Lasso_scores$Importance, decreasing = TRUE), ]  # Sort by importance score
Lasso_plot_data$Variables <- factor(Lasso_plot_data$Variables, levels = rev(Lasso_plot_data$Variables))  # Ensure correct ordering of features

Lasso_plot <- ggplot(Lasso_plot_data, aes(x = Variables, y = Importance)) +
  geom_bar(stat = "identity", fill = "pink") +
  labs(title = "Variable Importance - LASSO", x = "Variable", y = "Importance") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(filename = "~/Documents/OneDrive - University of North Carolina at Chapel Hill/MacBook/2024Spring/Bios735/FinalProject/LASSO_plot.png",
       plot = Lasso_plot,height = 4, width = 6, units = "in")

# plot of the prediction behavior

