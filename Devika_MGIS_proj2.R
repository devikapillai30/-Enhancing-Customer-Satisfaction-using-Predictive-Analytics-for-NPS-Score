options(scipen=999)

#for cross validation
library(boot)
library(caret)
#for oversampling
install.packages("ROSE")
library(ROSE)
#for knn
library(class)
#for decision tree
library(rpart)

# getting to know dataset
summary(subset)
table(subset$NPSnineormore)
# correlation
cor(subset[, c("Age", "Female", "Years", "Certficates", "Feedback", "Salary", "NPSnineormore")])



#### Dividing the dataset into train and test ####
set.seed(123)
trainIndex <- sample(nrow(subset), 0.8 * nrow(subset))
train <- subset[trainIndex, ]
test <- subset[-trainIndex, ]



#### Logistic regression ####
#model 1
log_fit = glm(NPSnineormore ~ Age + Female + Years + Personality +  Certficates + Feedback + Salary, data= train, family='binomial')
summary(log_fit)
# Make predictions on the test set
log_pred <- predict(log_fit, newdata=test, type="response")
predicted_classes <- ifelse(log_pred > 0.5, "1", "0")
# Evaluate the model
confusion_matrix <- table(predicted_classes, test$NPSnineormore)
TP <- confusion_matrix[2,2]
TN <- confusion_matrix[1,1]
FP <- confusion_matrix[1,2]
FN <- confusion_matrix[2,1]
# compute accuracy, precision, recall, f1 score
accuracy <- (TP + TN) / (TP + TN + FP + FN)
precision <- TP / (TP + FP)
recall <- TP / (TP + FN)
f1 <- 2 * precision * recall / (precision + recall)
# print results
cat("Accuracy:", round(accuracy, 3), "\n")
cat("Precision:", round(precision, 3), "\n")
cat("Recall:", round(recall, 3), "\n")
cat("F1 score:", round(f1, 3), "\n")


#model 2
log_fit = glm(NPSnineormore ~ Female + Years + Personality +  Certficates + Feedback + Salary + Years*Salary, data= train, family='binomial')
summary(log_fit)
# Make predictions on the test set
log_pred <- predict(log_fit, newdata=test, type="response")
predicted_classes <- ifelse(log_pred > 0.5, "1", "0")
# Evaluate the model
confusion_matrix <- table(predicted_classes, test$NPSnineormore)
TP <- confusion_matrix[2,2]
TN <- confusion_matrix[1,1]
FP <- confusion_matrix[1,2]
FN <- confusion_matrix[2,1]
# compute accuracy, precision, recall, f1 score
accuracy <- (TP + TN) / (TP + TN + FP + FN)
precision <- TP / (TP + FP)
recall <- TP / (TP + FN)
f1 <- 2 * precision * recall / (precision + recall)
# print results
cat("Accuracy:", round(accuracy, 3), "\n")
cat("Precision:", round(precision, 3), "\n")
cat("Recall:", round(recall, 3), "\n")
cat("F1 score:", round(f1, 3), "\n")


#model 3
log_fit = glm(NPSnineormore ~ Female + Years + Certficates + Personality + Feedback + Salary + Certficates*Feedback, data= train, family='binomial')
summary(log_fit)
# Make predictions on the test set
log_pred <- predict(log_fit, newdata=test, type="response")
predicted_classes <- ifelse(log_pred > 0.5, "1", "0")
# Evaluate the model
confusion_matrix <- table(predicted_classes, test$NPSnineormore)
TP <- confusion_matrix[2,2]
TN <- confusion_matrix[1,1]
FP <- confusion_matrix[1,2]
FN <- confusion_matrix[2,1]
# compute accuracy, precision, recall, f1 score
accuracy <- (TP + TN) / (TP + TN + FP + FN)
precision <- TP / (TP + FP)
recall <- TP / (TP + FN)
f1 <- 2 * precision * recall / (precision + recall)
# print results
cat("Accuracy:", round(accuracy, 3), "\n")
cat("Precision:", round(precision, 3), "\n")
cat("Recall:", round(recall, 3), "\n")
cat("F1 score:", round(f1, 3), "\n")



##### Oversampling ####
# Perform oversampling
train_oversampled <- ovun.sample(NPSnineormore ~ ., data=train, method = "over", p=0.5, seed = 123)
# Fit the logistic regression model on the oversampled data
log_fit <- glm(NPSnineormore ~ Female + Years + Certficates + Personality + Feedback + Salary + Certficates*Feedback, data = train_oversampled$data, family = "binomial")
summary(log_fit)
# Make predictions on the test set
log_predictions <- predict(log_fit, newdata = test, type = "response")
# Convert the predictions to binary labels
log_labels <- ifelse(log_predictions > 0.5, 1, 0)
# Evaluate the model
confusion_matrix <- table(log_labels, test$NPSnineormore)
TP <- confusion_matrix[2,2]
TN <- confusion_matrix[1,1]
FP <- confusion_matrix[1,2]
FN <- confusion_matrix[2,1]
# compute accuracy, precision, recall, f1 score
accuracy <- (TP + TN) / (TP + TN + FP + FN)
precision <- TP / (TP + FP)
recall <- TP / (TP + FN)
f1 <- 2 * precision * recall / (precision + recall)
# print results
cat("Accuracy:", round(accuracy, 3), "\n")
cat("Precision:", round(precision, 3), "\n")
cat("Recall:", round(recall, 3), "\n")
cat("F1 score:", round(f1, 3), "\n")



#### Decision Tree ####
treeModel <- rpart(NPSnineormore ~ Age + Female + Years + Personality +  Certficates + Feedback + Salary, data = train, method = "class")
summary(treeModel)
# Make predictions on the test set
treePredictions <- predict(treeModel, newdata = test, type = "class")
# Evaluate the model
confusion_matrix <- table(treePredictions, test$NPSnineormore)
TP <- confusion_matrix[2,2]
TN <- confusion_matrix[1,1]
FP <- confusion_matrix[1,2]
FN <- confusion_matrix[2,1]
# compute accuracy, precision, recall, f1 score
accuracy <- (TP + TN) / (TP + TN + FP + FN)
precision <- TP / (TP + FP)
recall <- TP / (TP + FN)
f1 <- 2 * precision * recall / (precision + recall)
# print results
cat("Accuracy:", round(accuracy, 3), "\n")
cat("Precision:", round(precision, 3), "\n")
cat("Recall:", round(recall, 3), "\n")
cat("F1 score:", round(f1, 3), "\n")



################################################################################
#### Cross validation #####
#divide the data into training and test
set.seed(42) 
n = dim(subset)[1] 
train = sample(1:n, .7*n)
#build the model on training set only
cross_fit = glm(NPSnineormore ~Female + Years + Certficates + Personality + Feedback + Salary + Certficates*Feedback, data= subset[train,], family='binomial')
summary(cross_fit)
#making predictions on test set
p <- predict(cross_fit, subset[-train,], type='response')
y_pred <- ifelse(p>=.5, "1", "0")
y_pred <- as.numeric(y_pred)
#report the test performance
table(subset$NPSnineormore[-train], y_pred)
prop.table(table(subset$NPSnineormore[-train], y_pred))
confusion <- table(subset$NPSnineormore[-train], y_pred)
TP <- confusion[2,2]
TN <- confusion[1,1]
FP <- confusion[1,2]
FN <- confusion[2,1]
# compute accuracy, precision, recall, f1 score
accuracy <- (TP + TN) / (TP + TN + FP + FN)
precision <- TP / (TP + FP)
recall <- TP / (TP + FN)
f1 <- 2 * precision * recall / (precision + recall)
# print results
cat("Accuracy:", round(accuracy, 3), "\n")
cat("Precision:", round(precision, 3), "\n")
cat("Recall:", round(recall, 3), "\n")
cat("F1 score:", round(f1, 3), "\n")



#### K - Fold Cross Validation ####
# set number of folds
k <- 10
# create a vector of indices for splitting into folds
fold_indices <- rep(1:k, length.out = n)
# randomly shuffle the indices
fold_indices <- sample(fold_indices)
# initialize empty vectors for storing evaluation metrics
accuracy_vec <- numeric(k)
precision_vec <- numeric(k)
recall_vec <- numeric(k)
f1_vec <- numeric(k)
# loop through each fold
for (i in 1:k) {
  # get the indices for the current fold
  fold_indices_i <- which(fold_indices == i)
  # split the data into training and test sets for this fold
  train_indices <- setdiff(1:n, fold_indices_i)
  test_indices <- fold_indices_i
  # fit the model on the training set
  cross_fit <- glm(NPSnineormore ~ Female + Years + Certficates + Personality + Feedback + Salary + Certficates*Feedback,
                   data = subset[train_indices,],
                   family = 'binomial')
  # make predictions on the test set
  p <- predict(cross_fit, subset[test_indices,], type='response')
  y_pred <- ifelse(p>=.5, "1", "0")
  y_pred <- as.numeric(y_pred)
  # evaluate the predictions
  confusion <- table(subset$NPSnineormore[test_indices], y_pred)
  TP <- confusion[2,2]
  TN <- confusion[1,1]
  FP <- confusion[1,2]
  FN <- confusion[2,1]
  # compute accuracy, precision, recall, f1 score for this fold
  accuracy_vec[i] <- (TP + TN) / (TP + TN + FP + FN)
  precision_vec[i] <- TP / (TP + FP)
  recall_vec[i] <- TP / (TP + FN)
  f1_vec[i] <- 2 * precision_vec[i] * recall_vec[i] / (precision_vec[i] + recall_vec[i])
}
# compute the average evaluation metrics across all folds
mean_accuracy <- mean(accuracy_vec)
mean_precision <- mean(precision_vec)
mean_recall <- mean(recall_vec)
mean_f1 <- mean(f1_vec)
# print the results
cat("Average accuracy:", round(mean_accuracy, 3), "\n")
cat("Average precision:", round(mean_precision, 3), "\n")
cat("Average recall:", round(mean_recall, 3), "\n")
cat("Average F1 score:", round(mean_f1, 3), "\n")


