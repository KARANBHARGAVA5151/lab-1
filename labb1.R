library(dplyr)
library(caret)
kbb_data <- read.csv("C:\\Users\\KARAN BHARGAVA\\Downloads\\oulad-students.csv")
kbb_data$final_result <- as.factor(kbb_data$final_result)
set.seed(123)  
# For reproducibility
trained_indices <- sample(1:nrow(kbb_data), 0.8 * nrow(kbb_data))
trained_data <- kbb_data[trained_indices, ]
test_data <- kbb_data[-trained_indices, ]
logit_model <- glm(final_result ~ ., data = trained_data, family = binomial(link = "logit"))
test_predictions <- predict(logit_model, newdata = test_data, type = "response")
threshold <- 0.5
predicted_classes <- ifelse(test_predictions > threshold, "Pass", "Fail")
actual_classes <- test_data$final_result
accuracy <- sum(predicted_classes == actual_classes) / length(actual_classes)
cat("Accuracy on test set:", accuracy, "\n")


