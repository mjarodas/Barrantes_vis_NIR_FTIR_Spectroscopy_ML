################################
## Load required packages ----
################################

library(readxl)
library(writexl)
library(caret)
library(ggplot2)
library(dplyr)
library(e1071)  
library(kernlab)

#######################################
## Load Raw Data
#######################################

## Load raw data ----
raw_data <- read_excel("~~~/~~/raw_data.xlsx")

#######################################
## SVM Analysis
#######################################

# Separate Variables
X <- datos[, -(1:2)]  # Espectra
Y <- as.factor(datos$Group)  # Group


# Divide the Train and Test
set.seed(123)  
trainIndex <- createDataPartition(Y, p = 0.7, list = FALSE)
X_train <- X[trainIndex, ]
X_test <- X[-trainIndex, ]
Y_train <- Y[trainIndex]
Y_test <- Y[-trainIndex]


ctrl <- trainControl(method = "cv", number = 5)  


# Definine the values to explore of sigma and C
my_grid <- expand.grid(
  sigma = c(0.01, 0.05, 0.1),
  C = c(1, 2, 5, 10)
)


svm_model <- train(X_train, Y_train, method = "svmLinear",
                   trControl = ctrl,
                   tuneLength = my_grid         
                    )

#Best model
print(svm_model$bestTune)


# Predictions
y_pred_train_svm <- predict(svm_model, X_train)
y_pred_test_svm <- predict(svm_model, X_test)

# Metrics
train_acc_svm <- sum(y_pred_train_svm == Y_train) / length(Y_train)
print (train_acc_svm)
test_acc_svm <- sum(y_pred_test_svm == Y_test) / length(Y_test)
print(test_acc_svm)


# RMSEE y RMSEP
RMSEE_svm <- sqrt(mean((as.numeric(Y_train) - as.numeric(y_pred_train_svm))^2))
RMSEP_svm <- sqrt(mean((as.numeric(Y_test) - as.numeric(y_pred_test_svm))^2))

# R2
R2_svm <- 1 - (sum((as.numeric(Y_train) - as.numeric(y_pred_train_svm))^2) /
                 sum((as.numeric(Y_train) - mean(as.numeric(Y_train)))^2))

# Q2
cv_preds_svm <- predict(svm_model, X_test)
cv_actual_svm <- as.numeric(Y_test)

Q2_svm <- 1 - (sum((cv_actual_svm - as.numeric(cv_preds_svm))^2) /
                 sum((cv_actual_svm - mean(cv_actual_svm))^2))

# Final Summary
results_svm <- data.frame(
  LVs = NA,  
  R2 = R2_svm,
  Q2 = Q2_svm,
  RMSEE = RMSEE_svm,
  RMSEP = RMSEP_svm,
  Train_ACC = train_acc_svm,
  Test_ACC = test_acc_svm
)

print(resultados_svm)

#Save the results

write_xlsx(resultss_svm, "results_SVM.Xlsx")

# Confusion Matrix
y_pred_test_svm <- as.factor(y_pred_test_svm)
Y_test <- as.factor(Y_test)

conf_matrix <- confusionMatrix(data = y_pred_test_svm, reference = Y_test)
conf_matrix_table <- as.data.frame(as.table(conf_matrix))


conf_matrix_plot <- ggplot(conf_matrix_table, aes(x = Prediction, y = Reference, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), color = "white", size = 5) +
  scale_fill_gradient(low = "white", high = "blue") +
  theme_minimal() +
  labs(x = "Predicted", y = "Actual", title = "Confusion Matrix")

print(conf_matrix_plot)

ggsave("confusion_matrix_SVM_Train_Test.png", plot = conf_matrix_plot, width = 7, height =7)
         

######################################################
## Export packages version that have been used
######################################################


# Specify the path to your script
script_file <- "~~/~~/SVM_Train_Test_script.R"

# Read the script and filter to lines with library or require
script_lines <- readLines(script_file)
library_lines <- grep("^(library|require)\\(", script_lines, value = TRUE)

# Extact the pakage names
pkg_names <- gsub("^(library|require)\\((['\"]?)([A-Za-z0-9\\.]+)(['\"]?)\\).*", "\\3", library_lines)
pkg_names <- unique(pkg_names)

cat("Packages found:\n")
print(pkg_names)

# Versions of the packages
pkg_versions <- sapply(pkg_names, function(pkg) as.character(packageVersion(pkg)))

req_lines <- paste0(pkg_names, "==", pkg_versions)

# specify the path to create requirements.txt
req_file <- "~~~/~~~/requirements.txt"
writeLines(req_lines, con = req_file)
