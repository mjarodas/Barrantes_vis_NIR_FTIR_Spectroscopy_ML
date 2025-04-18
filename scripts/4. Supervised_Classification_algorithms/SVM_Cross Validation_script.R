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

# Separate predictor variables (X) and class (Y)
X <- datos[, -(1:2)]            
Y <- as.factor(datos$Group) 

# Convert X to pure numeric if there are non-numeric columns
X <- as.data.frame(lapply(X, function(x) as.numeric(as.character(x))))

# Remove columns with NA, if any
keep_cols <- colSums(is.na(X)) == 0
X <- X[, keep_cols, drop = FALSE]

#  Convert to matrix if desired, caret usually accepts data.frame
X <- as.matrix(X)

cat("Final dimension:", dim(X), "\n")

#############################################################################
# 2) Define cross validation and hyperparameter grid
#############################################################################

# 2.1) Training control: CV of 5 folds
ctrl <- trainControl(
  method = "cv",    
  number = 5,     
  savePredictions = "final" 
)

# 2.2) Define the grid of values to be explored (example for radial SVM))
my_grid <- expand.grid(
  sigma = c(0.01, 0.05, 0.1),
  C = c(1, 2, 5, 10)
)

#############################################################################
# 3) Train SVM model with radial kernel (svmRadial)
#############################################################################

set.seed(123)
svm_model <- train(
  x = X,
  y = Y,
  method = "svmRadial",   
  trControl = ctrl,
  tuneGrid  = my_grid      
)

#############################################################################
# 4) Results of the optimal CV and hyperparameters
#############################################################################

cat("\nBest combination of C and sigma:\n")
print(svm_model$bestTune)

cat("\nResults of best combinations:\n")
print(svm_model$results)


#############################################################################
# 5) Metrics on the ENTIRE dataset (optional)
#############################################################################

# 5.1) Predictions on the ENTIRE dataset
y_pred_full <- predict(svm_model, X)

# 5.2) Accuracy
acc_full <- mean(y_pred_full == Y)

cat(sprintf("\nAccuracy: %.2f%%\n", 100 * acc_full))

# 5.3) RMSE, R2, Q2 (limited interpretation in classification)
# Convert classes to numeric
y_real_num <- as.numeric(Y)
y_pred_num <- as.numeric(y_pred_full)

RMSE <- sqrt(mean((y_real_num - y_pred_num)^2))

SSE  <- sum((y_real_num - y_pred_num)^2)
SST  <- sum((y_real_num - mean(y_real_num))^2)
R2   <- 1 - SSE / SST

Q2   <- 1 - SSE / SST  # igual a R2 si no hay pred de CV.

cat(sprintf("RMSE (full dataset): %.4f\n", RMSE))
cat(sprintf("R2   (full dataset): %.4f\n", R2))
cat(sprintf("Q2   (full dataset): %.4f\n", Q2))

#############################################################################
# 6) Create table of results and save to Excel
#############################################################################

resultados_svm <- data.frame(
  Kernel = "Radial",
  BestSigma = svm_model$bestTune$sigma,
  BestC     = svm_model$bestTune$C,
  Accuracy_CV   = max(svm_model$results$Accuracy),
  Accuracy_Full = acc_full,
  RMSE_Full     = RMSE,
  R2_Full       = R2,
  Q2_Full       = Q2
)

print(resultados_svm)

write_xlsx(resultados_svm, "resultss_SVM_CV.xlsx")

#############################################################################
# 7) Confusion matrix (optional, in ALL dataset)
#############################################################################

conf_matrix <- confusionMatrix(y_pred_full, Y)
print(conf_matrix)

df_conf <- as.data.frame(conf_matrix$table)
colnames(df_conf) <- c("Prediction", "Reference", "Freq")

# By percentage:

df_conf <- df_conf %>%
  group_by(Reference) %>%
  mutate(Perc = 100 * Freq / sum(Freq)) %>%
  ungroup()


conf_matrix_plot <- ggplot(df_conf, aes(x = Prediction, y = Reference, fill = Perc)) +
  geom_tile() +
    geom_text(aes(label = sprintf("%.1f%%", Perc)), color = "white", size = 5) +
  scale_fill_gradient(low = "white", high = "#DB8479") +
  theme_minimal() +
  labs(
    x = "Predicted",
    y = "Actual",
    title = "Confusion Matrix (Full Data, % by row)",
    fill = "Percentage"
  )

print(conf_matrix_plot)


ggsave("confusion_matrix_svm_cv.png", plot = conf_matrix_plot, width = 7, height = 7)

#############################################################################
# 8) Most important Wavelenghts - RFE (Recursive Feature Elimination)
#############################################################################

ctrl_rfe <- rfeControl(
  functions = caretFuncs, 
  method = "cv",          
  number = 5
)


svm_rfe <- rfe(
  x = X, 
  y = Y,
  sizes = c(5, 10, 20, 50, 100),  
  rfeControl = ctrl_rfe,
  method = "svmRadial"
)

svm_rfe



######################################################
## Export packages version that have been used
######################################################


# Specify the path to your script
script_file <- "~~/~~/SVM_Cross Validation_script.R"

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

