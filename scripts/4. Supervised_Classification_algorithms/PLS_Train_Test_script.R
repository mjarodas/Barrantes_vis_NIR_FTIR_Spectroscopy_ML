################################
## Load required packages ----
################################
library(readxl)
library(mixOmics)
library(caret)
library(ggplot2)
library(dplyr)
library(ROCR)
library(pls)
library(tidymodels)
library(themis)
library(tidymodels)
library(dplyr)

#######################################
## Load Data - replace for each pretreatment
#######################################

data <- read_excel("~~~/~~/data.xlsx") 

#--------------------------------------------------------------------------
#PLS-DA Analysis
#--------------------------------------------------------------------------

# Separate Variables
X <- datos[, -(1:2)]
Y <- as.factor(datos$Group)  
col_names_original <- colnames(X)

# Divide between train (70%) and test (30%)
set.seed(123) 
trainIndex <- createDataPartition(Y, p = 0.7, list = FALSE)
X_train <- X[trainIndex, ]
X_test <- X[-trainIndex, ]
Y_train <- Y[trainIndex]
Y_test <- Y[-trainIndex]

#Convert both to numeric dataframe
X_train <- as.data.frame(lapply(X_train, function(x) as.numeric(as.character(x))))
X_test  <- as.data.frame(lapply(X_test, function(x) as.numeric(as.character(x))))

# reassign the names
colnames(X_train) <- col_names_original
colnames(X_test)  <- col_names_original

# Delete the columns with NA
keep_cols <- colSums(is.na(X_train)) == 0
X_train <- X_train[, keep_cols, drop = FALSE]

X_test <- X_test[, names(keep_cols)[keep_cols], drop = FALSE]

X_train <- as.matrix(X_train)
X_test  <- as.matrix(X_test)


# Adjust the model
ctrl <- trainControl(method = "cv", number = 5)  


plsda_model <- mixOmics::plsda(X_train, Y_train, ncomp = 2) 

perf_plsda <- mixOmics::perf(plsda_model, validation = "Mfold", folds = 5, progressBar = FALSE)
print(perf_plsda$choice.ncomp)


# Optimal number of components
opt_ncomp <- perf_plsda$choice.ncomp["overall", "max.dist"]
print(opt_ncomp)  


# Evaluate Metrics
y_pred_train <- predict(plsda_model, X_train)$class$max.dist
y_pred_test <- predict(plsda_model, X_test)$class$max.dist  #

train_acc <- sum(as.character(y_pred_train) == as.character(Y_train)) / length(Y_train)
test_acc <- sum(as.character(y_pred_test) == as.character(Y_test)) / length(Y_test)

print(train_acc)
print(test_acc)

RMSEE <- sqrt(mean((as.numeric(Y_train) - as.numeric(y_pred_train))^2))
RMSEP <- sqrt(mean((as.numeric(Y_test) - as.numeric(y_pred_test))^2))

R2_final <- 1 - (sum((as.numeric(Y_train) - as.numeric(y_pred_train))^2) /
                   sum((as.numeric(Y_train) - mean(as.numeric(Y_train)))^2))

# Predictions
cv_preds <- predict(plsda_model, X_test)$class$max.dist  
cv_preds <- cv_preds[, opt_ncomp]
cv_actual <- as.numeric(Y_test) 


Q2_final <- 1 - (sum((cv_actual - as.numeric(cv_preds))^2) /
                   sum((cv_actual - mean(cv_actual))^2))



# Summary Table
results <- data.frame(
  LVs = opt_ncomp,
  R2 = R2_final,
  Q2 = Q2_final,
  RMSEE = RMSEE,
  RMSEP = RMSEP,
  Train_ACC = train_acc,
  Test_ACC = test_acc
)
print(results)

write_xlsx(results, "results_PLS_DA_Test_Train.Xlsx")



######################################################
## Export packages version that have been used
######################################################

# Specify the path to your script
script_file <- "~~/~~/PLS_Train_Test_script.R"

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