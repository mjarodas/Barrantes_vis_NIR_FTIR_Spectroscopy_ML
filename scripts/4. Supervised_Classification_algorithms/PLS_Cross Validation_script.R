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
#Start PLS-DA Analysis
#--------------------------------------------------------------------------
# Separate variables
X <- datos[, -(1:2)] 
Y <- as.factor(datos$Group)  
col_names_original <- colnames(X)

# Convert both to data frames of numbers
X_ <- as.data.frame(lapply(X, function(x) as.numeric(as.character(x))))

# Drop columns that have NA in X_train
keep_cols <- colSums(is.na(X)) == 0
X <- X[, keep_cols, drop = FALSE]

# Reassign original names (optional)
colnames(X) <- col_names_original[colnames(X) %in% names(keep_cols)[keep_cols]]

X <- as.matrix(X)

#############################################################################
# 3) Fit PLS-DA and do only cross-validation (M-fold)
#############################################################################

# 3.1)Select a "maximum" number of components we want to test (1 o 5)
max_ncomp <- 5

# 3.2) Create a PLS-DA model with ncomp = max_ncomp
plsda_model <- plsda(
  X, 
  Y, 
  ncomp = max_ncomp  
)

# 3.3) M-fold cross-validation (5 folds), without external "test"
perf_plsda <- perf(
  plsda_model,
  validation  = "Mfold", 
  folds       = 5,
  dist        = "max.dist", 
  progressBar = FALSE
)

# 3.4) view the optimal number of components
opt_ncomp <- perf_plsda$choice.ncomp["overall","max.dist"]
cat("Optimal number of components:", opt_ncomp, "\n")

#############################################################################
# 4) Train final model with opt_ncomp on ALL samples
#############################################################################
plsda_final <- plsda(
  X,
  Y,
  ncomp = opt_ncomp
)

#############################################################################
# 5) Predictions and metrics on the ENTIRE dataset (no separate tests)
#############################################################################

y_pred <- predict(plsda_final, X)$class$max.dist
acc_final <- mean(y_pred == Y)
cat(sprintf("Accuracy: %.2f%%\n", 100*acc_final))

y_real_num <- as.numeric(Y)
y_pred_num <- as.numeric(y_pred)

RMSE  <- sqrt(mean((y_real_num - y_pred_num)^2))

SSE   <- sum((y_real_num - y_pred_num)^2)
SST   <- sum((y_real_num - mean(y_real_num))^2)
R2    <- 1 - (SSE / SST)

Q2 <- 1 - (SSE / SST)  

#Print the results
cat(sprintf("RMSE: %.4f\n", RMSE))
cat(sprintf("R2   : %.4f\n", R2))
cat(sprintf("Q2   : %.4f\n", Q2))

#############################################################################
# 6) Save the results
#############################################################################

results <- data.frame(
  NComp         = opt_ncomp,
  Accuracy_CV   = 1 - perf_plsda$error.rate$max.dist[opt_ncomp],
  Accuracy_Full = acc_final,
  RMSE          = RMSE,
  R2            = R2,
  Q2            = Q2
)

print(results)


write_xlsx(resultados, "results_PLS_DA_CV.xlsx")


######################################################
## Export packages version that have been used
######################################################

# Specify the path to your script
script_file <- "~~/~~/PLS_Cross Validation_script.R"

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

