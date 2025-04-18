################################
## Load required packages ----
################################

library(readxl)
library(dplyr)
library(stringr)
library(caret)
library(prospectr)
library(ggplot2)
library(randomForest)
library(writexl)
library(tidyr)

if (!dir.exists("figures")) dir.create("figures")
if (!dir.exists("metrics")) dir.create("metrics")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Auxiliar Fuctions
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Save confusion matrix as image
guardar_matriz_confusion <- function(real, pred, nombre) {
  conf_matrix <- table(Predicho = pred, Real = real)
  conf_pct <- prop.table(conf_matrix, margin = 2) * 100
  conf_df <- as.data.frame(conf_pct)
  colnames(conf_df) <- c("Predicted", "Real", "Percentage")
  
  p <- ggplot(conf_df, aes(x = Real, y = Predicted, fill = Percentage)) +
    geom_tile(color = "white") +
    geom_text(aes(label = sprintf("%.1f%%", Porcentaje)), color = "black", size = 5) +
    scale_fill_gradient(low = "white", high = "darkgreen") +
    labs(title = paste("Confusion Matrix -", nombre), x = "Real class", y = "Predicted") +
    theme_minimal()
  
  ggsave(paste0("figures/confusion_rf_", gsub(" ", "_", tolower(nombre)), ".png"), 
         p, width = 6, height = 5, dpi = 300)
}

# Save metrics and kappa
guardar_metricas <- function(real, pred, nombre, train_real = NULL, train_pred = NULL) {
  conf <- confusionMatrix(pred, real)
  metrics_df <- as.data.frame(conf$byClass)
  metrics_df$Clase <- rownames(metrics_df)
  metrics_df <- metrics_df[, c("Class", setdiff(names(metrics_df), "Class"))]
  write_xlsx(metrics_df, paste0("metrics/metrics_rf_", gsub(" ", "_", tolower(nombre)), ".xlsx"))
  
  kappa_val <- data.frame(Set = "Test", Kappa = conf$overall["Kappa"])
  
  
  if (!is.null(train_real) && !is.null(train_pred)) {
    kappa_train <- confusionMatrix(train_pred, train_real)$overall["Kappa"]
    kappa_val <- rbind(kappa_val, data.frame(Set = "Train", Kappa = kappa_train))
  }
  
  write_xlsx(kappa_val, paste0("metrics/kappa_rf_", gsub(" ", "_", tolower(nombre)), ".xlsx"))
}


# Training and Evaluation RF
pipeline_rf_test <- function(X_train, Y_train, X_test, Y_test, nombre_modelo) {
    df_train <- as.data.frame(X_train)
  df_train$Clase <- Y_train
  df_test <- as.data.frame(X_test)
  colnames(df_train) <- make.names(colnames(df_train))
  colnames(df_test) <- make.names(colnames(df_test))
  
  modelo_rf <- randomForest(Clase ~ ., data = df_train, importance = TRUE, ntree = 100, mtry = 5)
  pred_rf <- predict(modelo_rf, newdata = df_test)
  pred_train <- predict(modelo_rf, newdata = df_train)
  
  acc_test <- mean(pred_rf == Y_test)
  acc_train <- mean(pred_train == Y_train)
  
  guardar_matriz_confusion(Y_test, pred_rf, nombre_modelo)
  guardar_metricas(Y_test, pred_rf, nombre_modelo, Y_train, pred_train)
  
  cat(sprintf("✅ Accuracy TRAIN (%s): %.2f%%\n", nombre_modelo, acc_train * 100))
  cat(sprintf("✅ Accuracy TEST  (%s): %.2f%%\n", nombre_modelo, acc_test * 100))
  
# Wavelenghts importance
  importance_df <- as.data.frame(importance(modelo_rf))
  importance_df$Wavelength <- as.numeric(gsub("X", "", rownames(importance_df)))
  importance_df <- importance_df[order(-importance_df$MeanDecreaseGini), ]
  write_xlsx(importance_df, paste0("metrics/importance_rf_", gsub(" ", "_", tolower(nombre_modelo)), ".xlsx"))
  
  top_n <- 30
  g <- ggplot(importance_df[1:top_n, ], aes(x = reorder(Wavelength, MeanDecreaseGini), y = MeanDecreaseGini)) +
    geom_col(fill = "steelblue") +
    coord_flip() +
    labs(title = paste("Wavelenthgs most importants -", nombre_modelo),
         x = "Wavelenghts (nm)", y = "Importance") +
    theme_minimal()
  
  ggsave(paste0("figures/importance_rf_", gsub(" ", "_", tolower(nombre_modelo)), ".png"), g, width = 7, height = 5, dpi = 300)
  
  return(data.frame(Modelo = nombre_modelo, Accuracy_Train = acc_train * 100, Accuracy_Test = acc_test * 100))
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load data
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Load data raw data - Replace for each pretreatment
data <- read_excel("~~~/~~/data.xlsx")


numeric_cols <- sapply(data, is.numeric)
X <- as.matrix(data[, numeric_cols])
Y <- as.factor(rdata$Group)

set.seed(123)
split_index <- createDataPartition(Y, p = 0.7, list = FALSE)
Y_train <- Y[split_index]
Y_test <- Y[-split_index]

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# APLICAR PRETRATAMIENTOS Y ENTRENAR RF
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


X_train <- X[split_index, ]; X_test <- X[-split_index, ]
resultados <- pipeline_rf_test(X_train, Y_train, X_test, Y_test)
print (resultados)

######################################################
## Export packages version that have been used
######################################################

# Specify the path to your script
script_file <- "~~/~~/RF_Train_Test_script.R"

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


