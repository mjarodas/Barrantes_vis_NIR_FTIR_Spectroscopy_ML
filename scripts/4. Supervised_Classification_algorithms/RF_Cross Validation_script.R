################################
## Load required packages ----
################################
library(caret)        
library(randomForest) 
library(ggplot2)      
library(writexl)    

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



pipeline_rf_cv <- function(X, Y, nombre_modelo) {
 
  X_df <- as.data.frame(X)
  colnames(X_df) <- make.names(colnames(X_df))
  
  set.seed(123)  
  folds <- createFolds(Y, k = 5, returnTrain = FALSE)
  pred_cv <- factor(rep(NA, length(Y)), levels = levels(Y))
  
  for (i in seq_along(folds)) {
    test_idx  <- folds[[i]]
    train_idx <- setdiff(seq_along(Y), test_idx)
    
    # Create training data.frame
    df_train <- X_df[train_idx, ]
    df_train$Clase <- Y[train_idx]
    
    # Create test data.frame
    df_test <- X_df[test_idx, ]
    
    # Train Random Forest
    modelo_rf <- randomForest(Clase ~ ., data = df_train,
                              importance = TRUE, ntree = 100, mtry = 5)
    
    # Predict
    pred_fold <- predict(modelo_rf, newdata = df_test)
    
    # Save predictions
    pred_cv[test_idx] <- pred_fold
  }
  
  # 2) Global metrics
  conf <- confusionMatrix(pred_cv, Y)
  acc_cv <- mean(pred_cv == Y)
  
  # Save confusion matrix
  guardar_matriz_confusion(Y, pred_cv, nombre_modelo)
    guardar_metricas(Y, pred_cv, nombre_modelo)
  
  cat(sprintf("✅ Accuracy 5-fold CV (%s): %.2f%%\n", nombre_modelo, acc_cv * 100))
  
  # 3) Train the final model
  df_full <- X_df
  df_full$Clase <- Y
  final_model <- randomForest(Clase ~ ., data = df_full,
                              importance = TRUE, ntree = 100, mtry = 5)
  
  # 4) Variables importance
  importance_df <- as.data.frame(importance(final_model))
  importance_df$Variable <- rownames(importance_df)
  rownames(importance_df) <- NULL
  importance_df <- importance_df[order(-importance_df$MeanDecreaseGini), ]
  write_xlsx(importance_df, paste0("metrics/importance_rf_", gsub(" ", "_", tolower(nombre_modelo)), ".xlsx"))
  
  # 30 variables most important
  top_n <- min(30, nrow(importance_df))  
  g <- ggplot(importance_df[1:top_n, ], aes(x = reorder(Variable, MeanDecreaseGini), 
                                            y = MeanDecreaseGini)) +
    geom_col() +
    coord_flip() +
    labs(title = paste("Most important variables -", nombre_modelo),
         x = "Variable", y = "Importance") +
    theme_minimal()
  
  ggsave(paste0("figures/importance_rf_", gsub(" ", "_", tolower(nombre_modelo)), ".png"), 
         g, width = 7, height = 5, dpi = 300)
  
  return(data.frame(Modelo = nombre_modelo, Accuracy_5foldCV = acc_cv * 100))
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load data
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Load data raw data - Replace for each pretreatment
data <- read_excel("~~~/~~/data.xlsx")

Y <- as.factor(data$Group)
X <- data[, sapply(data, is.numeric)]

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# EXECUTE RF
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

resultados <- pipeline_rf_cv(X, Y)
print(resultados)

######################################################
## Export packages version that have been used
######################################################

# Specify the path to your script
script_file <- "~~/~~/RF_Cross Validation_script.R"

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


