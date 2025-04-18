################################
## Load required packages ----
################################
library(tidyverse)     
library(readxl)        
library(caret)         
library(pls)           
library(e1071)         
library(randomForest)  
library(writexl)       

#######################################
## Load Data - replace for each adulterant
#######################################

data <- read_excel("~~~/~~/data.xlsx") 


###############################################################################
#EXTRACT THE PERCENTAGE OF ADULTERATION (Y) AND REPLICA
###############################################################################

df <- as.data.frame(datos_raw)

ID <- df[, 1]           
X  <- df[, -1, drop=FALSE]  


extract_percentage <- function(id_string) {as.numeric(sub("^(\\d+)%_.*$", "\\1", id_string))
}

extract_replica <- function(id_string) {sub("^.*_(R[0-9]+)$", "\\1", id_string)
}


#Create the new variables
Y <- sapply(ID, extract_percentage)      
Replica <- sapply(ID, extract_replica)   

###############################################################################
#Prepare X and Y
###############################################################################
X <- as.data.frame(lapply(X, function(col) as.numeric(as.character(col))))
sum(is.na(X))

data_for_model <- cbind(X, Y = Y)


###############################################################################
#DEFINE CROSS VALIDATION
###############################################################################
ctrl <- trainControl(
  method = "repeatedcv", 
  number = 5,            
  repeats = 2,           
  savePredictions = "all"  
)

# Define the three models: PLS, SVMRadial and RF
model_methods <- c("pls", "svmRadial", "rf")

###############################################################################
# TRAIN THE MODEL AND SAVE THE RESULTS
###############################################################################

results_list <- list()

for (met in model_methods) {
  cat("\nTraining model:", met, "\n")
  
  set.seed(123)
  model <- train(
    Y ~ ., 
    data = data_for_model,
    method = met,
    trControl = ctrl,
    preProcess = c("center", "scale"),  
    tuneLength = 5
  )
  
  results_list[[met]] <- model
}

lapply(results_list, print)

###############################################################################
#CALCULATE METRICS RMSE, R², RPD, Q²
###############################################################################
sd_y <- sd(data_for_model$Y, na.rm = TRUE)

get_metrics <- function(model_obj) {best <- model_obj$bestTune
  df_res <- model_obj$results
  
  for (nm in names(best)) {
    df_res <- df_res[df_res[[nm]] == best[[nm]], ]
  }
 
  rmse_cv <- df_res$RMSE
  r2_cv   <- df_res$Rsquared
  

  rpd_cv <- sd_y / rmse_cv
  
  pred_df <- model_obj$pred
  for (nm in names(best)) {
    pred_df <- pred_df[pred_df[[nm]] == best[[nm]], ]
  }
  
  
  sse_cv <- sum((pred_df$obs - pred_df$pred)^2)
 
  sst    <- sum((pred_df$obs - mean(pred_df$obs))^2)
  
  q2_cv  <- 1 - sse_cv / sst
  
  data.frame(
    Method = model_obj$method,
    RMSE_CV = rmse_cv,
    R2_CV   = r2_cv,
    RPD_CV  = rpd_cv,
    Q2_CV   = q2_cv
  )
}

#Final Table
results_table <- do.call(rbind, lapply(results_list, get_metrics))
results_table

write_xlsx(results_table, "~/~/Regression.xlsx")


######################################################
## Export packages version that have been used
######################################################

# Specify the path to your script
script_file <- "~~/~~/Regression_script.R"

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