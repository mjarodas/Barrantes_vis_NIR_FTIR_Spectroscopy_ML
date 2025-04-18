################################
## Load required packages ----
################################

library(readxl)
library(writexl)
library(signal)
#######################################
## Set output directory for figures
#######################################

output_dir <- "figures"
if (!dir.exists(output_dir)) dir.create(output_dir)

#######################################
## Load Raw Data
#######################################

## Load raw data ----

raw_data <- read_excel("~~~/~~/raw_data.xlsx")
data <- read_excel(NIRS_raw)

# Extract the spectra matrix (without the first two columns)
espectros <- as.matrix(data[, 3:ncol(data)])

# 1. Normalise to maximum (per row)

norm_max <- espectros / apply(espectros, 1, max)
data_norm <- data
data_norm[, 3:ncol(data)] <- norm_max
write_xlsx(data_norm, "Spectra_Norm.xlsx")

# 2. First derivative (finite differences)

d1 <- t(apply(espectros, 1, diff))
d1 <- cbind(d1, NA) 

data_d1 <- data
data_d1[, 3:ncol(data)] <- d1
write_xlsx(data_d1, "Spectra_D1.xlsx")

# 3. Second derivative (finite differences of the first derivative)
d2 <- t(apply(d1[, -ncol(d1)], 1, diff))
d2 <- cbind(d2, NA, NA) 

data_d2 <- data
data_d2[, 3:ncol(data)] <- d2
write_xlsx(data_d2, "Spectra_D2.xlsx")

# 4. Savitzky-Golay filter
sg <- t(apply(espectros, 1, sgolayfilt, p = 2, n = 11))
data_sg <- data
data_sg[, 3:ncol(data)] <- sg
write_xlsx(data_sg, "Spectra_SG.xlsx")

# 5. Multiplicative Scatter Correction (MSC)
msc <- function(X) {
  mean_spectrum <- colMeans(X)
  msc_X <- t(apply(X, 1, function(x) lm(x ~ mean_spectrum)$residuals))
  return(msc_X)
}
data_msc <- data
data_msc[, 3:ncol(data)] <- msc(espectros)
write_xlsx(data_msc, "Spectra_MSC.xlsx")

# 6. Standard Normal Variate (SNV)
snv <- function(X) {
  return((X - rowMeans(X)) / apply(X, 1, sd))
}
data_snv <- data
data_snv[, 3:ncol(data)] <- snv(espectros)
write_xlsx(data_snv, "Spectra_SNV.xlsx")


######################################################
## Export packages version that have been used
######################################################

# Specify the path to your script
script_file <- "~~/~~/pretreatments_script.R"

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
