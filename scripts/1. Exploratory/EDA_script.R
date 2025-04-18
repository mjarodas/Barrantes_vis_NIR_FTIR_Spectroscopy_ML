################################
## Load required packages ----
################################
library(readxl) #Read Excel files
library(ggplot2) #Charts in R
library(dplyr)  #Data manipulation
library(tidyr)  #Data transformation
library(ggcorrplot) #Correlation plots
library(gridExtra)  #Charts multiple graphs in a single layout
library(viridis)  #Viridis color palette
library(writexl) #Write Excel files

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

## Select numeric columns ----
numeric_data <- raw_data %>% select(where(is.numeric))

#######################################
## Handle Missing Values (NA)
#######################################

## Convert numeric data to long format for heatmap visualization ----
heatmap_data <- numeric_data %>%
  mutate(Row = row_number()) %>% # Add a column for row indices
  pivot_longer(cols = -Row, names_to = "Variable", values_to = "Value") %>%
  mutate(Missing = factor(ifelse(is.na(Value), "Missing", "Present")))

## Format X-axis labels to display two decimal places ----
formatted_labels <- function(labels) {
  sprintf("%.2f", as.numeric(labels))
}

######################################################
## Generate Heatmap for Missing and Present Values 
######################################################

# Create heatmap ----
heatmap_plot <- ggplot(heatmap_data, aes(x = Variable, y = Row, fill = Missing)) +
  geom_tile() +
  scale_fill_viridis_d(name = "Status", option = "cividis", direction = -1) +
  labs(
    title = "Heatmap of Present and Missing Values",
    x = "Variable",
    y = "Row"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 8),
    axis.text.y = element_text(size = 8),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 10)
  ) +
  scale_x_discrete(
    breaks = levels(factor(heatmap_data$Variable))[seq(1, ncol(numeric_data), by = 75)],
    labels = formatted_labels
  )

# Display the plot
print(heatmap_plot)

# Save the plot
ggsave(file.path(output_dir, "missing_values_plot.png"), plot = heatmap_plot, width = 10, height = 6, dpi = 300)


######################################################
## Visualize Outliers 
######################################################
## Create a scatter plot of Average Absolute Z-Scores ----
zscore_plot <- ggplot(numeric_data_with_outliers, aes(x = Observation, y = Average_Abs_Z)) +
  geom_point(aes(color = Is_Outlier), size = 2) +
  geom_hline(yintercept = z_threshold, linetype = "dashed", color = "red") +
  scale_color_viridis_d(
    option = "viridis",
    begin = 0.2, end = 0.8, # Adjust the range for better contrast
    name = "Status",
    labels = c("Inlier", "Outlier")
  ) +
  labs(
    title = "Outliers in Observations (Z-Scores)",
    x = "Observation",
    y = "Average Absolute Z-Score"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 10)
  )

# Display the plot
print(zscore_plot)

# Save the plot
ggsave(file.path(output_dir, "zscore_outliers_plot.png"), plot = zscore_plot, width = 10, height = 6, dpi = 300)


######################################################
## Summary Outliers 
######################################################

## Filter outliers ----
outliers_summary <- numeric_data_with_outliers %>%
  filter(Is_Outlier) %>%
  select(Observation, Average_Abs_Z)

## Print summary of outliers ----
print("Summary of Outlier Observations:")
print(outliers_summary)

######################################################
## Remove Outliers and Save Clean Data (if is necessary)
######################################################

## Remove outliers from the dataset ----
clean_data <- raw_data %>%
  filter(!row_number() %in% outliers_summary$Observation)

## Save the clean dataset to an Excel file ----
clean_data_path <- "~~/~~/raw_data_clean.xlsx"
write_xlsx(clean_data, clean_data_path)

######################################################
## Export packages version that have been used
######################################################
# Specify the path to your script
script_file <- "~~/~~/EDA_script.R"

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
