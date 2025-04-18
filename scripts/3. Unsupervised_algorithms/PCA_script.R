################################
## Load required packages ----
################################
library(readxl)
library(caret)
library(dplyr)
library(factoextra)
library(ggplot2)
library(ggrepel)
library(data.table)
library(reshape2)
library(scales)
library(plotly)

#######################################
## Set output directory for figures
#######################################
output_dir <- "figures"
if (!dir.exists(output_dir)) dir.create(output_dir)

#######################################
## Load Raw Data
#######################################

## Load dataset and remove unnecessary columns ----
PCA_data <- read_excel("~~/~~/raw_data.xlsx")
PCA_data[, -(1:2)] <- apply(PCA_data[, -(1:2)], 2, as.numeric)

# Average replica (finish in _R1 y _R2)
PCA_data$ID <- gsub("_R1|_R2", "", PCA_data$ID)
PCA_data <- PCA_data %>%
  group_by(ID) %>%
  summarise(across(everything(), mean))

# Save ID labels and remove it
ID_labels <- PCA_data$ID
PCA_data <- PCA_data[, -1]

## Preprocess
preprocessor <- preProcess(PCA_data, method = c("center", "scale"))
data_transformed <- predict(preprocessor, PCA_data)
data_transformed <- cbind(ID_labels, data_transformed)
rownames(data_transformed) <- data_transformed$ID_labels

#######################################
## Principal Component Analysis (PCA)
#######################################
pca_data <- data_transformed[, -1]
pca_result <- prcomp(pca_data, scale. = FALSE)

print(pca_result)
summary(pca_result)

#######################################
## Scree Plot (Eigenvalues)
#######################################

scree_plot <- fviz_eig(
  pca_result,
  xlab = "Principal Components (PCs)",
  ylab = "Explained Variance (%)",
  main = "",
  addlabels = TRUE,
  ggtheme = theme_minimal(),
  barcolor = "#404788FF",
  barfill = "#404788FF",
  linecolor = "#000000"
)

print(scree_plot)

ggsave(
  filename = file.path(output_dir, "PCA_scree_plot.png"),
  plot = scree_plot,
  width = 10,
  height = 6,
  dpi = 300
)

#######################################
## Score Plot (PC1 vs PC2)
#######################################

# Transform results to data frame
pca_scores <- as.data.frame(pca_result$x)
pca_scores$Sample <- rownames(pca_scores)
pca_scores <- pca_scores %>% mutate(Group = ID_labels)

# Extract adulterants and percentage
pca_scores <- pca_scores %>%
  mutate(
    Adulterant = gsub("_\\d+%$", "", Group),
    Percentage = as.numeric(gsub("^.*_(\\d+)%$", "\\1", Group))
  )

# Define colours
adulterant_colors <- c("CP" = "#ECA47E", "El" = "#C7536C", "PD" = "#A12F71", "BR" = "#F5DCA5","GD"="#B83C6B","DS"="#E4847B","Rm"="#7A1B6D")

# Generate colours according to the percentage

pca_scores$FillColor <- mapply(function(adulterant, percentage) {
  base_color <- adulterant_colors[adulterant]
  col_numeric(palette = c("white", base_color), domain = c(0, 100))(percentage)
}, pca_scores$Adulterant, pca_scores$Percentage)

pca_scores$FillColor[is.na(pca_scores$FillColor)] <- "grey"

# Create the PCA graph
score_plot <- ggplot(pca_scores, aes(x = PC1, y = PC2, fill = FillColor)) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.3) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.3) +
  geom_point(aes(color = Adulterant, fill = FillColor), size = 4, shape = 21, stroke = 1) +
  geom_text_repel(aes(label = Percentage), size = 3, max.overlaps = 15) +
  scale_color_manual(values = adulterant_colors) +
  scale_fill_identity() +
  labs(
    x = paste0("PC1 (", round(summary(pca_result)$importance[2,1] * 100, 1), "%)"),
    y = paste0("PC2 (", round(summary(pca_result)$importance[2,2] * 100, 1), "%)"),
    title = "PCA Score Plot - Adulteration Detection",
    color = "Adulterant",
    fill = "Adulteration %"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

print(score_plot)

# Save the plot
ggsave(
  filename = "score_plot_2d.png",
  plot = score_plot,
  width = 10, height = 6, dpi = 300
)

#######################################
## Loadings Plot
#######################################

loadings <- as.data.frame(pca_result$rotation[, 1:2])

setDT(loadings, keep.rownames = TRUE)[]
ld <- melt(loadings, "rn")
ld$rn <- as.numeric(ld$rn)

loadings_plot <- ggplot(ld, aes(x = rn, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_test() +
  theme(
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    axis.text = element_text(size = 8, hjust = 1, angle = 90),
    axis.title = element_text(size = 8),
    plot.margin = margin(5, 5, 5, 5)
  ) +
  labs(x = "ID", y = "Loadings PCs", title = "") +
  scale_x_continuous(
    breaks = scales::pretty_breaks(n = 50),
    labels = scales::number_format(accuracy = 0.01),
    expand = c(0, 0)
  ) +
  scale_fill_viridis_d(option = "D")

print(loadings_plot)

ggsave(
  filename = file.path(output_dir, "PCA_loadings_plot_2D.png"),
  plot = loadings_plot,
  width = 10,
  height = 6,
  dpi = 300
)


######################################################
## Export packages version that have been used
######################################################
# Specify the path to your script
script_file <- "~~/~~/PCA_script.R"

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
req_file <- "~~/~~/requirements.txt"
writeLines(req_lines, con = req_file)

