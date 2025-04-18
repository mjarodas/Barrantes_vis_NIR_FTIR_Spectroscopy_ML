################################
## Load required packages ----
################################

library(readxl)
library(caret)
library(dplyr)
library(cluster)
library(purrr)
library(factoextra)
library(rcartocolor)
library(ape)

#######################################
## Set output directory for figures
#######################################

output_dir <- "figures"
if (!dir.exists(output_dir)) dir.create(output_dir)

#######################################
## Load Raw Data
#######################################

## Load dataset and remove unnecessary columns ----
HCA_data <- read_excel("~~/~~/raw_data.xlsx")
HCA_data[, -(1:2)] <- apply(HCA_data[, -(1:2)], 2, as.numeric)

# Average replica (finish in _R1 y _R2)
HCA_data$ID <- gsub("_R1|_R2", "", HCA_data$ID)
HCA_data <- HCA_data %>%
  group_by(ID) %>%
  summarise(across(everything(), mean))

# Save the samples name and remove from dataframe
ID_labels <- HCA_data$ID

HCA_data <- HCA_data[, -1]

## Preprocessing ----
preprocessor <- preProcess(
  HCA_data,
  method = c("center", "scale")
)
data_transformed <- predict(preprocessor, HCA_data)

#######################################
## Determine Optimal Linkage Method
#######################################
## Define clustering methods ----
linkage_methods <- c("average", "single", "complete", "ward")
names(linkage_methods) <- linkage_methods

## Compute agglomerative coefficients for different methods ----
agglomerative_coefficients <- function(method) {
  agnes(data_transformed, method = method)$ac
}
linkage_scores <- map_dbl(linkage_methods, agglomerative_coefficients)

## Print agglomerative coefficients ----
print(linkage_scores)

## Select the best linkage method ----
best_method <- names(which.max(linkage_scores))
cat("Best linkage method:", best_method, "\n")

#######################################
## Hierarchical Clustering
#######################################

## Compute dissimilarity matrix (Euclidean distance) ----
distance_matrix <- dist(data_transformed, method = "euclidean")

## Perform hierarchical clustering using the best linkage method ----
hc_best <- hclust(distance_matrix, method = best_method)

# Add labels of samples
hc_best$labels <- ID_labels 

#######################################
## Determine Optimal Number of Clusters
#######################################
  
## Use Silhouette Method to find optimal number of clusters ----
silhouette_plot <- fviz_nbclust(
  data_transformed,
  FUN = hcut, # Hierarchical clustering
  method = "silhouette",
  diss = dist(data_transformed, method = "euclidean")
) +
  labs(
    title = "Silhouette Method for Optimal Clusters",
    x = "Number of Clusters",
    y = "Average Silhouette Width"
  ) +
  theme_minimal()

## Save silhouette plot ----
ggsave(
  filename = file.path(output_dir, "silhouette_plot.png"),
  plot = silhouette_plot,
  width = 10,
  height = 6,
  dpi = 300
)

## Display silhouette plot ----
silhouette_plot

#######################################
## Visualization: Dendrogram
#######################################

# Real labels based on HCA
ordered_labels <- hc_best$labels[hc_best$order]
print(ordered_labels)

#Extract adulterants from labels
adulterants <- substr(ordered_labels, 1, 2)
print(adulterants)

# Describing colors
adulterants_color <- c("CP" = "#ECA47E", "El" = "#C7536C", "PD" = "#A12F71", "BR" = "#F5DCA5","GD"="#B83C6B","DS"="#E4847B","Rm"="#7A1B6D")
label_colors <- adulterants_color[adulterants]

## Generate and customize dendrogram ----
dendrogram <- fviz_dend(
  x = hc_best,
  k = 2, # Set the number of clusters (from silhouette method)
  show_labels = TRUE,
  cex = 0.7,
  lwd = 0.5,
  main = "",
  xlab = "ID",
  ylab = "Height",
  sub = "",
  ggtheme = theme_classic(),
  k_colors = c("#DB8578", "#5C4845"),  # color of cluster
  horiz = FALSE,
  type = "rectangle",
  rect = TRUE,
  rect_fill = TRUE,
  label_cols = label_colors,
  color_labels_by_k=FALSE
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.title.x = element_text(size = 12, face = "italic"),
    axis.title.y = element_text(size = 12, face = "italic"),
    axis.text = element_text(size = 10)
  )

## Display the dendrogram ----
dendrogram

## Save dendrogram as PNG ----
ggsave(
  filename = file.path(output_dir, "hca_plot.png"),
  plot = dendrogram,
  width = 10,
  height = 6,
  dpi = 300
)

#######################################
## Phylogenetic-FAN tree dendrogram
#######################################
arbol_phylo <- as.phylo(hc_best)
adulterants_phylo <- substr(arbol_phylo$tip.label, 1, 2)
adulterants_color <- c(adulterants_color <- c("CP" = "#ECA47E", "El" = "#C7536C", "PD" = "#A12F71", "BR" = "#F5DCA5","GD"="#B83C6B","DS"="#E4847B","Rm"="#7A1B6D"))
tip_colors <- adulterants_color[adulterants_phylo]

#Display
plot(
  arbol_phylo,
  type = "fan",
  tip.color = tip_colors,
  cex = 0.8,
  label.offset = 0.1,
  no.margin = TRUE)

#Export
png("figures/phylogenetic_dendrogramn.png", width = 8, height = 8, units = "in", res = 300)
par(mar = c(1, 1, 3, 1))
plot(arbol_phylo,
     type = "fan",
     tip.color = tip_colors,
     cex = 0.8,
     label.offset = 0.1,
     no.margin = TRUE,
     main = "Phylo Dendrogram")
dev.off()


######################################################
## Export packages version that have been used
######################################################

# Specify the path to your script

# Specify the path to your script
script_file <- ""~~/~~/HCA_script.R"

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
req_file <- ""~~/~~/requirements.txt"
writeLines(req_lines, con = req_file)
