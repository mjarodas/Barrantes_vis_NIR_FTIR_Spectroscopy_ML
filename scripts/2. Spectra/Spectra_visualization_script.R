################################
## Load required packages ----
################################
library(readxl)
library(dplyr)
library(data.table)
library(ggplot2)
library(egg)
library(rcartocolor)
library(reshape2)
library(prospectr)  # Scatter Corrections (SNV, MSC)
library(openxlsx)  # To save Excel file

#######################################
## Set output directory for figures
#######################################
output_dir <- "figures"
if (!dir.exists(output_dir)) dir.create(output_dir)

#######################################
## Load Raw Data
#######################################

raw_data <- read_excel("~~/~~/raw_data.xlsx")

raw_data[, -(1:2)] <- apply(raw_data[, -(1:2)], 2, as.numeric)

# Average of spectra by Group
mean_data <- raw_data %>%
  group_by(Group) %>%
  summarise(across(-c(ID), mean, na.rm = TRUE))  # ignore NA values

#######################################
## Spectra Plot Generation
#######################################

df <- melt(mean_data, "Group")

df$variable <- as.numeric(as.character(df$variable))

x_min <- min(df$variable)
x_max <- max(df$variable)

spectra_plot <- ggplot(df, aes(x = variable, y = value, color = Group, group = Group)) +
  geom_line() +
  labs(
    x = "Wavelength (nm)",
    y = "Absorbance (UA)"
  ) +
  theme_test() +
  theme(
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    axis.text = element_text(size = 8, hjust = 1, angle = 90),
    axis.title = element_text(size = 8)
  ) +
  scale_x_continuous(
    breaks = seq(x_min, x_max, by = 1000),  # Mostrar etiquetas cada 1000 unidades
    labels = scales::number_format(accuracy = 1),
    expand = c(0, 0)
  ) +
  scale_color_carto_d(palette = "SunsetDark") +
  scale_fill_carto_d(palette = "SunsetDark")


#######################################
## Save and Display Plot ----
#######################################

ggsave(
  filename = file.path(output_dir, "Spectra_plot.png"),
  plot = spectra_plot,
  width = 10,
  height = 6,
  dpi = 300
)

spectra_plot

######################################################
## Export packages version that have been used
######################################################

# Specify the path to your script
script_file <- ""~~/~~/Spectra_visualization_script.R"

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
