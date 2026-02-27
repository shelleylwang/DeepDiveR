#!/usr/bin/env Rscript
# Load necessary libraries
#install.packages("pammtools")
library(ggplot2)
library(deeptime)
library(tidyr)
library(dplyr)
library(pammtools)
library(cowplot)

setwd("simulations_20250312_lstm64_32_d64_32") # GABRIELA, CHANGE THIS TO YOUR WORKING DIRECTORY

# Check if there is a folder called "feature_plots_formatted" in the working directory
# If there isn't, make one
if (!dir.exists("feature_plots_formatted")) {
  dir.create("feature_plots_formatted")
}

# Read the CSV file into a data frame
data <- read.csv("Empirical_features_.csv")

# Duplicate the first row of data, so that the first two rows are identical
# If you don't do this + add that first value in the year vector below, the very first value (first row) will not be plotted
data<- rbind(data[1, ], data)


# DEFINE YEAR VECTOR, 
# The first value in the year vector corresponds to the minimum MinAge value in the dataset
# Reptilia year vector: 
year <- c(199.5, 208, 217, 227, 237, 242, 247, 252, 259.5, 264.3, 273, 283.5, 290.1, 298.9) #GABRIELA, DEFINE YOUR OWN YEAR VECTOR

# Make the year vector negative
year <- -year

# Format axis labels
format_labels <- function(x) {
  return(sprintf("%.0f", abs(x)))
}

# ============================================================================
# REUSABLE HELPER FUNCTIONS
# ============================================================================

# Apply standard theme to all plots
apply_standard_theme <- function(plot, show_legend = FALSE) {
  plot <- plot + theme_classic() +
    theme(
      plot.margin = unit(c(2, 1, 1, 1), "cm"),
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
      axis.title.x = element_text(size = 14, face = "bold", margin = margin(t = 10)),
      axis.title.y = element_text(size = 14, face = "bold", margin = margin(r = 10)),
      axis.text = element_text(size = 12, face = "bold")
    )
  
  if (show_legend) {
    plot <- plot + theme(
      legend.position = "right",
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 10)
    )
  }
  
  return(plot)
}

# Apply standard coordinate system and scales
apply_standard_coords <- function(plot, xlim = c(-320, -190), # GABRIELA, CHANGE XLIM IF NEEDED
                                  x_breaks_by = 10) {
  plot <- plot +
    scale_x_reverse() +
    coord_geo(
      xlim = xlim,
      expand = FALSE,
      clip = "on",
      dat = list("international ages", "international periods"),
      abbrv = list(TRUE, FALSE),
      pos = list("bottom", "bottom"),
      alpha = 1,
      height = unit(1.5, "line"),
      rot = 0,
      size = list(6, 5),
      neg = TRUE
    ) +
    scale_x_continuous(
      limits = xlim,
      breaks = seq(xlim[1], xlim[2], by = x_breaks_by),
      labels = format_labels
    )
  
  return(plot)
}

# Create multi-line step plot with regions
create_multiline_plot <- function(year, data_columns, line_labels, 
                                   colors, x_label, y_label, legend_title) {
  # Create long format data
  plot_data <- data.frame(
    year = rep(year, length(data_columns)),
    columns_list = unlist(data_columns),
    columns_labels = factor(rep(line_labels, each = length(year)))
  )
  
  # Create base plot
  plot <- ggplot(plot_data, aes(x = year, y = columns_list, color = columns_labels)) +
    geom_step(size = 1) +
    scale_color_manual(values = colors) +
    labs(x = x_label, y = y_label, color = legend_title)
  
  # Apply standard formatting
  plot <- apply_standard_coords(plot)
  plot <- apply_standard_theme(plot, show_legend = TRUE)
  
  return(plot)
}

# Create single-line step plot
create_single_plot <- function(year, y, y_label, line_color = "black") {
  plot_data <- data.frame(year = year, y = y)
  
  plot <- ggplot(plot_data, aes(x = year, y = y)) +
    geom_step(size = 1, color = line_color) +
    labs(x = "Time (Ma)", y = y_label)
  
  plot <- apply_standard_coords(plot)
  plot <- apply_standard_theme(plot, show_legend = FALSE)
  
  return(plot)
}



############################ SINGLE GRAPHS ################################

# Speciation rate
speciation_rate = data$origination_events / ((data$n_species)*data$time_bin_duration)
plot_spec_rate <- create_single_plot(year, speciation_rate, "Speciation Rate", line_color = "blue")

# Extinction rate
extinction_rate = data$extinction_events / ((data$n_species)*data$time_bin_duration)
plot_ext_rate <- create_single_plot(year, extinction_rate, "Extinction Rate", line_color = "red")
# Net diversification rate
net_div_rate = speciation_rate - extinction_rate
plot_net_div_rate <- create_single_plot(year, net_div_rate, "Net Diversification Rate")



######################### GROUPED GRAPH #############################

combined_plot <- plot_grid(plot_spec_rate, plot_ext_rate, plot_net_div_rate, ncol = 2, nrow = 2, labels = c("A", "B", "C"), label_size = 20)

# Save the combined plot as a PDF
ggsave("rates.pdf", combined_plot, width = 25, height = 15) # GABRIELA, CHANGE TO OUTPUT PATH

