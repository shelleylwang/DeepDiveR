#!/usr/bin/env Rscript
# Load necessary libraries
#install.packages("pammtools")
library(ggplot2)
library(deeptime)
library(tidyr)
library(dplyr)
library(pammtools)
library(cowplot)

setwd("C:\\Users\\SimoesLabAdmin\\Documents\\DeepDiveR\\updated_occurrences\\reptilia_terr\\rep_terr_models\\simulations_20260218_lstm64_32_d64_32_autotuned")

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


# COMMENT OUT YEAR VECTOR DEPENDING ON GENUS
# The first value in the year vector corresponds to the minimum MinAge value in the dataset
# It needs to be added so that the first data point in the features_conditional csv is plotted
# Temnospondyli year vector
# year <- c(201.4, 208, 217, 227, 237, 242, 247, 252, 259.5, 264.3, 273, 283.5, 290.1, 309.8)

# Synapsida and Reptilia
year <- c(199, 208, 217, 227, 237, 242, 246.7, 252, 259.5, 264.3, 274.4, 283.5, 290.1, 298.9)

# Make the year vector negative
year <- -year

# Format axis labels
format_labels <- function(x) {
  return(sprintf("%.0f", abs(x)))
}

########################## DEFINE FUNCTIONS ##########################

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
apply_standard_coords <- function(plot, xlim = c(-300, -200), # CHANGE THIS IF YOU WANT TO CHANGE THE X AXIS LIMITS
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
      # size = list(6, 5),
      size = list(3, 5),
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
# add option for the color of the line
create_single_plot <- function(year, y, y_label, line_color = "black") {
  plot_data <- data.frame(year = year, y = y)
  
  plot <- ggplot(plot_data, aes(x = year, y = y)) +
    geom_step(size = 1, color = line_color) +
    labs(x = "Time (Ma)", y = y_label)
  
  plot <- apply_standard_coords(plot)
  plot <- apply_standard_theme(plot, show_legend = FALSE)
  
  return(plot)
}


#################### PLOT ORIGINATION + EXTINCTION EVENTS#######################
plot_spec_ext_events <- create_multiline_plot(
  year = year,
  data_columns = list(data$extinction_events, data$origination_events),
  line_labels = c("Extinction", "Speciation"),
  colors = c("Extinction" = "red", "Speciation" = "blue"),
  x_label = "Time (Ma)",
  y_label = "Extinction and Speciation Events",
  legend_title = " "
)


############################ SINGLE PLOTS ################################
plot_n_endemics <- create_single_plot(year, data$n_endemics, "Number of Endemics")
plot_n_singletons <- create_single_plot(year, data$n_singletons, "Number of Singletons")
plot_range_through_div <- create_single_plot(year, data$range_through_div, "Range-Through Diversity")


############################ SINGLE PLOTS, SELF-CALCULATED ################################

# Net diversification events
net_diversity = data$origination_events - data$extinction_events
plot_net_div_events <- create_single_plot(year, net_diversity, "Net Diversification Events")

# Speciation rate 
speciation_rate = data$origination_events / (data$n_species)*data$time_bin_duration
plot_spec_rate <- create_single_plot(year, speciation_rate, "Speciation Rate", line_color = "blue")

# Extinction rate
extinction_rate = data$extinction_events / (data$n_species)*data$time_bin_duration
plot_ext_rate <- create_single_plot(year, extinction_rate, "Extinction Rate", line_color = "red")
# Net diversification rate
net_div_rate = speciation_rate - extinction_rate
plot_net_div_rate <- create_single_plot(year, net_div_rate, "Net Diversification Rate")


######################### PLOT EMPIRICAL PREDICTIONS #############################

# Read data
emp_preds <- read.csv("Empirical_predictions_.csv")

# The first two columns of Empirical_predictions_.csv are exactly the same/repeats
# Which is what we need for graphing, so that that duplicated value gets plotted
# If you didn't keep that duplicated column and didn't add that first value
# in the year vector at the top of this script, the very first value (youngest) will not be plotted

# Transpose data so time bins are in rows
emp_preds_t <- as.data.frame(t(emp_preds))
emp_preds_t$timebins <- as.numeric(gsub("X", "", as.character(rownames(emp_preds_t))))


# Calculate statistics manually (b/c built in methods lead to a really
# oddly formatted cell type (it's some sort of named list))
stats_df <- data.frame(
  timebins = emp_preds_t$timebins,
  mean = rowMeans(emp_preds_t, na.rm = TRUE),
  sd = apply(emp_preds_t, 1, sd, na.rm = TRUE)
)

# Add confidence intervals
stats_df$ci95_lower <- stats_df$mean - (stats_df$sd * 1.96)
stats_df$ci95_upper <- stats_df$mean + (stats_df$sd * 1.96)
stats_df$ci50_lower <- stats_df$mean - (stats_df$sd * 0.6745)
stats_df$ci50_upper <- stats_df$mean + (stats_df$sd * 0.6745)

# Add range
stats_df$range_lower <- apply(emp_preds_t, 1, min, na.rm = TRUE)
stats_df$range_upper <- apply(emp_preds_t, 1, max, na.rm = TRUE)

# Remove sd column if you don't need it
stats_df$sd <- NULL

# Create the base dataframe with all the data
plot_data <- data.frame(
  year = year,
  mean = stats_df$mean,
  ci95_lower = stats_df$ci95_lower,
  ci95_upper = stats_df$ci95_upper,
  ci50_lower = stats_df$ci50_lower,
  ci50_upper = stats_df$ci50_upper,
  range_lower = stats_df$range_lower,
  range_upper = stats_df$range_upper
)

# Create the step line chart with ribbons
plot_emp_preds <- ggplot(plot_data) +
  # Add range ribbon (lightest shade)
  geom_stepribbon(aes(x = year, ymin = range_lower, ymax = range_upper),
                  fill = "#d2f7f8", alpha = 0.3) +
  # Add 95% CI ribbon (medium shade)
  geom_stepribbon(aes(x = year, ymin = ci95_lower, ymax = ci95_upper),
                  fill = "#a4f3f6", alpha = 0.3) +
  # Add 50% CI ribbon (darker shade)
  geom_stepribbon(aes(x = year, ymin = ci50_lower, ymax = ci50_upper),
                  fill = "#78F3F6", alpha = 0.3) +
  # Add mean line on top
  geom_step(aes(x = year, y = mean), color = "cyan2", size = 1) +
  labs(x = "Time (Ma)", y = "Diversity Through Time (# Genera)")

# Apply standard formatting
plot_emp_preds <- apply_standard_coords(plot_emp_preds)
plot_emp_preds <- apply_standard_theme(plot_emp_preds, show_legend = FALSE)

######################### PLOTS PER REGION ####################################

# PLOT N_OCCS BY REGION GRAPH
plot_n_occs_region <- create_multiline_plot(
  year = year,
  data_columns = list(data$n_occs_1.0, data$n_occs_2.0, 
                      data$n_occs_3.0, data$n_occs_4.0),
  line_labels = c("Region 1", "Region 2", "Region 3", "Region 4"),
  colors = c("Region 1" = "red", "Region 2" = "blue", 
             "Region 3" = "turquoise", "Region 4" = "orange1"),
  x_label = "Time (Ma)",
  y_label = "Raw Fossil Occurrences by Region",
  legend_title = "Region"
)

# PLOT N_SPECIES BY REGION GRAPH 
plot_n_species_region <- create_multiline_plot(
  year = year,
  data_columns = list(data$n_species_1.0, data$n_species_2.0, 
                      data$n_species_3.0, data$n_species_4.0),
  line_labels = c("Region 1", "Region 2", "Region 3", "Region 4"),
  colors = c("Region 1" = "red", "Region 2" = "blue", 
             "Region 3" = "turquoise", "Region 4" = "orange1"),
  x_label = "Time (Ma)",
  y_label = "Diversity Through Time (# Genera) by Region",
  legend_title = "Region"
)

# PLOT N_LOCS BY REGION GRAPH
plot_n_locs_region <- create_multiline_plot(
  year = year,
  data_columns = list(data$n_locs_1.0, data$n_locs_2.0, 
                      data$n_locs_3.0, data$n_locs_4.0),
  line_labels = c("Region 1", "Region 2", "Region 3", "Region 4"),
  colors = c("Region 1" = "red", "Region 2" = "blue", 
             "Region 3" = "turquoise", "Region 4" = "orange1"),
  x_label = "Time (Ma)",
  y_label = "Number of Localities by Region",
  legend_title = "Region"
)

######################### GROUPED GRAPH #############################

empty_plot <- ggplot() + theme_void()

# plot_spec_ext_events = speciation and extinction events
# plot_net_div_events= net diversification
# plot_emp_preds = empirical predictions (diversity through time # genera) NOT by region
# plot_n_occs_region = n occs by region (raw fossil occs)
# plot_n_species_region = diversity through time (# genera) by region

combined_plot <- plot_grid(
  plot_spec_ext_events, empty_plot, plot_net_div_events, plot_n_occs_region, plot_emp_preds, plot_n_species_region, plot_spec_rate, plot_ext_rate, plot_net_div_rate,
  ncol = 2, nrow = 3, labels = c("A", "", "B", "C", "D", "E"), label_size = 20, align = "hv", axis = "tblr")

# Save the combined plot as a PDF
ggsave("feature_plots_formatted/final_figure.pdf", combined_plot, width = 25, height = 20)


######################### SAVING EACH INDIVIDUAL GRAPH #############################
ggsave("feature_plots_formatted/spec_ext_events.pdf", plot_spec_ext_events, width = 10, height = 6)
ggsave("feature_plots_formatted/net_div_events.pdf", plot_net_div_events, width = 10, height = 6)
ggsave("feature_plots_formatted/emp_preds.pdf", plot_emp_preds, width = 10, height = 6)
ggsave("feature_plots_formatted/n_occs_region.pdf", plot_n_occs_region, width = 10, height = 6)
ggsave("feature_plots_formatted/n_species_region.pdf", plot_n_species_region, width = 10, height = 6)
ggsave("feature_plots_formatted/speciation_rate.pdf", plot_spec_rate, width = 10, height = 6)
ggsave("feature_plots_formatted/extinction_rate.pdf", plot_ext_rate, width = 10, height = 6)
ggsave("feature_plots_formatted/net_div_rate.pdf", plot_net_div_rate, width = 10, height = 6)
ggsave("feature_plots_formatted/n_endemics.pdf", plot_n_endemics, width = 10, height = 6)
ggsave("feature_plots_formatted/n_singletons.pdf", plot_n_singletons, width = 10, height = 6)
ggsave("feature_plots_formatted/range_through_div.pdf", plot_range_through_div, width = 10, height = 6)
ggsave("feature_plots_formatted/net_div_events.pdf", plot_net_div_events, width = 10, height = 6)
