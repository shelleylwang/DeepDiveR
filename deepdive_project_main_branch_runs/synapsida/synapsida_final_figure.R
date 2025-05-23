#!/usr/bin/env Rscript
# Load necessary libraries
#install.packages("pammtools")
library(ggplot2)
library(deeptime)
library(tidyr)
library(dplyr)
library(pammtools)
library(cowplot)

setwd("../../../synapsida/synapsida_models/simulations_20250320_lstm64_32_d64_32/")

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
# Temnospondyli and Synapsida year vector
year <- c(201.4, 208, 217, 227, 237, 242, 247, 252, 259.5, 264.3, 273, 283.5, 290.1, 309.8)

# Reptilia year vector:
# year <- c(199.5, 208, 217, 227, 237, 242, 247, 252, 259.5, 264.3, 273, 283.5, 290.1, 298.9)

# Make the year vector negative
year <- -year

# Format axis labels
format_labels <- function(x) {
  return(sprintf("%.0f", abs(x)))
}



#################### 4. PLOT_A ORIGINATION + EXTINCTION EVENTS#######################
# Create a longer format dataset combining all species columns
plot_data <- data.frame(
  year = rep(year, 2),
  columns_list = c(data$extinction_events, data$origination_events),
  columns_labels = factor(rep(c("Extinction", "Speciation"), each = length(year)))
)

# Create the step line chart with multiple lines
plot_a <- ggplot(plot_data, aes(x = year, y = columns_list, color = columns_labels)) +
  geom_step(size = 1) +  # Increase line thickness here
  scale_x_reverse() +
  # Add distinct colors for each species line
  scale_color_manual(values = c("Extinction" = "red",
                                "Speciation" = "blue")) +
  labs(x = "Time (Ma)",
       y = "Extinction and Speciation Events",
       color = " ") +
  coord_geo(xlim = c(-320, -190),
            expand = FALSE,
            clip = "on",
            dat = list("international epochs", "international periods"),
            abbrv = list(TRUE, FALSE),
            pos = list("bottom", "bottom"),
            alpha = 1,
            height = unit(1.5, "line"),
            rot = 0,
            size = list(6, 5),
            neg = TRUE) +
  scale_x_continuous(limits = c(-320, -190),
                     breaks = seq(-320, -190, by = 10),
                     labels = format_labels) +
  theme_classic() +
  theme(plot.margin = unit(c(2, 1, 1, 1), "cm"),
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        axis.title.x = element_text(size = 14, face = "bold", margin = margin(t = 10)),
        axis.title.y = element_text(size = 14, face = "bold", margin = margin(r = 10)),
        axis.text = element_text(size = 12, face = "bold"),
        legend.position = "none"
        # legend.position = c(0.12, 0.85),
        # legend.title = element_text(size = 12, face = "bold"),
        # legend.text = element_text(size = 10)
  )



############################ 5. SINGLE GRAPHS ################################

plot_graph <- function(year, y, y_label) {
  # Create the plot data
  plot_data <- data.frame(year = year, y = y)

  # Create the step line chart
  step_line_chart <- ggplot(plot_data, aes(x = year, y = y)) +
    geom_step(size = 1) +
    scale_x_reverse() +
    labs(x = "Time (Ma)", y = y_label) +
    coord_geo(xlim = c(-320, -190), expand = FALSE, clip = "on",
              dat = list("international epochs", "international periods"),
              abbrv = list(TRUE, FALSE), pos = list("bottom", "bottom"),
              alpha = 1, height = unit(1.5, "line"), rot = 0,
              size = list(6, 5), neg = TRUE) +
    scale_x_continuous(limits = c(-320, -190),
                       breaks = seq(-320, -190, by = 10),
                       labels = format_labels) +
    theme_classic() +
    theme(plot.margin = unit(c(2, 1, 1, 1), "cm"),
          plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
          axis.title.x = element_text(size = 14, face = "bold", margin = margin(t = 10)),
          axis.title.y = element_text(size = 14, face = "bold", margin = margin(r = 10)),
          axis.text = element_text(size = 12, face = "bold"))

  return(step_line_chart)
}

# Net diversity
net_diversity = data$origination_events - data$extinction_events
plot_b <- plot_graph(year, net_diversity, "Net Diversification Events")


######################### 6. PLOT_D EMPIRICAL PREDICTIONS #############################

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
plot_d <- ggplot(plot_data) +
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
  scale_x_reverse() +
  labs(x = "Time (Ma)", y = "Diversity Through Time (# Genera)") +
  coord_geo(xlim = c(-320, -190), expand = FALSE, clip = "on",
            dat = list("international epochs", "international periods"),
            abbrv = list(TRUE, FALSE), pos = list("bottom", "bottom"),
            alpha = 1, height = unit(1.5, "line"), rot = 0,
            size = list(6, 5), neg = TRUE) +
  scale_x_continuous(limits = c(-320, -190),
                     breaks = seq(-320, -190, by = 10),
                     labels = format_labels) +
  theme_classic() +
  theme(plot.margin = unit(c(2, 1, 1, 1), "cm"),
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        axis.title.x = element_text(size = 14, face = "bold", margin = margin(t = 10)),
        axis.title.y = element_text(size = 14, face = "bold", margin = margin(r = 10)),
        axis.text = element_text(size = 12, face = "bold"))

######################### 2. N_OCCS GRAPH ####################################

# Create a longer format dataset combining all species columns
plot_data <- data.frame(
  year = rep(year, 4),
  columns_list = c(data$n_occs_1.0, data$n_occs_2.0, data$n_occs_3.0, data$n_occs_4.0),
  columns_labels = factor(rep(c("Region 1", "Region 2", "Region 3", "Region 4"), each = length(year)))
)

# Create the step line chart with multiple lines
plot_c <- ggplot(plot_data, aes(x = year, y = columns_list, color = columns_labels)) +
  geom_step(size = 1) +  # Increase line thickness here
  scale_x_reverse() +
  # Add distinct colors for each species line
  scale_color_manual(values = c("Region 1" = "red",
                                "Region 2" = "blue",
                                "Region 3" = "turquoise",
                                "Region 4" = "orange1")) +
  labs(x = "Time (Ma)",
       y = "Raw Fossil Occurrences by Region",
       color = "Region") +
  coord_geo(xlim = c(-320, -190),
            expand = FALSE,
            clip = "on",
            dat = list("international epochs", "international periods"),
            abbrv = list(TRUE, FALSE),
            pos = list("bottom", "bottom"),
            alpha = 1,
            height = unit(1.5, "line"),
            rot = 0,
            size = list(6, 5),
            neg = TRUE) +
  scale_x_continuous(limits = c(-320, -190),
                     breaks = seq(-320, -190, by = 10),
                     labels = format_labels) +
  theme_classic() +
  theme(plot.margin = unit(c(2, 1, 1, 1), "cm"),
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        axis.title.x = element_text(size = 14, face = "bold", margin = margin(t = 10)),
        axis.title.y = element_text(size = 14, face = "bold", margin = margin(r = 10)),
        axis.text = element_text(size = 12, face = "bold"),
        legend.position = "right",
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10)
  )

######################### 1. PLOT_E N_SPECIES GRAPH ####################################

# Create a longer format dataset combining all species columns
plot_data <- data.frame(
  year = rep(year, 4),
  columns_list = c(data$n_species_1.0, data$n_species_2.0, data$n_species_3.0, data$n_species_4.0),
  columns_labels = factor(rep(c("Region 1", "Region 2", "Region 3", "Region 4"), each = length(year)))
)


# Create the step line chart with multiple lines
plot_e <- ggplot(plot_data, aes(x = year, y = columns_list, color = columns_labels)) +
  geom_step(size = 1) +  # Increase line thickness here
  scale_x_reverse() +
  # Add distinct colors for each species line
  scale_color_manual(values = c("Region 1" = "red",
                                "Region 2" = "blue",
                                "Region 3" = "turquoise",
                                "Region 4" = "orange1")) +
  labs(x = "Time (Ma)",
       y = "Diversity Through Time (# Genera) by Region",
       color = "Region") +
  coord_geo(xlim = c(-320, -190),
            expand = FALSE,
            clip = "on",
            dat = list("international epochs", "international periods"),
            abbrv = list(TRUE, FALSE),
            pos = list("bottom", "bottom"),
            alpha = 1,
            height = unit(1.5, "line"),
            rot = 0,
            size = list(6, 5),
            neg = TRUE) +
  scale_x_continuous(limits = c(-320, -190),
                     breaks = seq(-320, -190, by = 10),
                     labels = format_labels) +
  theme_classic() +
  theme(plot.margin = unit(c(2, 1, 1, 1), "cm"),
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        axis.title.x = element_text(size = 14, face = "bold", margin = margin(t = 10)),
        axis.title.y = element_text(size = 14, face = "bold", margin = margin(r = 10)),
        axis.text = element_text(size = 12, face = "bold"),
        legend.position = "right",
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10)
  )

######################### 7. GROUPED GRAPHS #############################

empty_plot <- ggplot() + theme_void()

# plot_a = speciation and extinction events
# plot_b = net diversification
# plot_d = empirical predictions (diversity through time # genera) NOT by region
# plot_c = n occs by region (raw fossil occs)
# plot_e = diversity through time (# genera) by region

combined_plot <- plot_grid(plot_a, empty_plot, plot_b, plot_c, plot_d, plot_e, ncol = 2, nrow = 3, labels = c("A", "", "B", "C", "D", "E"), label_size = 20)


# Save the combined plot as a PDF
ggsave("feature_plots_formatted/synapsida_final_figure.pdf", combined_plot, width = 25, height = 20)

