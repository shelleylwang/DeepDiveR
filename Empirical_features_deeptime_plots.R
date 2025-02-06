#!/usr/bin/env Rscript
# Load necessary libraries
library(ggplot2)
library(deeptime)
library(tidyr)
library(dplyr)

# Set working directory
setwd("reptilia/reptilia_models/simulations_20250204_lstm64_32_d64_32_conditional/")

# Read the CSV file into a data frame
data <- read.csv("Empirical_features__conditional.csv")

# Define the year vector (x axis), and make it negative
year <- c(199.3, 217, 227, 237, 242, 247, 252, 259.5, 264.3, 273, 283.5, 290.1)
year <- -year


######################### 1. N_SPECIES GRAPH ####################################

# Create a longer format dataset combining all species columns
plot_data <- data.frame(
  year = rep(year, 4),
  columns_list = c(data$n_species_area_1, data$n_species_area_2, data$n_species_area_3, data$n_species_area_4),
  columns_labels = factor(rep(c("Area 1", "Area 2", "Area 3", "Area 4"), each = length(year)))
)

# Format axis labels
format_labels <- function(x) {
  return(sprintf("%.0f", abs(x)))
}

# Create the step line chart with multiple lines
step_line_chart <- ggplot(plot_data, aes(x = year, y = columns_list, color = columns_labels)) +
  geom_step(size = 1) +  # Increase line thickness here
  scale_x_reverse() +
  # Add distinct colors for each species line
  scale_color_manual(values = c("Area 1" = "red",
                                "Area 2" = "blue",
                                "Area 3" = "darkgreen",
                                "Area 4" = "darkorange")) +
  labs(x = "Time (Ma)",
       y = "Number of Genera",
       color = "Area") +
  coord_geo(xlim = c(-300, -190),
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
  scale_x_continuous(limits = c(-300, -190),
                     breaks = seq(-300, -190, by = 10),
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

# Display the plot
print(step_line_chart)

# Save the plot as a PDF
ggsave("feature_plots_formatted/n_genera_by_area_formatted.pdf", plot = step_line_chart, width = 10, height = 6)

######################### 2. N_OCCS GRAPH ####################################

# Create a longer format dataset combining all species columns
plot_data <- data.frame(
  year = rep(year, 4),
  columns_list = c(data$n_occs_area_1, data$n_occs_area_2, data$n_occs_area_3, data$n_occs_area_4),
  columns_labels = factor(rep(c("Area 1", "Area 2", "Area 3", "Area 4"), each = length(year)))
)

  # Create the step line chart with multiple lines
  step_line_chart <- ggplot(plot_data, aes(x = year, y = columns_list, color = columns_labels)) +
    geom_step(size = 1) +  # Increase line thickness here
    scale_x_reverse() +
    # Add distinct colors for each species line
    scale_color_manual(values = c("Area 1" = "red",
                                  "Area 2" = "blue",
                                  "Area 3" = "darkgreen",
                                  "Area 4" = "darkorange")) +
    labs(x = "Time (Ma)",
         y = "Number of Occurrences",
         color = "Area") +
    coord_geo(xlim = c(-300, -190),
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
    scale_x_continuous(limits = c(-300, -190),
                       breaks = seq(-300, -190, by = 10),
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

  # Display the plot
  print(step_line_chart)

  # Save the plot as a PDF
  ggsave("feature_plots_formatted/n_occs_by_area_formatted.pdf", plot = step_line_chart, width = 10, height = 6)

 ######################## 3. N_LOCS GRAPH ####################################

  # Create a longer format dataset combining all species columns
  plot_data <- data.frame(
    year = rep(year, 4),
    columns_list = c(data$n_locs_area_1, data$n_locs_area_2, data$n_locs_area_3, data$n_locs_area_4),
    columns_labels = factor(rep(c("Area 1", "Area 2", "Area 3", "Area 4"), each = length(year)))
  )

    # Create the step line chart with multiple lines
    step_line_chart <- ggplot(plot_data, aes(x = year, y = columns_list, color = columns_labels)) +
      geom_step(size = 1) +  # Increase line thickness here
      scale_x_reverse() +
      # Add distinct colors for each species line
      scale_color_manual(values = c("Area 1" = "red",
                                    "Area 2" = "blue",
                                    "Area 3" = "darkgreen",
                                    "Area 4" = "darkorange")) +
      labs(x = "Time (Ma)",
           y = "Number of Localities",
           color = "Area") +
      coord_geo(xlim = c(-300, -190),
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
      scale_x_continuous(limits = c(-300, -190),
                         breaks = seq(-300, -190, by = 10),
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

    # Display the plot
    print(step_line_chart)

    # Save the plot as a PDF
    ggsave("feature_plots_formatted/n_locs_by_area_formatted.pdf", plot = step_line_chart, width = 10, height = 6)


  #################### 4. ORIGINATION + EXTINCTION EVENTS#######################
    # Create a longer format dataset combining all species columns
    plot_data <- data.frame(
      year = rep(year, 2),
      columns_list = c(data$extinction_events, data$origination_events),
      columns_labels = factor(rep(c("Extinction", "Origination"), each = length(year)))
    )

    # Create the step line chart with multiple lines
    step_line_chart <- ggplot(plot_data, aes(x = year, y = columns_list, color = columns_labels)) +
      geom_step(size = 1) +  # Increase line thickness here
      scale_x_reverse() +
      # Add distinct colors for each species line
      scale_color_manual(values = c("Extinction" = "red",
                                    "Origination" = "blue")) +
      labs(x = "Time (Ma)",
           y = "Extinction and Origination Events Through Time",
           color = "Area") +
      coord_geo(xlim = c(-300, -190),
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
      scale_x_continuous(limits = c(-300, -190),
                         breaks = seq(-300, -190, by = 10),
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

    # Display the plot
    print(step_line_chart)

    # Save the plot as a PDF
    ggsave("feature_plots_formatted/origination_extinction_formatted.pdf", plot = step_line_chart, width = 10, height = 6)


  ############################ 5. SINGLE GRAPHS ################################

  plot_graph <- function(year, y, y_label, pdf_name) {
    # Debugging: Print the PDF path
    print(paste("Saving plot to:", pdf_name))

    # Ensure the directory exists
    if (!dir.exists(dirname(pdf_name))) {
      dir.create(dirname(pdf_name), recursive = TRUE)
    }

    # Create the plot data
    plot_data <- data.frame(year = year, y = y)

    # Create the step line chart
    step_line_chart <- ggplot(plot_data, aes(x = year, y = y)) +
      geom_step(size = 1) +
      scale_x_reverse() +
      labs(x = "Time (Ma)", y = y_label) +
      coord_geo(xlim = c(-300, -190), expand = FALSE, clip = "on",
                dat = list("international epochs", "international periods"),
                abbrv = list(TRUE, FALSE), pos = list("bottom", "bottom"),
                alpha = 1, height = unit(1.5, "line"), rot = 0,
                size = list(6, 5), neg = TRUE) +
      scale_x_continuous(limits = c(-300, -190),
                         breaks = seq(-300, -190, by = 10),
                         labels = format_labels) +
      theme_classic() +
      theme(plot.margin = unit(c(2, 1, 1, 1), "cm"),
            plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
            axis.title.x = element_text(size = 14, face = "bold", margin = margin(t = 10)),
            axis.title.y = element_text(size = 14, face = "bold", margin = margin(r = 10)),
            axis.text = element_text(size = 12, face = "bold"))

    # Save the plot
    ggsave(pdf_name, plot = step_line_chart, width = 10, height = 6)

    return(step_line_chart)
  }

  # N_Endemics
  plot_graph(year, data$n_endemics, "Number of Endemics", "feature_plots_formatted/n_endemics_formatted.pdf")

  # N_Singletons
  plot_graph(year, data$n_singletons, "Number of Singletons", "feature_plots_formatted/n_singletons_formatted.pdf")

  # Range_through_div
  plot_graph(year, data$range_through_div, "Range Through Diversity", "feature_plots_formatted/range_through_div_formatted.pdf")


    # Net diversity
  net_diversity = data$origination_events - data$extinction_events
  plot_graph(year, net_diversity, "Net Diversity", "feature_plots_formatted/net_diversity_formatted.pdf")

