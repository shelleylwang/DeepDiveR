#!/usr/bin/env Rscript
# Load necessary libraries
#install.packages("pammtools")
library(ggplot2)
library(deeptime)
library(tidyr)
library(dplyr)
library(pammtools)
library(cowplot)

setwd("../DeepDiveR/deepdive_project_main_branch_runs/temnospondyli/temnospondyli_models/simulations_20250312_lstm64_32_d64_32/")

# Check if there is a folder called "feature_plots_formatted" in the working directory
# If there isn't, make one
if (!dir.exists("feature_plots_formatted")) {
  dir.create("feature_plots_formatted")
}


# Read the CSV file into a data frame
data <- read.csv("Empirical_features_.csv")

# Duplicate the first row of data, so that the first two rows are identical
data<- rbind(data[1, ], data)

# COMMENT OUT YEAR VECTOR DEPENDING ON GENUS

# Temnospondyli and Synapsida year vector
year <- c(201.4, 208, 217, 227, 237, 242, 247, 252, 259.5, 264.3, 273, 283.5, 290.1, 309.8)

# Reptilia year vector:
#year <- c(199.5, 208, 217, 227, 237, 242, 247, 252, 259.5, 264.3, 273, 283.5, 290.1, 298.9)

# Make the year vector negative
year <- -year

# Format axis labels
format_labels <- function(x) {
  return(sprintf("%.0f", abs(x)))
}

######################### 1. N_SPECIES GRAPH ####################################

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
       y = "Temnospondyli Diversity Through Time (# Genera) by Region",
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

# Display the plot
print(plot_e)

# Save the plot as a PDF
ggsave("feature_plots_formatted/n_genera_by_region_formatted.pdf", plot = plot_e, width = 10, height = 6)

######################### 2. N_OCCS GRAPH ####################################

# Create a longer format dataset combining all species columns
plot_data <- data.frame(
  year = rep(year, 4),
  columns_list = c(data$n_occs_1.0, data$n_occs_2.0, data$n_occs_3.0, data$n_occs_4.0),
  columns_labels = factor(rep(c("Region 1", "Region 2", "Region 3", "Region 4"), each = length(year)))
)

  # Create the step line chart with multiple lines
  plot_d <- ggplot(plot_data, aes(x = year, y = columns_list, color = columns_labels)) +
    geom_step(size = 1) +  # Increase line thickness here
    scale_x_reverse() +
    # Add distinct colors for each species line
    scale_color_manual(values = c("Region 1" = "red",
                                  "Region 2" = "blue",
                                  "Region 3" = "turquoise",
                                  "Region 4" = "orange1")) +
    labs(x = "Time (Ma)",
         y = "Temnospondyli Raw Fossil Occurrences by Region",
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

  # Display the plot
  print(plot_d)

  # Save the plot as a PDF
  ggsave("feature_plots_formatted/n_occs_by_region_formatted.pdf", plot = plot_d, width = 10, height = 6)

 ######################## 3. N_LOCS GRAPH ####################################

  # Create a longer format dataset combining all species columns
  plot_data <- data.frame(
    year = rep(year, 4),
    columns_list = c(data$n_locs_1.0, data$n_locs_2.0, data$n_locs_3.0, data$n_locs_4.0),
    columns_labels = factor(rep(c("Region 1", "Region 2", "Region 3", "Region 4"), each = length(year)))
  )

    # Create the step line chart with multiple lines
    step_line_chart <- ggplot(plot_data, aes(x = year, y = columns_list, color = columns_labels)) +
      geom_step(size = 1) +  # Increase line thickness here
      scale_x_reverse() +
      # Add distinct colors for each species line
      scale_color_manual(values = c("Region 1" = "red",
                                    "Region 2" = "blue",
                                    "Region 3" = "turquoise",
                                    "Region 4" = "orange1")) +
      labs(x = "Time (Ma)",
           y = "Number of Temnospondyli Localities",
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

    # Display the plot
    print(step_line_chart)

    # Save the plot as a PDF
    ggsave("feature_plots_formatted/n_locs_by_region_formatted.pdf", plot = step_line_chart, width = 10, height = 6)


  #################### 4. ORIGINATION + EXTINCTION EVENTS#######################
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
           y = "Temnospondyli Extinction and Speciation Events",
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

    # Display the plot
    print(plot_a)

    # Save the plot as a PDF
    ggsave("feature_plots_formatted/speciation_extinction_formatted.pdf", plot = plot_a, width = 10, height = 6)


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

    # Save the plot
    ggsave(pdf_name, plot = step_line_chart, width = 10, height = 6)

    return(step_line_chart)
  }

  # N_Endemics
  plot_graph(year, data$n_endemics, "Number of Temnospondyli Endemics", "feature_plots_formatted/n_endemics_formatted.pdf")

  # N_Singletons
  plot_graph(year, data$n_singletons, "Number of Temnospondyli Singletons", "feature_plots_formatted/n_singletons_formatted.pdf")

  # Range_through_div
  plot_graph(year, data$range_through_div, "Temnospondyli Range Through Diversity", "feature_plots_formatted/range_through_div_formatted.pdf")

  # Net diversity
  net_diversity = data$origination_events - data$extinction_events
  plot_graph(year, net_diversity, "Temnospondyli Net Diversification", "feature_plots_formatted/net_diversity_formatted.pdf")
  plot_b <- plot_graph(year, net_diversity, "Temnospondyli Net Diversification", "feature_plots_formatted/net_diversity_formatted.pdf")

  ######################### 6. EMPIRICAL PREDICTIONS #############################

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
    plot_c <- ggplot(plot_data) +
      # Add range ribbon (lightest shade)
      geom_stepribbon(aes(x = year, ymin = range_lower, ymax = range_upper),
                  fill = "#f9d1f3", alpha = 0.3) +
      # Add 95% CI ribbon (medium shade)
      geom_stepribbon(aes(x = year, ymin = ci95_lower, ymax = ci95_upper),
                  fill = "#f9b5ef", alpha = 0.3) +
      # Add 50% CI ribbon (darker shade)
      geom_stepribbon(aes(x = year, ymin = ci50_lower, ymax = ci50_upper),
                  fill = "#f38aee", alpha = 0.3) +
      # Add mean line on top
      geom_step(aes(x = year, y = mean), color = "magenta2", size = 1) +
      scale_x_reverse() +
      labs(x = "Time (Ma)", y = "Temnospondyli Diversity Through Time (# Genera)") +
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

    # Save the plot
    ggsave("feature_plots_formatted/empirical_predictions_formatted.pdf", plot = plot_c, width = 10, height = 6)


    ######################### 7. GROUPED GRAPHS #############################

    empty_plot <- ggplot() + theme_void()

final_plot <- plot_grid(plot_a, plot_b, plot_c, empty_plot, plot_d, plot_e,
                        ncol = 2,
                        labels = c("A", "B", "C", "", "D", "E"),
                        byrow = TRUE,
                        rel_heights = c(1, 1, 1, 1, 1, 1))


# Function to adjust individual plots
adjust_plot <- function(p) {
  p +
    theme(
      # Increase margin on the left for y-axis labels
      plot.margin = margin(5, 5, 5, 20, "pt"),
      # Make y-axis text smaller and possibly adjust angle
      axis.text.y = element_text(size = 8, angle = 0),
      # Optional: truncate very long labels
      # axis.text.y = element_text(size = 8, angle = 0, hjust = 1)
    )
}

# Apply adjustments to each plot
plot_a_adj <- adjust_plot(plot_a)
plot_b_adj <- adjust_plot(plot_b)
plot_c_adj <- adjust_plot(plot_c)
plot_d_adj <- adjust_plot(plot_d)
plot_e_adj <- adjust_plot(plot_e)

final_plot <- plot_grid(
  plot_a, plot_b,
  plot_c, empty_plot,
  plot_d, plot_e,
  ncol = 2,
  labels = c("A", "B", "C", "", "D", "E"),
  byrow = TRUE,
  # Give more horizontal space between plots to prevent label overlap
  align = "hv",       # Align both horizontally and vertically
  axis = "lr",        # Align by left and right axes
  rel_widths = c(1.3, 1),  # Give left column more space for y-axis labels
  rel_heights = c(1, 1, 1) # Keep equal heights for the rows
)

# Create wrapper function to resize axes without modifying plot content
resize_plot <- function(plot) {
  # Extract the plot's gtable structure
  g <- ggplotGrob(plot)

  # Find and modify the text grobs for y-axis labels
  for(i in which(g$layout$name == "axis-l")) {
    g$grobs[[i]]$children[[2]]$gp$fontsize <- 6  # Reduce font size dramatically
  }

  # Return the modified plot
  g
}

# Apply wrapper to each plot
plot_a_mod <- resize_plot(plot_a)
plot_b_mod <- resize_plot(plot_b)
plot_c_mod <- resize_plot(plot_c)
plot_d_mod <- resize_plot(plot_d)
plot_e_mod <- resize_plot(plot_e)

# Now create compact grid with modified plots
final_plot <- plot_grid(
  plot_a_mod, plot_b_mod,
  plot_c_mod, empty_plot,
  plot_d_mod, plot_e_mod,
  ncol = 2,
  labels = c("A", "B", "C", "", "D", "E"),
  byrow = TRUE,
  greedy = TRUE,
  scale = 1.0,
  align = "none"
)

# Save with compact dimensions
ggsave("final_figure.pdf", final_plot, width = 10, height = 12, units = "in")
                            rel_widths = c(1, 1))

    # Save the final plot as a PDF
    ggsave("grouped_plots.pdf", final_plot, width = 15, height = 12, units = "in")

    library(gtable)
    library(grid)

    # Alternative approach using gtable
    resize_plot <- function(plot) {
      # Convert to grob
      g <- ggplotGrob(plot)

      # Find and edit all text elements
      for (i in 1:length(g$grobs)) {
        if (grepl("axis-l", g$layout$name[i])) {
          # Find text within the axis grob
          g$grobs[[i]] <- editGrob(g$grobs[[i]], gp = gpar(fontsize = 6))
        }
      }

      return(g)
    }

    # Apply and create layout as before




    install.packages("patchwork")
    library(patchwork)

    # Create layout using patchwork which is often more forgiving with grobs
    final_plot <- (plot_a + plot_b) /
      (plot_c + empty_plot) /
      (plot_d + plot_e) +
      plot_annotation(tag_levels = 'A')

    # Save with compact dimensions
    ggsave("final_figure.pdf", final_plot, width = 10, height = 12, units = "in")
