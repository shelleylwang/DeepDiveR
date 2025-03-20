library(ggplot2)
library(deeptime)
library(tidyr)
library(dplyr)
library(pammtools)
library(gridExtra)


setwd("C:/Users/SimoesLabAdmin/Documents/DeepDiveR/deepdive_project_main_branch_runs")

# Temnospondyli and Synapsida year vector
year_tem_syn <- c(201.4, 208, 217, 227, 237, 242, 247, 252, 259.5, 264.3, 273, 283.5, 290.1, 309.8)
year_tem_syn <- -year_tem_syn

# Reptilia year vector:
year_rep <- c(199.5, 208, 217, 227, 237, 242, 247, 252, 259.5, 264.3, 273, 283.5, 290.1, 298.9)
year_rep <- -year_rep

# Format axis labels function
format_labels <- function(x) {
  return(sprintf("%.0f", abs(x)))
}

# Read in empirical predictions data
emp_preds_tem <- read.csv("./temnospondyli/temnospondyli_models/simulations_20250320_lstm64_32_d64_32/Empirical_predictions_.csv")
emp_preds_syn <- read.csv("./synapsida/synapsida_models/simulations_20250320_lstm64_32_d64_32/Empirical_predictions_.csv")
emp_preds_rep <- read.csv("./reptilia/reptilia_models/simulations_20250320_lstm64_32_d64_32/Empirical_predictions_.csv")


# Transpose data
emp_preds_tem_t <- t(emp_preds_tem)
emp_preds_syn_t <- t(emp_preds_syn)
emp_preds_rep_t <- t(emp_preds_rep)

# Get stats_df Temnospondyli
stats_df_tem <- data.frame(
  mean = rowMeans(emp_preds_tem_t, na.rm = TRUE),
  sd = apply(emp_preds_tem_t, 1, sd, na.rm = TRUE)
)

stats_df_tem$ci95_lower <- stats_df_tem$mean - (stats_df_tem$sd * 1.96)
stats_df_tem$ci95_upper <- stats_df_tem$mean + (stats_df_tem$sd * 1.96)
stats_df_tem$ci50_lower <- stats_df_tem$mean - (stats_df_tem$sd * 0.6745)
stats_df_tem$ci50_upper <- stats_df_tem$mean + (stats_df_tem$sd * 0.6745)
stats_df_tem$range_lower <- apply(emp_preds_tem_t, 1, min, na.rm = TRUE)
stats_df_tem$range_upper <- apply(emp_preds_tem_t, 1, max, na.rm = TRUE)

plot_data_tem <- data.frame(
  year = year_tem_syn,
  mean = stats_df_tem$mean,
  ci95_lower = stats_df_tem$ci95_lower,
  ci95_upper = stats_df_tem$ci95_upper,
  ci50_lower = stats_df_tem$ci50_lower,
  ci50_upper = stats_df_tem$ci50_upper,
  range_lower = stats_df_tem$range_lower,
  range_upper = stats_df_tem$range_upper
)

# stats_df Synapsida
stats_df_syn <- data.frame(
  mean = rowMeans(emp_preds_syn_t, na.rm = TRUE),
  sd = apply(emp_preds_syn_t, 1, sd, na.rm = TRUE)
)

stats_df_syn$ci95_lower <- stats_df_syn$mean - (stats_df_syn$sd * 1.96)
stats_df_syn$ci95_upper <- stats_df_syn$mean + (stats_df_syn$sd * 1.96)
stats_df_syn$ci50_lower <- stats_df_syn$mean - (stats_df_syn$sd * 0.6745)
stats_df_syn$ci50_upper <- stats_df_syn$mean + (stats_df_syn$sd * 0.6745)
stats_df_syn$range_lower <- apply(emp_preds_syn_t, 1, min, na.rm = TRUE)
stats_df_syn$range_upper <- apply(emp_preds_syn_t, 1, max, na.rm = TRUE)

plot_data_syn <- data.frame(
  year = year_tem_syn,
  mean = stats_df_syn$mean,
  ci95_lower = stats_df_syn$ci95_lower,
  ci95_upper = stats_df_syn$ci95_upper,
  ci50_lower = stats_df_syn$ci50_lower,
  ci50_upper = stats_df_syn$ci50_upper,
  range_lower = stats_df_syn$range_lower,
  range_upper = stats_df_syn$range_upper
)

# stats_df Reptilia
stats_df_rep <- data.frame(
  mean = rowMeans(emp_preds_rep_t, na.rm = TRUE),
  sd = apply(emp_preds_rep_t, 1, sd, na.rm = TRUE)
)

stats_df_rep$ci95_lower <- stats_df_rep$mean - (stats_df_rep$sd * 1.96)
stats_df_rep$ci95_upper <- stats_df_rep$mean + (stats_df_rep$sd * 1.96)
stats_df_rep$ci50_lower <- stats_df_rep$mean - (stats_df_rep$sd * 0.6745)
stats_df_rep$ci50_upper <- stats_df_rep$mean + (stats_df_rep$sd * 0.6745)
stats_df_rep$range_lower <- apply(emp_preds_rep_t, 1, min, na.rm = TRUE)
stats_df_rep$range_upper <- apply(emp_preds_rep_t, 1, max, na.rm = TRUE)

plot_data_rep <- data.frame(
  year = year_rep,
  mean = stats_df_rep$mean,
  ci95_lower = stats_df_rep$ci95_lower,
  ci95_upper = stats_df_rep$ci95_upper,
  ci50_lower = stats_df_rep$ci50_lower,
  ci50_upper = stats_df_rep$ci50_upper,
  range_lower = stats_df_rep$range_lower,
  range_upper = stats_df_rep$range_upper
)


#################### 4. ORIGINATION + EXTINCTION EVENTS#######################
# Add a new column 'dataset' to each dataframe to distinguish them
plot_data_tem$dataset <- "Temnospondyli"
plot_data_syn$dataset <- "Synapsida"
plot_data_rep$dataset <- "Reptilia"

# Combine the dataframes into one
combined_plot_data <- rbind(plot_data_tem, plot_data_syn, plot_data_rep)

# Create the step line chart with ribbons for all datasets
step_line_chart <- ggplot(combined_plot_data) +
  # Add range ribbon (lightest shade)
  geom_stepribbon(aes(x = year, ymin = range_lower, ymax = range_upper, fill = dataset),
                  alpha = 0.3) +
  # Add 95% CI ribbon (medium shade)
  geom_stepribbon(aes(x = year, ymin = ci95_lower, ymax = ci95_upper, fill = dataset),
                  alpha = 0.3) +
  # Add 50% CI ribbon (darker shade)
  geom_stepribbon(aes(x = year, ymin = ci50_lower, ymax = ci50_upper, fill = dataset),
                  alpha = 0.3) +
  # Add mean line on top
  geom_step(aes(x = year, y = mean, color = dataset), size = 1) +
  scale_x_reverse() +
  labs(x = "Time (Ma)", y = "Global Diversity Through Time (# Genera) by Clade") +
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
        axis.text = element_text(size = 12, face = "bold")) +
  scale_fill_manual(values = c("Temnospondyli" = "magenta2", "Synapsida" = "orangered1", "Reptilia" = "cyan2"),
                    guide = guide_legend(title = "Dataset")) +
  scale_color_manual(values = c("Temnospondyli" = "magenta2", "Synapsida" = "orangered1", "Reptilia" = "cyan2"),
                     guide = guide_legend(title = "Dataset"))

# Save the plot
ggsave("./empirical_predictions_ALL_formatted_p_mass_extinction.pdf", plot = step_line_chart, width = 10, height = 6)
