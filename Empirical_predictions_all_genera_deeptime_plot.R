library(ggplot2)
library(deeptime)
library(tidyr)
library(dplyr)

setwd("./")

# Read the CSV file into a data frame
data <- read.csv("Empirical_features__conditional.csv")

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
       y = "Temnospondyli Extinction and Origination Events",
       color = "Area") +
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
ggsave("feature_plots_formatted/origination_extinction_formatted.pdf", plot = step_line_chart, width = 10, height = 6)