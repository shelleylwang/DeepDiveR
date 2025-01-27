# Install and load required packages
install.packages("devtools")
devtools::install_github("Liudas-Dau/hespdiv")
install.packages("sp")
library(sp)
library(hespdiv)
setwd('C:/Users/SimoesLabAdmin/Documents/DeepDiveR/hespdiv')

###########################
# 1. STUDY AREA POLYGON   #
###########################
#For the sake of making hespdiv polygons comparable across bins, they should have the same area

x_coords <- c(-90, 155, 155, -90, -90)
y_coords <- c(-80, -80, 80, 80, -80)
study_area_polygon <- data.frame(x = x_coords, y = y_coords)
print(study_area_polygon)
plot(study_area_polygon$x, study_area_polygon$y, type = "l", xlab = "Longitude", ylab = "Latitude", main = "Rectangle Shape")

########################################
# 2. FUNCTION: LOAD DATA + RUN HESPDIV #
########################################

# Take in the hespdiv data and the bin number
# Return the hespdiv analysis result and the original dataset in a list

hespdiv_analysis <- function(tb) {
  # Format data for hespdiv
  species <- tb$Taxon  # or species column
  coords <- data.frame(
    x = tb$'Rotated.Lon',
    y = tb$'Rotated.Lat'
    )

  # Run basic hespdiv analysis
  result <- hespdiv(
    data = species,
    xy.dat = coords,
    study.pol = study_area_polygon,
    n.split.pts = 15  # Make this much higher in the future
    # Default 15 will generate 120 split-lines for each subdivision attempt.
    # Increasing this value improves the fit of straight split-lines to the data
    # but also increases computation time
  )

  # Print initial results
  print("Basic Statistics about Split Lines:")
  print(result$poly.stats)
  print(result$split.stats)
  print(result$split.lines)
  print(result$polygons.xy)

  # Return hespdiv object
  return(result)
}

# How to call it:
# Load data
tb_1 <- read.csv("../../BDNN_Arielli/data/hespdiv/hespdiv_bin1.csv") # Load the data
result_1 <- hespdiv_analysis(tb_1) # Run hespdiv analysis


################################
# 3. VISUALIZATION FUNCTIONS   #
################################

visualization <- function(result_num, output_file) {
  # Load required libraries
  library(gridExtra)
  library(ggplot2)

  # Create 2D plots first
  print("Creating visualizations...")
  p1 <- plot_hespdiv(result_num, n.loc = TRUE)  # Show number of occurrences
  p2 <- plot_hespdiv(result_num, type = "w") # Use line width for performance
  p3 <- poly_scheme(result_num, seed = 4)

  # Initialize PDF device with appropriate dimensions
  pdf(output_file, width = 10, height = 15)  # Height adjusted for 3 plots

  # Arrange all plots vertically with appropriate spacing
  grid.arrange(
    p1,
    p2,
    p3,
    ncol = 1,
    heights = c(1, 1, 1),
    padding = unit(2, "line")
  )

  # Close the PDF device
  dev.off()

  print(paste("Visualizations saved to", output_file))

  # 3D visualization with blok3d
  print("Creating 3D visualization with height = rank...")
  blok3d(result_num, height = "rank")
  # Interactively remove specific polygons if needed
  # polypop(result, height = "mean") # Set the "height" to the same value as was set in the blok3D function
  # not all HespDiv polygons are displayed in the output of blok3d(hd)
  # if not enough split-lines were evaluated to obtain a metric for height.
  # By checking the hd$poly.stats[,c(“plot.id”,“n.splits”)] data frame,
  # you can see how many split-lines were evaluated in each polygon.

}

# Example usage
visualization(result_1, "hespdiv_bin1_plots.pdf")


###############################################
# 4. ASSIGNING OCCS POLYGONS BY RANK CHOICE   #
###############################################

# For each occurrence, assign the polygon of the user specified rank choice

assign_polygons_by_rank <- function(tb, result, rank_choice) {
  point_assignments_rank_filter <- sapply(1:nrow(tb), function(i) {
    # Takes each occurrence's coordinates
    point <- tb[i, c("Rotated.Lon", "Rotated.Lat")]  # Gets lng (x) and lat (y) for current occurrence

    # If you only want polygon assignments for a specific rank, you can filter here
    poly_ids <- names(result$polygons.xy)[result$poly.stats$rank == rank_choice]

    # Only check polygons of the specified rank
    for(poly_id in poly_ids) {
      polygon <- result$polygons.xy[[poly_id]]
      if (!is.null(polygon) && length(polygon$x) > 0 && length(polygon$y) > 0) {
        if (sp::point.in.polygon(point[1], point[2], polygon$x, polygon$y) > 0) {  # Ray-casting algorithm
          return(poly_id)  # Returns polygon ID if point is inside
        }
      }
    }
    return(NA)  # Returns NA if point doesn't have a polygon assignment of that rank
  })

  # Add column to original df w/ polygon assignments
  tb$chosen_rank_polygon_id <- point_assignments_rank_filter
  View(tb)
  return(tb)
}

# Example usage:
tb_1 <- assign_polygons_by_rank(tb_1, result_1, 3) # Assigns polygon IDs of rank 3 to tb_1


###############################################
# 5. ASSIGNING OCCS POLYGONS BY HIGHEST RANK  #
###############################################
# For each occurrence, assign the highest rank (= finest scale = smallest) rank polygon
assign_highest_rank_polygon <- function(tb, result) {
  point_assignments_highest_rank <- sapply(1:nrow(tb), function(i) {
    point <- tb[i, c("Rotated.Lon", "Rotated.Lat")]  # Gets lng (x) and lat (y) for current occurrence

    # Order polygons by descending rank
    poly_ids <- names(result$polygons.xy)[order(result$poly.stats$rank, decreasing=TRUE)]

    # Check polygons in order of rank
    for(poly_id in poly_ids) {
      polygon <- result$polygons.xy[[poly_id]]
      if (!is.null(polygon) && length(polygon$x) > 0 && length(polygon$y) > 0) {
        if (sp::point.in.polygon(point[1], point[2],
                                 polygon$x,
                                 polygon$y) > 0) {
          return(poly_id)  # Returns polygon ID if point is inside
        }
      }
    }
    return(NA)  # Returns NA if point isn't in any polygon
  })

  # Add column to original df w/ polygon assignments
  tb$highest_rank_polygon_id <- point_assignments_highest_rank
  View(tb)
  return(tb)
}

# Example usage:
tb_1 <- assign_highest_rank_polygon(tb_1, result_1)

#########################
# 6. CALLING FUNCTIONS  #
#########################

# Function for running hespdiv analysis, visualization, and polygon assignment
run_hespdiv <- function(tb, bin_num) {
  result <- hespdiv_analysis(tb)
  visualization(result, paste("hespdiv_bin", bin_num, "_plots.pdf", sep = ""))
  tb <- assign_polygons_by_rank(tb, result, 2) # Assigns polygon IDs of rank 2 to tb_1
  tb <- assign_highest_rank_polygon(tb, result)
  return(tb)
  View(tb)
}

tb_2 <- read.csv("../../BDNN_Arielli/data/hespdiv/hespdiv_bin2.csv") # Load the data
tb_2 <- run_hespdiv(tb_2, 2)

tb_3 <- read.csv("../../BDNN_Arielli/data/hespdiv/hespdiv_bin3.csv") # Load the data
tb_3 <- run_hespdiv(tb_3, 3)

tb_4 <- read.csv("../../BDNN_Arielli/data/hespdiv/hespdiv_bin4.csv") # Load the data
tb_4 <- run_hespdiv(tb_4, 4)

tb_5 <- read.csv("../../BDNN_Arielli/data/hespdiv/hespdiv_bin5.csv") # Load the data
tb_5 <- run_hespdiv(tb_5, 5)

