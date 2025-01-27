# Install and load required packages
install.packages("devtools")
devtools::install_github("Liudas-Dau/hespdiv")
install.packages("sp")
library(sp)
library(hespdiv)
setwd('C:/Users/SimoesLabAdmin/Documents/DeepDiveR')

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
# Take in path to hespdiv_bin#.csv and the bin number
# Return the hespdiv analysis result dataframe (result_#)
# and the original dataset as a dataframe (df_#)

hespdiv_analysis <- function(path, bin_num) {
  # Load data
  tb_name <- paste0("tb_", bin_num) # Rename the dataframe
  tb <- read.csv(path) # Load the data
  assign(tb_name, tb, envir = .GlobalEnv) # Assign the dataframe to the global environment

  # Format data for hespdiv
  species <- tb$Taxon  # or species column
  coords <- data.frame(
    x = tb$'Rotated.Lon',
    y = tb$'Rotated.Lat'
  )

  # Run basic hespdiv analysis
  result_name <- paste0("result_", bin_num)
  result <- hespdiv(
    data = species,
    xy.dat = coords,
    study.pol = study_area_polygon,
    n.split.pts = 15  # Make this much higher in the future
    # Default 15 will generate 120 split-lines for each subdivision attempt.
    # Increasing this value improves the fit of straight split-lines to the data
    # but also increases computation time
  )
  assign(result_name, result, envir = .GlobalEnv)

  # Print initial results
  print("Basic Statistics about Split Lines:")
  print(result$poly.stats)
  print(result$split.stats)
  print(result$split.lines)
  print(result$polygons.xy)

  # Return the result dataframe
  return(result)
  return(tb)
}

# How to call it:
hespdiv_analysis("../BDNN_Arielli/data/hespdiv/hespdiv_bin1.csv", 1) # Leads to result_1, tb_1

################################
# 3. VISUALIZATION FUNCTIONS   #
################################

visualization <- function(result_num) {

  # A. Basic visualization with plot_hespdiv
  print("Creating basic visualization...")
  plot_hespdiv(result_num, n.loc = TRUE)  # Show number of occurrences
  plot_hespdiv(result_num, type = "w")    # Use line width for performance

  # B. 3D visualization with blok3d
  print("Creating 3D visualization...")
  blok3d(result_num, height = "rank")
  # Taller polygons = higher MH's across all tested split-lines (more spatial
  # homogeneity in taxa composition)

  # Interactively remove specific polygons if needed
  # polypop(result, height = "mean") # Set the "height" to the same value as was set in the blok3D function
  # not all HespDiv polygons are displayed in the output of blok3d(hd)
  # if not enough split-lines were evaluated to obtain a metric for height.
  # By checking the hd$poly.stats[,c(“plot.id”,“n.splits”)] data frame,
  # you can see how many split-lines were evaluated in each polygon.

  # C. Polygon scheme visualization
  print("Creating polygon scheme...")
  poly_scheme(result_num, seed = 4)

}

# Example usage
visualization(result_1)


###############################################
# 4. ASSIGNING OCCS POLYGONS BY RANK CHOICE   #
###############################################
# For each occurrence, assign the polygon of the user specified rank choice
assign_polygons_by_rank <- function(tb_num, result_num, rank_choice) {

  point_assignments_rank_filter <- sapply(1:nrow(tb_num), function(i) {

    # Takes each occurrence's coordinates
    point <- tb_num[i, c("Rotated.Lon", "Rotated.Lat")]  # Gets lng (x) and lat (y) for current occurrence

    # If you only want polygon assignments for a specific rank, you can filter here
    poly_ids <- names(result_num$polygons.xy)[result_num$poly.stats$rank == rank_choice]

    # Only check polygons of the specified rank
    for(poly_id in poly_ids) {
      polygon <- result_num$polygons.xy[[poly_id]]
      if (!is.null(polygon) && length(polygon$x) > 0 && length(polygon$y) > 0) {
        if (sp::point.in.polygon(point[1], point[2], polygon$x, polygon$y) > 0) {  # Ray-casting algorithm
          return(poly_id)  # Returns polygon ID if point is inside
        }
      }
    }
    return(NA)  # Returns NA if point doesn't have a polygon assignment of that rank
  })

  # Add column to original df w/ polygon assignments
  tb_num$chosen_rank_polygon_id <- point_assignments_rank_filter

  View(tb_num)
  return(tb_num)
}

# Example usage:
assign_polygons_by_rank(tb_1, result_1, 2) # Assigns polygon IDs of rank 2 to tb_1


###############################################
# 5. ASSIGNING OCCS POLYGONS BY HIGHEST RANK  #
###############################################
# For each occurrence, assign the highest rank (= finest scale = smallest) rank polygon

assign_highest_rank_polygon <- function(tb_num, result_num) {

  point_assignments_highest_rank <- sapply(1:nrow(tb_num), function(i) {

    point <- tb_num[i, c("Rotated.Lon", "Rotated.Lat")]  # Gets lng (x) and lat (y) for current occurrence

    # Order polygons by descending rank
    poly_ids <- names(result_num$polygons.xy)[order(result_num$poly.stats$rank, decreasing=TRUE)]

    # Check polygons in order of rank
    for(poly_id in poly_ids) {
      if(point.in.polygon(point$x, point$y,
                          result_num$polygons.xy[[poly_id]]$x,
                          result_num$polygons.xy[[poly_id]]$y)) {
        return(poly_id) # Returns polygon ID if point is inside
      }
    }
    return(NA)
  })

  # Add column to original df w/ polygon assignments
  tb1$highest_rank_polygon_id <- point_assignments_highest_rank

  View(tb_num)
  return(tb_num)
}

# View tb1
head(tb1)
# View full tb1
View(tb1)

# Example usage
assign_highest_rank_polygon(tb_1, result_1

#########################
# 6. CALLING FUNCTIONS  #
#########################
hespdiv_analysis("../BDNN_Arielli/data/hespdiv/hespdiv_bin1.csv", 2)
visualization(result_2)
assign_polygons_by_rank(tb_2, result_2, 2) # Assigns polygon IDs of rank 2 to tb_1


