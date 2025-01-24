# Install and load required packages
install.packages("devtools")
devtools::install_github("Liudas-Dau/hespdiv")
library(hespdiv)
install.packages("sp")
library(sp)
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
  tb_name <- paste0("df_", bin_num)
  tb <- read.csv(path)
  assign(tb_name, tb, envir = .GlobalEnv)

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

################################
# 3. VISUALIZATION FUNCTIONS   #
################################

# A. Basic visualization with plot_hespdiv
print("Creating basic visualization...")
plot_hespdiv(result, n.loc = TRUE)  # Show number of occurrences
plot_hespdiv(result, type = "w")    # Use line width for performance

# B. 3D visualization with blok3d
print("Creating 3D visualization...")
blok3d(result, height = "rank")
# Taller polygons = higher MH's across all tested split-lines (more spatial
# homogeneity in taxa composition)

# Interactively remove specific polygons if needed
polypop(result, height = "mean") # Set the "height" to the same value as was set in the blok3D function
# not all HespDiv polygons are displayed in the output of blok3d(hd)
# if not enough split-lines were evaluated to obtain a metric for height.
# By checking the hd$poly.stats[,c(“plot.id”,“n.splits”)] data frame,
# you can see how many split-lines were evaluated in each polygon.

# C. Polygon scheme visualization
print("Creating polygon scheme...")
poly_scheme(result, seed = 4)


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
      if(point.in.polygon(point$x, point$y, # Ray-casting algorithm
                          result_num$polygons.xy[[poly_id]]$x,
                          result_num$polygons.xy[[poly_id]]$y)) {
        return(poly_id)  # Returns polygon ID if point is inside
      }
    }
    return(NA)  # Returns NA if point doesn't have a polygon assignment of that rank
  })

  # Add column to original df w/ polygon assignments
  tb_num$chosen_rank_polygon_id <- point_assignments_rank_filter
  return(tb_num)
}

###############################################
# 5. ASSIGNING OCCS POLYGONS BY HIGHEST RANK  #
###############################################
# For each occurrence, assign the highest rank (= finest scale = smallest) rank polygon

assign_highest_rank_polygon <- function(tb_num, result_num, rank_choice) {
  point_assignments_highest_rank <- sapply(1:nrow(tb_num), function(i) {
    point <- tb_num[i, c("Rotated.Lon", "Rotated.Lat")]  # Gets lng (x) and lat (y) for current occurrence
    # Order polygons by descending rank
    poly_ids <- names(result_num$polygons.xy)[order(result_num$poly.stats$rank, decreasing=TRUE)]
    for(poly_id in poly_ids) {
      if(point.in.polygon(point$x, point$y,
                          result_num$polygons.xy[[poly_id]]$x,
                          result_num$polygons.xy[[poly_id]]$y)) {
        return(poly_id)
      }
    }
    return(NA)
  })

  # Add column to original df w/ polygon assignments
  tb1$highest_rank_polygon_id <- point_assignments_highest_rank
}

# View tb1
head(tb1)
# View full tb1
View(tb1)

