##########################################
# HESPDIV ANALYSIS #
##########################################

# Install and load required packages
install.packages("devtools")
devtools::install_github("Liudas-Dau/hespdiv")
library(hespdiv)
library(sp)
setwd('C:/Users/SimoesLabAdmin/Documents/DeepDiveR')

#######################
# 1. DATA PREPARATION #
#######################

# Read your data
tb1 <- read.csv('../BDNN_Arielli/data/hespdiv/hespdiv_bin1.csv') 

# Format data for hespdiv
species <- tb1$genus  # or species column
coords <- data.frame(
  x = tb1$'Rotated.Lon',
  y = tb1$'Rotated.Lat'
)

################################
# 2. BASIC HESPDIV ANALYSIS    #
################################

# Run basic hespdiv analysis
result <- hespdiv(
  data = species,
  xy.dat = coords,
  n.split.pts = 15  # Make this much higher in the future
  # Default 15 will generate 120 split-lines for each subdivision attempt. 
  # Increasing this value improves the fit of straight split-lines to the data 
  # but also increases computation time
)

?hespdiv::hespdiv # Show options for the analysis! Lots of params


# Output: result = information about the split-lines
# performance = MH index, smaller is better
# mean and sd = avg of all the split lines tested in a single subdivision
# z.score = "outstandingness" of the best split-line = (performance-mean)/sd.
  # larger magnitude is better

# Print initial results
print("Basic Statistics about Split Lines:")
print(result$poly.stats)
result$split.stats
result$split.lines

################################
# 3. VISUALIZATION FUNCTIONS   #
################################

# A. Basic visualization with plot_hespdiv
print("Creating basic visualization...")
plot_hespdiv(result, n.loc = TRUE)  # Show number of occurrences
plot_hespdiv(result, type = "w")    # Use line width for performance

# B. 3D visualization with blok3d
print("Creating 3D visualization...")
blok3d(result)
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

####################################
# 4. CROSS-COMPARISON ANALYSIS     #
####################################

print("Performing cross-comparison analysis...")
# Generate cross-comparison matrix
sim_matrix <- cross_comp(result)

# Visualize as dendrogram
cluster_analysis <- hclust(as.dist(1-sim_matrix))
plot(cluster_analysis, main = "Cluster Analysis of Areas")

# Visualize as network
library(igraph)
graph <- graph.adjacency(
  as.matrix(as.dist(sim_matrix)),
  mode = "undirected",
  weighted = TRUE,
  diag = FALSE
)
plot(graph, main = "Network of Area Relationships")

####################################
# 5. NULL HYPOTHESIS TESTING       #
####################################

print("Performing null hypothesis testing...")
set.seed(123)  # For reproducibility
null_results <- nulltest(result, n = 1000)
plot(null_results)
print(null_results)  # Print statistics

####################################
# 6. SENSITIVITY ANALYSIS          #
####################################

print("Performing sensitivity analysis...")

# A. Basic sensitivity analysis
sensitivity_results <- hsa(
  obj = result,
  n.runs = 100,
  n.split.pts = 8:30,
  same.n.split = FALSE,
  c.splits = FALSE,
  c.X.knots = 3:8,
  c.Y.knots = 5:15,
  c.fast.optim = TRUE,
  use.chull = FALSE
)

# B. Detailed sensitivity analysis
detailed_sensitivity <- hsa_detailed(
  obj = result,
  n.split.pts = c(15, 30, 45),
  same.n.split = c(TRUE, FALSE)
)

# C. Change base subdivision if needed
new_base <- change_base(sensitivity_results, id = 1)

# D. Quantitative analysis of stability
stability_quant <- hsa_quant(sensitivity_results)

# E. Visualization of sensitivity results
# Plot all alternatives
plot_hsa(sensitivity_results, type = 1)

# Plot by rank
plot_hsa(sensitivity_results, 
         type = 4, 
         alpha = 0.3,
         basal.col = 1,
         split.col.seed = 1)

# Plot stability quantiles
plot_hsa_q(stability_quant, hist = TRUE)

####################################
# 7. COMPREHENSIVE SPECIES MAPPING #
####################################

# Function to process species data with hespdiv and assign polygons
process_species_areas <- function(species_data) {
  # 1. Format data for hespdiv
  species <- species_data$genus
  coords <- data.frame(x = species_data$'Rotated.Lon', 
                       y = species_data$'Rotated.Lat')
  
  # 2. Run hespdiv
  result <- hespdiv(data = species, 
                    xy.dat = coords)
  
  # 3. Function to find all polygons containing a point
  find_all_polygons <- function(x, y, result) {
    point <- c(x, y)
    polygons <- result$polygons.xy
    ranks <- result$poly.stats$rank
    
    # Check each polygon
    contains_point <- logical(length(polygons))
    
    for(i in seq_along(polygons)) {
      poly <- polygons[[i]]
      # point.in.polygon returns 0 if point is outside, 1 if inside, 2 if on boundary
      inside <- point.in.polygon(point[1], point[2], 
                                 poly$x, poly$y) > 0
      contains_point[i] <- inside
    }
    
    return(contains_point)
  }
  
  # 4. Apply to all species and create columns for each rank
  # First get all polygon assignments
  polygon_assignments <- mapply(find_all_polygons, 
                                species_data$'Rotated.Lon', 
                                species_data$'Rotated.Lat',
                                MoreArgs = list(result = result))
  
  # Transpose to get matrix where rows are species and columns are polygons
  polygon_assignments <- t(polygon_assignments)
  
  # Create new columns for each rank
  max_rank <- max(result$poly.stats$rank)
  
  # Initialize new columns in species_data for each rank
  for(rank in 1:max_rank) {
    col_name <- paste0("area_rank_", rank)
    species_data[[col_name]] <- NA
  }
  
  # For each species (row)
  for(i in 1:nrow(species_data)) {
    # Get polygons this species belongs to
    species_polygons <- which(polygon_assignments[i,])
    
    if(length(species_polygons) > 0) {
      # Get ranks for these polygons
      polygon_ranks <- result$poly.stats$rank[species_polygons]
      
      # For each rank present
      for(rank in unique(polygon_ranks)) {
        # Get polygon ID for this rank
        rank_polygons <- species_polygons[polygon_ranks == rank]
        
        if(length(rank_polygons) > 0) {
          col_name <- paste0("area_rank_", rank)
          species_data[i, col_name] <- paste(rank_polygons, collapse = ";")
        }
      }
    }
  }
  
  # Add polygon statistics to help interpret the results
  attr(species_data, "polygon_stats") <- result$poly.stats
  
  return(species_data)
}

# Process all data
final_results <- process_species_areas(data)

####################################
# 8. SAVE RESULTS                  #
####################################

# Save processed data
write.csv(final_results, "hespdiv_results.csv", row.names = FALSE)

# Save visualizations
pdf("hespdiv_visualizations.pdf")
plot_hespdiv(result)
plot_hsa(sensitivity_results, type = 1)
plot_hsa_q(stability_quant, hist = TRUE)
dev.off()

# Save statistics
capture.output(
  list(
    "Polygon Statistics" = result$poly.stats,
    "Null Test Results" = null_results,
    "Stability Analysis" = stability_quant
  ),
  file = "hespdiv_statistics.txt"
)

print("Analysis complete! Check output files for results.")

####################################
# 9. OPTIONAL: ERROR HANDLING      #
####################################

# Function to check if results are valid
check_results <- function(result) {
  if(is.null(result$poly.stats)) {
    warning("No polygon statistics generated")
    return(FALSE)
  }
  if(nrow(result$poly.stats) < 2) {
    warning("insufficient subdivisions created")
    return(FALSE)
  }
  return(TRUE)
}

# Check results
valid_results <- check_results(result)
if(!valid_results) {
  warning("Analysis may need to be rerun with different parameters")
}

