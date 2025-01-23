# Hespdiv Analysis for DeepDive
install.packages("devtools")
devtools::install_github("Liudas-Dau/hespdiv")
library(hespdiv)
library(sp)

devtools::install_github("Liudas-Dau/hespdiv_data")
library(HDData)
mio_mams

mio_mams <- data.frame(mio_mams)

species <- mio_mams$accepted_name # Taxa names
species
#find unique number of species
unique(species)

sp_coords <- data.frame(x = mio_mams$lng, y = mio_mams$lat) # Coordinates of observations

str(us) # Tutorial provides a polygon representing the study 
# area as a ref point when interpreting results (shows up as background in plot)

# The 'hd' object exists pre-calculated in HDData library, so don't need to run this code:
hd <- hespdiv(data = species, xy.dat = sp_coords, study.pol = us)

?hespdiv::hespdiv # Show options for the analysis! Lots of params

hd$poly.stats

plot_hespdiv(hd)
block3d(hd, height = "rank")
block3d(hd, height = "sd")
block3d(hd, height = "mean")
polypop(hd, height = "mean")

?hespdiv::assign_polygons

poly_scheme(result)

sim_m <- cross_comp(hd) # Obtain cross-comparison matrix
cl <- hclust(as.dist(1-sim_m)) # Convert similarity to distance and perfomring cluster analysis
plot(cl)


install.packages("igprah")
library(igraph)
gr <- graph.adjacency(
  as.matrix(as.dist(sim_m)), # Zero the diagonal of the similarity matrix for the graph
  mode = "undirected",
  weighted = TRUE,
  diag = FALSE
)
plot(gr)

# The 'nl' object exists pre-calculated in HDData library, so don't need to run this code:
set.seed(1) # The seed is used to obtain the same result of an experiment with random properties.
nl <- nulltest(hd, n = 1000) 
plot(n1)



# Reading CSV file for 1
tb_1 <- read.csv('../BDNN_Arielli/data/hespdiv/hespdiv_bin1.csv')

## Old code
#spec_1 <- tb_1$genus
#coords_1 <- data.frame(
 # x = tb_1$'Rotated.Lon',
#  y = tb_1$'Rotated.Lat'
#)

# Run hespdiv
#result_1 <- hespdiv(
    # Required arguments
 #   data = spec_1,
  #  xy.dat = coords_1,
#)

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



# Visualization function
plot_species_areas <- function(result_data, rank = 1) {
  col_name <- paste0("area_rank_", rank)
  
  if(!(col_name %in% colnames(result_data))) {
    stop(paste("No data for rank", rank))
  }
  
  # Plot base map
  plot(result_data$'Rotated.Lon', result_data$'Rotated.Lat',
       type = "n", 
       xlab = "Longitude", 
       ylab = "Latitude",
       main = paste("Species Distribution by Area, Rank", rank))
  
  # Add points colored by area
  areas <- unique(result_data[[col_name]])
  areas <- areas[!is.na(areas)]
  colors <- rainbow(length(areas))
  
  for(i in seq_along(areas)) {
    area_species <- result_data[[col_name]] == areas[i]
    points(result_data$'Rotated.Lon'[area_species],
           result_data$'Rotated.Lat'[area_species],
           col = colors[i],
           pch = 19)
  }
  
  # Add legend
  legend("topright", 
         legend = areas,
         col = colors,
         pch = 19,
         title = "Areas")
}





### RUNNING THE PROGRAM: ###
# Process data
result_tb1 <- process_species_areas(tb_1)

# View results
head(result_tb1)

# Visualization for the first rank:
plot_species_areas(result_tb1, rank = 1)

# View polygon statistics
print(attr(result_tb1, "polygon_stats"))

