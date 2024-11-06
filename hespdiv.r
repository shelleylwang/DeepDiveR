# Hespdiv Analysis for DeepDive
install.packages("devtools")
devtools::install_github("Liudas-Dau/hespdiv")
library(hespdiv)
library(sp)


# Reading CSV file for 1
tb_1 <- read.csv('../BDNN_Arielli/data/hespdiv/hespdiv_bin1.csv')

spec_1 <- tb_1$genus
coords_1 <- data.frame(
  x = tb_1$'Rotated.Lon',
  y = tb_1$'Rotated.Lat'
)

# Run hespdiv
result_1 <- hespdiv(
    # Required arguments
    data = spec_1,
    xy.dat = coords_1,
)
# Create your species data
species_data <- data.frame(
  species_name = c("Species1", "Species2", "Species3"),
  longitude = c(-100, -90, -80),
  latitude = c(40, 35, 30)
)


# Function to process species data with hespdiv and assign polygons
process_species_areas <- function(species_data) {
  # 1. Format data for hespdiv
  species <- species_data$species_name
  coords <- data.frame(x = species_data$longitude, 
                       y = species_data$latitude)
  
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
                                species_data$longitude, 
                                species_data$latitude,
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

# Example usage:
# Assuming species_data has columns: species_name, longitude, latitude

# species_data <- data.frame(
#     species_name = c("Species1", "Species2", "Species3"),
#     longitude = c(-100, -90, -80),
#     latitude = c(40, 35, 30)
# )

# Add polygon assignments
# result_data <- process_species_areas(species_data)

# View results
# head(result_data)

# View polygon statistics
# attr(result_data, "polygon_stats")

# Visualization function
plot_species_areas <- function(result_data, rank = 1) {
  col_name <- paste0("area_rank_", rank)
  
  if(!(col_name %in% colnames(result_data))) {
    stop(paste("No data for rank", rank))
  }
  
  # Plot base map
  plot(result_data$longitude, result_data$latitude,
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
    points(result_data$longitude[area_species],
           result_data$latitude[area_species],
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

# Example visualization:
# plot_species_areas(result_data, rank = 1)



### RUNNING THE PROGRAM: ###
# Process data
result_data <- process_species_areas(species_data)

# View results for first rank
print(result_data$area_rank_1)

# Plot results for second rank
plot_species_areas(result_data, rank = 2)

# View polygon statistics
print(attr(result_data, "polygon_stats"))

