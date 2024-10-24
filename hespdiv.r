# Hespdiv Analysis for DeepDive
install.packages("devtools")
devtools::install_github("Liudas-Dau/hespdiv")
library(hespdiv)

# Reading CSV file for 290.1
tb_290.1 <- read.csv('../BDNN_Arielli/data/hespdiv/hespdiv_290.csv')

spec_290.1 <- tb_290.1$genus
coords_290.1 <- data.frame(
  x = tb_290.1$'Rotated.Lon',
  y = tb_290.1$'Rotated.Lat'
)

# Run hespdiv
result_290.1 <- hespdiv(
    # Required arguments
    data = spec_290.1,
    xy.dat = coords_290.1,
)


### ANALYZE RESULTS ###

# Plotting
plot_hespdiv(result_290.1)

# Cross-compare HespDiv clusters
cross_comp_matrix_290.1 <- cross_comp(result_290.1)
print("Cross-comparison matrix of clusters:")
print(cross_comp_matrix)

# Test significance of splits
null_test_results_290.1 <- nulltest(
  result_290.1,
  n.perm = 100,        # Number of permutations
  parallel = TRUE,     # Use parallel processing
  cores = 2            # Number of cores
)
plot(null_test_results_290.1)

# Sensitivity analysis
sens_analysis_290.1 <- sensitivity(
  result_290.1,
  n.runs = 100,        # Number of alternative runs
  sample.prop = 0.8,   # Proportion of data to sample
  parallel = TRUE,     # Use parallel processing
  cores = 2            # Number of cores
) 
plot(sens_analysis)

# Basic Statistics
print("Split Statistics:")
print(result_290.1$split.stats)

print("Polygon Statistics:")
print(result_290.1$poly.stats)

# Plot Split-Lines of all Alternative Subdivisions from a hespdiv
# sensitivity analysis
# A, all ranks. B, first rank polygons. 
# C, second rank polygons. D, third rank polygons. Plots of 
# higher ranks are provided in Figure S2.
plot_hsa(result_290.1)
