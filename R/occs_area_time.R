#' Summarise occurrences per area per time bin
#'
#' A function to produce a `dataframe` of the number of occurrences found in
#'    each geographic area in each time bin.
#' @param dat \code{dataframe}. The `dataframe` describing the occurrence data,
#'    including `Taxon`, `Area`, `Locality` and `SampledAge` columns. The
#'    supplied `dataframe` should not contain any `NA` values.
#' @param bins \code{numeric}. A numerical `vector` designating the boundaries
#'    of the time bins used in the analysis.
#'
#' @returns A `dataframe` of shape 'region x time bin' describing the number of
#'    occurrences in each category.
#'
#' @importFrom graphics hist
#' @examples
#' # Import internal dataset
#' data(carnivora)
#' # Generate vector describing time bin boundaries
#' bins <- c(66, 23, 2.6, 0)
#' # Generate sampled ages using the "median" method
#' carnivora <- ages(dat = carnivora)
#' # Compute occurrences per area and time
#' occs_area_time(dat = carnivora, bins = bins)
#' @export
occs_area_time <- function(dat = NULL, bins = NULL){

  # Handling errors
  if (is.data.frame(dat) == FALSE) {
    stop("`dat` should be a dataframe.")
  }

  if (any(is.na(dat))) {
    stop(paste("NA values detected in dataframe."))
  }

  if (is.vector(bins) == FALSE) {
    stop("`bins` should be a vector.")
  }

  if (is.numeric(bins) == FALSE) {
    stop("`bins` should contain numeric values (the ages of bin boundaries).")
  }

  if ("Taxon" %in% colnames(dat) == FALSE ||
      "Area" %in% colnames(dat) == FALSE ||
      "Locality" %in% colnames(dat) == FALSE ||
      "SampledAge" %in% colnames(dat) == FALSE) {
    stop("`dat` does not contain columns `Taxon`, `Area`, `Locality` and
         `SampledAge`")
  }

  if (!is.numeric(dat$SampledAge)) {
    stop("`SampledAge` column is not of numeric class")
  }

  # Split data by area
  area_tables <- split(dat, f = dat$Area)

  # List unique areas
  list_areas <-  sort(unique(dat$Area))

  # Create empty dataframe
  n_occurrences <- data.frame(matrix(0, length(list_areas), length(bins) - 1))
  bins <- sort(-abs(bins))

  for (i in seq_len(length(list_areas))){
    # Identify occurrences within the area
    indices_areas <- which(dat$Area == list_areas[i])
    # Count occurrences
    total_occs_for_area <- length(indices_areas)
    area_dat <- dat[indices_areas,]
    age_occs <- area_dat$SampledAge
    # Bin the occurrences through time
    h <- hist(x = -as.numeric(age_occs), breaks = bins, plot = FALSE)
    n_occurrences[i,] <- h$counts
  }

  # Annotate table
  colnames(n_occurrences) <- sprintf("t%d", seq(length(bins) - 1))
  row.names(n_occurrences) <- list_areas

  return(data.frame(n_occurrences))
}
