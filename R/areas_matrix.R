#' Make discrete geographic regions appear or disappear
#'
#' A function to modify the simulations module by providing a maximum and
#'   minimum age range for geographic regions being connected to the study
#'   system.
#' @param config \code{character}. The name of the configuration object, created
#'   using `create_config()`, that will be edited.
#' @param area_ages \code{dataframe}. If left NULL (default), all geographic
#'   regions are configured to be present throughout the simulations. Otherwise,
#'   a `dataframe` describing the `MinAge` and `MaxAge` for each geographic
#'   `Area`, with older ages in column one. The number of rows in the
#'   `dataframe` must match `n_areas` in the configuration file. The
#'    supplied `dataframe` should not contain any `NA` values.
#' @param presence \code{logical}. When TRUE (the default), regions become
#'   available to occupy within the time frames specified. When FALSE, regions
#'   will be removed from the simulations between the ages specified.
#'
#' @returns Adds an attribute for each area (named "area1", "area2"... etc) with
#'   minimum and maximum age of migration becoming possible.
#'
#' @importFrom R6 is.R6
#' @examples
#' # Generate dataframe describing area presence
#' area_ages <- rbind(c("Africa", 66, 66),
#'                    c("Asia", 66, 66),
#'                    c("Europe", 66, 66),
#'                    c("North America", 66, 66),
#'                    c("South America", 11.608, 7.3))
#'  area_ages <- as.data.frame(area_ages)
#'
#' areas_matrix(config = config, area_ages = area_ages)
#' @export
areas_matrix <- function(config = NULL, area_ages = NULL,
                         presence = TRUE){

  # Handling errors
  if (is.R6(config) == FALSE) {
    stop("`config` should be a configuration file.")
  }

  # Reorder areas alphabetically, then remove column
  area_ages <- area_ages[order(area_ages$Area),]

  # Ensure older ages are first column
  area_ages <- data.frame(MaxAge = area_ages$MaxAge, MinAge = area_ages$MinAge)

  # Retrieve n_areas from configuration file
  n_areas <- config$data$general$n_areas

  # Retrieve time bins from configuration file
  bins <- config$data$general$time_bins

  if(is.null(area_ages) & presence == TRUE){
    # for each region, specify two ages between which connection to others will
    # be established
    area_ages <- c()
    for(i in 1:n_areas){
      area_ages <- rbind(area_ages, c(max(bins), max(bins)))
    }
    edit_config(config, module = "simulations",
      parameter = paste0("area_", "start", i),
      value = c(area_ages[i, 1], area_ages[i, 2]))
  }

  if(is.null(area_ages) & presence == FALSE){
    # for each region, specify two ages between which connection to others will
    # be removed
    area_ages <- c()
    for(i in 1:n_areas){
      area_ages <- rbind(area_ages, c(min(bins), min(bins)))
    }
    edit_config(config, module = "simulations",
      parameter = paste0("area_", "end", i),
      value = c(area_ages[i, 1], area_ages[i, 2]))
  }

  if(!is.null(area_ages) & presence == TRUE){
    for(i in 1:n_areas){
      edit_config(config, module = "simulations",
        parameter = paste0("area_", "start", i),
        value = c(area_ages[i, 1], area_ages[i, 2]))
    }
  }

  if(!is.null(area_ages) & presence == FALSE){
    for(i in 1:n_areas){
      edit_config(config, module = "simulations",
                  parameter = paste0("area_", "end", i),
                  value = c(area_ages[i, 1], area_ages[i, 2]))
    }
  }
}
