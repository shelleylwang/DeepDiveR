#' Edit values in the configuration file
#'
#' A function to modify parameters within a configuration object for DeepDive,
#'   created using `create_config()`. Default values are listed below, while
#'   the values currently held by each parameter can be checked using the
#'   notation `config$data$[module]$[parameter]`. Extensive checks ensure that
#'   it is possible for the specific parameter to hold the desired value.
#' @param config \code{character}. The name of the configuration object, created
#'   using `create_config()`, that will be edited.
#' @param module \code{character}. The name of the block of the configuration
#'   object to which the parameter belongs.
#' @param parameter \code{character}. The name of the parameter to edit in
#'   the configuration file.
#' @param value \code{}. The value you want to assign to the parameter. The type
#'   of value is dependent upon the parameter.

#' @returns A configuration object with the value of the desired parameter
#' changed.
#'
#' @details Add full list of default parameters.
#'
#' @import ConfigParser
#' @importFrom R6 is.R6
#' @examples
#' # Import internal dataset
#' data(carnivora)
#' # Generate vector describing time bin boundaries
#' bins <- c(66, 23, 2.6, 0)
#' # Create configuration object
#' config <- create_config(file_prefix = "carnivora",
#'                         data_file = "data/carnivora_deepdive_input.csv",
#'                         bins = bins,
#'                         n_areas = length(unique(carnivora$Area)))
#' # Edit configuration object
#' config <- edit_config(config = config,
#'                       module = "general",
#'                       parameter = "present_diversity",
#'                       value = 313)
#' # Save configuration object to file
#' config$write("carnivora_config.ini")
#'
#' @export
edit_config <- function(config = NULL, module = NULL,
                        parameter = NULL, value = NULL){

  # Handling errors
  if (is.R6(config) == FALSE) {
    stop("`config` should be a configuration file.")
  }

  if (module %in% names(config$data) == FALSE) {
    stop("`module` name does not exist in configuration file.")
  }

  if (parameter %in%
      names(config$data[[which(names(config$data) == module)]]) == FALSE) {
    stop("`parameter` name does not exist in given module in configuration
         file.")
  }


  variable <- paste(value, collapse = " ")
  config$data$module$parameter <- variable
  # pass lists, vectors etc and then turn into a string
}
