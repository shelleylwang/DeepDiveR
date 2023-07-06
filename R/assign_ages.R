#' Ages function
#'
#' 'ages()' assigns sampled ages to each occurrence within it's age range using
#' either the median of the minimum and maximum age estimate, a random age drawn
#' from a uniform distribution, or a random age drawn from uniform distribution
#' that is kept the same for occurrences which are sampled from the same
#' locality
#' @param dat Occurrence data table
#' @param method Age assignment method ("median", "random" or
#' "random_by_loc")
#' @returns Occurrence data table with additional SampledAge column
#' @examples
#' ages(dat, method="median")
#' @export
ages <- function(dat, method){
  if (method == "median") {
    dat <- mutate(rowwise(dat), SampledAge = median(c(MinAge, MaxAge)))
  }
  if (method == "random") {
    SampledAge <- runif(length(dat$Complete_name), min = dat$MinAge, max = dat$MaxAge)
    dat <- cbind(dat, SampledAge)
  }
  if (method == "random_by_loc") {
    dat$SampledAge <- dat$Locality
    for (i in 1:length(unique(dat$Locality))) {
      locate <- which(dat$Locality == i)
      loc <- dat[locate, ]
      Age <- runif(n = 1, min = loc$MinAge, max = loc$MaxAge)
      dat$SampledAge[dat$SampledAge == i] <- Age
    }
  }
  return(dat)
}
