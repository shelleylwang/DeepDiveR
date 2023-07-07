#' Create DeepDive config file
#'
#' 'create_config()' generates a config file with settings needed to launch
#' DeepDive scripts in python. Setting identifiers are tab sepparated, values
#' space separated.
#' @param 
#' @param 
#' @returns Saves a txt config file with settings to launch DeepDive.
#' @examples
#' 
#' @export
create_config <- function(){
  
  # simulations 
  n_CPUS <- 50  # number of CPUs used for simulations
  n_training_simulations <- 2000  # simulations per CPU (total should be ~10,000)
  n_test_simulations <- 20  # simulations per CPU (the total should be eg 100 or 1000)
  n_areas <- 5  # number of discrete regions
  time_bins <- sort(c(0, 10, 20))  # CHANGE SCRIPTS SO ALWAYS JUST CALLED TIME_BINS
  freq_low_res <- 0.6323944  # frequency of low res occurrences
  training_seed <- 123
  test_seed <- 432

  s_species <- 1  # number of starting species
  rangeSP <- c(100, 5000)  # min/max size data set
  minEX_SP <- 0  # minimum number of extinct lineages allowed
  minEXTANT_SP <- 3  # min number of living species
  maxEXTANT_SP <- 30
  # root_r <- root_age  # root age
  rangeL <- c(0.02, 0.5)  # range of birth rates - range based on Cantalapiedra et al 2022
  rangeM <- c(0.02, 0.5)  # range of death rates
  log_uniform_rates <- F
  p_mass_extinction <- 0.01  # probability of mass extinction per my
  poiL <- 4  # expected number of birth rate shifts
  poiM <- 4  # expected number of death rate shifts
  # seed <- rseed   # if > 0 fixes the random seed to make simulations reproducible
  
  
  # fossil simulator settings
  eta <- c(1, 1.75)  # area-sp stochasticity
  p_gap <- c(0.01, 0.95)  # probability of 0 preservation in a time bin
  dispersal_rate <- NULL
  max_dist <- 1
  disp_rate_mean <- c(0, 1)
  disp_rate_variance <- 1
  area_mean <- 20  # G(a,b) distributed preservation rates across areas
  area_variance <- 5
  size_concentration_parameter <- c(0.1, 3)  # single value or array of length n_areas
  link_area_size_carrying_capacity <- c(1, 10) # positive, larger numbers = stronger link between area size and carrying capacity
  p_origination_a_slope_mean <- 2  # mean slope of probability of origination area mean
  p_origination_a_slope_sd <- 0.5  # std of the slopes
  sp_mean <- c(0.1, 0.5)  # G(a,b) distributed preservation rates across species
  sp_variance <- 2
  slope <- c(-0.01, 0)  # change in log-sampling rate through time (log-linear)
  intercept <- c(0.1, 0.5)  # initial sampling rate
  sd_through_time <- c(0.001, 0.01)  # st dev in log-sampling rate through time
  sd_through_time_skyline <- 1
  mean_n_epochs_skyline <- 4
  fraction_skyline_sampling <- 0.5
  maximum_localities_per_bin <- 200
  
  # settings for areas appearing and disappearing 
  
  # settings to adjust area bias
  
  # settings to adjust time bias
  
  # settings to adjust taxonomic bias
  
  # Settings for training models
  
  
  # For the prediction scripts - remove any redundancies from below
  n_predictions <- 10  # number of predictions per input file
  replicates <- 100  # numer of age randomisation replicates used in data_pipeline.R
  CI <- 0.95  # confidence intervals
  plotting_range <- c(66, 2)  # age range used for plotting (truncates the predictions)
  max_age <- -66
  min_age <- -0
  plot_mean_prediction <- F  # if true add line with mean prediction (one line per replicate)
  plot_median_prediction <- T  # if true add line with median prediction across all models
  plot_all_predictions <- F
  alpha <- 0.2
  prediction_color <- "b"
  plot_shaded_area <- T
  scaling <- "1-mean"
  
  config <- list(n_CPUS, n_training_simulations, n_test_simulations, n_areas, 
                 time_bins, freq_low_res, training_seed, test_seed, s_species, 
                 rangeSP, minEX_SP, minEXTANT_SP, maxEXTANT_SP, rangeL, rangeM, 
                 log_uniform_rates, p_mass_extinction, poiL, poiM, eta, p_gap, 
                 dispersal_rate, max_dist, disp_rate_mean, disp_rate_variance, 
                 area_mean, area_variance, size_concentration_parameter, 
                 link_area_size_carrying_capacity, p_origination_a_slope_mean, 
                 p_origination_a_slope_sd, sp_mean, sp_variance, slope, 
                 intercept, sd_through_time, sd_through_time_skyline, 
                 mean_n_epochs_skyline, fraction_skyline_sampling, 
                 maximum_localities_per_bin, n_predictions, replicates, CI, 
                 plotting_range, max_age, min_age, plot_mean_prediction, 
                 plot_median_prediction, plot_all_predictions, alpha, 
                 prediction_color, plot_shaded_area, scaling)
  names(config) <- c("n_CPUS", "n_training_simulations", "n_test_simulations", 
                     "n_areas", "time_bins", "freq_low_res", "training_seed", 
                     "test_seed", "s_species", "rangeSP", "minEX_SP", 
                     "minEXTANT_SP", "maxEXTANT_SP", "rangeL", "rangeM", 
                     "log_uniform_rates", "p_mass_extinction", "poiL", "poiM", 
                     "eta", "p_gap", "dispersal_rate", "max_dist", 
                     "disp_rate_mean", "disp_rate_variance", "area_mean", 
                     "area_variance", "size_concentration_parameter", 
                     "link_area_size_carrying_capacity", 
                     "p_origination_a_slope_mean", "p_origination_a_slope_sd", 
                     "sp_mean", "sp_variance", "slope", "intercept", 
                     "sd_through_time", "sd_through_time_skyline", 
                     "mean_n_epochs_skyline", "fraction_skyline_sampling", 
                     "maximum_localities_per_bin", "n_predictions", 
                     "replicates", "CI", "plotting_range", "max_age", "min_age", 
                     "plot_mean_prediction", "plot_median_prediction", 
                     "plot_all_predictions", "alpha", "prediction_color", 
                     "plot_shaded_area", "scaling")
  
  # write to a txt file
  capture.output(config, file = "config.txt")
  return(config)
}
