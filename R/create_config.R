#' Create DeepDive config object and file
#'
#' 'create_config()' generates config object with settings needed to launch
#' DeepDive scripts in python. 
#' @param 
#' @param 
#' @returns Creates config with settings to launch DeepDive.
#' @examples
#' 
#' @export
# create_config <- function(){
  library(ConfigParser)
  config <- ConfigParser$new()
  sims <- c()
  # SPECIFY CONFIG PATH IN PYTHON
  
  if(n_training_simulations){
    # simulations 
    sims$n_CPUS <- 50  # number of CPUs used for simulations
    sims$n_training_simulations <- 2000  # simulations per CPU (total should be ~10,000)
    sims$n_test_simulations <- 20  # simulations per CPU (the total should be eg 100 or 1000)
    sims$n_areas <- 5  # number of discrete regions
    sims$time_bins <- sort(paste(0, 10, 20, collapse=""))  # CHANGE SCRIPTS SO ALWAYS JUST CALLED TIME_BINS
    sims$freq_low_res <- 0.6323944  # frequency of low res occurrences
    sims$training_seed <- 123
    sims$test_seed <- 432

    sims$s_species <- 1  # number of starting species
    sims$rangeSP <- paste(100, 5000, collapse="")  # min/max size data set
    sims$minEX_SP <- 0  # minimum number of extinct lineages allowed
    sims$minEXTANT_SP <- paste(3, 5, collapse="")  # min number of living species  # MAYBE MAKE THESE VECTORS INSTEAD?
    sims$maxEXTANT_SP <- paste(30, 50, collapse="")
    sims$rangeL <- paste(0.02, 0.5, collapse="")  # range of birth rates - range based on Cantalapiedra et al 2022
    sims$rangeM <- paste(0.02, 0.5, collapse="")  # range of death rates
    sims$log_uniform_rates <- 0  # boolean true or false
    sims$p_mass_extinction <- 0.01  # probability of mass extinction per my
    sims$poiL <- 4  # expected number of birth rate shifts
    sims$poiM <- 4  # expected number of death rate shifts
    
    
    # fossil simulator settings
    sims$eta <- paste(1, 1.75, collapse="")  # area-sp stochasticity
    sims$p_gap <- paste(0.01, 0.95, collapse="")  # probability of 0 preservation in a time bin
    sims$dispersal_rate <- "None"
    sims$max_dist <- 1
    sims$disp_rate_mean <- paste(0, 1, collapse="")
    sims$disp_rate_variance <- 1
    sims$area_mean <- 20  # G(a,b) distributed preservation rates across areas
    sims$area_variance <- 5
    sims$size_concentration_parameter <- paste(0.1, 3, collapse="")  # single value or array of length n_areas
    sims$link_area_size_carrying_capacity <- paste(1, 10, collapse="") # positive, larger numbers = stronger link between area size and carrying capacity
    sims$p_origination_a_slope_mean <- 2  # mean slope of probability of origination area mean
    sims$p_origination_a_slope_sd <- 0.5  # std of the slopes
    sims$sp_mean <- paste(0.1, 0.5, collapse="")  # G(a,b) distributed preservation rates across species
    sims$sp_variance <- 2
    sims$slope <- paste(-0.01, 0, collapse="")  # change in log-sampling rate through time (log-linear)
    sims$intercept <- paste(0.1, 0.5, collapse="")  # initial sampling rate
    sims$sd_through_time <- paste(0.001, 0.01, collapse="")  # st dev in log-sampling rate through time
    sims$sd_through_time_skyline <- 1
    sims$mean_n_epochs_skyline <- 4
    sims$fraction_skyline_sampling <- 0.5
    sims$maximum_localities_per_bin <- 200
    
    config$data$simulations <- sims
    
    # settings for areas appearing and disappearing 
  
    # settings to adjust area bias
  
    # settings to adjust time bias
  
    # settings to adjust taxonomic bias
    }


  if(model_training){
    # Settings for training models
    mt <- c()
    mt$simulations_file <- "simulations"
    mt$lstm_layers <- paste(64, 32, collapse="")
    mt$dense_layer <- paste(64, 32, collapse="")
    mt$dropout <- 0.5
    mt$max_epochs <- 1000
    mt$patience <- 10
    mt$batch_size <- 100
    mt$validation_split <- 0.2
    config$data$model_training <- mt
  }
  
  # For the prediction scripts - remove any redundancies from below
  # ADD SPECIFYING THE DATA, SIMULATION OUTPUT FILES ARE.
  # CLARIFY USE PYTHON FOR FULL FELEXIBILITY.
  test <- c()
  test$sim_wd <- "simulations"
  test$model_wd <- "trained_models" # in python path.join to get to folders
  test$feat_files <- paste('sim_features20220617.npy', 'sim_features_age_u20220617.npy', collapse="")
  test$lab_file <- 'sim_labels20220617.npy'
  test$feat_files_test <- paste('test_sim_features20220617.npy', 'test_sim_features_age_u20220617.npy', collapse="")
  test$lab_file_test <- 'test_sim_labels20220617.npy'
  test$output_names <- paste('rnn20220617', 'rnn20220617_age_u', collapse="")
  test$n_predictions <- 100
  test$sqs_6 <- "" # file path
  test$j <- 2  # index of the best performing model
  config$data$test_predictions <- test
  
  
  # For empirical predictions
  e <- c()
  e$empirical_input_file <- ".\R\test_empirical_data"  
  e$n_predictions <- 10  # number of predictions per input file
  e$replicates <- 100  # number of age randomisation replicates used in data_pipeline.R
  e$CI <- 0.95  # confidence intervals
  e$plotting_range <- paste(66, 2, collapse="")  # age range used for plotting (truncates the predictions)
  e$max_age <- -66
  e$min_age <- -0
  e$plot_mean_prediction <- 0  # if true add line with mean prediction (one line per replicate)
  e$plot_median_prediction <- 1  # if true add line with median prediction across all models
  e$plot_all_predictions <- 0  # logical
  e$alpha <- 0.2
  e$prediction_color <- "b"
  e$plot_shaded_area <- 1  # logical
  e$scaling <- "1-mean"
  e$models <- "None"  # or provide a list of file names which could have a single item ##NULL DOESNT READ CORRECLT IN PYTHON
  config$data$empirical_predictions <- e
  
  # saveRDS(config, file = "config.rds")

  config$write("config.ini")

}  
  
  
    
write_config <- function(config){
  for(i in 1:length(config)){
  if(i == 1){
    file.create("config.txt")
    }
  cat(paste(names(config[i]), toString(config[[i]]), "\n", sep="\t"), 
      file = "config.txt", append=T)
  }
}  
