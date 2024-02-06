#' Create DeepDive config object and file
#'
#' 'create_config()' generates config object with settings needed to launch
#' DeepDive scripts in python. 
#' @param train_sim A true or false statement that switches on and off the generation of simulated datasets used in model training.
#' @param model_training A true or false statement that switches on and off the training of new RNN models.
#' @param test_sim A true or false statement that switches on and off the generation of simulated datasets used in testing the model.
#' @param empirical_predictions A true or false statement that switches on and off the set up for empirical analyses. 
#' @param time_bins A vector of bin boundary ages.
#' @param n_areas An integer number of discrete sampling regions e.g. continents, basins.
#' @param simulations_file File path to where simulations will be saved.
#' @param feature_files File name for training features.
#' @param label_files File name for training labels.
#' @param feature_files_test File name for test features.
#' @param label_files_test File name for test labels.
#' @param rnn_names Name of saved rnn file.
#' @param empirical_input_file File path to empirical data in the input format for use in deepdive.
#' @param max_age A negative number indicating the oldest age of the study interval.
#' @param min_age A negative number indicating the youngest age of the study interval.
#' @returns Creates config with settings to launch DeepDive.
#' @examples
#' 
#' @export ###create_config and an edit_config
create_config <- function(simulate = T, model_training = T, test_sim = T,
                          empirical_predictions = F, outputs=F,
                          
                          # settings needed for simulation module
                          wd = NULL, 
                          time_bins = NULL, 
                          sim_name = NULL,
                          n_areas = NULL, 
                          simulations_file = NULL,
                          add_test = F,
                          
                          models_file = NULL,
                          feature_file = NULL, label_file = NULL, 
                          feature_files_test = NULL, label_files_test = NULL,
                          rnn_names = NULL, empirical_input_file = NULL,
                          max_age = NULL, min_age = NULL, 
                          include_present_diversity = TRUE, present_diversity = NULL){
### use a function to turn TRUE and FALSE to True and False in python
  library(ConfigParser)
  config <- ConfigParser$new()
  sims <- c()
  # specify where you will save and load the config from? keep paths consistent across scripts.
  # then this will create a folder with a sub-folder system to keep from specifying too many paths
  # add a user spcified path option, or default to a folder structure
  # path to input, output files etc. 
  
  # directory
  general <- c()
  general$wd <- wd
  folders <- c(simulations_file, "trained_models")
  paths <- paste(wd, folders, sep="/")
  sapply(folders, dir.create)
  
  general$time_bins <- sort(paste(time_bins, collapse=" "))
  if(is.null(general$time_bins)){
    print("Warning: time_bins is set to NULL, please adjust using argument in create_config or using the set function below to provide values")
  }
  general$include_present_diversity <- FALSE
  
  
  config$data$general <- general
  
  # simulations
  if(simulate == TRUE){
    sims$sim_name <- sim_name
    sims$n_CPUS <- 1  # number of CPUs used for simulations
    sims$n_training_simulations <- 2000  # simulations per CPU (total should be ~10,000)
    sims$n_test_simulations <- 20  # simulations per CPU (the total should be e.g. 100 or 1000)
    sims$n_areas <- n_areas  # number of discrete regions
    if(is.null(sims$n_areas)){
      print("Warning: n_areas is set to NULL, please adjust using argument in create_config or using the set function below")
      }
    sims$training_seed <- 123
    sims$test_seed <- 432
    sims$include_present_diversity <- include_present_diversity 
    sims$add_test <- add_test
    
    sims$s_species <- 1  # number of starting species
    sims$rangeSP <- paste(100, 5000, collapse="")  # min/max size data set
    sims$root_r <- paste(median(time_bins), max(time_bins), collapse="")  # range of ages for origin of clade
    sims$min_extinct_sp <- 0  # minimum number of extinct lineages allowed
    sims$extant_sp <- paste(0, 10000, collapse="")  # min/max number of living species  # MAYBE MAKE THESE VECTORS INSTEAD?
    sims$rangeL <- paste(0.02, 0.5, collapse="")  # range of birth rates - range based on Cantalapiedra et al 2022 for elephant example 
    sims$rangeM <- paste(0.02, 0.5, collapse="")  # range of death rates
    sims$log_uniform_rates <- FALSE 
    sims$p_mass_extinction <- 0.01  # probability of mass extinction per my
    sims$p_equilibrium <- 0.01 # probability of equilibrium per my  ### ADJUST DEFAULT VALUES FOR PROBABILITIES THAT WILL MAKE SENSE
    sims$p_constant_bd <- 0.01 # probability of constant birth-death rates per my
    sims$p_mass_speciation <- 0.01 # probability of mass speciation per my
    sims$poiL <- 4  # expected number of birth rate shifts
    sims$poiM <- 4  # expected number of death rate shifts
    sims$scale <- 10 # scaling 
    sims$vectorize <- TRUE  # True will vectorise the birth-death simulation.
    
    
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
    sims$link_area_size_carrying_capacity <- paste(1, 10, collapse="")  # positive, larger numbers = stronger link between area size and carrying capacity
    sims$p_origination_a_slope_mean <- 2  # mean slope of probability of origination area mean
    sims$p_origination_a_slope_sd <- 0.5  # stds of the slopes
    sims$sp_mean <- paste(0.1, 0.5, collapse="")  # G(a,b) distributed preservation rates across species
    sims$sp_variance <- 2
    sims$slope <- paste(-0.01, 0, collapse="")  # change in log-sampling rate through time (log-linear)
    sims$intercept <- paste(0.1, 0.5, collapse="")  # initial sampling rate
    sims$sd_through_time <- paste(0.001, 0.01, collapse="")  # st dev in log-sampling rate through time
    sims$sd_through_time_skyline <- 1
    sims$mean_n_epochs_skyline <- 4
    sims$fraction_skyline_sampling <- 0.5
    sims$maximum_localities_per_bin <- 200
    sims$singletons_frequency <- 0.1  # MAKE SURE YOU ASJUST THIS TO A SENSIBLE NO.
    sims$sims_folder <- simulations_file
    
    config$data$simulations <- sims
    
    # settings for areas appearing and disappearing 
  
    # settings to adjust area bias
  
    # settings to adjust time bias
  
    # settings to adjust taxonomic bias
    }


  if(model_training == TRUE){
    ## add if statement if no simulations run need to specify where the sims are
    # Settings for training models
    mt <- c()
    mt$sims_folder <- simulations_file
    mt$model_folder <- models_file
    mt$lstm_layers <- paste(64, 32, collapse="")
    mt$dense_layer <- paste(64, 32, collapse="")
    mt$dropout <- 0.5
    mt$max_epochs <- 1000
    mt$patience <- 10
    mt$batch_size <- 100
    mt$validation_split <- 0.2
    mt$f <- feature_file
    mt$l <- label_file
    mt$include_present_diversity <- include_present_diversity
    # add if statement if none and it's just the mt block must specify by user,
    # if there's also the sim module accept the none and resolve in python. In python
    # ignore the none and take the simulation output. 
    mt$l <- label_file 
    config$data$model_training <- mt
  }
  
  # For test simulations
  # ADD SPECIFYING THE DATA, SIMULATION OUTPUT FILES ARE.
  # CLARIFY USE PYTHON FOR FULL FELEXIBILITY.
  if(test_sim == TRUE){
    test <- c()
    test$sim_wd <- simulations_file # "simulations"
    if(is.null(simulations_file)){
      print("Must specify where test simulations will be saved in simulations_file argument or using the set() function")
    }
    test$model_wd <- "trained_models" # in python path.join to get to folders
    test$feat_files <- feature_file  # paste('sim_features20220617.npy', collapse="")
    if(is.null(test$feat_files)){
      print("Warning: feature_file is set to NULL, please adjust using argument in create_config or using the set() function")
    }
    test$lab_file <- label_file  # 'sim_labels20220617.npy'
    if(is.null(test$lab_files)){
      print("Warning: label_file is set to NULL, please adjust using argument in create_config or using the set() function")
    }
    test$feat_files_test <- feature_files_test # paste('test_sim_features20220617.npy', collapse="")
    if(is.null(test$feat_files_test)){
      print("Warning: feature_files_test is set to NULL, please adjust using argument in create_config or using the set() function")
    }
    test$lab_file_test <- label_files_test # 'test_sim_labels20220617.npy'
    if(is.null(test$lab_file_test)){
      print("Warning: lab_fle_test is set to NULL, please adjust using argument in create_config or using the set() function")
    }
    test$output_names <- rnn_names  # paste('rnn20220617', collapse="")
    if(is.null(test$output_names)){
      print("Warning: rnn_names is set to NULL, please adjust using argument in create_config or using the set() function")
    }
    test$n_predictions <- 100
    # test$sqs_6 <- "" # file path
    test$j <- 2  # index of the best performing model
    config$data$test_predictions <- test
  }
  
  # For empirical predictions
  if(empirical_predictions == TRUE){
    e <- c()
    e$empirical_input_file <- empirical_input_file # ".\R\test_empirical_data" 
    # if you want empirical but don't specify a file will give a warning, please use the output file prepared in the correct format for use in deepdive
    if(is.null(e$empirical_input_file)){
      print("Warning: empirical_input_file is set to NULL, please provide a file path using the argument in create_config or using the set() function")
    }
    e$model_folder <- models_file
    e$n_predictions <- 10  # number of predictions per input file 
    e$replicates <- 100  # number of age randomisation replicates used in data_pipeline.R
    e$CI <- 0.95  # confidence intervals
    
#    e$max_age <- max_age  # -66
#    if(is.null(e$max_age)){
#      print("Warning: max_age is set to NULL, please provide max_age using the argument in create_config")
#    }
#    e$min_age <- min_age   # -0
#    if(is.null(e$min_age)){
#      print("Warning: min_age is set to NULL, please provide min_age using the argument in create_config")
#    }
#    if(e$min_age & e$max_age ){
#        e$plotting_range <- paste(-e$max_age, -e$min_age, collapse="")  # age range used for plotting (truncates the predictions)
#    }
#    else{
#      e$plotting_range <- NULL
#      print("To set plotting range provide min_age and max_age arguments to create config, otherwise NULL")
#    }
  
    e$random_seed <- 123
    e$plot_mean_prediction <- FALSE  # if true add line with mean prediction (one line per replicate)
    e$plot_median_prediction <- TRUE  # if true add line with median prediction across all models
    e$plot_all_predictions <- FALSE 
    e$alpha <- 0.2
    e$prediction_color <- "b"
    e$plot_shaded_area <- TRUE
    e$scaling <- "1-mean"
    e$models <- "None"  # or provide a list of file names which could have a single item ##NULL DOESNT READ CORRECLT IN PYTHON
    config$data$empirical_predictions <- e
  }
  
  if (outputs){
      outputs <- c()
      outputs$path <- ""  # folder where outputs will be saved
      outputs$histograms <- TRUE
      outputs$training_stats <- TRUE 
      outputs$features <- TRUE  # if TRUE will save features
      outputs$predictions <- TRUE  # if TRUE will save predictions
      outputs$stats_results <- TRUE  # saves e.g. R2 and rMSE among other summary stats
      config$data$outputs <- outputs       
  }

  return(config)
}  
