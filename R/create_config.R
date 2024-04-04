#' Create DeepDive config object and file
#'
#' 'create_config()' generates config object with settings needed to launch
#' DeepDive scripts in python. 
#' @param simulate A true or false statement that switches on and off the generation of simulated datasets used in model training and testing.
#' @param model_training A true or false statement that switches on and off the training of new RNN models.
#' @param empirical_predictions A true or false statement that switches on and off the set up for empirical analyses. 
#' @param path_wd The working directory where files for analyses can be found.
#' @param time_bins A vector of bin boundary ages.
#' @param sim_name A string that identified a set of simulations.
#' @param n_areas An integer number of discrete sampling regions e.g. continents, basins.
#' @param simulations_file File path to where simulations will be saved.
#' @param add_test TRUE/FALSE statement, when T a test set with the same settings as the training set are generated.
#' @param models_file Path to a folder where trained models can be saved and retrieved.
#' @param feature_file File name for training features.
#' @param label_file File name for training labels.
#' @param empirical_input_file File path to empirical data in the input format for use in deepdive.
#' @param include_present_diversity TRUE/FALSE statement, if T will condition predictions on modern diversity.
#' @param present_diverstiy A number of extant taxa, which predictions will be conditioned on.
#' @param taxonomic_level String of the name of the taxonomic data column, e.g. "Species", "Genus".
#' @param output_file Path where outputs will be saved.
#' @returns Creates config with settings to launch DeepDive.
#' @examples
#' config <- create_config(simulate = T,  model_training = F, 
#' empirical_predictions = F, wd = paste0(getwd()), time_bins = bins, 
#' sim_name="test", n_areas = length(unique(dat$Area)), 
#' simulations_file = paste0(path_dat, "simulations"), add_test = F)
#' @export 
create_config <- function(simulate = T, model_training = T,
                          empirical_predictions = F, outputs=F,
                          
                          autotune=FALSE,
                          
                          # settings needed for simulation module
                          path_wd = NULL, 
                          time_bins = NULL, 
                          sim_name = NULL,
                          n_areas = NULL, 
                          simulations_file = NULL,
                          add_test = F,
                          
                          # for the training module
                          models_file = NULL,
                          feature_file = NULL, label_file = NULL, 
                          
                          # test section - might be redundant
                          #feature_files_test = NULL, label_files_test = NULL,
                          #rnn_names = NULL, 
                          
                          empirical_input_file = dd_dataset,
                          include_present_diversity = FALSE, 
                          present_diversity = NA,
                          calibrate_diversity = FALSE,
                          taxonomic_level = taxon_level,
                          output_file = NULL){
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
  general$wd <- path_wd
  general$time_bins <- sort(paste(time_bins, collapse=" "))
  if(is.null(general$time_bins)){
    print("Warning: time_bins is set to NULL, please adjust using argument in create_config or using the set function below to provide values")
  }
  general$n_areas <- n_areas  # number of discrete regions
  if(is.null(general$n_areas)){
    print("Warning: n_areas is set to NULL, please adjust using argument in create_config or using the set function below")
  }
  general$autotune <- autotune
  general$include_present_diversity <- include_present_diversity
  general$calibrate_diversity <- calibrate_diversity
  
  config$data$general <- general
  
  # simulations
  if(simulate == TRUE){
    folders <- simulations_file
    paths <- paste(path_wd, folders, sep="/")
    sapply(paths, dir.create)
    sims$sim_name <- sim_name
    sims$n_CPUS <- 1  # number of CPUs used for simulations
    sims$n_training_simulations <- 2000  # simulations per CPU (total should be ~10,000)
    sims$training_seed <- 123
    if(add_test == TRUE){
      sims$test_seed <- 432
      sims$n_test_simulations <- 100  # simulations per CPU (the total should be e.g. 100 or 1000)
    }
    
    
    sims$s_species <- 1  # number of starting species
    sims$total_sp <- paste(100, 5000, collapse="")  # min/max size data set
    sims$root_r <- paste(0.8*(max(time_bins)-min(time_bins))+min(time_bins), max(time_bins), collapse="")  # range of ages for origin of clade
    sims$min_extinct_sp <- 0  # minimum number of extinct lineages allowed
    sims$extant_sp <- paste(0, 10000, collapse="")  # min/max number of living species  # MAYBE MAKE THESE VECTORS INSTEAD?
    sims$rangeL <- paste(0.02, 0.5, collapse="")  # range of birth rates - range based on Cantalapiedra et al 2022 for elephant example 
    sims$rangeM <- paste(0.02, 0.5, collapse="")  # range of death rates
    sims$log_uniform_rates <- FALSE 
    sims$p_mass_extinction <- 0.01  # probability of mass extinction per my
    sims$magnitude_mass_ext <- paste(0.5, 1, collapse="")
    sims$p_equilibrium <- 0.01 # probability of equilibrium per my  ### ADJUST DEFAULT VALUES FOR PROBABILITIES THAT WILL MAKE SENSE
    sims$p_constant_bd <- 0.01 # probability of constant birth-death rates per my
    sims$p_mass_speciation <- 0.01 # probability of mass speciation per my
    sims$p_dd_model <- 0.05 # probability of diversity-dependent diversification
    sims$dd_K <- paste(100,1000, collapse = "")  # carrying capacity for dd clades
    sims$dd_maxL <- 1 # starting sp rate for dd
    sims$pr_extant_clade <- 0.7 # probability of simulating an extant clade
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
    sims$mean_skyline_sampling <- paste(0.1, 10, collapse="")
    sims$maximum_localities_per_bin <- 200
    sims$species_per_locality_multiplier <- 1
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
    folders <- models_file
    paths <- paste(path_wd, folders, sep="/")
    sapply(paths, dir.create)
    mt <- c()
    mt$sims_folder <- simulations_file
    mt$model_folder <- models_file
    mt$lstm_layers <- paste(64, 32, collapse="")
    mt$dense_layer <- paste(64, 32, collapse="")
    mt$dropout <- 0
    mt$max_epochs <- 1000
    mt$patience <- 10
    mt$batch_size <- 100
    mt$validation_split <- 0.2
    mt$f <- feature_file
    mt$l <- label_file
    config$data$model_training <- mt
  }
  
  # For test simulations
  # ADD SPECIFYING THE DATA, SIMULATION OUTPUT FILES ARE.
  # CLARIFY USE PYTHON FOR FULL FELEXIBILITY.
  #if(test_sim == TRUE){
  #  test <- c()
  #  test$sim_wd <- simulations_file # "simulations"
  #  if(is.null(simulations_file)){
  #    print("Must specify where test simulations will be saved in simulations_file argument or using the set() function")
  #  }
  #  test$model_wd <- "trained_models" # in python path.join to get to folders
  #  test$feat_files <- feature_file  # paste('sim_features20220617.npy', collapse="")
  #  if(is.null(test$feat_files)){
  #    print("Warning: feature_file is set to NULL, please adjust using argument in create_config or using the set() function")
  #  }
  #  test$lab_file <- label_file  # 'sim_labels20220617.npy'
  #  if(is.null(test$lab_files)){
  #    print("Warning: label_file is set to NULL, please adjust using argument in create_config or using the set() function")
  #  }
  #  test$feat_files_test <- feature_files_test # paste('test_sim_features20220617.npy', collapse="")
  #  if(is.null(test$feat_files_test)){
  #    print("Warning: feature_files_test is set to NULL, please adjust using argument in create_config or using the set() function")
  #  }
  #  test$lab_file_test <- label_files_test # 'test_sim_labels20220617.npy'
  #  if(is.null(test$lab_file_test)){
  #    print("Warning: lab_fle_test is set to NULL, please adjust using argument in create_config or using the set() function")
  #  }
  #  test$output_names <- rnn_names  # paste('rnn20220617', collapse="")
  #  if(is.null(test$output_names)){
  #    print("Warning: rnn_names is set to NULL, please adjust using argument in create_config or using the set() function")
  #  }
  #  test$n_predictions <- 100
  #  # test$sqs_6 <- "" # file path
  #  test$j <- 2  # index of the best performing model
  #  config$data$test_predictions <- test
  #}
  
  # For empirical predictions
  if(empirical_predictions == TRUE){
    e <- c()
    e$empirical_input_file <- empirical_input_file # ".\R\test_empirical_data" 
    if(is.null(e$empirical_input_file)){
      print("Warning: empirical_input_file is set to NULL, please provide a file path using the argument in create_config or using the set() function")
    }
    e$model_folder <- models_file
    e$n_predictions <- 1  # number of predictions per input file 
    e$replicates <- 100  # number of age randomisation replicates used in data_pipeline.R
#    e$CI <- 0.95  # confidence intervals
#    e$random_seed <- 123
#    e$plot_mean_prediction <- FALSE  # if true add line with mean prediction (one line per replicate)
#    e$plot_median_prediction <- TRUE  # if true add line with median prediction across all models
#    e$plot_all_predictions <- FALSE 
#    e$alpha <- 0.2
#    e$prediction_color <- "b"
#    e$plot_shaded_area <- TRUE
    e$scaling <- "1-mean"
    e$present_diversity <- present_diversity
    e$taxon_level <- taxonomic_level
#    e$models <- "None"  # or provide a list of file names which could have a single item ##NULL DOESNT READ CORRECLT IN PYTHON
    e$output_file <- output_file
    folders <- output_file
    paths <- paste(path_wd, folders, sep="/")
    sapply(paths, dir.create)
    config$data$empirical_predictions <- e
  }
  
  #if (outputs){
  #    outputs <- c()
  #    outputs$path <- ""  # folder where outputs will be saved
  #    outputs$histograms <- TRUE
  #    outputs$training_stats <- TRUE 
  #    outputs$features <- TRUE  # if TRUE will save features
  #    outputs$predictions <- TRUE  # if TRUE will save predictions
  #    outputs$stats_results <- TRUE  # saves e.g. R2 and rMSE among other summary stats
  #    config$data$outputs <- outputs       
  #}

  return(config)
}  


#' Edit values in config
#'
#' 'set_value()' accesses attributes in the config file and adjusts the assigned 
#' value(s) to that of a provided integer or vector.
#' @param attribute_name A string providing direction to the attribute you want to edit e.g. "total_sp"
#' @param value An integer or vector value you want to assign to the attribute.
#' @param module A string indicating the block of the config the attribute can be found within.
#' @param config A config which can be generated in create_config(), that you will edit here. 
#' @returns Edits settings in the config .ini file.
#' @examples 
#' set_value(attribute_name = "extant_sp", value=c(10, 10000), module="simulations", config)
#' @export
set_value <- function(attribute_name, value, module, config){
  variable <- paste(value, collapse=" ")
  config$data[module][[1]][attribute_name] <- variable
  # pass lists, vectors etc and then turn into a string
}


#' Make discrete geographic regions appear or disappear
#'
#' 'areas_matrix()' adds attributes in the simulations module which provide a 
#' maximum and minimum age range for regions becoming connected. 
#' @param area_ages A matrix of minimum and maximum ages for areas appearing, with older ages in column one. Must have a number of rows = number of discrete areas.
#' @param n_areas The number of unique discrete areas used in the analysis.
#' @param config A config which can be generated in create_config(), that you will add attributes to here. 
#' @returns Adds an attribute for each area (named "area1", "area2"... etc) with min and max age of migration becoming possible.
#' @examples 
#' areas_matrix(area_ages, n_areas = length(unique(dat$Area)), config)
#' @export
areas_matrix <- function(area_ages = NULL, n_areas, config, bins, label = "start"){
  if(is.null(area_ages) & label=="start"){
    # for each region, specify two ages between which connection to others will be established.
    area_ages <- c()
    for(i in 1:n_areas){
      area_ages <- rbind(area_ages,
                         c(max(bins), max(bins)))
    }
    set_value(attribute_name = paste0("area_", label, i), value=c(area_ages[i, 1], area_ages[i, 2]), module="simulations", config)
  }
  if(is.null(area_ages) & label=="end"){
    # for each region, specify two ages between which connection to others will be removed.
    area_ages <- c()
    for(i in 1:n_areas){
      area_ages <- rbind(area_ages,
                         c(min(bins), min(bins)))
    }
    set_value(attribute_name = paste0("area_", label, i), value=c(area_ages[i, 1], area_ages[i, 2]), module="simulations", config)
  }
  if(!is.null(area_ages)){
      for(i in 1:n_areas){
        set_value(attribute_name = paste0("area_", label, i), value=c(area_ages[i, 1], area_ages[i, 2]), module="simulations", config)
      }
  }
}
