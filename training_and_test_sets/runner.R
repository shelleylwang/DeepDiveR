# Runner to generate:
# - A general training set with 0.5 probability extant
# - Test set with everything recently extinct.
# - Test set with everything extant.
library(devtools)

# Set the session wd to the location of DeepDiveR
DeepDiveR_path <- ""
setwd("DeepDiveR_path")

load_all(".")
library(DeepDiveR)
library(readxl)  # if your occurrence data is in an xl file
library(data.table)

path_wd <- "/Users/CooperR/Documents/GitHub/DeepDiveR/training_and_test_sets"

# Specify a vector of time bins
bins <- sort(seq(0, 100, by=10), decreasing=TRUE)


# To make a general training set and train a model
config <- create_config(
  simulate = T,  model_training = T, empirical_predictions = F,
  path_wd = path_wd,  
  time_bins = bins,
  sim_name="general",
  n_areas = 5,
  simulations_file = "simulations_general", 
  add_test = T, 
  autotune = T,
  models_file = "trained_models_general", 
  include_present_diversity = TRUE,
  calibrate_diversity = FALSE
)

set_value(attribute_name = "pr_extant_clade", value=0.5, module="simulations", config)
config$write(paste(path_wd, "general.ini", sep="/")) # write config


# Test set extant - unconstrained
config <- create_config(
  simulate = T,  model_training = F, empirical_predictions = F,
  path_wd = path_wd,  
  time_bins = bins,
  sim_name="extant",
  n_areas = 5,
  simulations_file = "simulations_extant_u", 
  add_test = F, 
  autotune = T,
  include_present_diversity = FALSE
)

set_value(attribute_name = "n_training_simulations", value=100, module="simulations", config)
set_value(attribute_name = "pr_extant_clade", value=1, module="simulations", config)
config$write(paste(path_wd, "extant_unconstrained.ini", sep="/")) # write config


# Test set extant - conditioned
config <- create_config(
  simulate = T,  model_training = F, empirical_predictions = F,
  path_wd = path_wd,  
  time_bins = bins,
  sim_name="extant",
  n_areas = 5,
  simulations_file = "simulations_extant_con", 
  add_test = F, 
  autotune = T,
  include_present_diversity = TRUE,
  present_diversity = 100,  # extant species-level diversity
  calibrate_diversity = FALSE
)

set_value(attribute_name = "n_training_simulations", value=100, module="simulations", config)
set_value(attribute_name = "pr_extant_clade", value=1, module="simulations", config)
config$write(paste(path_wd, "extant_condition.ini", sep="/")) # write config


# Test set extant - calibrate
config <- create_config(
  simulate = T,  model_training = F, empirical_predictions = F,
  path_wd = path_wd,  
  time_bins = bins,
  sim_name="extant",
  n_areas = 5,
  simulations_file = "simulations_extant_cal", 
  add_test = F, 
  autotune = T,
  include_present_diversity = TRUE,
  present_diversity = 100,  # extant species-level diversity
  calibrate_diversity = TRUE
)

set_value(attribute_name = "n_training_simulations", value=100, module="simulations", config)
set_value(attribute_name = "pr_extant_clade", value=1, module="simulations", config)
config$write(paste(path_wd, "extant_calibrate.ini", sep="/")) # write config


# Test set extinct - unconstrained
config <- create_config(
  simulate = T,  model_training = F, empirical_predictions = F,
  path_wd = path_wd,  
  time_bins = bins,
  sim_name="extinct",
  n_areas = 5,
  simulations_file = "simulations_extinct_u", 
  add_test = F, 
  autotune = T,
  include_present_diversity = FALSE
)

set_value(attribute_name = "n_training_simulations", value=100, module="simulations", config)
set_value(attribute_name = "pr_extant_clade", value=0, module="simulations", config)
config$write(paste(path_wd, "extinct_unconstrained.ini", sep="/")) # write config


# Test set extant - conditioned
config <- create_config(
  simulate = T,  model_training = F, empirical_predictions = F,
  path_wd = path_wd,  
  time_bins = bins,
  sim_name="extinct",
  n_areas = 5,
  simulations_file = "simulations_extinct_con", 
  add_test = F, 
  autotune = T,
  include_present_diversity = TRUE,
  present_diversity = 0,  # extant species-level diversity
  calibrate_diversity = FALSE
)

set_value(attribute_name = "n_training_simulations", value=100, module="simulations", config)
set_value(attribute_name = "pr_extant_clade", value=0, module="simulations", config)
config$write(paste(path_wd, "exinct_condition.ini", sep="/")) # write config


# Test set extant - calibrate
config <- create_config(
  simulate = T,  model_training = F, empirical_predictions = F,
  path_wd = path_wd,  
  time_bins = bins,
  sim_name="extinct",
  n_areas = 5,
  simulations_file = "simulations_extinct_cal", 
  add_test = F, 
  autotune = T, 
  include_present_diversity = TRUE,
  present_diversity = 0,  # extant species-level diversity
  calibrate_diversity = TRUE
)

set_value(attribute_name = "n_training_simulations", value=100, module="simulations", config)
set_value(attribute_name = "pr_extant_clade", value=0, module="simulations", config)
config$write(paste(path_wd, "extinct_calibrate.ini", sep="/")) # write config
