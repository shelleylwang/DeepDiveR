library(devtools)
load_all(".")
library(DeepDiveR)

# load an occurrence table here
# your_dat <- read.csv("your_file_path")
path_dat <- "R/test_empirical_data/carnivora_analysis/"
dat <- read_xlsx(paste0(path_dat, "/carnivora_data_cleaned.xlsx"), sheet="Records")

dat <- dat[-which(dat$Maximum_Age=="Unknown"),]

# Organise your data in the expected format
# In this case some 2496 of 15882 records are not identified to the species 
# level, so here we will use genera
dat <- data.frame(dat$Genus, dat$Species, dat$Continent, 
                  as.numeric(dat$Minimum_Age), as.numeric(dat$Maximum_Age), 
                  dat$Latitude, dat$Longitude)
colnames(dat) <- c('Genus', 'Species', 'Area', 'MinAge', 'MaxAge', 
                   'Latitude', 'Longitude')

# re-code geography
setDT(dat)[, Locality := .GRP, by =.(Latitude, Longitude, MinAge, MaxAge)]

dat <- dat[!duplicated(dat), ]


# Specify a vector of time bins
bins <- c(72.1, 66, 65, 64, 63, 61.6, 60, 59.2, 58.13333, 57.06667, 56, 54.975, 53.95, 
          52.925, 51.9, 50.875, 49.85, 48.825, 47.8, 46.85714, 45.91429, 
          44.97143, 44.02857, 43.08571, 42.14286, 41.2, 40.03667, 38.87333, 
          37.71, 36.7575, 35.805, 34.8525, 33.9, 32.88667, 31.87333, 30.86, 
          29.84667, 28.83333, 27.82, 26.862, 25.904, 24.946, 23.988, 23.03, 
          22.16667, 21.30333, 20.44, 19.3225, 18.205, 17.0875, 15.97, 14.895, 
          13.82, 12.725, 11.63, 10.534, 9.438, 8.342, 7.246, 6.2895, 5.333, 
          4.4665, 3.6, 2.58, 1.8, 0.774, 0.129, 0.0117, 0.0082, 0.0042, 0)

dd_dataset <- paste0(path_dat, "deepdive_input")

# Prepare input file for deepdive, set output_file name to save
prep_dd_input(dat = dat, bins = bins, r = 100, 
              age_m = "random_by_loc", taxon_level = "Genus", 
              output_file = dd_dataset)


# Create config file with settings to run analysis
#create_config(simulations=T, n_CPUS=20, p_mass_extinction=0.01,
#              model_training=T, test=T, empirical_predictions=T)

# Check the number of extant taxa
summary_dat <- read_xlsx(paste0(path_dat, "/carnivora_data_cleaned.xlsx"), sheet="Species summary")
summary_dat <- summary_dat[-which(summary_dat$Minimum_age=="Unknown"),]
summary_dat <- summary_dat[-which(summary_dat$Status == "Extinct"),]
summary_dat <- data.frame(summary_dat$Genus, summary_dat$Status)
colnames(summary_dat) <- c("Genus", "Status")
summary_dat <- summary_dat[!duplicated(summary_dat), ]
length(summary_dat$Status == "Extant")


# Just the simulations
config <- create_config(
  simulate = T,  model_training = F, test_sim = F, empirical_predictions = F,
  wd = paste0(getwd()),  
  time_bins = bins,
  sim_name="try1",
  n_areas = length(unique(dat$Area)),
  simulations_file = paste0(path_dat, "simulations_1"), 
  add_test = F
)
config$write(paste0(path_dat,"try1.ini"))


# Just simulations, with test batch of simulations with the same settings as training data
config <- create_config(
  simulate = T,  model_training = F, test_sim = F, empirical_predictions = F,
  wd = getwd(),  
  time_bins = bins,
  sim_name="try1.2",
  n_areas = length(unique(dat$Area)),
  simulations_file = paste0(path_dat, "simulations_2"), 
  add_test = T
)
config$write(paste0(path_dat, "try1.2.ini"))


# Just the model_training
config <- create_config(
  simulate = F,  model_training = T, test_sim = F, empirical_predictions = F, 
  wd = getwd(),
  time_bins = bins,
  simulations_file = paste0(path_dat, "simulations_1"), 
  models_file = paste0(path_dat, "trained_models_1"), 
  feature_file = paste0(path_dat, "simulations_1/try1_20240208_training_features.npy") , 
  label_file = paste0(path_dat, "simulations_1/try1_20240208_training_labels.npy")
)
config$write(paste0(path_dat, "try2.ini"))


# Just the predictions
config <- create_config(
  simulate = F,  model_training = F, test_sim = F, empirical_predictions = T, 
  wd = getwd(),
  models_file = paste0(path_dat, "trained_models_1"), 
  time_bins = bins,
  include_present_diversity = T,
  present_diversity = ,
  empirical_input_file = dd_dataset
)
config$write(paste0(path_dat, "try3.ini"))



config <- create_config(simulate = T, model_training = T, 
                        test_sim = T, empirical_predictions = FALSE, 
                        time_bins = c(0, 5, 10, 15, 20), 
                        n_areas = 5, simulations_file = "simulations", 
                        # feature_files = paste('sim_features20220617.npy', collapse=""),
                        # label_files = 'sim_labels20220617.npy',
                        # feature_files_test = paste('test_sim_features20220617.npy', collapse=""),
                        label_files_test = 'test_sim_labels20220617.npy',
                        rnn_names = paste('rnn20220617', collapse=""), 
                        empirical_input_file = NULL,
                        max_age = -66, min_age = -0)

# use this syntax to edit entries in the config.ini file
config$set(option="n_areas", value="6", section="simulations")

# move this so the user instead will call this in the run line.
config$write("config.ini")

