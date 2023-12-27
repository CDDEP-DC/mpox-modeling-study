# Running Scenarios #
# This script will be loaded from a shell script, and will accept values from the shell script for the proportion asymptomatic and proporiton infectious

# Suppress warnings
options(warn=-1)

# 1. Asymptomatic transmission --------------------------------------------------------------------------------------------------

# Load the necessary packages
load_pckg = function(pck_names){
  suppressPackageStartupMessages(library(pck_names, character.only = TRUE, quietly=TRUE))
}

# List of packages
packages <- c("here", 
              "EpiModel", 
              "EpiModelHIV", 
              "dplyr", 
              "ggplot2", 
              "doParallel", 
              "foreach", 
              "lhs", 
              "kableExtra", 
              "GGally", 
              "tidyr")

#  Load packages
sapply(packages, load_pckg)

# Load necessary objects
load(here("mpox","R", "Network Objects", "mpx_network_object.rda"))
nets <- est
rm(est)

# Load the script containing the network modules
source(here("mpox","R", "Model", "network_modules_no_demo.R"))

# Load behavior adjustment
load(here("mpox","R", "Model", "adjust_matrix.Rdata"))

args <- commandArgs(trailingOnly = TRUE)
city_to_run <- args[1]
prop_asymp <- as.numeric(args[2])
prob_infection <- as.numeric(args[3])


# Load  vax rates for a specific city and set the object names and file path for saving at the end
if(city_to_run == "nyc"){
  load(here("mpox","R", "Model", "vaxDataFit_NYC.Rdata"))
  obj_name <-  paste0("sim_nyc_", "as_", prop_asymp, "_inf_", prob_infection)
  file_path_to_save <- paste0(here("mpox","Output", "New York City", "Asymptomatic Scenario/"), obj_name, ".RData")
} else if(city_to_run == "chi"){
  load(here("mpox","R", "Model", "vaxDataFit_CHI.Rdata"))
  obj_name <-  paste0("sim_chi_", "as_", prop_asymp, "_inf_", prob_infection)
  file_path_to_save <- paste0(here("mpox","Output", "Chicago", "Asymptomatic Scenario/"), obj_name, ".RData")
} else if(city_to_run == "sf"){
  load(here("mpox","R", "Model", "vaxDataFit_SF.Rdata"))
  obj_name <-  paste0("sim_sf_", "as_", prop_asymp, "_inf_", prob_infection)
  file_path_to_save <- paste0(here("mpox","Output", "San Francisco", "Asymptomatic Scenario/"), obj_name, ".RData")
}

# Set-up the behaviors needed

travel.change.switch = FALSE
asymp.change.switch = TRUE
behavior.change.switch = TRUE
wane.change.switch = FALSE
num.cores <- detectCores() - 1
type_of_vax = "leaky"
vacEffect1 = 0.37
vacEffect2 = 0.69
vacEffectdelayedVax = 0.54
num_travelers_seed = 0
initial_infections = 10
r_to_s_time = NULL
reinfections.change.switch = FALSE
num.timesteps <- 456     # The number of time steps to run this model
num.nsims <- 100
prop_reinfection = NULL
  
params <- param_msm()
controls <- control_msm(
  nsteps = num.timesteps,
  nsims = num.nsims,
  ncores = num.cores,
  verbose = FALSE)
  
inits <- init_msm()
  
set.seed(12345)
sim_output <- netsim(nets, params, inits, controls)

# Rename the object
assign(obj_name, sim_output)
  
# Save the output to HDD and clear it from the environment
save(list = obj_name, file = file_path_to_save)

gc()
