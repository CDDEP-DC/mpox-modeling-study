#### Run extended models
library(here)
library(parallel)
library(tidyverse)
library(EpiModel)
library(EpiModelHIV)

# Pass city to run
args <- commandArgs(trailingOnly = TRUE)

# Check if a city name argument is provided
if (length(args) == 0) {
  stop("Some arguments are missing")
}

# Assign the first argument to city_name variable
city_name <- args[1]
asymp_switch <- as.logical(args[2])
reduce_asymp <- as.logical(args[3])
reduce_asymp_timing <- if (is.null(args[4]) == T) NULL else as.numeric(args[4])
reinfection <- as.logical(args[5])
reinfection_time <- if (is.null(args[6]) == T) NULL else as.numeric(args[6])
vaxwane <- as.logical(args[7])
vaxwane_p <- if (is.null(args[8]) == T) NULL else as.numeric(args[8])
vaxwane_f <- if (is.null(args[9]) == T) NULL else as.numeric(args[9])
post_pride_seeds <- as.logical(args[10])
mech_name <- args[11]

#### LOAD OTHER RELEVANT DATA;
if(city_name == "CHI"){
  
  load(here("mpox","R", "Model", "adjust_matrix_CHI.Rdata"))
  load(here("mpox","R", "Model", "vaxDataFit_CHI_new.Rdata"))
  load(here("mpox","R", "Model", "params_sample_CHI.Rdata"))
}else if(city_name == "NYC"){
  
  load(here("mpox","R", "Model", "adjust_matrix_NYC.Rdata"))
  load(here("mpox","R", "Model", "vaxDataFit_NYC_new.Rdata"))
  load(here("mpox","R", "Model", "params_sample_NYC.Rdata"))
  
  
}else if(city_name == "SF"){
  load(here("mpox","R", "Model", "adjust_matrix_SF.Rdata"))
  load(here("mpox","R", "Model", "vaxDataFit_SF_new.Rdata"))
  load(here("mpox","R", "Model", "params_sample_SF.Rdata"))
}

load(here("mpox","R", "Network Objects", "mpx_network_object.rda"))

nets <- est
rm(est)

print("Finished loading objects");

run_model = function(params_sample, corenum, ncores, log_file){

  # Calculate the range of rows each core should process
  total_samples <- nrow(params_sample)
  rows_per_core <- total_samples %/% ncores
  remainder <- total_samples %% ncores
  
  if (corenum == ncores) {
    start_row <- (corenum - 1) * rows_per_core + 1
    end_row <- total_samples
  } else {
    start_row <- (corenum - 1) * rows_per_core + 1
    end_row <- corenum * rows_per_core
  }
  
  params_sample_subsample <- params_sample[start_row:end_row, ]
  
  num_rows <- end_row - start_row + 1
  
  runs <- list(Params = params_sample_subsample,
               Trackers = list());
  
  log_file <- paste0("/home/ec2-user/mpox/Output/", new_folder_name, paste0("/Log_",corenum, ".txt"))
  
  cat(paste0("Core Number: ", corenum), file = log_file, append = TRUE)
  cat("\n", file = log_file, append = TRUE)  # Add a newline for readability
  cat("Starting Runs...", file = log_file, append = TRUE)
  cat("\n", file = log_file, append = TRUE)  # Add a newline for readability
  
  for(sample_i in 1:num_rows){
    
    start_time <- Sys.time()
    
    if (sample_i %% save_interval == 0) {
      # Update file name to include chain ID and iteration number
      new_name_i = paste0("Runs_corenum_",corenum);
      assign(new_name_i, runs)
      save(list = new_name_i, file = paste0("/home/ec2-user/mpox/Output/", new_folder_name, paste0("/Subsample_", corenum, ".RData")))
      
      cat("* Saved output up to iteration", sample_i, "for subsample number", corenum, file = log_file, append = TRUE)
      cat("\n", file = log_file, append = TRUE)  # Add a newline for readability
    }
    
    prob_infection_i = as.numeric(params_sample_subsample[sample_i,"prob.U"]);
    seeding_pre_pride_i = as.numeric(params_sample_subsample[sample_i,"num_seeds"]);
    detect_prob_i = as.numeric(params_sample_subsample[sample_i, "prob.I"]);

    asymp_switch_i <- asymp_switch;
    
    reduce_asymp_i <- reduce_asymp;
    reduce_asymp_timing_i <- reduce_asymp_timing;    #274
    reinfections_i <- reinfection;
    reinfection_time_i <- reinfection_time;
    
    vax_wane_i <- vaxwane;
    vaxwane_p_i <- vaxwane_p;
    vaxwane_f_i <- vaxwane_f;
    post_pride_seeds_i <- post_pride_seeds;
    
    source("/home/ec2-user/mpox/R/Model/network_modules.R")
    
    params <- param_msm(prob_infection_i,
                        seeding_pre_pride_i,
                        detect_prob_i,
                        
                        asymp_switch_i,
                        
                        reduce_asymp_i,
                        reduce_asymp_timing_i,
                        
                        reinfections_i,
                        reinfection_time_i,
                        
                        vax_wane_i,
                        vaxwane_p_i,
                        vaxwane_f_i,
                        
                        post_pride_seeds_i) 
    
    inits <- init_msm();
    controls <- control_msm();
    
    # This function should compute the NLL for the provided parameters 'params'
    sim_i = netsim(nets, params, inits, controls);
    
    runs[["Trackers"]][[sample_i]] = sim_i[["epi"]]
    
    end_time <- Sys.time()
    elapsed_time <- end_time - start_time
    elapsed_time <- round(elapsed_time,2)
    cat(paste0("Core Num: ", corenum, " | Run number: ", sample_i," | Runtime: ", elapsed_time), file = log_file, append = TRUE)
    cat("\n", file = log_file, append = TRUE)  # Add a newline for readability
    
  }
  
  cat("Runs completed!\n", file = log_file, append = TRUE)
  cat("\n", file = log_file, append = TRUE)  # Add a newline for readability
  
  new_name_i = paste0("Runs_corenum_",corenum);
  assign(new_name_i, runs)

  save(list = new_name_i, file = paste0("/home/ec2-user/mpox/Output/", new_folder_name, paste0("/Subsample_", corenum, ".RData")))
   
}

n_draws <- 1000

ncores_total <- detectCores() - 92
ncores <- min(ncores_total, 192)  # Use up to 192 cores, adjust if needed
save_interval <- 5
ncounter = 1

new_folder_name = paste0(mech_name, city_name, " - ", Sys.Date())

dir.create(paste0("/home/ec2-user/mpox/Output/", new_folder_name), recursive = FALSE)

log_file <- paste0("/home/ec2-user/mpox/Output/", new_folder_name, paste0("/run_details", ".txt"))
  

lhs_full_run_start = Sys.time()
cat("Runs \n", file = log_file, append = TRUE)
cat(paste0("Number of draws: ", n_draws, "\n"), file = log_file, append = TRUE)
cat(paste0("Number of cores: ", ncores, "\n"), file = log_file, append = TRUE)
cat(paste0("Simulations started at: ", lhs_full_run_start ,"\n"), file = log_file, append = TRUE)

# Initialize cluster for parallel execution
cl <- makeCluster(ncores)

# Load necessary libraries on all worker nodes
clusterEvalQ(cl, library(here))
clusterEvalQ(cl, library(EpiModel))
clusterEvalQ(cl, library(EpiModelHIV))
clusterEvalQ(cl, library(EpiModelHPC))
clusterEvalQ(cl, library(dplyr))
clusterEvalQ(cl, library(ggplot2))
clusterEvalQ(cl, library(doParallel))
clusterEvalQ(cl, library(foreach))
clusterEvalQ(cl, library(lhs))
clusterEvalQ(cl, library(kableExtra))
clusterEvalQ(cl, library(GGally))
clusterEvalQ(cl, library(tidyr))

# Export necessary functions
clusterExport(cl, c("run_model",
                    "n_draws",
                    "ncores",
                    "vd",
                    "adjust_matrix",
                    "nets",
                    "save_interval",
                    "new_folder_name",
                    "log_file",
                    "params_sample",
                    "asymp_switch",
                    "reduce_asymp",
                    "reduce_asymp_timing",
                    "reinfection",
                    "reinfection_time",
                    "vaxwane",
                    "vaxwane_p",
                    "vaxwane_f",
                    "post_pride_seeds",
                    "mech_name"), 
              envir = .GlobalEnv)

clusterExport(cl, c("cl"))

# Calculate the appropriate inputs for each core
core_numbers <- seq_len(ncores)
parLapply(cl, 1:min(ncores, 192), function(corenum) run_model(params_sample, corenum, min(ncores, 192), log_file))

lhs_full_run_end = Sys.time()
cat(paste0("Simulations ended at: ", lhs_full_run_end ,"\n"), file = log_file, append = TRUE)
cat(paste0("Total run time: ", lhs_full_run_end - lhs_full_run_start,"\n"), file = log_file, append = TRUE)

# Stop cluster
stopCluster(cl)

