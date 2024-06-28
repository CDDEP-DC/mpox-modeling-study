# LHS SAMPLER
library(lhs)
library(here)
library(parallel)
library(tidyverse)
library(EpiModel)
library(EpiModelHIV)

# Capture command-line arguments
args <- commandArgs(trailingOnly = TRUE)

# Check if a city name argument is provided
if (length(args) == 0) {
  stop("No city name provided. Usage: Rscript LHS_simulations_2.R <city_name>")
}

# Assign the first argument to city_name variable
city_name <- args[1]

# Set seed to generate LHS;
set.seed(123)
lhs_raw = randomLHS(10000, 3)
inf_prob = qunif(lhs_raw[,1], min = 0.1, max = 0.99)
seeds = round(qunif(lhs_raw[,2], min = 1, max = 7),0)
detect_prob = qunif(lhs_raw[,3], min = 0.1, max = 0.99)

# Generate data frame with the LHS draws;
LHS_samples = data.frame(inf_prob,seeds,detect_prob)

#### LOAD OTHER RELEVANT DATA;
if(city_name == "CHI"){
  load(here("mpox","R", "Model", "adjust_matrix_CHI.Rdata"))
  load(here("mpox","R", "Model", "vaxDataFit_CHI_new.Rdata"))
}else if(city_name == "NYC"){
  load(here("mpox","R", "Model", "adjust_matrix_NYC.Rdata"))
  load(here("mpox","R", "Model", "vaxDataFit_NYC_new.Rdata"))
}else if(city_name == "SF"){
  load(here("mpox","R", "Model", "adjust_matrix_SF.Rdata"))
  load(here("mpox","R", "Model", "vaxDataFit_SF_new.Rdata"))
}


load(here("mpox","R", "Network Objects", "mpx_network_object.rda"))

# Prepare imported items -----
nets <- est
rm(est)


######### DEFINE RUN LHS FUNCTION

run_LHS <- function(LHS_samples, corenum, ncores, log_file){
  
  # Calculate the range of rows each core should process
  total_samples <- nrow(LHS_samples)
  rows_per_core <- total_samples %/% ncores
  remainder <- total_samples %% ncores
  
  if (corenum == ncores) {
    start_row <- (corenum - 1) * rows_per_core + 1
    end_row <- total_samples
  } else {
    start_row <- (corenum - 1) * rows_per_core + 1
    end_row <- corenum * rows_per_core
  }
  
  LHS_samples_subsample <- LHS_samples[start_row:end_row, ]
  
  num_rows <- end_row - start_row + 1
  
  LHS_runs <- list(Params = LHS_samples_subsample,
                   Flow_S_E = list(),
                   Flow_E_I = list(),
                   Flow_E_A = list(),
                   Flow_I_R = list(),
                   FLow_A_R = list(),
                   Cuml_Inf = list(),
                   Cuml_Cases = list(),
                   Num_p = list());

  log_file <- here("mpox","Output","LHS",new_folder_name,paste0("LHS_log_", corenum, ".txt"))
  
  cat(paste0("Core Number: ", corenum), file = log_file, append = TRUE)
  cat("\n", file = log_file, append = TRUE)  # Add a newline for readability
  cat("Starting LHS Runs...", file = log_file, append = TRUE)
  cat("\n", file = log_file, append = TRUE)  # Add a newline for readability
  
  for(i in 1:num_rows){
    start_time <- Sys.time()
    
    if (i %% save_interval == 0) {
      # Update file name to include chain ID and iteration number
      new_name_i = paste0("LHS_runs_corenum_",corenum);
      assign(new_name_i, LHS_runs)
      save(list = new_name_i, file = here("mpox","Output","LHS",new_folder_name,paste0("LHS_subsample_", corenum, ".RData")))
      cat("* Saved output up to iteration", i, "for subsample number", corenum, file = log_file, append = TRUE)
      cat("\n", file = log_file, append = TRUE)  # Add a newline for readability
    }
    
    params_i = LHS_samples_subsample[i,]
    
    prob_infection_i = as.numeric(params_i[1])
    seeding_pre_pride_i = as.numeric(params_i[2])
    detect_prob_i = as.numeric(params_i[3])
    reduce_asymp_i = FALSE
    reduce_asymp_timing_i = NULL
    
    source(here("mpox","R", "Model", "network_modules.R"))
    
    params <- param_msm(prob_infection_i,
                        seeding_pre_pride_i,
                        detect_prob_i,
                        reduce_asymp_i,
                        reduce_asymp_timing_i) 
    
    inits <- init_msm();
    controls <- control_msm();
    
    # This function should compute the NLL for the provided parameters 'params'
    sim_i = netsim(nets, params, inits, controls);
    
    incidence_e = sim_i[["epi"]][["se.flow"]][["sim1"]]
    incidence_i = sim_i[["epi"]][["ei.flow"]][["sim1"]]
    incidence_a = sim_i[["epi"]][["ea.flow"]][["sim1"]]
    flow_i_to_r = sim_i[["epi"]][["ir.flow"]][["sim1"]]
    flow_a_to_r = sim_i[["epi"]][["ar.flow"]][["sim1"]]
    num_partners = sim_i[["epi"]][["num.one.time.p"]][["sim1"]]
    cuml_inf = sim_i[["epi"]][["cuml.infs"]][["sim1"]]
    cuml_cases = sim_i[["epi"]][["cuml.cases"]][["sim1"]]
    
    LHS_runs[["Flow_S_E"]][[i]] = incidence_e
    LHS_runs[["Flow_E_I"]][[i]] = incidence_i
    LHS_runs[["Flow_E_A"]][[i]] = incidence_a
    LHS_runs[["Flow_I_R"]][[i]] = flow_i_to_r
    LHS_runs[["Flow_A_R"]][[i]] = flow_a_to_r
    LHS_runs[["Num_p"]][[i]] = num_partners
    LHS_runs[["Cuml_Inf"]][[i]] = cuml_inf
    LHS_runs[["Cuml_Cases"]][[i]] = cuml_cases;
    
    end_time <- Sys.time()
    elapsed_time <- end_time - start_time
    elapsed_time <- round(elapsed_time,2)
    cat(paste0("Core Num: ", corenum, " | Run number: ", i," | Runtime: ", elapsed_time), file = log_file, append = TRUE)
    cat("\n", file = log_file, append = TRUE)  # Add a newline for readability
  }
  
  cat("LHS draws complete.\n", file = log_file, append = TRUE)
  cat("\n", file = log_file, append = TRUE)  # Add a newline for readability
  new_name_i = paste0("LHS_runs_corenum_",corenum);
  assign(new_name_i, LHS_runs)
  save(list = new_name_i, file = here("mpox","Output","LHS",new_folder_name,paste0("LHS_subsample_", corenum, ".RData")))
}


n_draws <- 10000

ncores_total <- 80
ncores <- min(ncores_total, 192)  # Use up to 192 cores, adjust if needed
save_interval <- 10
ncounter = 1

new_folder_name = paste0("LHS Run - ",city_name," - ", Sys.Date())

dir.create(here("mpox","Output","LHS",new_folder_name), recursive = FALSE)

save(LHS_samples, file = here("mpox","Output","LHS",new_folder_name,paste0("LHS_Samples.RData")))

log_file <- here("mpox","Output","LHS",new_folder_name,paste0("run_details", ".txt"))

lhs_full_run_start = Sys.time()
cat("LHS Runs \n", file = log_file, append = TRUE)
cat(paste0("Number of draws: ", n_draws, "\n"), file = log_file, append = TRUE)
cat(paste0("Number of cores: ", ncores, "\n"), file = log_file, append = TRUE)
cat(paste0("LHS simulations started at: ", lhs_full_run_start ,"\n"), file = log_file, append = TRUE)

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
clusterExport(cl, c("run_LHS",
                    "n_draws",
                    "ncores",
                    "vd",
                    "adjust_matrix",
                    "nets",
                    "save_interval",
                    "new_folder_name",
                    "LHS_samples",
                    "log_file"), 
              envir = .GlobalEnv)

clusterExport(cl, c("cl"))

# Calculate the appropriate inputs for each core
core_numbers <- seq_len(ncores)
parLapply(cl, 1:min(ncores, 192), function(corenum) run_LHS(LHS_samples, corenum, min(ncores, 192), log_file))

lhs_full_run_end = Sys.time()
cat(paste0("LHS simulations end at: ", lhs_full_run_end ,"\n"), file = log_file, append = TRUE)
cat(paste0("Total run time: ", lhs_full_run_end - lhs_full_run_start,"\n"), file = log_file, append = TRUE)

# Stop cluster
stopCluster(cl)