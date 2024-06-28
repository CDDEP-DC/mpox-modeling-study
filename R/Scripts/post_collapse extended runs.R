##      This code block reads in the runs from the LHS and calculates the NLL
##      By: Nodar Kipshidze
##      Last updated: May 17, 2024
# -----------------------------------------------------------------------------

library(here)
library(tidyverse)
library(data.table)
library(MASS)
library(pracma)  # For peak detection
library(ConnMatTools)
library(dtw)
library(gghalves)

######| Loads files into your environment ##############
# Define the folder path using here package
# May need to change depending on your set-up
# Also this will be repeated for each city...

folder_paths <- list.dirs(path = here("Output","AWS Runs"), full.names = TRUE, recursive = FALSE)

for(folder in folder_paths){
    
    # Create a vector with all the file names
    # And then load in each file into the environment
    file_list <- list.files(path = folder, pattern = "\\.RData$", full.names = F)
    
    city_code <- sub(".*- ([A-Z]+)$", "\\1", folder)
    
    for (file_i in 1:length(file_list)) {
      load(here(folder, file_list[file_i]))
    }
    
    obj_names <- paste0("Runs_corenum_", 1:100)   # Create a vector with the obj names
    
    
    ######| Loop through and pull data out ##############
    # Initialize the required vectors for the data frame
    niters <- 1000
    num_timesteps <- 759
    total_rows <- niters * num_timesteps # Total runs x number of time steps per run
    
    # Initialize a data.table
    epi_trackers <- data.table(
      seqID = integer(total_rows),
      S_to_E = numeric(total_rows),
      E_to_I = numeric(total_rows),
      E_to_A = numeric(total_rows),
      Cuml_Infs = numeric(total_rows),
      Cuml_Cases = numeric(total_rows),
      num.s = numeric(total_rows),
      total.s = numeric(total_rows),
      Rt_avg = numeric(total_rows),
      timestep = rep(1:759, niters),
      date = as.Date(rep(seq(as.Date("2022-04-01"), as.Date("2024-04-28"), by = 1), niters))
    )
    
    parameters <- data.table(
      seqID = integer(niters),
      prob.I = numeric(niters),
      num_seeds = integer(niters),
      prob.U = numeric(niters)
    )
    
    seqID_j <- 1
    index <- 1
    
    # Loop through each object name and access the required items
    for (obj_i in obj_names) {
      
      # Dynamically get the object by name
      obj <- get(obj_i)
      num_runs <- length(obj[["Trackers"]])
      
      prob.I_j <- obj[["Params"]][,'prob.I']
      num_seeds_j <- obj[["Params"]][,'num_seeds']
      prob.U_j <- obj[["Params"]][,'prob.U']
      
      parameters[seqID_j:(seqID_j+num_runs - 1), `:=`(
        seqID = seqID_j:(seqID_j+num_runs - 1),
        prob.I = prob.I_j,
        num_seeds = num_seeds_j,
        prob.U = prob.U_j
      )]
      
      for(run_i in 1:num_runs){
        # Access the relevant data from each nested list
        S_to_E_j <- obj[["Trackers"]][[run_i]][["se.flow"]][["sim1"]]   # Num of ppl who become exposed (i.e., infected not yet infectious) @ time t
        E_to_I_j <- obj[["Trackers"]][[run_i]][["ei.flow"]][["sim1"]]   # Num of ppl who move from exposed to infected @ time t
        E_to_A_j <- obj[["Trackers"]][[run_i]][["ea.flow"]][["sim1"]]   # Num of ppl who move from exposed to asymptomatic/undetected @ time t
        Cuml_Infs_j <- obj[["Trackers"]][[run_i]][["cuml.infs"]][["sim1"]] # Total ppl who have moved into E by time t
        Cuml_Cases_j <- obj[["Trackers"]][[run_i]][["cuml.cases"]][["sim1"]] # Total ppl who have moved into I by time t
        Num_s_j <- obj[["Trackers"]][[run_i]][["num.s"]][["sim1"]] # Total ppl who have moved into I by time t
        Num_tot_s_j <- obj[["Trackers"]][[run_i]][["total.s"]][["sim1"]] # Total ppl who have moved into I by time t
        Rt_avg_j <- obj[["Trackers"]][[run_i]][["rt.avg"]][["sim1"]] # Total ppl who have moved into I by time t
        
        epi_trackers[index:(index + num_timesteps - 1), `:=`(
          seqID = seqID_j,
          S_to_E = S_to_E_j,
          E_to_I = E_to_I_j,
          E_to_A = E_to_A_j,
          Cuml_Infs = Cuml_Infs_j,
          Cuml_Cases = Cuml_Cases_j,
          num.s = Num_s_j,
          total.s = Num_tot_s_j,
          Rt_avg = Rt_avg_j
        )]
        
        seqID_j <- seqID_j + 1
        index <- index + num_timesteps
        
      }
    }

    new_name <- sub(".*/", "", folder)
    new_name <- sub(" - $", "", new_name)
    new_name <- gsub(" *\\+ *", "+", new_name)
    new_name <- gsub(" ", "_", new_name)
    new_name <- paste0(new_name)
    
    sim_output <- list(epi_trackers = epi_trackers,
                       parameters = parameters)
    
    assign(paste0("sim_output",new_name), sim_output)
    
    save(list = paste0("sim_output",new_name), file = here("Output","Mechanism Summaries","Collapsed Runs",paste0(new_name, "_sim_output.Rdata")));

}
