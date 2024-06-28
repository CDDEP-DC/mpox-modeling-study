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

######| Loads files into your environment ##############
# Define the folder path using here package
# May need to change depending on your set-up
# Also this will be repeated for each city...

cities = c("CHI","NYC","SF")
#city_code <- "CHI"


for(city in cities){
  
  
  folder_path <- here("Output", "LHS", "New Runs", paste0("LHS Run - ", city ," - 2024-06-01"), "LHS Runs")
  
  # Create a vector with all the file names
  # And then load in each file into the environment
  file_list <- list.files(path = folder_path, pattern = "\\.RData$", full.names = F)
  
  for (file_i in 1:length(file_list)) {
    load(here(folder_path, file_list[file_i]))
  }
  
  obj_names <- paste0("LHS_runs_corenum_", 1:80)   # Create a vector with the obj names
  
  ######| Loop through and pull data out ##############
  # Initialize the required vectors for the data frame
  niters <- 10000
  num_timesteps <- 183
  total_rows <- niters * num_timesteps # Total runs x number of time steps per run
  
  # Initialize a data.table
  incidence <- data.table(
    seqID = integer(total_rows),
    S_to_E = numeric(total_rows),
    E_to_I = numeric(total_rows),
    E_to_A = numeric(total_rows),
    Cuml_Infs = numeric(total_rows),
    Cuml_Cases = numeric(total_rows),
    timestep = rep(1:183, niters),
    date = as.Date(rep(seq(as.Date("2022-04-01"), as.Date("2022-09-30"), by = 1), niters))
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
    num_runs <- length(obj[["Flow_E_I"]])
    
    # Access the relevant data from each nested list
    S_to_E <- obj[["Flow_S_E"]][1:num_runs]   # Num of ppl who become exposed (i.e., infected not yet infectious) @ time t
    E_to_I <- obj[["Flow_E_I"]][1:num_runs]   # Num of ppl who move from exposed to infected @ time t
    E_to_A <- obj[["Flow_E_A"]][1:num_runs]   # Num of ppl who move from exposed to asymptomatic/undetected @ time t
    I_to_R <- obj[["Flow_I_R"]][1:num_runs]   # Num of ppl who move from exposed to asymptomatic/undetected @ time t
    A_to_R <- obj[["Flow_A_R"]][1:num_runs]   # Num of ppl who move from exposed to asymptomatic/undetected @ time t
    Num_p <- obj[["Num_p"]][1:num_runs]   # Num of ppl who move from exposed to asymptomatic/undetected @ time t
    Cuml_Infs <- obj[["Cuml_Inf"]][1:num_runs] # Total ppl who have moved into E by time t
    Cuml_Cases <- obj[["Cuml_Cases"]][1:num_runs] # Total ppl who have moved into I by time t
    
    prob.I_j <- obj[["Params"]][,'inf_prob']
    num_seeds_j <- obj[["Params"]][,'seeds']
    prob.U_j <- obj[["Params"]][,'detect_prob']
    
    parameters[seqID_j:(seqID_j+num_runs - 1), `:=`(
      seqID = seqID_j:(seqID_j+num_runs - 1),
      prob.I = prob.I_j,
      num_seeds = num_seeds_j,
      prob.U = prob.U_j
    )]
    
    # Loop through each incidence vector in incidence_i and assign seqID
    for (j in 1:num_runs) {
      
      S_to_E_j <- S_to_E[[j]]
      E_to_I_j <- E_to_I[[j]]
      E_to_A_j <- E_to_A[[j]]
      
      I_to_R_j <- I_to_R[[j]]
      A_to_R_j <- A_to_R[[j]]
      
      Cuml_Infs_j <- Cuml_Infs[[j]]
      Cuml_Cases_j <- Cuml_Cases[[j]]
      
      Num_p_j <- Num_p[[j]]
      
      
      incidence[index:(index + num_timesteps - 1), `:=`(
        seqID = seqID_j,
        S_to_E = S_to_E_j,
        E_to_I = E_to_I_j,
        E_to_A = E_to_A_j,
        I_to_R = I_to_R_j,
        A_to_R = A_to_R_j,
        Cuml_Infs = Cuml_Infs_j,
        Cuml_Cases = Cuml_Cases_j,
        Num_p = Num_p_j
      )]
      
      seqID_j <- seqID_j + 1
      index <- index + num_timesteps
    }
  }
  
  save(incidence, file = here("Output","Fit Summaries",paste0("LHS_", city, "_output_incidence.Rdata")));
  save(parameters, file = here("Output","Fit Summaries",paste0("LHS_", city, "_output_params.Rdata")))
  
}





