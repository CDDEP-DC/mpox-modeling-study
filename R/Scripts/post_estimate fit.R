##      This code block reads in the runs from the LHS and calculates the NLL
##      By: Nodar Kipshidze
##      Last updated: May 22, 2024
# -----------------------------------------------------------------------------

library(here)
library(tidyverse)
library(data.table)
library(MASS)
library(pracma)  # For peak detection
library(ConnMatTools)
library(dtw)

######| Calculate negative log-likelihood ##############

# Load the case data
## NYC <- 225235.80
## CHI <- 131704
## SF <- 69643

cities = c("CHI","NYC","SF")

for(city_code in cities){
  if(city_code == "NYC"){
    
    load(here("Output","Fit Summaries","LHS_NYC_output_incidence.Rdata"));
    load(here("Output","Fit Summaries","LHS_NYC_output_params.Rdata"));
    
    city_denom <- 225235.80     # The estimated number of MSM for the given city
    
    ### NYC 
    observed_cases <- read.csv(here("Data","Cities","New York City","cases_NYC.csv"))
    
    observed_cases$DATE <- as.Date(observed_cases$DATE)
    
    observed_cases <- observed_cases %>%
      mutate(DATE = DATE - 7)
    
    observed_cases$normalized <- (observed_cases$NEW_CASES / city_denom) * 100000
    
    observed_cases_fitting <- observed_cases %>%
      filter(DATE <= as.Date("2022-09-30") & DATE >= as.Date("2022-04-01"))
    
  }else if(city_code == "SF"){
    
    load(here("Output","Fit Summaries","LHS_SF_output_incidence.Rdata"));
    load(here("Output","Fit Summaries","LHS_SF_output_params.Rdata"));
    
    city_denom <- 69643     # The estimated number of MSM for the given city
    
    ### SF
    observed_cases <- read.csv(here("Data","Cities","San Francisco","cases_SF_current.csv"))
    
    observed_cases$DATE <- as.Date(observed_cases$DATE)
    
    # Shift the DATE column back by 7 days
    observed_cases <- observed_cases %>%
      mutate(DATE = DATE - 7)
    
    observed_cases$normalized <- (observed_cases$NEW_CASES / city_denom) * 100000
    
    observed_cases_fitting <- observed_cases %>%
      filter(DATE <= as.Date("2022-09-30") & DATE >= as.Date("2022-04-01"))
    
  }else if(city_code == "CHI"){
    
    load(here("Output","Fit Summaries","LHS_CHI_output_incidence.Rdata"));
    load(here("Output","Fit Summaries","LHS_CHI_output_params.Rdata"));
    
    # NOTE: Chicago reports on a weekly basis ... more on that below
    city_denom <- 131704     # The estimated number of MSM for the given city
    
    ### CHI
    observed_cases <- read.csv(here("Data","Cities","Chicago","cases_chicago.csv"))
    observed_cases$DATE <- as.Date(observed_cases$DATE)
    
    observed_cases <- observed_cases %>%
      mutate(DATE = DATE - 7)
    
    observed_cases$normalized <- (observed_cases$NEW_CASES / city_denom) * 100000
    
    observed_cases_fitting <- observed_cases %>%
      filter(DATE <= as.Date("2022-09-30") & DATE >= as.Date("2022-04-01"))
  }
  
  # Transform our observed cases to a moving average and b/c of how it is being fitted, we are adding a very small value (1e-6) to anything that is recorded as 0
  
  observed_cases_vec <- observed_cases_fitting$normalized
  observed_cases_vec[observed_cases_vec == 0] <- 1e-6
  observed_cases_vec <- observed_cases_vec
  
  # Create 7-day moving average for NYC and SF or 2-week moving average for CHI
  ifelse(city_code %in% c("NYC","SF"), 
         observed_cases_vec_ma <- zoo::rollmean(observed_cases_vec, k = 7, fill = 1e-6), 
         observed_cases_vec_ma <- zoo::rollmean(observed_cases_vec, k = 2, fill = 1e-6))
  
  observed_cases_vec_cuml <- cumsum(observed_cases_vec)
  
  
  #Define weights (e.g., higher weights for higher case counts)
  
  calculate_dtw_weights <- function(predicted_cases_ma, observed_cases_vec_ma) {
    
    # Calculate DTW alignment
    alignment <- dtw(predicted_cases_ma, observed_cases_vec_ma, keep = TRUE)
    weights <- alignment$distance
    weights <- rep(weights,length(observed_cases_vec_ma))
    return(weights)
    
  }
  
  # Pre-allocate a vector to store the NLL for each run
  num_runs <- 10000  # Number of runs
  nll_values <- numeric(num_runs)
  
  # Loop through each run to calculate the NLL
  for (run in 1:num_runs) {
    
    # Extract the predicted cases for the current run
    predicted_cases <- incidence[seqID == run, E_to_I]
    predicted_cuml_cases <- incidence[seqID == run, Cuml_Cases]
    
    if(city_code == "CHI"){
      
      # Incidence
      predicted_cases = as.data.frame(predicted_cases)
      
      predicted_cases = predicted_cases %>%
        mutate(dates = as.Date(seq(as.Date("2022-04-01"),
                                   as.Date("2022-09-30"),
                                   by = 1)),
               week_start = floor_date(dates, unit = "week", week_start = 6))
      
      predicted_cases = predicted_cases %>%
        group_by(week_start) %>%
        summarize(weekly_count = sum(predicted_cases, na.rm = TRUE))      
      
      predicted_cases = predicted_cases %>%
        filter(week_start < as.Date("2022-09-29") & week_start > as.Date("2022-03-29"))
      
      predicted_cases = c(predicted_cases$weekly_count)
      
      # Cumulative Cases
      predicted_cuml_cases = as.data.frame(predicted_cuml_cases)
      
      predicted_cuml_cases = predicted_cuml_cases %>%
        mutate(dates = as.Date(seq(as.Date("2022-04-01"),
                                   as.Date("2022-09-30"),
                                   by = 1)),
               week_start = floor_date(dates, unit = "week", week_start = 6))
      
      predicted_cuml_cases = predicted_cuml_cases %>%
        filter(week_start < as.Date("2022-09-30") & week_start > as.Date("2022-03-29"))
      
      predicted_cuml_cases = predicted_cuml_cases %>%
        group_by(week_start) %>%
        summarize(weekly_count = last(predicted_cuml_cases, na.rm = TRUE))
      
      predicted_cuml_cases = c(predicted_cuml_cases$weekly_count)
      
    }
    
    
    # predicted_cases <- c(predicted_cases, rep(0, 7))
    # predicted_cuml_cases <- c(predicted_cuml_cases, rep(0, 7))
    # predicted_cases <- predicted_cases[1:(length(predicted_cases)-7)]
    # predicted_cuml_cases <- predicted_cuml_cases[1:(length(predicted_cuml_cases)-7)]
    
    #Skip runs with no significant cases (e.g., stochastic extinction)
    expected_cases <- (length(seq(1,30,parameters[seqID == run, num_seeds])) + 30) * 2
    
    if (all(predicted_cuml_cases <= expected_cases)) {
      nll_values[run] <- NA  # Assign NA for these runs
      next
    }
    
    predicted_cases <- (predicted_cases / 10000) * 100000 # Normalize to cases per 100,000
    predicted_cases[predicted_cases == 0] <- 1e-6
    predicted_cases[is.na(predicted_cases)] <- 1e-6  # Replace NA values with 1e-6
    
    predicted_cuml_cases <- (predicted_cuml_cases / 10000) * 100000 # Normalize to cases per 100,000
    predicted_cuml_cases[predicted_cuml_cases == 0] <- 1e-6
    predicted_cuml_cases[is.na(predicted_cuml_cases)] <- 1e-6  # Replace NA values with 1e-6
    
    # Calculate moving average for predicted cases for SF and NYC
    # Since Chicago is reporting on a 7 day 
    ifelse(city_code %in% c("NYC","SF"), 
           predicted_cases_ma <- zoo::rollmean(predicted_cases, k = 7, fill = 1e-6),
           predicted_cases_ma <- predicted_cases) #zoo::rollmean(predicted_cases, k = 2, fill = 1e-6))
    
    # CALCULATE NLL based on moving average of incidence
    weights_valid <- calculate_dtw_weights(predicted_cases_ma, observed_cases_vec_ma)
    
    # PENALTY for instances where incidence is way higher
    penalty_factor <- 1.5 # Adjust the penalty factor as needed
    
    # Calculate the NLL using Gaussian distribution with moving averages
    valid_indices <- !is.na(observed_cases_vec_ma) & !is.na(predicted_cases_ma)
    obs_valid <- observed_cases_vec_ma[valid_indices]
    pred_valid <- predicted_cases_ma[valid_indices]
    
    # Calculate the NLL using the Gamma distribution
    nll_incidence <- 0
    for (i in 1:length(obs_valid)) {
      shape_params <- gammaParamsConvert(mean = obs_valid[i], sd = sqrt(obs_valid[i]))  # Assuming variance ~ mean for simplicity
      shape <- shape_params$shape
      scale <- shape_params$scale
      nll_incidence <- nll_incidence - ((dgamma(obs_valid[i], shape = shape, scale = scale * pred_valid[i], log = TRUE)) * (1/weights_valid[i]))
      
      # # Apply penalty if the predicted cumulative incidence exceeds twice the observed cumulative incidence
      # if (pred_valid[i] > 2 * obs_valid[i] | pred_valid[i] < 2 * obs_valid[i]) {
      #   nll_incidence <- nll_incidence * penalty_factor
      # }
    }
    
    # CALCULATE NLL based on cumulative case counts
    valid_indices_cumulative <- !is.na(observed_cases_vec_cuml) & !is.na(predicted_cuml_cases)
    obs_valid_cumulative <- observed_cases_vec_cuml[valid_indices_cumulative]
    pred_valid_cumulative <- predicted_cuml_cases[valid_indices_cumulative]
    
    
    nll_cumulative <- 0
    
    for (i in 1:length(obs_valid)) {
      sd_obs <- sqrt(obs_valid_cumulative[i])  # Assuming variance ~ mean for simplicity
      
      # Apply penalty if the predicted cumulative incidence exceeds twice the observed cumulative incidence
      if (pred_valid_cumulative[i] > 2 * obs_valid_cumulative[i] | pred_valid_cumulative[i] < 2 * obs_valid_cumulative[i]) {
        nll_cumulative <- nll_cumulative - (penalty_factor * dnorm(obs_valid_cumulative[i], mean = obs_valid_cumulative[i], sd = sd_obs * pred_valid_cumulative[i], log = TRUE))
      }else{
        nll_cumulative <- nll_cumulative - dnorm(obs_valid_cumulative[i], mean = obs_valid_cumulative[i], sd = sd_obs * pred_valid_cumulative[i], log = TRUE)
        
      }
    }
    
    if(is.na(nll_incidence) == TRUE){
      nll_values[run] <- NA
      next
    }else if(is.na(nll_cumulative) == TRUE){
      nll_values[run] <- NA
      next
    }else{
      # Combine the NLLs
      nll_values[run] <- nll_incidence + nll_cumulative
    }
    
  }
  
  # Merge the parameter values sampled and the NLL values
  model_fit_assess = cbind(parameters,nll_values)
  
  # Remember to update the suffixes for these objects so it's named as the correct city
  # Create a list object with all our data
  
  fitted_data = list(fit_summaries = model_fit_assess,
                     simulated_runs = incidence,
                     observed_data = observed_cases_fitting,
                     normalized_sim_incidence = data.frame(
                     seqID = incidence$seqID,
                     timestep = incidence$timestep,
                     date = incidence$date,
                     incidence = (incidence$E_to_I / 10000) * 100000))
  
  new_name <- paste0("fitted_data_",city_code);
  
  assign(new_name, fitted_data);
  
  save(list = new_name,
       file = here("Output","Fit Summaries",paste0("fit_LHS_",city_code,".Rdata")))
}



