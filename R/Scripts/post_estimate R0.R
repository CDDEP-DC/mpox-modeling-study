## Estimate the R0 for each run

list_files = list.files(here("Output", "Selected Fits"))

for(file in list_files){
  load(here("Output", "Selected Fits", paste0(file)))
  
  # Initialize an empty list to store data frames
  dfs <- list()
  
  # Loop through each simulation run and extract data
  for (i in 1:100) {
    simobject = get(paste0(gsub("\\.RData$", "", file)))
    sim_obj_name = paste0(paste0(gsub("\\.RData$", "", file)))
    sim_name <- paste0("sim", i)
    
    df <- data.frame(node_ID = simobject[["attr"]][[sim_name]][["unique_id"]],
                     generation = simobject[["attr"]][[sim_name]][["infGen"]],
                     time_recovered = simobject[["attr"]][[sim_name]][["recTime"]],
                     secondary_infections = simobject[["attr"]][[sim_name]][["secInfs"]],
                     simrun = sim_name)
    # Append to the list
    dfs[[i]] <- df
  }
  
  # Combine all data frames into a single data frame
  all_data <- do.call(rbind, dfs)
  
  all_data <- all_data %>%
    filter(!is.na(generation))

  R0_full = all_data %>%
    group_by(simrun, generation) %>%
    reframe(mean_sec_inf = mean(secondary_infections),
            max_recovery = max(time_recovered))
  
  R0_full$city = sub("sim_([^_]*)_.*", "\\1",  file)
  R0_full$scene = sub("sim_[^_]*_([^_]*)_.*", "\\1", file)
  save(R0_full, file=here("Output","R0 Estimates",paste0("RO_all_runs_", "All Runs R Data Files", sim_obj_name, ".Rdata")))
  
  R0_full = R0_full %>%
    filter(generation <= 2) %>%
    summarise(
      Median = median(mean_sec_inf, na.rm = TRUE),
      Mean = mean(mean_sec_inf, na.rm = TRUE),
      Q2_5 = quantile(mean_sec_inf, 0.025, na.rm = TRUE),
      Q25 = quantile(mean_sec_inf, 0.25, na.rm = TRUE),
      Q75 = quantile(mean_sec_inf, 0.75, na.rm = TRUE),
      Q97_5 = quantile(mean_sec_inf, 0.975, na.rm = TRUE),
      max_recovery = max(max_recovery, na.rm = TRUE)
    )
  
  write.csv(R0_full, here("Output","R0 Estimates",paste0("RO_summary_", sim_obj_name, ".csv")))
}

