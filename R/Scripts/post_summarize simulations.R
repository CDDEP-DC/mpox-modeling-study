library(here)
library(tidyverse)
library(zoo)

file_list <- list.files(path = here("Output","Mechanism Summaries","Collapsed Runs"), pattern = "\\_sim_output.Rdata$", full.names = F)

for(file in file_list){
  
  # Create new environment
  temp <- new.env();

  # Load item into temp environment
  load(here("Output","Mechanism Summaries","Collapsed Runs",file), envir = temp);
  loaded_item <- ls(envir = temp)
  loaded_item <- get(loaded_item[1], envir = temp)
  
  loaded_item_trackers <- loaded_item[["epi_trackers"]]
  loaded_item_params <- loaded_item[["parameters"]]
  
  # Remove those with stochastic extinction
  seqIDs_to_keep = loaded_item_trackers %>%
    group_by(seqID) %>%
    slice_tail(n = 1) %>%
    dplyr::select(seqID, Cuml_Infs)

  # # Check to see if we added additional travel seeds
  if(grepl("Seeds", file) == TRUE){

    travel_seeds = length(seq(as.Date("2022-10-01"),as.Date("2024-05-01"),by = "1 month") - as.Date("2022-04-01"))

    expected_cases = loaded_item_params %>%
      rowwise() %>%
      mutate(expected_cases = (length(seq(1,30,by=num_seeds)) + (3*10) + travel_seeds)*2)

  }else{
    expected_cases = loaded_item_params %>%
      rowwise() %>%
      mutate(expected_cases = (length(seq(1,30,by=num_seeds)) + (3*10))*2)
  }

  seqIDs_to_keep = left_join(seqIDs_to_keep,expected_cases)

  seqIDs_to_keep = seqIDs_to_keep %>%
    filter(Cuml_Infs > expected_cases) %>%
    dplyr::select(seqID)

  seqIDs_to_keep = c(seqIDs_to_keep$seqID)
  
  simulated_incidence = loaded_item_trackers %>%
    filter(seqID %in% seqIDs_to_keep) %>%
    group_by(seqID) %>%
    mutate(mov_avg = zoo::rollmean(((E_to_I+E_to_A)/10000)*100000, k = 7, fill = 0))
  
  simulated_incidence_summary = simulated_incidence %>%
    group_by(date) %>%
    summarise(median = median(mov_avg, na.rm = TRUE),
              mean = mean(mov_avg, na.rm = TRUE),
              P25 = quantile(mov_avg, probs = 0.25, na.rm = TRUE),
              P75 = quantile(mov_avg, probs = 0.75, na.rm = TRUE),
              P025 = quantile(mov_avg, probs = 0.025, na.rm = TRUE),
              P975 = quantile(mov_avg, probs = 0.975, na.rm = TRUE),
              max = replace(max(mov_avg, na.rm = TRUE), is.infinite(max(mov_avg, na.rm = TRUE)), 0),
              min = replace(min(mov_avg, na.rm = TRUE), is.infinite(min(mov_avg, na.rm = TRUE)), 0))
  
  simulated_detected_cases = loaded_item_trackers %>%
    filter(seqID %in% seqIDs_to_keep) %>%
    group_by(seqID) %>%
    mutate(mov_avg = zoo::rollmean(((E_to_I)/10000)*100000, k = 7, fill = 0))
  
  simulated_detected_cases_summary = simulated_detected_cases %>%
    group_by(date) %>%
    summarise(median = median(mov_avg, na.rm = TRUE),
              mean = mean(mov_avg, na.rm = TRUE),
              P25 = quantile(mov_avg, probs = 0.25, na.rm = TRUE),
              P75 = quantile(mov_avg, probs = 0.75, na.rm = TRUE),
              P025 = quantile(mov_avg, probs = 0.025, na.rm = TRUE),
              P975 = quantile(mov_avg, probs = 0.975, na.rm = TRUE),
              max = replace(max(mov_avg, na.rm = TRUE), is.infinite(max(mov_avg, na.rm = TRUE)), 0),
              min = replace(min(mov_avg, na.rm = TRUE), is.infinite(min(mov_avg, na.rm = TRUE)), 0))
  
  simulated_undetected_cases = loaded_item_trackers %>%
    filter(seqID %in% seqIDs_to_keep) %>%
    group_by(seqID) %>%
    mutate(mov_avg = zoo::rollmean(((E_to_A)/10000)*100000, k = 7, fill = 0))
  
  simulated_undetected_cases_summary = simulated_undetected_cases %>%
    group_by(date) %>%
    summarise(median = median(mov_avg, na.rm = TRUE),
              mean = mean(mov_avg, na.rm = TRUE),
              P25 = quantile(mov_avg, probs = 0.25, na.rm = TRUE),
              P75 = quantile(mov_avg, probs = 0.75, na.rm = TRUE),
              P025 = quantile(mov_avg, probs = 0.025, na.rm = TRUE),
              P975 = quantile(mov_avg, probs = 0.975, na.rm = TRUE),
              max = replace(max(mov_avg, na.rm = TRUE), is.infinite(max(mov_avg, na.rm = TRUE)), 0),
              min = replace(min(mov_avg, na.rm = TRUE), is.infinite(min(mov_avg, na.rm = TRUE)), 0))
  
  monthly_cases = loaded_item_trackers %>%
    group_by(seqID, year = year(date), month = month(date)) %>%
    summarise(monthly_cases_total = (sum(E_to_I + E_to_A, na.rm = TRUE)/10000)*100000,
              monthly_cases_undetected = (sum(E_to_A, na.rm = TRUE)/10000)*100000,
              monthly_cases_detected = (sum(E_to_I, na.rm = TRUE)/10000)*100000) %>%
    ungroup() %>%
    mutate(full_date = ymd(paste(year, month, "01", sep = "-")))
  
  monthly_cases_total_summary = monthly_cases %>%
    filter(seqID %in% seqIDs_to_keep) %>%
    group_by(full_date) %>%
    summarise(median = median(monthly_cases_total, na.rm = TRUE),
              mean = mean(monthly_cases_total, na.rm = TRUE),
              P25 = quantile(monthly_cases_total, probs = 0.25, na.rm = TRUE),
              P75 = quantile(monthly_cases_total, probs = 0.75, na.rm = TRUE),
              P025 = quantile(monthly_cases_total, probs = 0.025, na.rm = TRUE),
              P975 = quantile(monthly_cases_total, probs = 0.975, na.rm = TRUE),
              max = replace(max(monthly_cases_total, na.rm = TRUE), is.infinite(max(monthly_cases_total, na.rm = TRUE)), 0),
              min = replace(min(monthly_cases_total, na.rm = TRUE), is.infinite(min(monthly_cases_total, na.rm = TRUE)), 0))
  
  monthly_cases_undetected_summary = monthly_cases %>%
    filter(seqID %in% seqIDs_to_keep) %>%
    group_by(full_date) %>%
    summarise(median = median(monthly_cases_undetected, na.rm = TRUE),
              mean = mean(monthly_cases_undetected, na.rm = TRUE),
              P25 = quantile(monthly_cases_undetected, probs = 0.25, na.rm = TRUE),
              P75 = quantile(monthly_cases_undetected, probs = 0.75, na.rm = TRUE),
              P025 = quantile(monthly_cases_undetected, probs = 0.025, na.rm = TRUE),
              P975 = quantile(monthly_cases_undetected, probs = 0.975, na.rm = TRUE),
              max = replace(max(monthly_cases_undetected, na.rm = TRUE), is.infinite(max(monthly_cases_undetected, na.rm = TRUE)), 0),
              min = replace(min(monthly_cases_undetected, na.rm = TRUE), is.infinite(min(monthly_cases_undetected, na.rm = TRUE)), 0))
  
  monthly_cases_detected_summary = monthly_cases %>%
    filter(seqID %in% seqIDs_to_keep) %>%
    group_by(full_date) %>%
    summarise(median = median(monthly_cases_detected, na.rm = TRUE),
              mean = mean(monthly_cases_detected, na.rm = TRUE),
              P25 = quantile(monthly_cases_detected, probs = 0.25, na.rm = TRUE),
              P75 = quantile(monthly_cases_detected, probs = 0.75, na.rm = TRUE),
              P025 = quantile(monthly_cases_detected, probs = 0.025, na.rm = TRUE),
              P975 = quantile(monthly_cases_detected, probs = 0.975, na.rm = TRUE),
              max = replace(max(monthly_cases_detected, na.rm = TRUE), is.infinite(max(monthly_cases_detected, na.rm = TRUE)), 0),
              min = replace(min(monthly_cases_detected, na.rm = TRUE), is.infinite(min(monthly_cases_detected, na.rm = TRUE)), 0))
  
  Rt_summary = loaded_item_trackers %>%
    filter(seqID %in% seqIDs_to_keep) %>%
    group_by(date) %>%
    summarise(median = median(Rt_avg, na.rm = TRUE),
              mean = mean(Rt_avg, na.rm = TRUE),
              P25 = quantile(Rt_avg, probs = 0.25, na.rm = TRUE),
              P75 = quantile(Rt_avg, probs = 0.75, na.rm = TRUE),
              P025 = quantile(Rt_avg, probs = 0.025, na.rm = TRUE),
              P975 = quantile(Rt_avg, probs = 0.975, na.rm = TRUE),
              max = replace(max(Rt_avg, na.rm = TRUE), is.infinite(max(Rt_avg, na.rm = TRUE)), 0),
              min = replace(min(Rt_avg, na.rm = TRUE), is.infinite(min(Rt_avg, na.rm = TRUE)), 0))
  
  Cuml_Infs_summary = loaded_item_trackers %>%
    filter(seqID %in% seqIDs_to_keep) %>%
    filter(date == "2024-04-28") %>%
    mutate(Cuml_Infs = (Cuml_Infs/10000 * 100000)) %>%
    summarise(median = median(Cuml_Infs, na.rm = TRUE),
              mean = mean(Cuml_Infs, na.rm = TRUE),
              P25 = quantile(Cuml_Infs, probs = 0.25, na.rm = TRUE),
              P75 = quantile(Cuml_Infs, probs = 0.75, na.rm = TRUE),
              P025 = quantile(Cuml_Infs, probs = 0.025, na.rm = TRUE),
              P975 = quantile(Cuml_Infs, probs = 0.975, na.rm = TRUE),
              max = replace(max(Cuml_Infs, na.rm = TRUE), is.infinite(max(Cuml_Infs, na.rm = TRUE)), 0),
              min = replace(min(Cuml_Infs, na.rm = TRUE), is.infinite(min(Cuml_Infs, na.rm = TRUE)), 0))
  
  
  Cuml_Cases_summary = loaded_item_trackers %>%
    filter(seqID %in% seqIDs_to_keep) %>%
    filter(date == "2024-04-28") %>%
    mutate(Cuml_Cases = (Cuml_Cases/10000 * 100000)) %>%
    summarise(median = median(Cuml_Cases, na.rm = TRUE),
              mean = mean(Cuml_Cases, na.rm = TRUE),
              P25 = quantile(Cuml_Cases, probs = 0.25, na.rm = TRUE),
              P75 = quantile(Cuml_Cases, probs = 0.75, na.rm = TRUE),
              P025 = quantile(Cuml_Cases, probs = 0.025, na.rm = TRUE),
              P975 = quantile(Cuml_Cases, probs = 0.975, na.rm = TRUE),
              max = replace(max(Cuml_Cases, na.rm = TRUE), is.infinite(max(Cuml_Cases, na.rm = TRUE)), 0),
              min = replace(min(Cuml_Cases, na.rm = TRUE), is.infinite(min(Cuml_Cases, na.rm = TRUE)), 0))
  
  
  Frac_S_minus_vwane_summary = loaded_item_trackers %>%
    filter(seqID %in% seqIDs_to_keep) %>%
    group_by(date) %>%
    mutate(num.s = (num.s/10000)) %>%
    summarise(median = median(num.s, na.rm = TRUE),
              mean = mean(num.s, na.rm = TRUE),
              P25 = quantile(num.s, probs = 0.25, na.rm = TRUE),
              P75 = quantile(num.s, probs = 0.75, na.rm = TRUE),
              P025 = quantile(num.s, probs = 0.025, na.rm = TRUE),
              P975 = quantile(num.s, probs = 0.975, na.rm = TRUE),
              max = replace(max(num.s, na.rm = TRUE), is.infinite(max(num.s, na.rm = TRUE)), 0),
              min = replace(min(num.s, na.rm = TRUE), is.infinite(min(num.s, na.rm = TRUE)), 0))
  
  Frac_S_total_summary = loaded_item_trackers %>%
    filter(seqID %in% seqIDs_to_keep) %>%
    group_by(date) %>%
    mutate(total.s = (total.s/10000)) %>%
    summarise(median = median(total.s, na.rm = TRUE),
              mean = mean(total.s, na.rm = TRUE),
              P25 = quantile(total.s, probs = 0.25, na.rm = TRUE),
              P75 = quantile(total.s, probs = 0.75, na.rm = TRUE),
              P025 = quantile(total.s, probs = 0.025, na.rm = TRUE),
              P975 = quantile(total.s, probs = 0.975, na.rm = TRUE),
              max = replace(max(total.s, na.rm = TRUE), is.infinite(max(total.s, na.rm = TRUE)), 0),
              min = replace(min(total.s, na.rm = TRUE), is.infinite(min(total.s, na.rm = TRUE)), 0))
  
  # Step 1: Identify the last date with cases for each simulation
  last_case_dates <- loaded_item_trackers %>%
    filter(seqID %in% seqIDs_to_keep) %>%
    filter(E_to_I > 0 | E_to_A > 0) %>%
    group_by(seqID) %>%
    slice_max(order_by = date, n = 1) %>%
    dplyr::select(seqID, date)
  
  # Exclude simulations that end on the last date of the simulation period
  end_date <- as.Date("2024-04-28")
  last_case_dates <- last_case_dates %>%
    filter(date != end_date)
  
  # Step 2: Create a full date range and initialize die-out counts
  all_dates <- data.frame(date = seq.Date(from = as.Date("2022-04-01"), to = end_date, by = "day"))
  
  # Step 3: Count the number of runs that die out on each day
  daily_die_outs <- last_case_dates %>%
    group_by(date) %>%
    summarise(daily_die_outs = n())
  
  # Step 4: Merge with all dates to ensure the entire span is covered
  daily_die_outs <- left_join(all_dates, daily_die_outs, by = "date") 
  daily_die_outs <- daily_die_outs %>% replace_na(list(daily_die_outs = 0))
  
  # Step 5: Calculate the cumulative number of runs that have died out up to each date
  daily_die_outs <- daily_die_outs %>%
    arrange(date) %>%
    mutate(cumulative_die_outs = cumsum(daily_die_outs))
  
  # # Step 6: Fill forward the last available cumulative die-out value if there are no new die-outs on a particular date
  # daily_die_outs <- daily_die_outs %>%
  #   mutate(cumulative_die_outs = zoo::na.locf(cumulative_die_outs, na.rm = FALSE))
  # 
  # Step 7: Calculate the proportion of total runs that have died out up to each date
  total_runs <- length(unique(simulated_incidence$seqID)) #length((seqIDs_to_keep))
  daily_die_outs <- daily_die_outs %>%
    mutate(proportion_die_outs = cumulative_die_outs / total_runs * 100)
  
  # # Ensure the proportion is 100% for all dates after the runs have died out
  # daily_die_outs <- daily_die_outs %>%
  #   mutate(proportion_die_outs = if_else(cumulative_die_outs == total_runs, 100, proportion_die_outs))
  
  summary = list(incidence_per_run = simulated_incidence,
                 summarized_incidence = simulated_incidence_summary,
                 summarized_incidence_detected = simulated_detected_cases_summary,
                 summarized_incidence_undetected = simulated_undetected_cases_summary,
                 simulated_detected_cases = simulated_detected_cases,
                 simulated_undetected_cases = simulated_undetected_cases,
                 monthly_cases_per_run = monthly_cases,
                 monthly_total_cases_summary = monthly_cases_total_summary,
                 monthly_undetected_cases_summary = monthly_cases_undetected_summary,
                 monthly_detected_cases_summary = monthly_cases_detected_summary,
                 Rt_summary = Rt_summary,
                 Cuml_Infs_summary = Cuml_Infs_summary,
                 Cuml_Cases_summary = Cuml_Cases_summary,
                 Frac_S_minus_vwane_summary = Frac_S_minus_vwane_summary,
                 Frac_S_total_summary = Frac_S_total_summary,
                 last_case_dates = last_case_dates,
                 daily_die_outs = daily_die_outs,
                 parameters = loaded_item_params)
  
  new_name <- sub("\\_sim_output.Rdata$", "", file)

  assign(paste0(new_name,"_sim_summary"), summary)
  
  save(list = paste0(new_name,"_sim_summary"), file = here("Output","Mechanism Summaries","Summaries",paste0(new_name, "_sim_summary.Rdata")));

}
