##      This code block takes the estimated NLLs for each run and preforms some simple data manipulation to assess the fit of the models
##      By: Nodar Kipshidze
##      Last updated: May 17, 2024
# -----------------------------------------------------------------------------

library(tidyverse)
library(ggdist)
library(data.table)
library(MASS)
library(ConnMatTools)

cities = c("CHI","NYC","SF")

#select_top_p <- 0.10
#percentiles = c(0.10,0.05,0.01);
percentiles = c(0.10)

for(city_code in cities){
  for(select_top_p in percentiles){
    if(city_code == "CHI"){
      load(here("Output","Fit Summaries","fit_LHS_CHI.Rdata"))
      
      model_fit_assess = fitted_data_CHI[[1]]
      
      model_fit_assess_filtered <- model_fit_assess[!is.na(nll_values)]
      
      model_fit_assess_filtered <- model_fit_assess_filtered %>%
        arrange(nll_values)
      
      top_percent_index <- floor(select_top_p*nrow(model_fit_assess_filtered))
      top_percent_index <- model_fit_assess_filtered[1:top_percent_index]
      
      simulated_incidence = fitted_data_CHI[[4]]
      
      simulated_incidence = simulated_incidence %>%
        filter(seqID %in% unique(top_percent_index$seqID))
      
      simulated_incidence = simulated_incidence %>%
        mutate(date = floor_date(date, unit = "week", week_start = 6))
      
      simulated_incidence = simulated_incidence %>%
        group_by(seqID, date) %>%
        summarize(weekly_count = sum(incidence, na.rm = TRUE))      
      
      simulated_incidence = simulated_incidence %>%
        filter(date < as.Date("2022-09-29") & date > as.Date("2022-03-29"))
      
      simulated_incidence = simulated_incidence %>%
        group_by(seqID) %>%
        mutate(mov_avg = weekly_count) #zoo::rollmean(weekly_count, k = 2, fill = 0))
      
      observed_incidence = fitted_data_CHI[[3]]
      observed_incidence$mov_avg <- observed_incidence$normalized #zoo::rollmean(observed_incidence$normalized, k = 2 , fill = 0)
      city_tag = "Chicago"
      
      
    }else if(city_code == "NYC"){
      load(here("Output","Fit Summaries","fit_LHS_NYC.Rdata"))
      
      model_fit_assess = fitted_data_NYC[[1]]
      
      model_fit_assess_filtered <- model_fit_assess[!is.na(nll_values)]
      
      model_fit_assess_filtered <- model_fit_assess_filtered %>%
        arrange(nll_values)
      
      top_percent_index <- floor(select_top_p*nrow(model_fit_assess_filtered))
      top_percent_index <- model_fit_assess_filtered[1:top_percent_index]
      
      simulated_incidence = fitted_data_NYC[[4]]
      
      simulated_incidence = simulated_incidence %>%
        filter(seqID %in% unique(top_percent_index$seqID))
      
      simulated_incidence = simulated_incidence %>%
        group_by(seqID) %>%
        mutate(mov_avg = zoo::rollmean(incidence, k = 7, fill = 0))
      
      observed_incidence = fitted_data_NYC[[3]]
      observed_incidence$mov_avg <- zoo::rollmean(observed_incidence$normalized, k = 7 , fill = 0)
      city_tag = "New York City"
      
    }else if(city_code == "SF"){
      load(here("Output","Fit Summaries","fit_LHS_SF.Rdata"))
      
      model_fit_assess = fitted_data_SF[[1]]
      
      model_fit_assess_filtered <- model_fit_assess[!is.na(nll_values)]
      
      model_fit_assess_filtered <- model_fit_assess_filtered %>%
        arrange(nll_values)
      
      top_percent_index <- floor(select_top_p*nrow(model_fit_assess_filtered))
      top_percent_index <- model_fit_assess_filtered[1:top_percent_index]
      
      simulated_incidence = fitted_data_SF[[4]]
      
      simulated_incidence = simulated_incidence %>%
        filter(seqID %in% unique(top_percent_index$seqID))
      
      simulated_incidence = simulated_incidence %>%
        group_by(seqID) %>%
        mutate(mov_avg = zoo::rollmean(incidence, k = 7, fill = 0))
      
      observed_incidence = fitted_data_SF[[3]]
      observed_incidence$mov_avg <- zoo::rollmean(observed_incidence$normalized, k = 7 , fill = 0)
      
      city_tag = "San Francisco"
      
    }
    
    best_fitting_params = data.frame(prob_U = top_percent_index$prob.U,
                                     prob_I = top_percent_index$prob.I,
                                     num_seeds = top_percent_index$num_seeds,
                                     city = city_code,
                                     city_name = city_tag)
    
    new_name <- paste0("best_fitting_params_",city_code)
    assign(new_name,best_fitting_params)
    
    save(list = new_name,
         file = here("Output","Fit Summaries",paste0("fitted_params_new_",city_code,".Rdata")))
    
    simulated_incidence_summary <- simulated_incidence %>%
      group_by(date) %>%
      mutate(median = median(mov_avg, na.rm = TRUE),
             mean = mean(mov_avg, na.rm = TRUE),
             P25 = quantile(mov_avg, probs = 0.25, na.rm = TRUE),
             P75 = quantile(mov_avg, probs = 0.75, na.rm = TRUE),
             P025 = quantile(mov_avg, probs = 0.025, na.rm = TRUE),
             P975 = quantile(mov_avg, probs = 0.975, na.rm = TRUE),
             max = replace(max(mov_avg, na.rm = TRUE), is.infinite(max(mov_avg, na.rm = TRUE)), 0),
             min = replace(min(mov_avg, na.rm = TRUE), is.infinite(min(mov_avg, na.rm = TRUE)), 0))
    
    if(city_code == "CHI"){
      y_title <- "Incidence\n(per 10,000 person-week)"
      y_scale_in <- scale_y_continuous(expand = c(0, 0),limits=c(0,100), breaks = seq(0,1000,25))
      y_scale_summary <- scale_y_continuous(expand=c(0,0),limits = c(0, 100), breaks = seq(0, 300, 25))
      
    }else{
      y_title <- "Incidence\n(per 10,000 person-days)"
      y_scale_in <- scale_y_continuous(expand = c(0, 0),limits=c(0,40), breaks = seq(0,1000,5))
      y_scale_summary <- scale_y_continuous(expand=c(0,0),limits = c(0, 25), breaks = seq(0, 300, 5))
    }
    
    ggplot()+
      geom_ribbon(data = simulated_incidence_summary, aes(x = date, ymin = P025/100000 * 10000, ymax = P975/100000 * 10000), alpha = 0.25, fill = "#295FA6") +
      geom_ribbon(data = simulated_incidence_summary, aes(x = date, ymin = P25/100000 * 10000, ymax = P75/100000 * 10000), alpha = 0.5, fill = "#295FA6") +
      geom_line(data = simulated_incidence_summary, aes(x = date, y = median/100000 * 10000), linewidth = 1, alpha = 1, color = "#295FA6") +
      geom_line(data = observed_incidence, aes(x = DATE, y = mov_avg/100000 * 10000), linewidth = 1, alpha = 1, color = "#F25041", linetype="dashed") +
      scale_x_date(date_breaks = "1 month", date_labels = "%b-%Y", expand = c(0,0),
                   limits = c(as.Date("2022-04-01"),as.Date("2022-09-27"))) +
      y_scale_summary +
      theme_bw() +
      theme(text = element_text(size = 18, family = "Arial", color = 'black'),
            axis.text.x = element_text(size = 14, angle = 45, hjust = 1, color = 'black'),
            
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank(),
            panel.border = element_blank(),
            
            plot.background = element_blank(),
            plot.margin = margin(t = 25, b = 10, l = 10, r = 10), 

            axis.line.y.right = element_blank(),
            axis.line.x.top = element_blank(),
            axis.line.x.bottom = element_line(linewidth = 1, color = 'black'),
            axis.line.y.left = element_line(linewidth = 1, color = 'black'),
            axis.ticks.length=unit(-0.25, "cm")
            ) +
      labs(y = y_title, x = NULL)
    
    ggsave(here("Output","Figures",paste0("plotting_fit_new_",city_code,select_top_p,".png")),dpi=300, width = 7, height=5, units = "in")
    ggsave(here("Output","Figures",paste0("plotting_fit_new_",city_code,select_top_p,".svg")),dpi=300, width = 5.5, height=4, units = "in")
    
    }
    
    parameter_summary = data.frame();
    
    for(top_p in percentiles){
      
      top_percent_index <- floor(top_p*nrow(model_fit_assess_filtered))
      top_percent_index <- model_fit_assess_filtered[1:top_percent_index]
      
      parameter_summary_i <- top_percent_index %>%
        summarise(
          prob.I_num = n(),
          prob.I_median = median(prob.I),
          prob.I_P025 = quantile(prob.I, 0.025),
          prob.I_P25 = quantile(prob.I, 0.25),
          prob.I_P75 = quantile(prob.I, 0.75),
          prob.I_P975 = quantile(prob.I, 0.975),
          
          prob.U_num = n(),
          prob.U_median = median(prob.U),
          prob.U_P025 = quantile(prob.U, 0.025),
          prob.U_P25 = quantile(prob.U, 0.25),
          prob.U_P75 = quantile(prob.U, 0.75),
          prob.U_P975 = quantile(prob.U, 0.975),
          
          prob.U_num = n(),
          num.seeds_median = median(num_seeds),
          num.seeds_P025 = quantile(num_seeds, 0.025),
          num.seeds_P25 = quantile(num_seeds, 0.25),
          num.seeds_P75 = quantile(num_seeds, 0.75),
          num.seeds_P975 = quantile(num_seeds, 0.975),
        ) %>%
        pivot_longer(everything(), names_to = "parameter", values_to = "value") %>%
        separate(parameter, into = c("parameter", "statistic"), sep = "_") %>%
        pivot_wider(names_from = "statistic", values_from = "value")
      
      parameter_summary_i$percentile_models = top_p;
      parameter_summary <- rbind(parameter_summary,parameter_summary_i)
    }
    
    parameter_summary = parameter_summary %>%
      arrange(parameter, top_p)
    
    write.csv(parameter_summary, file = here("Output","Tables",paste0("parameter_summaries_new_",city_code, ".csv")),row.names = F)
}


