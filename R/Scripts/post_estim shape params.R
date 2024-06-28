##      This code block pulls the selected params and estimates the shape parameters using the fitdistrplus pacakge
##      By: Nodar Kipshidze
##      Last updated: May 29, 2024
# -----------------------------------------------------------------------------

library(fitdistrplus)
library(tidyverse)

load(here("Output","Fit Summaries",paste0("fitted_params_","CHI",".Rdata")))
load(here("Output","Fit Summaries",paste0("fitted_params_","NYC",".Rdata")))
load(here("Output","Fit Summaries",paste0("fitted_params_","SF",".Rdata")))
               

## Plot the empirical params     
combined_cities = rbind(best_fitting_params_CHI,
                        best_fitting_params_NYC,
                        best_fitting_params_SF)



ggplot(data = combined_cities, aes(y = city_name, x = prob_I)) +
  stat_slab(aes(fill = after_stat(level)), .width = c(.66, .95), point_interval = median_qi, alpha=0.85, color = "gray50", size = 0.5) +
  stat_spike(aes(linetype = after_stat(at)), at = c("median")) +
  scale_fill_brewer(na.translate = FALSE) +
  scale_x_continuous(breaks = seq(0,1,by=0.1)) +
  scale_thickness_shared() +
  theme_bw() +
  theme(text = element_text(size = 20, family = "Skolar Sans Latin Condensed"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none") +
  labs(y = "City", x = "Probability of Infection")

ggsave(here("Output","Figures","params_infect.png"),dpi=300, width = 2500, height = 1500, units = "px")

ggplot(data = combined_cities, aes(y = city_name, x = prob_U)) +
  stat_slab(aes(fill = after_stat(level)), .width = c(.66, .95), point_interval = median_qi, alpha=0.85, color = "gray50", size = 0.5,) +
  stat_spike(aes(linetype = after_stat(at)), at = c("median")) +
  scale_fill_brewer(na.translate = FALSE) +
  scale_x_continuous(breaks = seq(0,1,by=0.1)) +
  scale_thickness_shared() +
  theme_bw() +
  theme(text = element_text(size = 20, family = "Skolar Sans Latin Condensed"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none") +
  labs(y = "City", x = "Proportion of Undetected Cases")

ggsave(here("Output","Figures","params_undetected.png"),dpi=300, width = 2500, height = 1500, units = "px")

ggplot(data = combined_cities, aes(y = city_name, x = num_seeds)) +
  stat_histinterval(breaks = 7, slab_color = "gray45", outline_bars = T) +
  stat_spike(aes(linetype = after_stat(at)), at = c("median")) +
  scale_fill_brewer(na.translate = FALSE) +
  scale_x_continuous(breaks = seq(0,7,by=1)) +
  scale_thickness_shared() +
  theme_bw() +
  theme(text = element_text(size = 20, family = "Skolar Sans Latin Condensed"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none") +
  labs(y = "City", x = "Frequency of Seeding Pre-Pride")

ggsave(here("Output","Figures","params_pre_pride_seeds.png"),dpi=300, width = 2500, height = 1500, units = "px")

params = c("prob_U","prob_I")
cities = c("CHI","SF","NYC")

fitted_params = list();

for(param in params){
  for(select_city in cities){
    selected_param_fitting <- combined_cities %>% 
      filter(city == select_city) %>% 
      dplyr::select(param)
    
    selected_param_fitting = c(selected_param_fitting[,param])
    fit <- fitdist(selected_param_fitting, "beta")
    
    fitted_params[[paste0(param,"_",select_city)]] = fit
  }
}

for(select_city in cities){
  selected_param_fitting <- combined_cities %>% 
    filter(city == select_city) %>% 
    dplyr::select(num_seeds)
  
  selected_param_fitting = c(selected_param_fitting[,"num_seeds"])
  
  # Create the file name
  file_name <- here("Output","Figures", paste0("plot_num_seeds_graph", "_", select_city, ".png"))
  
  # Open the PNG device
  png(file_name, width = 1500, height = 1500, units = "px", res = 220)
  
  # Plot the distribution
  descdist(selected_param_fitting, boot = 1000)
  
  # Close the PNG device
  dev.off()
}



# Save the plots as PNG files
for(param in params) {
  for(select_city in cities) {
    fit <- fitted_params[[paste0(param, "_", select_city)]]
    
    # Create the file name
    file_name <- here("Output","Figures", paste0("plot_", param, "_", select_city, ".png"))
    
    # Open the PNG device
    png(file_name, width = 1500, height = 1500, units = "px", res = 220)
    
    # Plot the distribution
    plot(fit)
    
    # Close the PNG device
    dev.off()
    
  }
}

save(fitted_params,
     file = here("Output","Fit Summaries",paste0("fitted_shape_params",".Rdata")))



