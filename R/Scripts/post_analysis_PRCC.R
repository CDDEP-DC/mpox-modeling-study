## PRCC 
library(here)
library(ppcor)
library(knitr)
library(officer)
library(tidyverse)

last_day_as = read.csv(here("Output", "Processed Files", "last_day_asy_all.csv"))
last_day_vw = read.csv(here("Output", "Processed Files", "last_day_vw_all.csv"))
last_day_re = read.csv(here("Output", "Processed Files", "last_day_re_all.csv"))


unique_cities = unique(last_day_as$city)
pcrr_as = data.frame(cities = as.character(unique_cities),
                     PRCC_AS = rep(0,3),
                     PRCC_INF = rep(0,3))

for(this_city in unique_cities) {
  city_data <- last_day_as %>%
    filter(city == this_city)
  
  # Partial correlation for as_value and last_day while controlling for inf_value
  result_as_value <- pcor(cbind(city_data$as_value, city_data$last_day, city_data$inf_value), method="spearman")
  pcor_as_value <- result_as_value$estimate[1,2]
  
  # Partial correlation for inf_value and last_day while controlling for as_value
  result_inf_value <- pcor(cbind(city_data$inf_value, city_data$last_day, city_data$as_value), method="spearman")
  pcor_inf_value <- result_inf_value$estimate[1,2]
 
  pcrr_as[which(pcrr_as[,1] == this_city), 2] = pcor_as_value
  pcrr_as[which(pcrr_as[,1] == this_city), 3] = pcor_inf_value
  
}

rm(this_city)

unique_cities = unique(last_day_vw$city)
pcrr_vw = data.frame(cities = as.character(unique_cities),
                     PRCC_WP = rep(0,3),
                     PRCC_WF = rep(0,3),
                     PRCC_INF = rep(0,3))

for(this_city in unique_cities) {
  city_data <- last_day_vw %>%
    filter(city == this_city)
  
  result_wP_value <- pcor(cbind(city_data$wP, city_data$last_day, city_data$wF, city_data$inf_value), method="spearman")
  pcor_wP_value <- result_wP_value$estimate[1,2]
  
  result_wF_value <- pcor(cbind(city_data$wF, city_data$last_day, city_data$wP, city_data$inf_value), method="spearman")
  pcor_wF_value <- result_wF_value$estimate[1,2]

  result_inf_value <- pcor(cbind(city_data$inf_value, city_data$last_day, city_data$wP, city_data$wF), method="spearman")
  pcor_inf_value <- result_inf_value$estimate[1,2]
  
  pcrr_vw[which(pcrr_vw[,1] == this_city), 2] = pcor_wP_value
  pcrr_vw[which(pcrr_vw[,1] == this_city), 3] = pcor_wF_value
  pcrr_vw[which(pcrr_vw[,1] == this_city), 4] = pcor_inf_value
  
}

unique_cities = unique(last_day_re$city)
pcrr_re = data.frame(cities = as.character(unique_cities),
                     PRCC_RE = rep(0,3),
                     PRCC_INF = rep(0,3))

for(this_city in unique_cities) {
  city_data <- last_day_re %>%
    filter(city == this_city)
  
  result_re_value <- pcor(cbind(city_data$reinfect, city_data$last_day, city_data$inf_value), method="spearman")
  pcor_re_value <- result_re_value$estimate[1,2]
  
  result_inf_value <- pcor(cbind(city_data$inf_value, city_data$last_day, city_data$wP, city_data$wF), method="spearman")
  pcor_inf_value <- result_inf_value$estimate[1,2]
  
  pcrr_re[which(pcrr_re[,1] == this_city), 2] = pcor_re_value
  pcrr_re[which(pcrr_re[,1] == this_city), 3] = pcor_inf_value
  
}

decimal_places <- 4  # Change this to the desired number of decimal places
pcrr_as[, 2:ncol(pcrr_as)] <- round(pcrr_as[, 2:ncol(pcrr_as)], decimal_places)
pcrr_vw[, 2:ncol(pcrr_vw)] <- round(pcrr_vw[, 2:ncol(pcrr_vw)], decimal_places)
pcrr_re[, 2:ncol(pcrr_re)] <- round(pcrr_re[, 2:ncol(pcrr_re)], decimal_places)

# Pivot each table to long format
pcrr_as_long <- pcrr_as %>%
  pivot_longer(cols = starts_with("PRCC"), names_to = "Scenario", values_to = "Value")

pcrr_vw_long <- pcrr_vw %>%
  pivot_longer(cols = starts_with("PRCC"), names_to = "Scenario", values_to = "Value")

pcrr_re_long <- pcrr_re %>%
  pivot_longer(cols = starts_with("PRCC"), names_to = "Scenario", values_to = "Value")

# Combine the long-format tables
combined_long <- bind_rows(
  pcrr_as_long %>% mutate(Type = "Asymp"),
  pcrr_vw_long %>% mutate(Type = "Vax Wane"),
  pcrr_re_long %>% mutate(Type = "Reinf")
)

# Pivot combined long-format table to wide format
combined_wide <- combined_long %>%
  pivot_wider(names_from = cities, values_from = Value)

# View the combined and reshaped table
combined_wide

formatted_table <- knitr::kable(combined_wide)

doc <- read_docx() 
doc <- body_add_table(doc, combined_wide)
print(doc, target = here("Output", "Processed Files","PRCC_analysis.docx"))