library(here)
library(tidyverse)

file_list <- list.files(path = here("Output","Mechanism Summaries","Summaries"), pattern = "\\.Rdata$", full.names = F)

Sum_inc_combined = data.frame();
Sum_inc_undetected = data.frame();
Sum_inc_detected = data.frame();

for(file in file_list){
  
  # Create new environment
  temp <- new.env();
  
  # Load item into temp environment
  load(here("Output","Mechanism Summaries","Summaries",file), envir = temp);
  loaded_item <- ls(envir = temp)
  loaded_item <- get(loaded_item[1], envir = temp)
  
  Sum_inc <- loaded_item[["summarized_incidence"]]
  Detected_cases <- loaded_item[["summarized_incidence_detected"]]
  Undetected_cases <- loaded_item[["summarized_incidence_undetected"]]
  
  mech_name <- ls(envir = temp)
  mech_name <- sub("\\_sim_summary$", "", mech_name)
  
  if(grepl("CHI", mech_name) == TRUE){
    city_code <- "Chicago"
    mech_name <-   mech_name <- sub("\\_CHI$", "", mech_name)
    mech_name <- gsub("_", " ", mech_name)
    mech_name <- gsub("and", "+", mech_name)
    mech_name <- gsub("(?<! )\\+(?! )", " + ", mech_name, perl = TRUE)
  }else if(grepl("NYC", mech_name) == TRUE){
    city_code <- "New York City"
    mech_name <-   mech_name <- sub("\\_NYC$", "", mech_name)
    mech_name <- gsub("_", " ", mech_name)
    mech_name <- gsub("and", "+", mech_name)
    mech_name <- gsub("(?<! )\\+(?! )", " + ", mech_name, perl = TRUE)
  }else if(grepl("SF", mech_name) == TRUE){
    city_code <- "San Francisco"
    mech_name <-   mech_name <- sub("\\_SF$", "", mech_name)
    mech_name <- gsub("_", " ", mech_name)
    mech_name <- gsub("and", "+", mech_name)
    mech_name <- gsub("(?<! )\\+(?! )", " + ", mech_name, perl = TRUE)
  }
  
  Sum_inc_combined <- rbind(Sum_inc_combined,
                        cbind(Sum_inc,
                              city = city_code,
                              mechanism = mech_name))
  
  Sum_inc_undetected <- rbind(Sum_inc_undetected,
                            cbind(Undetected_cases,
                                  city = city_code,
                                  mechanism = mech_name))
  
  Sum_inc_detected <- rbind(Sum_inc_detected,
                            cbind(Detected_cases,
                                  city = city_code,
                                  mechanism = mech_name))
}

mechanism_map <- c("Underdetection Only -" = "U",
                   "No Underdetection + Inf Wane - 6m -" = "I",
                   "No Underdetection + Vax Wane -" = "V",
                   "Underdetection Only with reduction -" = "U+R",
                   "Underdetection + PostPrideSeeds -" = "U+S",
                   "Underdetection + Vax Wane -" = "U+V",
                   "Underdetection + Inf Wane -" = "U+I",
                   "Underdetection + Inf Wane - 12m -" = "U+I - 12m",
                   "Underdetection + Inf Wane - 18m -" = "U+I - 18m",
                   "Underdetection + VWane + IWane -" = "U+V+I")


# Recode the 'mechanism' column using the named vector
Sum_inc_combined$new_mechanism <- factor(Sum_inc_combined$mechanism,
                                     levels = names(mechanism_map),
                                     labels = mechanism_map)

Sum_inc_undetected$new_mechanism <- factor(Sum_inc_undetected$mechanism,
                                         levels = names(mechanism_map),
                                         labels = mechanism_map)

Sum_inc_detected$new_mechanism <- factor(Sum_inc_detected$mechanism,
                                         levels = names(mechanism_map),
                                         labels = mechanism_map)

Incidence_summarized = list(Total_incidence = Sum_inc_combined,
                            Undeteceted_cases = Sum_inc_undetected,
                            Detected_cases = Sum_inc_detected)

save(Incidence_summarized,
     file = here(here("Output","Mechanism Summaries","Combined Summaries","Sum_inc_combined.Rdata")))



### CHI
# NOTE: Chicago reports on a weekly basis ... more on that below

observed_cases_CHI <- read.csv(here("Data","Cities","Chicago","cases_chicago.csv"))
observed_cases_CHI$DATE <- as.Date(observed_cases_CHI$DATE)
city_denom <- 131704     # The estimated number of MSM for the given city

observed_cases_CHI <- observed_cases_CHI %>%
  mutate(DATE = DATE - 7)

observed_cases_CHI$normalized <- (observed_cases_CHI$NEW_CASES / city_denom) * 100000
observed_cases_CHI$city = "Chicago"


### SAN FRAN
observed_cases_SF <- read.csv(here("Data","Cities","San Francisco","cases_SF_current.csv"))
observed_cases_SF$DATE <- as.Date(observed_cases_SF$DATE)
city_denom <- 69643     # The estimated number of MSM for the given city

# Shift the DATE column back by 7 days
observed_cases_SF <- observed_cases_SF %>%
  mutate(DATE = DATE - 7)

observed_cases_SF$normalized <- (observed_cases_SF$NEW_CASES / city_denom) * 100000
observed_cases_SF$movAg <- zoo::rollmean(observed_cases_SF$normalized, k = 7, fill = 0)
observed_cases_SF$city = "San Francisco"

### NYC

### NYC 
observed_cases_NYC <- read.csv(here("Data","Cities","New York City","cases_NYC.csv"))
observed_cases_NYC$DATE <- as.Date(observed_cases_NYC$DATE)
observed_cases_NYC <- observed_cases_NYC %>%
  mutate(DATE = DATE - 7)
city_denom <- 225235.80     # The estimated number of MSM for the given city
observed_cases_NYC$normalized <- (observed_cases_NYC$NEW_CASES / city_denom) * 100000
observed_cases_NYC$movAg <- zoo::rollmean(observed_cases_NYC$normalized, k = 7, fill = 0)
observed_cases_NYC$city = "New York City"


ggplot(Sum_inc_detected %>% filter(new_mechanism %in% c("U","U+S","U+I","U+V","U+V+I")), aes(x = date, y = median, group = interaction(city,new_mechanism))) +
  geom_line(aes(color = "Detected cases"), , linewidth = 1) +
  geom_line(data = Sum_inc_undetected %>% filter(new_mechanism %in% c("U","U+S","U+I","U+V","U+V+I")), 
            aes(x = date, y = median, group = interaction(city,new_mechanism), color = "Undetected casses"), linewidth = 1) +
  geom_ribbon(data = Sum_inc_undetected %>% filter(new_mechanism %in% c("U","U+S","U+I","U+V","U+V+I")), aes(ymin = P25 , ymax = P75), fill = "#F49F0A", alpha = 0.25) +
  geom_ribbon(aes(ymin =P25 , ymax = P75), fill = "#00A6A6", alpha = 0.25) +
  facet_grid(cols = vars(city),
             rows = vars(new_mechanism)) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b-%Y", limits = c(as.Date("2022-04-01"),as.Date("2024-04-21")),expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0),breaks=seq(0,500,50),limits=c(-1,300)) +
  scale_color_manual(values = c("Undetected casses" = "#F49F0A",
                                "Detected cases" = "#00A6A6",
                                "Observed data" = "#56494C")) +
  theme_bw() +
  theme(text = element_text(family = "Helvetica", size = 18, color = "black"),
        axis.text.x = element_text(angle=90, vjust = 0.5, size = 10),
        axis.text.y = element_text(size = 10),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),

        strip.background = element_blank(),
        strip.text = element_text(family = "Helvetica", face = "bold", size = 12, hjust =0),

        panel.spacing = unit(0.5, "cm"),
        axis.ticks.length.y =unit(-0.15, "cm"),
        axis.ticks.length.x =unit(-0.15, "cm"),
        
        panel.border = element_rect(color = 'black', linewidth = 0.5), # Add panel borders

        legend.position = "inside",
        legend.position.inside = c(0.93,0.95),
        legend.box.background = element_rect(color = "black", fill = "white", linewidth = 0.5),
        legend.text = element_text(size = 10)
  ) +
  labs(x = "Date", y = "Incidence (per 100,000 MSM person-days)", color = NULL)

ggsave(filename = here("Output","Figures","Overall_incidence_by_case_type_full_length.png"), height = 8.5, width = 14, units = "in", dpi = 300)

ggplot(Sum_inc_detected %>% filter(new_mechanism %in% c("U","I","V")), aes(x = date, y = median, group = interaction(city,new_mechanism))) +
  geom_line(aes(color = "Detected cases"), , linewidth = 1) +
  geom_line(data = Sum_inc_undetected %>% filter(new_mechanism %in% c("U","I","V")), 
            aes(x = date, y = median, group = interaction(city,new_mechanism), color = "Undetected casses"), linewidth = 1) +
  geom_ribbon(data = Sum_inc_undetected %>% filter(new_mechanism %in% c("U","I","V")), aes(ymin =P25 , ymax = P75), fill = "#F49F0A", alpha = 0.25) +
  #geom_line(data = observed_cases_CHI, aes(x = DATE, y = normalized, group = city, color = "Observed data"), linetype = "solid", linewidth = 1) +
  # geom_line(data = observed_cases_SF, aes(x = DATE, y = movAg, group = city, color = "Observed data"), linetype = "solid", linewidth = 1)+
  # geom_line(data = observed_cases_NYC, aes(x = DATE, y = movAg, group = city, color = "Observed data"), linetype = "solid", linewidth = 1)+
  geom_ribbon(aes(ymin =P25 , ymax = P75), fill = "#00A6A6", alpha = 0.25) +
  facet_grid(cols = vars(city),
             rows = vars(new_mechanism)) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b-%Y", limits = c(as.Date("2022-04-01"),as.Date("2024-04-21")),expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0),breaks=seq(0,500,50),limits=c(-10,300)) +
  scale_color_manual(values = c("Undetected casses" = "#F49F0A",
                                "Detected cases" = "#00A6A6",
                                "Observed data" = "#56494C")) +
  theme_bw() +
  theme(text = element_text(family = "Helvetica", size = 18, color = "black"),
        axis.text.x = element_text(angle=90, vjust = 0.5, size = 10),
        axis.text.y = element_text(size = 10),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        
        strip.background = element_blank(),
        strip.text = element_text(family = "Helvetica", face = "bold", size = 12, hjust =0),
        
        panel.spacing = unit(0.5, "cm"),
        
        legend.position = "inside",
        legend.position.inside = c(0.92,0.90),
        legend.box.background = element_rect(color = "black", fill = "white", linewidth = 0.5),
        legend.text = element_text(size = 10)
  ) +
  labs(x = "Date", y = "Incidence (per 100,000 MSM person-days)", color = NULL)

ggsave(filename = here("Output","Figures","Turning off underdetection.png"), height = 2000, width = 5000, units = "px", dpi = 300)

ggplot(Sum_inc_detected %>% filter(new_mechanism %in% c("U+I","U+I - 12m", "U+I - 18m")), aes(x = date, y = median, group = interaction(city,new_mechanism))) +
  geom_line(aes(color = "Simulated: detected cases"), , linewidth = 1) +
  geom_line(data = Sum_inc_undetected %>% filter(new_mechanism %in% c("U+I","U+I - 12m", "U+I - 18m")), 
            aes(x = date, y = median, group = interaction(city,new_mechanism), color = "Simulated: undetected casses"), linewidth = 1) +
  geom_ribbon(data = Sum_inc_undetected %>% filter(new_mechanism %in% c("U+I","U+I - 12m", "U+I - 18m")), aes(ymin =P25 , ymax = P75), fill = "#F49F0A", alpha = 0.25) +
  # geom_line(data = observed_cases_CHI, aes(x = DATE, y = normalized, group = city, color = "Observed data"), linetype = "solid", linewidth = 1) +
  # geom_line(data = observed_cases_SF, aes(x = DATE, y = movAg, group = city, color = "Observed data"), linetype = "solid", linewidth = 1)+
  # geom_line(data = observed_cases_NYC, aes(x = DATE, y = movAg, group = city, color = "Observed data"), linetype = "solid", linewidth = 1)+
  geom_ribbon(aes(ymin =P25 , ymax = P75), fill = "#00A6A6", alpha = 0.25) +
  facet_grid(cols = vars(city),
             rows = vars(new_mechanism)) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b-%Y", limits = c(as.Date("2023-04-01"),as.Date("2024-04-21")),expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0),breaks=seq(0,500,50),limits=c(-10,150)) +
  scale_color_manual(values = c("Simulated: undetected casses" = "#F49F0A",
                                "Simulated: detected cases" = "#00A6A6",
                                "Observed data" = "#56494C")) +
  theme_bw() +
  theme(text = element_text(family = "Helvetica", size = 24, color = "black"),
        axis.text.x = element_text(angle=90, vjust = 0.5, size = 18),
        axis.text.y = element_text(size = 18),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        
        strip.background = element_blank(),
        strip.text = element_text(family = "Helvetica", face = "bold", size = 18, hjust =0),
        
        panel.spacing = unit(0.5, "cm"),
        
        legend.position = "inside",
        legend.position.inside = c(0.92,0.90),
        legend.box.background = element_rect(color = "black", fill = "white", linewidth = 0.5),
        legend.text = element_text(size = 10)
  ) +
  labs(x = "Date", y = "Incidence (per 100,000 MSM person-days)", color = NULL)


ggsave(filename = here("Output","Figures","Incidence_by_immunity waning length_zoomed_in.png"), height = 2000, width = 5000, units = "px", dpi = 300)
