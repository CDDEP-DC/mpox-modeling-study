library(here)
library(tidyverse)

file_list <- list.files(path = here("Output","Mechanism Summaries","Summaries"), pattern = "\\.Rdata$", full.names = F)

cuml_cases_incidence_params_combined = data.frame();

for(file in file_list){
  
  # Create new environment
  temp <- new.env();
  
  # Load item into temp environment
  load(here("Output","Mechanism Summaries","Summaries",file), envir = temp);
  loaded_item <- ls(envir = temp)
  loaded_item <- get(loaded_item[1], envir = temp)
  
  last_days <- loaded_item[["last_case_dates"]]
  cuml_cases <- loaded_item[["incidence_per_run"]]
  
  cuml_cases <- cuml_cases %>%
    group_by(seqID) %>%
    filter(date %in% c(as.Date("2023-01-01"),as.Date("2024-04-28")))
  
  cuml_cases <- cuml_cases %>%
    group_by(seqID) %>%
    mutate(additional_cases = Cuml_Cases - lag(Cuml_Cases)) %>%
    filter(date == as.Date("2024-04-28"))
    
  cuml_cases <- cuml_cases %>%
    dplyr::select(seqID, additional_cases)
  
  params <- loaded_item[["parameters"]]
  
  full_df = left_join(cuml_cases,params)
  full_df_days = left_join(last_days, params)
   
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
  
  cuml_cases_incidence_params_combined <- rbind(cuml_cases_incidence_params_combined,
                             cbind(full_df,
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
cuml_cases_incidence_params_combined$new_mechanism <- factor(cuml_cases_incidence_params_combined$mechanism,
                                          levels = names(mechanism_map),
                                          labels = mechanism_map)


ggplot(cuml_cases_incidence_params_combined %>% filter(new_mechanism %in% c("U","U+S","U+I","U+I - 12m", "U+I - 18m","U+V","U+V+I")), aes(y=new_mechanism, x=log((additional_cases/10000) * 100000), fill=city, group=interaction(new_mechanism,city))) +
  geom_violin(scale = "width", alpha = 0.75, color = NA, width = 1) +
  geom_boxplot(fill = NA, outlier.shape = NA, color = "black", linewidth = 0.25, width = 0.5, position=position_dodge(width = 1)) +
  scale_fill_brewer(palette = "Blues") +
  #scale_x_continuous(breaks=seq(0,20,2),expand=c(0,0),limits=c(0,9)) +
  theme_bw()+
  theme(text = element_text(family = "Helvetica", size = 18, color = "black"),
        axis.text.x = element_text(angle=45, hjust = 1, size = 16),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        
        strip.background = element_blank(),
        strip.text = element_text(family = "Helvetica", face = "bold", size = 12, hjust =0),
        
        legend.position = "inside",
        legend.position.inside = c(0.89,0.15),
        legend.text = element_text(size = 10),
        legend.box.background = element_rect(color = "black", fill = "white", linewidth = 0.5))+
  labs(y="Mechansim", x = "Log of Additional Cases (per 100,000 MSM)", fill = "City")
  
ggsave(filename = here("Output","Figures","log_additional_cases.png"), height = 3000, width = 4000, units = "px", dpi = 300)