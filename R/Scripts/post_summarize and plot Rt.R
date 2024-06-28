library(here)
library(tidyverse)

file_list <- list.files(path = here("Output","Mechanism Summaries","Summaries"), pattern = "\\.Rdata$", full.names = F)

Rts_combined = data.frame();

for(file in file_list){
  
  # Create new environment
  temp <- new.env();
  
  # Load item into temp environment
  load(here("Output","Mechanism Summaries","Summaries",file), envir = temp);
  loaded_item <- ls(envir = temp)
  loaded_item <- get(loaded_item[1], envir = temp)
  
  Rts <- loaded_item[["Rt_summary"]]
  
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
  
  Rts_combined <- rbind(Rts_combined,
                             cbind(Rts,
                                   city = city_code,
                                   mechanism = mech_name))
}

mechanism_map <- c("Underdetection Only -" = "U",
                   "No Underdetection + Inf Wane - 6m -" = "I",
                   "No Underdetection + Vax Wane -" = "V",
                   "Underdetection Only with reduction -" = "U+R",
                   "Underdetection + PostPrideSeeds -" = "U+S",
                   "Underdetection + Vax Wane -" = "U+V",
                   "Underdetection + Inf Wane -" = "U+I - 6m",
                   "Underdetection + Inf Wane - 12m -" = "U+I - 12m",
                   "Underdetection + Inf Wane - 18m -" = "U+I - 18m",
                   "Underdetection + VWane + IWane -" = "U+V+I")

# Recode the 'mechanism' column using the named vector
Rts_combined$new_mechanism <- factor(Rts_combined$mechanism,
                                          levels = names(mechanism_map),
                                          labels = mechanism_map)

save(Rts_combined,
     file = here(here("Output","Mechanism Summaries","Summaries","Rt_combined.Rdata")))

ggplot(Rts_combined %>% filter(new_mechanism %in% c("U","V","I","U+I - 6m","U+I - 18m","U+I - 12m")), aes(x = date, y = median, group = interaction(city,new_mechanism))) +
  geom_hline(yintercept = 1, color="black",linewidth=0.5,alpha=0.5) +
  geom_line(aes(), color = "#2364AA", linewidth = 0.5) +
  geom_ribbon(aes(ymin =P25 , ymax = P75), fill = "#2364AA", alpha = 0.25) +
  facet_grid(cols = vars(new_mechanism),
             rows = vars(city)) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b-%Y", limits = c(as.Date("2022-04-01"),as.Date("2024-04-29")),expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0),breaks=seq(1,100,0.5), limits = c(0,3)) +
  theme_bw() +
  theme(text = element_text(family = "Helvetica", size = 18, color = "black"),
        axis.text.x = element_text(angle=90, vjust = 0.5, size = 10),
        axis.text.y = element_text(size = 10),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        
        strip.background = element_blank(),
        strip.text = element_text(family = "Helvetica", face = "bold", size = 12, hjust =0),
        
        legend.position = "inside",
        legend.position.inside = c(0.06,0.75),
        legend.text = element_text(size = 10),
        legend.box.background = element_rect(color = "black", fill = "white", linewidth = 0.5),
        
        panel.spacing = unit(0.5, "cm")
  ) +
  labs(x = "Date", y = expression(R[t]), color = "Mechanism")

ggsave(filename = here("Output","Figures","Rt.png"), height = 7, width = 11, units = "in", dpi = 300)




file_list <- list.files(path = here("Output","Mechanism Summaries","Summaries"), pattern = "\\.Rdata$", full.names = F)

Rts_combined = data.frame();

for(file in file_list){
  
  # Create new environment
  temp <- new.env();
  
  # Load item into temp environment
  load(here("Output","Mechanism Summaries","Summaries",file), envir = temp);
  loaded_item <- ls(envir = temp)
  loaded_item <- get(loaded_item[1], envir = temp)
  
  Rts <- loaded_item[["Rt_summary"]]
  
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
  
  Rts_combined <- rbind(Rts_combined,
                        cbind(Rts,
                              city = city_code,
                              mechanism = mech_name))
}

mechanism_map <- c("Underdetection Only -" = "U",
                   "No Underdetection + Inf Wane - 6m -" = "I",
                   "No Underdetection + Vax Wane -" = "V",
                   "Underdetection Only with reduction -" = "U+R",
                   "Underdetection + PostPrideSeeds -" = "U+S",
                   "Underdetection + Vax Wane -" = "U+V",
                   "Underdetection + Inf Wane -" = "U+I - 6m",
                   "Underdetection + Inf Wane - 12m -" = "U+I - 12m",
                   "Underdetection + Inf Wane - 18m -" = "U+I - 18m",
                   "Underdetection + VWane + IWane -" = "U+V+I")

# Recode the 'mechanism' column using the named vector
Rts_combined$new_mechanism <- factor(Rts_combined$mechanism,
                                     levels = names(mechanism_map),
                                     labels = mechanism_map)

save(Rts_combined,
     file = here(here("Output","Mechanism Summaries","Combined Summaries","Rt_combined.Rdata")))

ggplot(Rts_combined %>% filter(new_mechanism %in% c("U","V","I")), aes(x = date, y = median, group = interaction(city,new_mechanism))) +
  geom_hline(yintercept = 1, color="black",linewidth=0.5,alpha=0.5) +
  geom_line(aes(), color = "#2364AA", linewidth = 0.5) +
  geom_ribbon(aes(ymin =P25 , ymax = P75), fill = "#2364AA", alpha = 0.25) +
  facet_grid(cols = vars(new_mechanism),
             rows = vars(city)) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b-%Y", limits = c(as.Date("2022-04-01"),as.Date("2024-04-29")),expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0),breaks=seq(1,100,0.5), limits = c(0,3)) +
  theme_bw() +
  theme(text = element_text(family = "Helvetica", size = 18, color = "black"),
        axis.text.x = element_text(angle=90, vjust = 0.5, size = 10),
        axis.text.y = element_text(size = 10),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        
        strip.background = element_blank(),
        strip.text = element_text(family = "Helvetica", face = "bold", size = 12, hjust =0),
        
        legend.position = "inside",
        legend.position.inside = c(0.06,0.75),
        legend.text = element_text(size = 10),
        legend.box.background = element_rect(color = "black", fill = "white", linewidth = 0.5),
        
        panel.spacing = unit(0.5, "cm")
  ) +
  labs(x = "Date", y = expression(R[t]), color = "Mechanism")

ggsave(filename = here("Output","Figures","Rt_turning off underdetection.png"), height = 7, width = 11, units = "in", dpi = 300)


ggplot(Rts_combined %>% filter(new_mechanism %in% c("U","U+S","U+V","U+I - 6m","U+I - 12m", "U+I - 18m")), aes(x = date, y = median, group = interaction(city,new_mechanism))) +
  geom_hline(yintercept = 1, color="black",linewidth=0.5,alpha=0.5) +
  geom_line(aes(), color = "#2364AA", linewidth = 0.5) +
  geom_ribbon(aes(ymin =P25 , ymax = P75), fill = "#2364AA", alpha = 0.25) +
  facet_grid(cols = vars(new_mechanism),
             rows = vars(city)) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b-%Y", limits = c(as.Date("2022-04-01"),as.Date("2024-04-29")),expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0),breaks=seq(1,100,0.5), limits = c(0,3)) +
  theme_bw() +
  theme(text = element_text(family = "Helvetica", size = 18, color = "black"),
        axis.text.x = element_text(angle=90, vjust = 0.5, size = 10),
        axis.text.y = element_text(size = 10),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        
        strip.background = element_blank(),
        strip.text = element_text(family = "Helvetica", face = "bold", size = 12, hjust =0),
        
        legend.position = "inside",
        legend.position.inside = c(0.06,0.75),
        legend.text = element_text(size = 10),
        legend.box.background = element_rect(color = "black", fill = "white", linewidth = 0.5),
        
        panel.spacing = unit(0.5, "cm")
  ) +
  labs(x = "Date", y = expression(R[t]), color = "Mechanism")

ggsave(filename = here("Output","Figures","Rt_by mechanism.png"), height = 7, width = 11, units = "in", dpi = 300)
