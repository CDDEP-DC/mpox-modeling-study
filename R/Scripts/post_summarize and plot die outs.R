library(here)
library(tidyverse)
library(RColorBrewer)

file_list <- list.files(path = here("Output","Mechanism Summaries","Summaries"), pattern = "\\.Rdata$", full.names = F)

die_outs_combined = data.frame();

for(file in file_list){
  
  # Create new environment
  temp <- new.env();
  
  # Load item into temp environment
  load(here("Output","Mechanism Summaries","Summaries",file), envir = temp);
  loaded_item <- ls(envir = temp)
  loaded_item <- get(loaded_item[1], envir = temp)
  
  die_outs <- loaded_item[["daily_die_outs"]]
  
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
    
  die_outs_combined <- rbind(die_outs_combined,
                             cbind(die_outs,
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
die_outs_combined$new_mechanism <- factor(die_outs_combined$mechanism,
                                          levels = names(mechanism_map),
                                          labels = mechanism_map)

save(die_outs_combined,
     file = here(here("Output","Mechanism Summaries","Combined Summaries","die_outs_combined.Rdata")))


my_palette <- brewer.pal(name="Spectral",n=8)[c(1:4,6:8)]

ggplot(die_outs_combined %>% filter(new_mechanism %in% c("U","U+S","U+V","U+V+I","U+I - 6m", "U+I - 12m","U+I - 18m")), aes(x = date, y = proportion_die_outs, group = interaction(city,new_mechanism))) +
  geom_line(aes(color = new_mechanism), linewidth = 1) +
  scale_color_manual(values = my_palette) +
  facet_grid(cols = vars(city)) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b-%Y", limits = c(as.Date("2022-04-01"),as.Date("2024-04-29")),expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0),breaks=seq(0,100,10),limits=c(-5,105)) +
  theme_bw() +
  theme(text = element_text(family = "Helvetica", size = 18, color = "black"),
        axis.text.x = element_text(angle=45, hjust = 1, size = 10),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        #panel.background = element_rect(fill = "#e0e0e050"),
        strip.background = element_blank(),
        strip.text = element_text(family = "Helvetica", face = "bold", size = 12, hjust =0),
        
        legend.position = "inside",
        legend.position.inside = c(0.06,0.85),
        legend.text = element_text(size = 10),
        legend.box.background = element_rect(color = "black", fill = "white", linewidth = 0.5),
        
        panel.spacing = unit(0.5, "cm")
  ) +
  labs(x = "Date", y = "Cumulative Proportion of Outbreaks Ended (%)", color = "Mechanism")

ggsave(filename = here("Output","Figures","die_outs_main.png"), height = 8.5, width = 15, units = "in", dpi = 300)
