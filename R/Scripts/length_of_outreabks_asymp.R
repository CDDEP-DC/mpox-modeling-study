## Get last days for chicago
library(here)
library(tidyverse)
library(zoo)
library(lubridate)

load(here("Output","Processed Files","asy_chi_processed_files.RData"))
assign("asymptomatic_chi", get("processed_files"))
rm("processed_files")

# Overwrite your list to only contain the 'ei.flow' data frames from each sublist
asymptomatic_chi <- lapply(asymptomatic_chi, function(x) x[['ei.flow']])

# Function to get the last non-zero incidence day for each column
get_last_non_zero_day_per_column <- function(df) {
  sapply(df, function(col) {
    last_day <- max(which(col > 0))
    return(ifelse(length(last_day) > 0, last_day, NA))
  })
}

# Apply the function to each data frame in the list
last_days_per_dataframe <- lapply(asymptomatic_chi, get_last_non_zero_day_per_column)

# Convert each vector in last_days_per_dataframe to a data frame
dfs <- lapply(names(last_days_per_dataframe), function(name) {
  vec <- last_days_per_dataframe[[name]]
  data.frame(
    data_frame_name = name,
    sim_num = names(vec),
    last_day = as.numeric(vec)
  )
})

# Bind all data frames together
last_days_asy_chi <- bind_rows(dfs)

last_days_asy_chi <- last_days_asy_chi %>%
  separate(data_frame_name, into = c("as_value", "inf_value"), sep = "_inf_") %>%
  separate(as_value, into = c("discard", "as_value"), sep = "as_") %>%
  select(-discard) %>%
  mutate(as_value = as.numeric(as_value),
         inf_value = as.numeric(inf_value))

last_days_asy_chi$city = "Chicago"

load(here("Output","Processed Files","asy_new_processed_files.RData"))
assign("asymptomatic_nyc", get("processed_files"))
rm("processed_files")

# Overwrite your list to only contain the 'ei.flow' data frames from each sublist
asymptomatic_nyc <- lapply(asymptomatic_nyc, function(x) x[['ei.flow']])

# Apply the function to each data frame in the list
last_days_per_dataframe <- lapply(asymptomatic_nyc, get_last_non_zero_day_per_column)

# Convert each vector in last_days_per_dataframe to a data frame
dfs <- lapply(names(last_days_per_dataframe), function(name) {
  vec <- last_days_per_dataframe[[name]]
  data.frame(
    data_frame_name = name,
    sim_num = names(vec),
    last_day = as.numeric(vec)
  )
})

# Bind all data frames together
last_days_asy_nyc <- bind_rows(dfs)

last_days_asy_nyc <- last_days_asy_nyc %>%
  separate(data_frame_name, into = c("as_value", "inf_value"), sep = "_inf_") %>%
  separate(as_value, into = c("discard", "as_value"), sep = "as_") %>%
  select(-discard) %>%
  mutate(as_value = as.numeric(as_value),
         inf_value = as.numeric(inf_value))

last_days_asy_nyc$city = "New York City"


#### SAN FRAN

load(here("Output","Processed Files","asy_san_processed_files.RData"))
assign("asymptomatic_sf", get("processed_files"))
rm("processed_files")

# Overwrite your list to only contain the 'ei.flow' data frames from each sublist
asymptomatic_sf <- lapply(asymptomatic_sf, function(x) x[['ei.flow']])

# Apply the function to each data frame in the list
last_days_per_dataframe <- lapply(asymptomatic_sf, get_last_non_zero_day_per_column)

# Convert each vector in last_days_per_dataframe to a data frame
dfs <- lapply(names(last_days_per_dataframe), function(name) {
  vec <- last_days_per_dataframe[[name]]
  data.frame(
    data_frame_name = name,
    sim_num = names(vec),
    last_day = as.numeric(vec)
  )
})

# Bind all data frames together
last_days_asy_sf <- bind_rows(dfs)

last_days_asy_sf <- last_days_asy_sf %>%
  separate(data_frame_name, into = c("as_value", "inf_value"), sep = "_inf_") %>%
  separate(as_value, into = c("discard", "as_value"), sep = "as_") %>%
  select(-discard) %>%
  mutate(as_value = as.numeric(as_value),
         inf_value = as.numeric(inf_value))

last_days_asy_sf$city = "San Francisco"

last_days_asy_all = rbind(last_days_asy_chi,last_days_asy_nyc, last_days_asy_sf)
last_days_asy_all$last_day_date <- as.Date("2022-05-01") + as.numeric(last_days_asy_all$last_day)

last_days_asy_all_stats = last_days_asy_all %>%
  group_by(city, as_value) %>%
  summarise(median = median(last_day_date))

save(last_days_asy_all, file = here("Output","Processed Files", "last_day_asy_all.Rdata"))

write.csv(last_days_asy_all, here("Output","Processed Files", "last_day_asy_all.csv"))

my_cols = brewer.pal(9, "BrBG")

ggplot(last_days_asy_all, aes(x=as_value, y=last_day_date, group=interaction(as_value, city))) +
  geom_violin(aes(fill=city), color = NA, scale = "count", bw = 2)+
  geom_boxplot(width = 0.03, position=position_dodge(width=0.09),
               outlier.color = NA, coef = 0, size=0.1,
               linewidth=0.20, col="black", fill=NA) +
  scale_fill_manual(values=c("Chicago" = my_cols[1], 
                             "New York City" = my_cols[3], 
                             "San Francisco" = my_cols[9]))+
  scale_x_continuous(breaks=seq(0,0.9,by=0.1)) +
  scale_y_date(limits=c(as.Date("2022-05-01"), as.Date("2023-09-01")), 
               breaks = seq(as.Date("2022-04-01"), as.Date("2023-09-01"), by="2 months"),
               date_labels = "%b %Y") +
  coord_flip() +
  facet_grid(cols = vars(city), scales = "free_y", space = "free_y") +
  labs(x = NULL, y = NULL) +
  theme(text = element_text(family = "Helvetica", size = 18, color = "#000000"),
    strip.background = element_blank(),
    legend.position = "none",
    legend.justification = c(1, 1),
    legend.background = element_blank(),
    legend.key = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    panel.grid.major.x = element_line(color="gray", linewidth=0.25),
    axis.text.x = element_text(angle = 45, hjust=1),
    panel.border = element_rect(colour = "black", fill = NA))

ggsave(here("Output", "Figures", "outbreak_length_as_as_t.svg"),
       height = 500,
       width = 800,
       units = "px",
       dpi = 72)




ggplot(last_days_asy_all, aes(x=inf_value, y=last_day_date, group=interaction(inf_value, city))) +
  geom_violin(aes(fill=city), color = NA, scale = "count", bw = 2)+
  geom_boxplot(width = 0.03, position=position_dodge(width=0.09),
               outlier.color = NA, coef = 0, size=0.1,
               linewidth=0.20, col="black", fill=NA) +
  scale_fill_manual(values=c("Chicago" = my_cols[1], 
                             "New York City" = my_cols[3], 
                             "San Francisco" = my_cols[9]))+
  scale_x_continuous(breaks=seq(0.3,0.9,by=0.1)) +
  scale_y_date(limits=c(as.Date("2022-05-01"), as.Date("2023-09-01")), 
               breaks = seq(as.Date("2022-04-01"), as.Date("2023-09-01"), by="2 months"),
               date_labels = "%b %Y") +
  coord_flip() +
  facet_grid(cols = vars(city), scales = "free_y", space = "free_y") +
  labs(x = NULL, y = NULL) +
  theme(text = element_text(family = "Helvetica", size = 18, color = "#000000"),
        strip.background = element_blank(),
        legend.position = "none",
        legend.justification = c(1, 1),
        legend.background = element_blank(),
        legend.key = element_blank(),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        panel.grid.major.x = element_line(color="gray", linewidth=0.25),
        axis.text.x = element_text(angle = 45, hjust=1),
        panel.border = element_rect(colour = "black", fill = NA))

ggsave(here("Output", "Figures", "outbreak_length_as_inf_t.svg"),
       height = 500,
       width = 800,
       units = "px",
       dpi = 72)


ggplot(last_days_asy_all, aes(x=inf_value, y=last_day_date, group=interaction(inf_value, city), fill=city)) +
  geom_boxplot(position="dodge", outlier.shape = NA, linewidth=0.25) + 
  scale_fill_brewer(palette="PuRd") +
  scale_x_continuous(breaks=seq(0,0.9,by=0.1)) +
  scale_y_date(limits=c(as.Date("2022-05-01"), as.Date("2023-09-01")), 
               breaks = seq(as.Date("2022-04-01"), as.Date("2023-09-01"), by="2 months"),
               date_labels = "%b %Y") +
  labs(x = NULL, y = NULL) +
  theme(
    text = element_text(family = "Helvetica", size = 6, color = "#000000"),
    legend.position = "none",
    legend.justification = c(1, 1),
    legend.background = element_blank(),
    legend.key = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(color="gray", linewidth=0.25),
    panel.border = element_rect(colour = "black", fill = NA))

ggsave(here("Output", "Figures", "outbreak_length_as_inf.svg"),
       height = 140,
       width = 176,
       units = "px",
       dpi = 72)


heatmap_data <- last_days_asy_all %>%
  group_by(as_value, inf_value, city) %>%
  summarise(median_day = median(last_day, na.rm = TRUE))

heatmap_data$median_date <- as.Date("2022-05-01") + heatmap_data$median_day

ggplot(heatmap_data, aes(x = inf_value, y = as_value)) +
  geom_tile(aes(fill = median_date)) +
  facet_grid(~city) +
  scale_fill_distiller(palette = "RdBu", name = "Date of Last Case", direction = -1) +
  scale_x_continuous(breaks=seq(0.3,0.9,by=0.1), expand = c(0,0)) +
  scale_y_continuous(breaks=seq(0.0,0.9,by=0.1), expand = c(0,0)) +
  labs(title = NULL,
       x = NULL,
       y = NULL) +
  theme_minimal()


# Create the temporal heatmap
ggplot(last_days_asy_all, aes(x = last_day_date, y = fct_rev(city), fill = as_value)) +
  geom_tile() +
  scale_fill_distiller(palette = "RdYlBu", name = "Asymptomatic Transmission Level (as_value)",
                       breaks=seq(0.0,0.9,by=0.3)) +
  scale_y_discrete(expand=c(0,0))+
  scale_x_date(limits=c(as.Date("2022-05-01"), as.Date("2023-08-01")), 
               breaks = seq(as.Date("2022-04-01"), as.Date("2023-09-01"), by="2 months"),
               date_labels = "%b %Y",
               expand=c(0,0)) +
  labs(x = NULL, y = NULL) +
  theme(
    text = element_text(family = "Helvetica", size = 8, color = "#000000"),
    legend.position = "none",
    legend.justification = c(1, 1),
    legend.background = element_blank(),
    legend.key = element_blank(),
    panel.background = element_rect(colour = NA, fill = "#BCBCBC"),
    panel.grid = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA),
    axis.text.x = element_text(angle=45, hjust=1))

ggsave(here("Output", "Figures", "outbreak_length_as_as_hmap_nolegend.svg"),
       height = 140,
       width = 500,
       units = "px",
       dpi = 72)


## Second heatmap
ggplot(last_days_asy_all, aes(x = last_day_date, y = fct_rev(city), fill = inf_value)) +
  geom_tile() +
  scale_fill_distiller(palette = "RdYlBu", name = "Infection prob",
                       breaks=seq(0.3,0.9,by=0.1)) +
  scale_y_discrete(expand=c(0,0))+
  scale_x_date(limits=c(min(last_days_asy_all$last_day_date), as.Date("2023-08-01")), 
               breaks = seq(min(last_days_asy_all$last_day_date), as.Date("2023-09-01"), by="2 months"),
               date_labels = "%b %Y",
               expand=c(0,0)) +
  labs(x = NULL, y = NULL) +
  theme(
    text = element_text(family = "Helvetica", size = 8, color = "#000000"),
    legend.position = "none",
    legend.justification = c(1, 1),
    legend.background = element_blank(),
    legend.key = element_blank(),
    panel.background = element_rect(colour = NA, fill = "#BCBCBC"),
    panel.grid = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA),
    axis.text.x = element_text(angle=45, hjust=1))

ggsave(here("Output", "Figures", "outbreak_length_as_inf_hmap_nolegend.svg"),
       height = 140,
       width = 500,
       units = "px",
       dpi = 72)




k_test_as_as = kruskal.test(last_day_date ~ as.factor(inf_value), data=last_days_asy_all) 

comparing_grps_as_nyc = last_days_asy_all %>% 
  mutate(as_value = as.factor(as_value)) %>%
  filter(city == "New York City") %>%
  wilcox_test(last_day ~ as_value, p.adjust.method = "bonferroni")

comparing_grps_inf_nyc = last_days_asy_all %>% 
  mutate(as_value = as.factor(inf_value)) %>%
  filter(city == "New York City") %>%
  wilcox_test(last_day ~ as_value, p.adjust.method = "bonferroni")