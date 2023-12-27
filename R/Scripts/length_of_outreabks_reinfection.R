## Get last days for chicago

load(here("Output","Processed Files","rei_chi_processed_files.RData"))
assign("reinfection_chi", get("processed_files"))
rm("processed_files")

# Overwrite your list to only contain the 'ei.flow' data frames from each sublist
reinfection_chi  <- lapply(reinfection_chi, function(x) x[['ei.flow']])

# Function to get the last non-zero incidence day for each column
get_last_non_zero_day_per_column <- function(df) {
  sapply(df, function(col) {
    last_day <- max(which(col > 0))
    return(ifelse(length(last_day) > 0, last_day, NA))
  })
}

# Apply the function to each data frame in the list
last_days_per_dataframe <- lapply(reinfection_chi, get_last_non_zero_day_per_column)

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
last_days_re_chi <- bind_rows(dfs)

last_days_re_chi <- last_days_re_chi %>%
  separate(data_frame_name, into = c("reinfect", "inf_value"), sep = "_inf_") %>%
  separate(reinfect, into = c("discard", "reinfect"), sep = "reinfect_") %>%
  select(-discard) %>%
  mutate(reinfect = as.numeric(reinfect),
         inf_value = as.numeric(inf_value))

last_days_re_chi$city = "Chicago"

### NEW YORK CITY

load(here("Output","Processed Files","rei_new_processed_files.RData"))
assign("reinfection_nyc", get("processed_files"))
rm("processed_files")

# Overwrite your list to only contain the 'ei.flow' data frames from each sublist
reinfection_nyc <- lapply(reinfection_nyc, function(x) x[['ei.flow']])

# Apply the function to each data frame in the list
reinfection_nyc = reinfection_nyc[-which(names(reinfection_nyc) == "reinfect_0.005556_inf_NA")]
last_days_per_dataframe <- lapply(reinfection_nyc, get_last_non_zero_day_per_column)

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
last_days_re_nyc <- bind_rows(dfs)

last_days_re_nyc <- last_days_re_nyc %>%
  separate(data_frame_name, into = c("reinfect", "inf_value"), sep = "_inf_") %>%
  separate(reinfect, into = c("discard", "reinfect"), sep = "reinfect_") %>%
  select(-discard) %>%
  mutate(reinfect = as.numeric(reinfect),
         inf_value = as.numeric(inf_value))

last_days_re_nyc$city = "New York City"


#### SAN FRAN

load(here("Output","Processed Files","rei_san_processed_files.RData"))
assign("reinfection_sf", get("processed_files"))
rm("processed_files")

# Overwrite your list to only contain the 'ei.flow' data frames from each sublist
reinfection_sf <- lapply(reinfection_sf, function(x) x[['ei.flow']])

# Apply the function to each data frame in the list
last_days_per_dataframe <- lapply(reinfection_sf, get_last_non_zero_day_per_column)

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
last_days_re_sf <- bind_rows(dfs)

last_days_re_sf <- last_days_re_sf %>%
  separate(data_frame_name, into = c("reinfect", "inf_value"), sep = "_inf_") %>%
  separate(reinfect, into = c("discard", "reinfect"), sep = "reinfect_") %>%
  select(-discard) %>%
  mutate(reinfect = as.numeric(reinfect),
         inf_value = as.numeric(inf_value))

last_days_re_sf$city = "San Francisco"

last_days_re_all = rbind(last_days_re_chi,last_days_re_nyc, last_days_re_sf)
last_days_re_all$last_day_date <- as.Date("2022-05-01") + as.numeric(last_days_re_all$last_day)

last_days_re_all = last_days_re_all %>%
  mutate(waning_months = round(1/(reinfect*30),0))

save(last_days_re_all, file = here("Output","Processed Files", "last_day_re_all.Rdata"))

write.csv(last_days_re_all, here("Output","Processed Files", "last_day_re_all.csv"))


ggplot(last_days_re_all, aes(x=round((1/reinfect/30),2), y=last_day_date, group=interaction(round((1/reinfect/30),2), city), fill=city)) +
  geom_boxplot(position="dodge", outlier.shape = NA, linewidth=0.25) + 
  scale_fill_brewer(palette="PuRd") +
  scale_x_continuous(breaks=seq(6,16,by=1)) +
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

ggsave(here("Output", "Figures", "outbreak_length_re_re.svg"),
       height = 140,
       width = 176,
       units = "px",
       dpi = 72)

ggplot(last_days_re_all, aes(x=inf_value, y=last_day_date, group=interaction(inf_value, city), fill=city)) +
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

ggsave(here("Output", "Figures", "outbreak_length_re_inf.svg"),
       height = 140,
       width = 176,
       units = "px",
       dpi = 72)

# Create the temporal heatmap
ggplot(last_days_re_all, aes(x = last_day_date, y = fct_rev(city), fill = round(1/(reinfect*30),0))) +
  geom_tile() +
  scale_fill_distiller(palette = "RdYlBu", name = "Full Vaccine",
                       breaks=seq(6,16,by=3), na.value = "grey50") +
  scale_y_discrete(expand=c(0,0))+
  scale_x_date(limits=c(min(last_days_re_all$last_day_date), as.Date("2023-08-01")), 
               breaks = seq(min(last_days_re_all$last_day_date), as.Date("2023-09-01"), by="2 months"),
               date_labels = "%b %Y",
               expand=c(0,0)) +
  labs(x = NULL, y = NULL) +
  theme(
    text = element_text(family = "Helvetica", size = 8, color = "#000000"),
    legend.position = "none",
    legend.justification = c(1, 1),
    legend.background = element_blank(),
    legend.key = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA),
    axis.text.x = element_text(angle=45, hjust=1))

ggsave(here("Output", "Figures", "outbreak_length_reinfect_wane_hmap_no_legend.svg"),
       height = 140,
       width = 500,
       units = "px",
       dpi = 72)

## Second heatmap
ggplot(last_days_re_all, aes(x = last_day_date, y = fct_rev(city), fill = inf_value)) +
  geom_tile() +
  scale_fill_distiller(palette = "RdYlBu", name = "Infection prob",
                       breaks=seq(0.3,0.9,by=0.1)) +
  scale_y_discrete(expand=c(0,0))+
  scale_x_date(limits=c(min(last_days_re_all$last_day_date), as.Date("2023-08-01")), 
               breaks = seq(min(last_days_re_all$last_day_date), as.Date("2023-09-01"), by="2 months"),
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

ggsave(here("Output", "Figures", "outbreak_length_reinfect_inf_hmap_nolegend.svg"),
       height = 140,
       width = 500,
       units = "px",
       dpi = 72)
