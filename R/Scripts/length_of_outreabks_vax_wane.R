## Get last days for chicago

load(here("Output","Processed Files","vac_chi_processed_files.RData"))
assign("vaxwane_chicago", get("processed_files"))
rm("processed_files")

# Overwrite your list to only contain the 'ei.flow' data frames from each sublist
vaxwane_chicago  <- lapply(vaxwane_chicago, function(x) x[['ei.flow']])

# Function to get the last non-zero incidence day for each column
get_last_non_zero_day_per_column <- function(df) {
  sapply(df, function(col) {
    last_day <- max(which(col > 0))
    return(ifelse(length(last_day) > 0, last_day, NA))
  })
}

# Apply the function to each data frame in the list
last_days_per_dataframe <- lapply(vaxwane_chicago, get_last_non_zero_day_per_column)

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
last_days_vw_chi <- bind_rows(dfs)

last_days_vw_chi <- last_days_vw_chi %>%
  separate(data_frame_name, into = c("wP", "temp"), sep = "_wF_") %>%
  separate(temp, into = c("wF", "inf_value"), sep = "_inf_") %>%
  mutate(
    wP = as.numeric(gsub("wP_", "", wP)),
    wF = as.numeric(gsub("wF_", "", wF)),
    inf_value = as.numeric(inf_value)
  )

last_days_vw_chi$city = "Chicago"

### NEW YORK CITY

load(here("Output","Processed Files","vac_new_processed_files.RData"))
assign("vaxwane_nyc", get("processed_files"))
rm("processed_files")

# Overwrite your list to only contain the 'ei.flow' data frames from each sublist
vaxwane_nyc <- lapply(vaxwane_nyc, function(x) x[['ei.flow']])

# Apply the function to each data frame in the list
last_days_per_dataframe <- lapply(vaxwane_nyc, get_last_non_zero_day_per_column)

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
last_days_vw_nyc <- bind_rows(dfs)

last_days_vw_nyc <- last_days_vw_nyc %>%
  separate(data_frame_name, into = c("wP", "temp"), sep = "_wF_") %>%
  separate(temp, into = c("wF", "inf_value"), sep = "_inf_") %>%
  mutate(
    wP = as.numeric(gsub("wP_", "", wP)),
    wF = as.numeric(gsub("wF_", "", wF)),
    inf_value = as.numeric(inf_value)
  )

last_days_vw_nyc$city = "New York City"


#### SAN FRAN

load(here("Output","Processed Files","vac_san_processed_files.RData"))
assign("vaxwane_sf", get("processed_files"))
rm("processed_files")

# Overwrite your list to only contain the 'ei.flow' data frames from each sublist
vaxwane_sf <- lapply(vaxwane_sf, function(x) x[['ei.flow']])

# Apply the function to each data frame in the list
last_days_per_dataframe <- lapply(vaxwane_sf, get_last_non_zero_day_per_column)

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
last_days_vw_sf <- bind_rows(dfs)

last_days_vw_sf <- last_days_vw_sf %>%
  separate(data_frame_name, into = c("wP", "temp"), sep = "_wF_") %>%
  separate(temp, into = c("wF", "inf_value"), sep = "_inf_") %>%
  mutate(
    wP = as.numeric(gsub("wP_", "", wP)),
    wF = as.numeric(gsub("wF_", "", wF)),
    inf_value = as.numeric(inf_value)
  )

last_days_vw_sf$city = "San Francisco"

last_days_ww_all = rbind(last_days_vw_chi,last_days_vw_nyc, last_days_vw_sf)
last_days_ww_all$last_day_date <- as.Date("2022-05-01") + as.numeric(last_days_ww_all$last_day)
last_days_ww_all = last_days_ww_all %>%
  mutate(wP_wF = paste0(wP, " / ", wF))

last_days_ww_all = last_days_ww_all %>%
  mutate(wP_wF = factor(wP_wF,
                        levels = c("6 / 9", "6 / 12", "6 / 15", "9 / 12", "9 / 15", "12 / 15"),
                        labels = c("6 / 9", "6 / 12", "6 / 15", "9 / 12", "9 / 15", "12 / 15")))

save(last_days_ww_all, file = here("Output","Processed Files", "last_day_vw_all.Rdata"))

write.csv(last_days_ww_all, here("Output","Processed Files", "last_day_vw_all.csv"))


ggplot(last_days_ww_all, aes(x=wP, y=last_day_date, group=interaction(wP, city), fill=city)) +
  geom_boxplot(position="dodge", outlier.shape = NA, linewidth=0.25) + 
  scale_fill_brewer(palette="PuRd") +
  scale_x_continuous(breaks=c(6,9,12)) +
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

ggsave(here("Output", "Figures", "outbreak_length_vw_wP.svg"),
       height = 140,
       width = 176,
       units = "px",
       dpi = 72)

ggplot(last_days_ww_all, aes(x=wF, y=last_day_date, group=interaction(wF, city), fill=city)) +
  geom_boxplot(position="dodge", outlier.shape = NA, linewidth=0.25) + 
  scale_fill_brewer(palette="PuRd") +
  scale_x_continuous(breaks=c(9,12,15)) +
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

ggsave(here("Output", "Figures", "outbreak_length_vw_wF.svg"),
       height = 140,
       width = 176,
       units = "px",
       dpi = 72)


ggplot(last_days_ww_all, aes(x=inf_value, y=last_day_date, group=interaction(inf_value, city), fill=city)) +
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

ggsave(here("Output", "Figures", "outbreak_length_vw_inf.svg"),
       height = 140,
       width = 176,
       units = "px",
       dpi = 72)

# Create the temporal heatmap
ggplot(last_days_ww_all, aes(x = last_day_date, y = fct_rev(city), fill =wP)) +
  geom_tile() +
  scale_fill_distiller(palette = "RdYlBu", name = "Partial Vaccine",
                       breaks=seq(6,12,by=3), na.value = "grey50") +
  scale_y_discrete(expand=c(0,0))+
  scale_x_date(limits=c(min(last_days_ww_all$last_day_date), as.Date("2023-08-01")), 
               breaks = seq(min(last_days_ww_all$last_day_date), as.Date("2023-09-01"), by="2 months"),
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

ggsave(here("Output", "Figures", "outbreak_length_vaxanw_wP_hmap_no_legend.svg"),
       height = 140,
       width = 500,
       units = "px",
       dpi = 72)


# Create the temporal heatmap - 2 full vax
ggplot(last_days_ww_all, aes(x = last_day_date, y = fct_rev(city), fill =wF)) +
  geom_tile() +
  scale_fill_distiller(palette = "RdYlBu", name = "Full Vaccine",
                       breaks=seq(9,15,by=3), na.value = "grey50") +
  scale_y_discrete(expand=c(0,0))+
  scale_x_date(limits=c(min(last_days_ww_all$last_day_date), as.Date("2023-08-01")), 
               breaks = seq(min(last_days_ww_all$last_day_date), as.Date("2023-09-01"), by="2 months"),
               date_labels = "%b %Y",
               expand=c(0,0)) +
  labs(x = NULL, y = NULL) +
  theme(
    text = element_text(family = "Helvetica", size = 8, color = "#000000"),
    legend.position = "right",
    legend.justification = c(1, 1),
    legend.background = element_blank(),
    legend.key = element_blank(),
    panel.background = element_rect(colour = NA, fill = "#BCBCBC"),
    panel.grid = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA),
    axis.text.x = element_text(angle=45, hjust=1))

ggsave(here("Output", "Figures", "outbreak_length_vaxanw_wF_hmap_legend.svg"),
       height = 140,
       width = 500,
       units = "px",
       dpi = 72)

## Second heatmap
ggplot(last_days_ww_all, aes(x = last_day_date, y = fct_rev(city), fill = inf_value)) +
  geom_tile() +
  scale_fill_distiller(palette = "RdYlBu", name = "Infection prob",
                       breaks=seq(0.3,0.9,by=0.1)) +
  scale_y_discrete(expand=c(0,0))+
  scale_x_date(limits=c(min(last_days_ww_all$last_day_date), as.Date("2023-08-01")), 
               breaks = seq(min(last_days_ww_all$last_day_date), as.Date("2023-09-01"), by="2 months"),
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

ggsave(here("Output", "Figures", "outbreak_length_vaxwane_inf_hmap_nolegend.svg"),
       height = 140,
       width = 500,
       units = "px",
       dpi = 72)

