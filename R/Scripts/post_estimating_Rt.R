# 1. Load Libraries & Read Data
library(dplyr)
library(ggplot2)
library(zoo)
library(here)

# Initialize an empty list to store data frames
dfs <- list()


# Loop through each simulation run and extract data
for (i in 1:100) {
  simobject = get("sim_as_0.7_inf_0.7")
  sim_obj_name = "sim_as_0.7_inf_0.7"
  sim_name <- paste0("sim", i)
  
  df <- data.frame(node_ID = simobject[["attr"]][[sim_name]][["unique_id"]],
                   generation = simobject[["attr"]][[sim_name]][["infGen"]],
                   time_infected = simobject[["attr"]][[sim_name]][["infTime"]],
                   risk_group = simobject[["attr"]][[sim_name]][["riskg"]],
                   time_recovered = simobject[["attr"]][[sim_name]][["recTime"]],
                   secondary_infections = simobject[["attr"]][[sim_name]][["secInfs"]],
                   simrun = sim_name)
  df$period_infectiousness = df$time_recovered - df$time_infected
  # Append to the list
  dfs[[i]] <- df
}

# Combine all data frames into a single data frame
all_data <- do.call(rbind, dfs)

all_data <- all_data %>%
  filter(!is.na(time_infected))

# 2. Basic Data Processing
# Calculate the start of the infectious period for each individual
delta <- 6.6
all_data$time_infectious_start <- all_data$time_infected + delta

# Define the weights based on risk group
weights <- c(rep(10000/1900, 5), 10000/500)
names(weights) <- 1:6
all_data$weight <- weights[all_data$risk_group]

# 3. Compute Weighted Rt
days <- max(all_data$time_infectious_start)
weighted_Rt <- data.frame(Day = 0:days)

# Round the time_infectious_start values to nearest integer
all_data$time_infectious_start <- round(all_data$time_infectious_start)

# Recalculate the daily_infectious data frame
daily_infectious <- all_data %>%
  group_by(time_infectious_start, simrun) %>%
  summarize(total_weight = sum(weight), 
            total_secondary_infections_weighted = sum(secondary_infections * weight))

# Merge with the weighted_Rt dataframe and recompute Weighted_Rt
weighted_Rt <- left_join(weighted_Rt, daily_infectious, by = c("Day" = "time_infectious_start"))
weighted_Rt$total_weight[is.na(weighted_Rt$total_weight)] <- 0
weighted_Rt$total_secondary_infections_weighted[is.na(weighted_Rt$total_secondary_infections_weighted)] <- 0
weighted_Rt$Weighted_Rt <- weighted_Rt$total_secondary_infections_weighted / weighted_Rt$total_weight
weighted_Rt$Weighted_Rt[is.na(weighted_Rt$Weighted_Rt) | is.infinite(weighted_Rt$Weighted_Rt)] <- 0

# Compute 7-day moving average
weighted_Rt$moving_avg <- rollmean(weighted_Rt$Weighted_Rt, 7, fill = NA, align = "right")

weighted_Rt_summary = weighted_Rt %>%
  group_by(Day) %>%
  summarise(
    Median = median(moving_avg, na.rm = TRUE),
    Mean = mean(moving_avg, na.rm = TRUE),
    Q2_5 = quantile(moving_avg, 0.025, na.rm = TRUE),
    Q25 = quantile(moving_avg, 0.25, na.rm = TRUE),
    Q75 = quantile(moving_avg, 0.75, na.rm = TRUE),
    Q97_5 = quantile(moving_avg, 0.975, na.rm = TRUE)
  )

weighted_Rt_summary$Day <- as.Date("2022-05-01") + as.numeric(weighted_Rt_summary$Day)


# 4. Visualization
ggplot(weighted_Rt_summary, aes(x = Day, y = Mean)) +
  annotate(geom = "rect", xmin = as.Date("2022-05-10"), xmax=as.Date("2023-08-01"), ymin=0, ymax=1, fill='#DC0000FF', alpha=0.1)+
  geom_line(color = "#3C5488FF") +
  geom_ribbon(aes(ymin=Q25, ymax=Q75), fill="#3C5488FF", alpha=0.4) +
  geom_ribbon(aes(ymin=Q2_5, ymax=Q97_5), fill="#3C5488FF", alpha=0.2) +
  labs(x = NULL, y = NULL) +
  scale_y_continuous(breaks=seq(0,6,by=0.5), limits=c(0,6), expand=c(0,0)) +
  scale_x_date(limits=c(as.Date("2022-05-10"), as.Date("2023-08-01")), 
               breaks = seq(as.Date("2022-05-01"), as.Date("2023-09-01"), by="2 months"),
               date_labels = "%b %Y", expand=c(0,0)) +
  theme(
    text = element_text(family = "Helvetica", size = 14, color = "#000000"),
    legend.position = "none",
    legend.justification = c(1, 1),
    legend.background = element_blank(),
    legend.key = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    panel.grid.major.x = element_line(color="gray", linewidth=0.25),
    #axis.text.x = element_text(angle = 45, hjust = 1),    # If bottom part of panel, then enable this and make sure to use right dimensions on export
    axis.text.x = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA))


ggsave(here("Output", "Figures", paste0("Rt_", sim_obj_name, ".svg")),
       height = 223.4276,
       width = 446.85,
       # height = 245.961,     # If bottom of panel
       # width = 446.85,        # If bottom of panel
       units = "px",
       dpi = 72)

# Compute total weighted secondary infections and total weighted infectious individuals
first_gen <- all_data %>% 
  filter(generation %in% 1:2) %>%
  group_by(risk_group, simrun) %>%
  summarise(
    total_weighted_secondary_infections = sum(secondary_infections * weight),
    total_weighted_infectious_individuals = sum(weight)
  )

# Compute R0 for each simulation run
R0_per_run <- first_gen %>%
  group_by(simrun) %>%
  summarise(R0 = sum(total_weighted_secondary_infections) / sum(total_weighted_infectious_individuals))

# Compute summary statistics for R0 across all simulation runs
R0_summary <- R0_per_run %>%
  summarise(
    Mean = mean(R0),
    Median = median(R0),
    Q2_5 = quantile(R0, 0.025),
    Q25 = quantile(R0, 0.25),
    Q75 = quantile(R0, 0.75),
    Q97_5 = quantile(R0, 0.975)
  )

write.csv(R0_summary, here("Output","R0 Estimates",paste0("RO_summary_", sim_obj_name, ".csv")))
