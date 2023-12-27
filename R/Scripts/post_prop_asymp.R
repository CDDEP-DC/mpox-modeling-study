## Get last days for chicago

load(here("Output","Processed Files","asy_chi_processed_files.RData"))
assign("asymptomatic_chi", get("processed_files"))
rm("processed_files")

# Overwrite your list to only contain the 'ei.flow' data frames from each sublist
asymptomatic_chi_num.a <- lapply(asymptomatic_chi, function(x) x[['ea.flow']])


# Convert daily data to weekly
asymptomatic_chi_num.a_weekly <- lapply(asymptomatic_chi_num.a, function(df) {
  n_rows <- nrow(df)
  n_weeks <- n_rows %/% 7
  
  df_weekly <- matrix(0, nrow = n_weeks, ncol = ncol(df))
  colnames(df_weekly) <- colnames(df)
  
  for (i in seq_len(n_weeks)) {
    weekly_rows = ((i - 1) * 7 + 1):(i * 7)
    df_weekly[i, ] <- colSums(df[weekly_rows, ], na.rm = TRUE)
  }
  
  return(as.data.frame(df_weekly))
})

# Add dates to the aggregated data
date_sequence_weekly <- seq.Date(from = as.Date("2022-05-01"), by = "weeks", length.out = nrow(asymptomatic_chi_num.a_weekly[[1]]))

# Apply summary statistics calculation to each data frame in the list
asymptomatic_chi_num.a_stats <- lapply(asymptomatic_chi_num.a_weekly, function(df) {
  stats_summary <- data.frame(
    median = apply(df, 1, median, na.rm = TRUE),
    mean = apply(df, 1, mean, na.rm = TRUE),
    sd = apply(df, 1, sd, na.rm = TRUE),
    min = apply(df, 1, min, na.rm = TRUE),
    max = apply(df, 1, max, na.rm = TRUE),
    perc2.5 = apply(df, 1, quantile, probs = 0.025, na.rm = TRUE),
    perc97.5 = apply(df, 1, quantile, probs = 0.975, na.rm = TRUE),
    perc25 = apply(df, 1, quantile, probs = 0.25, na.rm = TRUE),
    perc75 = apply(df, 1, quantile, probs = 0.75, na.rm = TRUE)
  )
  
  stats_summary$date <- as.POSIXct(date_sequence_weekly[1:nrow(stats_summary)], tz = "UTC")
  return(stats_summary)
})

save(asymptomatic_chi_num.a_stats, file = here("Output","Processed Files", "prop_asymp_CHI_stats.Rdata"))


## NYC ##

load(here("Output","Processed Files","asy_new_processed_files.RData"))
assign("asymptomatic_nyc", get("processed_files"))
rm("processed_files")

# Overwrite your list to only contain the 'ei.flow' data frames from each sublist
asymptomatic_nyc_num.a <- lapply(asymptomatic_nyc, function(x) x[['ea.flow']])

# Convert daily data to weekly
asymptomatic_nyc_num.a_weekly <- lapply(asymptomatic_nyc_num.a, function(df) {
  n_rows <- nrow(df)
  n_weeks <- n_rows %/% 7
  
  df_weekly <- matrix(0, nrow = n_weeks, ncol = ncol(df))
  colnames(df_weekly) <- colnames(df)
  
  for (i in seq_len(n_weeks)) {
    weekly_rows = ((i - 1) * 7 + 1):(i * 7)
    df_weekly[i, ] <- colSums(df[weekly_rows, ], na.rm = TRUE)
  }
  
  return(as.data.frame(df_weekly))
})

# Add dates to the aggregated data
date_sequence_weekly <- seq.Date(from = as.Date("2022-05-01"), by = "weeks", length.out = nrow(asymptomatic_nyc_num.a_weekly[[1]]))

# Apply summary statistics calculation to each data frame in the list
asymptomatic_nyc_num.a_stats <- lapply(asymptomatic_nyc_num.a_weekly, function(df) {
  stats_summary <- data.frame(
    median = apply(df, 1, median, na.rm = TRUE),
    mean = apply(df, 1, mean, na.rm = TRUE),
    sd = apply(df, 1, sd, na.rm = TRUE),
    min = apply(df, 1, min, na.rm = TRUE),
    max = apply(df, 1, max, na.rm = TRUE),
    perc2.5 = apply(df, 1, quantile, probs = 0.025, na.rm = TRUE),
    perc97.5 = apply(df, 1, quantile, probs = 0.975, na.rm = TRUE),
    perc25 = apply(df, 1, quantile, probs = 0.25, na.rm = TRUE),
    perc75 = apply(df, 1, quantile, probs = 0.75, na.rm = TRUE)
  )
  
  stats_summary$date <- as.POSIXct(date_sequence_weekly[1:nrow(stats_summary)], tz = "UTC")
  return(stats_summary)
})

save(asymptomatic_nyc_num.a_stats, file = here("Output","Processed Files", "prop_asymp_NYC_stats.Rdata"))

#### SAN FRAN

load(here("Output","Processed Files","asy_san_processed_files.RData"))
assign("asymptomatic_sf", get("processed_files"))
rm("processed_files")

# Overwrite your list to only contain the 'ei.flow' data frames from each sublist
asymptomatic_sf_num.a <- lapply(asymptomatic_sf, function(x) x[['ea.flow']])

# Convert daily data to weekly
asymptomatic_sf_num.a_weekly <- lapply(asymptomatic_sf_num.a, function(df) {
  n_rows <- nrow(df)
  n_weeks <- n_rows %/% 7
  
  df_weekly <- matrix(0, nrow = n_weeks, ncol = ncol(df))
  colnames(df_weekly) <- colnames(df)
  
  for (i in seq_len(n_weeks)) {
    weekly_rows = ((i - 1) * 7 + 1):(i * 7)
    df_weekly[i, ] <- colSums(df[weekly_rows, ], na.rm = TRUE)
  }
  
  return(as.data.frame(df_weekly))
})

# Add dates to the aggregated data
date_sequence_weekly <- seq.Date(from = as.Date("2022-05-01"), by = "weeks", length.out = nrow(asymptomatic_sf_num.a_weekly[[1]]))

# Apply summary statistics calculation to each data frame in the list
asymptomatic_sf_num.a_stats <- lapply(asymptomatic_sf_num.a_weekly, function(df) {
  stats_summary <- data.frame(
    median = apply(df, 1, median, na.rm = TRUE),
    mean = apply(df, 1, mean, na.rm = TRUE),
    sd = apply(df, 1, sd, na.rm = TRUE),
    min = apply(df, 1, min, na.rm = TRUE),
    max = apply(df, 1, max, na.rm = TRUE),
    perc2.5 = apply(df, 1, quantile, probs = 0.025, na.rm = TRUE),
    perc97.5 = apply(df, 1, quantile, probs = 0.975, na.rm = TRUE),
    perc25 = apply(df, 1, quantile, probs = 0.25, na.rm = TRUE),
    perc75 = apply(df, 1, quantile, probs = 0.75, na.rm = TRUE)
  )
  
  stats_summary$date <- as.POSIXct(date_sequence_weekly[1:nrow(stats_summary)], tz = "UTC")
  return(stats_summary)
})

save(asymptomatic_sf_num.a_stats, file = here("Output","Processed Files", "prop_asymp_SF_stats.Rdata"))


mypal <- brewer.pal(9, "RdBu")

index_of_first_zero <- min(which(asymptomatic_sf_num.a_stats[["as_0.9_inf_0.9"]][["median"]] <= 1.0))
date_of_first_zero_median <- asymptomatic_sf_num.a_stats[["as_0.9_inf_0.9"]][["date"]][index_of_first_zero]

index_of_first_zero <- min(which(asymptomatic_sf_num.a_stats[["as_0.9_inf_0.9"]][["mean"]] <= 1.0))
date_of_first_zero_mean <- asymptomatic_sf_num.a_stats[["as_0.9_inf_0.9"]][["date"]][index_of_first_zero]

ggplot(asymptomatic_sf_num.a_stats$as_0.9_inf_0.9, aes(x=date, y=median))+
  geom_vline(aes(xintercept = date_of_first_zero_median), linetype = "longdash",size=0.25, col=mypal[8])+
  geom_vline(aes(xintercept = date_of_first_zero_mean), linetype = "longdash",size=0.25, col=mypal[8])+
  geom_line(aes(col="Model"),size=0.5)+
  geom_line(aes(x=date, y=mean, col="Model"), linetype = "dotted",size=0.5)+
  geom_ribbon(aes(ymin=perc25, ymax=perc75, fill="Model"), alpha=0.20)+
  geom_ribbon(aes(ymin=perc2.5, ymax=perc97.5, fill="Model"), alpha=0.10)+
  scale_x_datetime(date_breaks = "2 months", 
                   date_labels = "%b %Y", 
                   limits = c(as.POSIXct("2022-05-07"), as.POSIXct("2023-07-23")),
                   expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0), limits=c(0,350), breaks = seq(0,350,50)) +
  scale_color_manual(name = "Legend", values = c("Model" = mypal[2], "Observed" = "black")) +
  scale_fill_manual(name = "Legend", values = c("Model" = mypal[2])) +
  theme(
    text = element_text(family = "Helvetica", size = 8),
    legend.position = "NONE",
    legend.justification = c(1, 1),
    legend.background = element_blank(),
    legend.key = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA),
    axis.text.x = element_text(angle = 45, hjust = 1)) + 
  guides(fill = "none", color = guide_legend(title = NULL)) +
  labs(x = NULL, y = NULL)

ggsave(here("Output", "Figures", "num_a_as_sf.svg"),
       height = 140,
       width = 176,
       units = "px",
       dpi = 72)

index_of_first_zero <- min(which(asymptomatic_nyc_num.a_stats[["as_0.7_inf_0.7"]][["median"]][5:65] <= 1.0))
date_of_first_zero_median <- asymptomatic_nyc_num.a_stats[["as_0.7_inf_0.7"]][["date"]][index_of_first_zero]

index_of_first_zero <- min(which(asymptomatic_nyc_num.a_stats[["as_0.7_inf_0.7"]][["mean"]][5:65] <= 1.0))
date_of_first_zero_mean <- asymptomatic_nyc_num.a_stats[["as_0.7_inf_0.7"]][["date"]][index_of_first_zero]


ggplot(asymptomatic_nyc_num.a_stats$as_0.7_inf_0.7, aes(x=date, y=median))+
  geom_vline(aes(xintercept = date_of_first_zero_median), linetype = "longdash",size=0.25, col=mypal[8])+
  geom_vline(aes(xintercept = date_of_first_zero_mean), linetype = "longdash",size=0.25, col=mypal[8])+
  geom_line(aes(col="Model"),size=0.5)+
  geom_line(aes(x=date, y=mean, col="Model"), linetype = "dotted",size=0.5)+
  geom_ribbon(aes(ymin=perc25, ymax=perc75, fill="Model"), alpha=0.20)+
  geom_ribbon(aes(ymin=perc2.5, ymax=perc97.5, fill="Model"), alpha=0.10)+
  scale_x_datetime(date_breaks = "2 months", 
                   date_labels = "%b %Y", 
                   limits = c(as.POSIXct("2022-05-07"), as.POSIXct("2023-07-23")),
                   expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0), limits=c(0,350), breaks = seq(0,350,50)) +
  scale_color_manual(name = "Legend", values = c("Model" = mypal[2], "Observed" = "black")) +
  scale_fill_manual(name = "Legend", values = c("Model" = mypal[2])) +
  theme(
    text = element_text(family = "Helvetica", size = 8),
    legend.position = "NONE",
    legend.justification = c(1, 1),
    legend.background = element_blank(),
    legend.key = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA),
    axis.text.x = element_text(angle = 45, hjust = 1)) + 
  guides(fill = "none", color = guide_legend(title = NULL)) +
  labs(x = NULL, y = NULL)

ggsave(here("Output", "Figures", "num_a_as_nyc.svg"),
       height = 140,
       width = 176,
       units = "px",
       dpi = 72)


index_of_first_zero <- min(which(asymptomatic_chi_num.a_stats[["as_0.9_inf_0.7"]][["median"]][5:65] <= 1.0))
date_of_first_zero_median <- asymptomatic_chi_num.a_stats[["as_0.9_inf_0.7"]][["date"]][index_of_first_zero]

index_of_first_zero <- min(which(asymptomatic_chi_num.a_stats[["as_0.9_inf_0.7"]][["mean"]][5:65] <= 1.0))
date_of_first_zero_mean <- asymptomatic_chi_num.a_stats[["as_0.9_inf_0.7"]][["date"]][index_of_first_zero]


ggplot(asymptomatic_chi_num.a_stats$as_0.9_inf_0.7, aes(x=date, y=median))+
  geom_vline(aes(xintercept = date_of_first_zero_median), linetype = "longdash",size=0.25, col=mypal[8])+
  geom_vline(aes(xintercept = date_of_first_zero_mean), linetype = "longdash",size=0.25, col=mypal[8])+
  geom_line(aes(col="Model"),size=0.5)+
  geom_line(aes(x=date, y=mean, col="Model"), linetype = "dotted",size=0.5)+
  geom_ribbon(aes(ymin=perc25, ymax=perc75, fill="Model"), alpha=0.20)+
  geom_ribbon(aes(ymin=perc2.5, ymax=perc97.5, fill="Model"), alpha=0.10)+
  scale_x_datetime(date_breaks = "2 months", 
                   date_labels = "%b %Y", 
                   limits = c(as.POSIXct("2022-05-07"), as.POSIXct("2023-07-23")),
                   expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0), limits=c(0,350), breaks = seq(0,350,50)) +
  scale_color_manual(name = "Legend", values = c("Model" = mypal[2], "Observed" = "black")) +
  scale_fill_manual(name = "Legend", values = c("Model" = mypal[2])) +
  theme(
    text = element_text(family = "Helvetica", size = 8),
    legend.position = "NONE",
    legend.justification = c(1, 1),
    legend.background = element_blank(),
    legend.key = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA),
    axis.text.x = element_text(angle = 45, hjust = 1)) + 
  guides(fill = "none", color = guide_legend(title = NULL)) +
  labs(x = NULL, y = NULL)

ggsave(here("Output", "Figures", "num_a_as_chi.svg"),
       height = 140,
       width = 176,
       units = "px",
       dpi = 72)
