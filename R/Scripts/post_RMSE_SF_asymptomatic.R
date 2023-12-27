# CHICAGO
# Extracting only ei.flow
library(zoo)
library(here)
library(ggplot2)
library(ggsci)
library(scales)
library(hrbrthemes)
library(tidyverse)

load(here("Output","Processed Files","asy_san_processed_files.RData"))
assign("asymptomatic_sf", get("processed_files"))
rm("processed_files")

# Overwrite your list to only contain the 'ei.flow' data frames from each sublist
asymptomatic_sf <- lapply(asymptomatic_sf, function(x) x[['ei.flow']])

# Convert daily data to weekly
asymptomatic_sf_weekly <- lapply(asymptomatic_sf, function(df) {
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
date_sequence_weekly <- seq.Date(from = as.Date("2022-05-01"), by = "weeks", length.out = nrow(asymptomatic_chicago_weekly[[1]]))

# Apply summary statistics calculation to each data frame in the list
asymptomatic_sf_stats <- lapply(asymptomatic_sf_weekly, function(df) {
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

# Extract the 'median' column from each data frame and store it in a list
extracted_medians <- lapply(asymptomatic_sf_stats, function(df) {
  return(df$median)
})

# Convert the list to a data frame
combined_medians_sf_asymp <- do.call(cbind, extracted_medians)

# Rename the columns to match the names of the original list elements
colnames(combined_medians_sf_asymp) <- names(asymptomatic_sf_stats)

# Initialize an empty data frame to store RMSE values
rmse_values_as_sf <- data.frame('ColumnName' = character(ncol(combined_medians_sf_asymp)),
                          'RMSE' = numeric(ncol(combined_medians_sf_asymp)),
                          "R2" = numeric(ncol(combined_medians_sf_asymp)),
                          stringsAsFactors = FALSE)

# Loop through each column of all_medians_7day_ma to calculate RMSE
for(i in 1:ncol(combined_medians_sf_asymp)) {
  sim_column <- combined_medians_sf_asymp[, i]
  
  # Get the overlapping range
  n_overlap <- min(length(sim_column), length(SF_weekly_data$weekly_scaled_cases))
  sim_column_overlap <- sim_column[1:n_overlap]
  SF_weekly_cases_overlap <- SF_weekly_data$weekly_scaled_cases[1:n_overlap]
  
  rmse_values_as_sf$ColumnName[i] <- colnames(combined_medians_sf_asymp)[i]
  rmse_values_as_sf$RMSE[i] <-  sqrt(mean((sim_column_overlap - SF_weekly_cases_overlap)^2))
  
  # Calculate R^2
  model <- lm(SF_weekly_cases_overlap ~ sim_column_overlap)
  rmse_values_as_sf$R2[i] <- summary(model)$r.squared
}

save(rmse_values_as_sf, file = here("Output", "Processed Files", "RMSE_sf_as.Rdata"))
save(combined_medians_sf_asymp, file = here("Output", "Processed Files", "simulated_cases_weekly_as_sf.Rdata"))
save(asymptomatic_sf_stats, file = here("Output", "Processed Files", "as_sf_stats_daily.Rdata"))
save(asymptomatic_sf_weekly, file = here("Output", "Processed Files", "as_sf_stats_weekly.Rdata"))

mypal <- pal_npg("nrc", alpha = 1)(5)

ggplot(asymptomatic_sf_stats$as_0.9_inf_0.9, aes(x=date, y=median))+
  geom_line(aes(col="Model"),size=0.5)+
  geom_line(aes(x=date, y=mean, col="Model"), linetype = "dotted",size=0.5)+
  geom_ribbon(aes(ymin=perc25, ymax=perc75, fill="Model"), alpha=0.20)+
  geom_ribbon(aes(ymin=perc2.5, ymax=perc97.5, fill="Model"), alpha=0.10)+
  geom_point(data = SF_weekly_data, aes(x=week_starting, y =weekly_scaled_cases, col="Observed"), size=0.5, shape = 3)+
  scale_x_datetime(date_breaks = "2 months", 
                   date_labels = "%b %Y", 
                   limits = c(as.POSIXct("2022-05-07"), as.POSIXct("2023-07-23")),
                   expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0), limits=c(0,50), breaks = seq(0,50,5)) +
  scale_color_manual(name = "Legend", values = c("Model" = mypal[1], "Observed" = "black")) +
  scale_fill_manual(name = "Legend", values = c("Model" = mypal[1])) +
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

ggsave(here("Output", "Figures", "as_sf.svg"),
       height = 140,
       width = 176,
       units = "px",
       dpi = 72)

rmse_values_as_sf <- rmse_values_as_sf %>%
  mutate(
    "Asymptomatic Transmission" = ifelse(str_detect(ColumnName, "^\\w+_0(?!\\.)"), "0", str_extract(ColumnName, "(?<=_)(\\d+\\.\\d+)(?=_inf_)")),
    "Infection Probability" = str_extract(ColumnName, "(?<=_inf_)(\\d+\\.\\d+)")
  ) %>%
  mutate(`Asymptomatic Transmission` = as.numeric(`Asymptomatic Transmission`),
         `Infection Probability` = as.numeric(`Infection Probability`))

ggplot(rmse_values_as_sf, aes(x=`Infection Probability`,y=`Asymptomatic Transmission`, fill=log(RMSE))) + 
  geom_tile() +
  scale_fill_material("pink") +
  scale_x_continuous(breaks=c(seq(0.3,0.9,0.1)), expand=c(0,0)) +
  scale_y_continuous(breaks=c(seq(0.0,0.9,0.1)), expand=c(0,0)) +
  guides(fill = guide_colourbar(barwidth = 0.5, barheight = 9)) +
  theme(
    text = element_text(family = "Helvetica", size = 8),
    legend.position = "right",
    legend.justification = c(1, 1),
    legend.background = element_blank(),
    legend.key = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.ticks = element_blank()) +
  labs(x = NULL, y = NULL)

ggsave(here("Output", "Figures", "as_sf_RMSE_grid.svg"),
       height = 200,
       width = 250,
       units = "px",
       dpi = 72)