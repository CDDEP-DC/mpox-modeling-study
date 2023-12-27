# Scale case data

library(tidyr)
library(dplyr)
library(lubridate)
library(here)
library(zoo)

NYC_data_cases = read.csv(here("Data","Cities","New York City","cases_nyc.csv"))
colnames(NYC_data_cases) = c("date", "cases")
NYC_data_cases$date = parse_date_time(NYC_data_cases$date, "m%d%y")
#NYC_data_cases$scaled_daily_cases = (NYC_data_cases$cases / 91576 )* 10000
NYC_data_cases$scaled_daily_cases = (NYC_data_cases$cases / 225235.80 )* 10000

NYC_data_cases$moving_avg_daily = rollmean(NYC_data_cases$scaled_daily_cases, 7, align = "right", fill = NA)

NYC_data_cases$moving_avg_daily[is.na(NYC_data_cases$moving_avg_daily)] <- 0

# Per NYC DOH GitHub
# https://github.com/nychealth/monkeypox-data/blob/main/totals/summary-cases.csv
# There were about 49 cases reported in Staten Island

# So we need to adjust by some fraction


# Convert the date to a "week starting" date
NYC_data_cases <- NYC_data_cases %>%
  mutate(week_starting = floor_date(date, unit="week"))

# Sum up the cases and scaled_daily_cases by week
NYC_weekly_data <- NYC_data_cases %>%
  group_by(week_starting) %>%
  summarise(
    weekly_cases = sum(cases),
    weekly_scaled_cases = sum(scaled_daily_cases)) 

adj_frac = (sum(NYC_weekly_data$weekly_cases)-49-2) / sum(NYC_weekly_data$weekly_cases) 


NYC_weekly_data <- NYC_weekly_data %>%
  mutate(weekly_scaled_cases_adjusted = weekly_scaled_cases * adj_frac)

save(NYC_weekly_data, file = here("Output", "Processed Files", "scaled_cases_nyc.Rdata"))