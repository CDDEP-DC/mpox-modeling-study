# Scale case data

library(tidyr)
library(dplyr)
library(lubridate)
library(here)

SF_data_cases = read.csv(here("Data","Cities","San Francisco","cases_sanfran_updated.csv"))
SF_data_cases = SF_data_cases[,1:2]
colnames(SF_data_cases) = c("date", "cases")
SF_data_cases$date = parse_date_time(SF_data_cases$date, "m%d%y")
SF_data_cases$scaled_daily_cases = (SF_data_cases$cases / 69643 )* 10000

# Convert the date to a "week starting" date
SF_data_cases <- SF_data_cases %>%
  mutate(week_starting = floor_date(date, unit="week"))

# Sum up the cases and scaled_daily_cases by week
SF_weekly_data <- SF_data_cases %>%
  group_by(week_starting) %>%
  summarise(
    weekly_cases = sum(cases),
    weekly_scaled_cases = sum(scaled_daily_cases)
  )

save(SF_weekly_data, file = here("Output", "Processed Files", "scaled_cases_sf.Rdata"))