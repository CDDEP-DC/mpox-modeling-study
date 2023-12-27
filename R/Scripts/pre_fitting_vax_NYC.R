###############################
# FITTING TO RW VAX DATA
# This script takes data from the local health department dashboard on vaccination rates
# It then readjusts it to the network size

library(tidyverse)
library(lubridate)
library(here)

#458 2023-08-01                 0                 0       458              0              0 3364.208 1883.956
#458 2023-08-01                 0                 0       458              0              0 8274.439 4633

NYC_data_vax = read.csv(here("Data","Cities","New York City","vaccines_nyc.csv"))

NYC_data_vax_summary_totals = read.csv(here("Data","Cities","New York City","weekly-doses-borough.csv"))

NYC_data_vax_summary_totals$week_start_date = parse_date_time(NYC_data_vax_summary_totals$week_start_date, "m%d%y")

NYC_data_vax_summary_totals = NYC_data_vax_summary_totals %>%
  group_by(week_start_date) %>%
  reframe(admin_dose1 = sum(admin_dose1),
           admin_dose2 = sum(admin_dose2)) 

# Single dose
summary(NYC_data_vax_summary[,2] / NYC_data_vax_summary_totals[,2])
adj_single_dose = 1-0.1296

# Double dose
summary(NYC_data_vax_summary[,3] / NYC_data_vax_summary_totals[,3])
adj_double_doses = 1-0.09128

# First step is to break up into daily timesteps
NYC_data_vax <- NYC_data_vax %>%
  mutate(admin_dose1_daily = ifelse(is.na(admin_dose1_daily),0,admin_dose1_daily),
         admin_dose2_daily = ifelse(is.na(admin_dose2_daily),0,admin_dose2_daily))

NYC_data_vax$date = parse_date_time(NYC_data_vax$date, "m%d%y")
NYC_data_vax$TIME_STEP = seq(1,nrow(NYC_data_vax),1)

# Per NYC Dashboard
# Total Partial: 102183 (all sexes) / 75774 (men)
# Total Full: 52374 (all sexes) / 42433.44 (men)
# Source: https://www.nyc.gov/assets/doh/downloads/pdf/monkeypox/mpox-response-data-summary.pdf


# Apply the MSM proportion and scaling factor to normalize the daily rates
NYC_data_vax = NYC_data_vax %>%
  mutate(FD_ACTUAL_NORM = (admin_dose1_daily * (75774/102183)) /  225235.80 ,
         SD_ACTUAL_NORM = (admin_dose2_daily * (42433.44/52374)) /  225235.80 ) %>%
  mutate(FD_ACTUAL_NORM = FD_ACTUAL_NORM*adj_single_dose,
         SD_ACTUAL_NORM = SD_ACTUAL_NORM*adj_double_doses) %>%
  mutate(fd_check = cumsum(FD_ACTUAL_NORM*10000),
         sd_check = cumsum(SD_ACTUAL_NORM*10000))

assign("vd", NYC_data_vax)
save(vd,file=here("R","Model","vaxDataFit_NYC.Rdata"))

max(NYC_data_vax$FD_ACTUAL_NORM) * 10000
