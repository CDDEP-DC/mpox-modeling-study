###############################
# FITTING TO RW VAX DATA
# This script takes data from the local health department dashboard on vaccination rates
# It then readjusts it to the network size

library(tidyr)
library(lubridate)
library(here)

SF_data_vax = read.csv(here("Data","Cities","San Francisco","Mpox_Vaccinations_Given_to_SF_Residents_Over_Time.csv"))
SF_data_vax$date_administered = parse_date_time(SF_data_vax$date_administered, "y%m%d")

# Per SF Dashboard
# 25,920 recipients for the vax self-identified as male
# Total Partial: 28,874 (all sexes)
# Total Full: 15,797 (all sexes)
# Source: https://sf.gov/data/mpox-vaccinations-demographics#vaccinations-by-sex

# Calculate the real-world MSM proportion
msm_proportion = 25920 / 28874

# Apply the MSM proportion and scaling factor to normalize the daily rates
SF_data_vax = SF_data_vax %>%
  mutate(FD_ACTUAL_NORM = new_first_dose_recipients * msm_proportion / 69643,
         SD_ACTUAL_NORM = new_second_dose_recipients * msm_proportion / 69643) %>%
  mutate(TIME_STEP = seq(1,nrow(SF_data_vax),by=1)) %>%
  mutate(fd_check = cumsum(FD_ACTUAL_NORM*10000),
         sd_check = cumsum(SD_ACTUAL_NORM*10000))  
  

date_diff = as.numeric(difftime(as.Date("2023-08-01"),as.Date("2022-12-31"), units = "days"))

for(i in 1:date_diff){
  SF_data_vax = SF_data_vax %>%
    add_row(date_administered=as.Date(as.Date("2022-12-31")+(1*i)),
            FD_ACTUAL_NORM = 0,
            SD_ACTUAL_NORM = 0,
            TIME_STEP = nrow(SF_data_vax) + 1)
}
assign("vd", SF_data_vax)
save(vd,file=here("R","Model","vaxDataFit_SF.Rdata"))

load(here("R","Model","vaxDataFit_SF.Rdata"))