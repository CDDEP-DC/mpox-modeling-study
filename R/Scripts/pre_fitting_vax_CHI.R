###############################
# FITTING TO RW VAX DATA
# This script takes data from the local health department dashboard on vaccination rates
# It then readjusts it to the network size

library(tidyr)
library(lubridate)
library(here)

CHI_data_vax = readxl::read_xlsx(here("Data","Cities","Chicago","vaccines_chicago.xlsx"))

# First step is to break up into daily timesteps
CHI_data_vax <- CHI_data_vax %>%
  rowwise() %>%
  do(data.frame(
    Date = seq(.data$Week, length.out = 7, by="days"),
    Actual_FD_Daily = .data$Actual_FD / 7,
    Actual_SD_Daily = .data$Actual_SD / 7
  )) %>%
  ungroup()

# Show the head of the daily data
CHI_data_vax = CHI_data_vax[2:nrow(CHI_data_vax),]

# Per SF Dashboard
# Total Partial: 30557 (all sexes) / 28040 (men)
# Total Full: 19711 (all sexes) / 18538 (men)
# Source: https://www.chicago.gov/city/en/sites/monkeypox/home/data.html


# Apply the MSM proportion and scaling factor to normalize the daily rates
CHI_data_vax = CHI_data_vax %>%
  mutate(FD_ACTUAL_NORM = Actual_FD_Daily * (28040/30557) / 131704,
         SD_ACTUAL_NORM = Actual_SD_Daily * (18538/19711) / 131704) %>%
  mutate(TIME_STEP = seq(1,nrow(CHI_data_vax),by=1)) %>%
  mutate(fd_check = cumsum(FD_ACTUAL_NORM*10000),
         sd_check = cumsum(SD_ACTUAL_NORM*10000))  
  
assign("vd", CHI_data_vax)
save(vd,file=here("R","Model","vaxDataFit_CHI.Rdata"))