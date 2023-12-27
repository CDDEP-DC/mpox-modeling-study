# Scale case data

library(tidyr)
library(dplyr)
library(lubridate)
library(here)

CHI_data_cases = readxl::read_xlsx(here("Data","Cities","Chicago","cases_chicago.xlsx"))
colnames(CHI_data_cases) = c("week", "weekly_cases")
CHI_data_cases$scaled_weekly_cases = (CHI_data_cases$weekly_cases / 131704 )* 10000

write.csv(CHI_data_cases, here("Data", "Cities","Chicago", "scaled_cases.csv"))
save(CHI_data_cases, file = here("Output", "Processed Files", "scaled_cases_chicago.Rdata"))