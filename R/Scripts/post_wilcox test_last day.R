library(rstatix)
library(here)
library(tidyverse)

load(here("Output", "Processed Files", "last_day_asy_all.Rdata"))
load(here("Output", "Processed Files", "last_day_vw_all.Rdata"))
load(here("Output", "Processed Files", "last_day_re_all.Rdata"))


last_days_asy_all$mechanism = "Underdetection"
last_days_re_all$mechanism = "Reinfection"
last_days_ww_all$mechanism = "Vaccine waning"

all_last_days = bind_rows(last_days_asy_all,last_days_re_all,last_days_ww_all)


# Comparing between NYC Mechanisms

all_last_days_NYC = all_last_days %>%
  filter(city == "New York City")

all_last_days_NYC <- all_last_days_NYC %>%
  filter(
    (mechanism == "Underdetection" & as_value == 0.7 & inf_value == 0.7) |
      (mechanism == "Reinfection" & waning_months == 6 & inf_value == 0.8) |
      (mechanism == "Vaccine waning" & wP_wF == "6 / 9" & inf_value == 0.8) 
  )

wilcox_test(last_day ~ mechanism, p.adjust.method = "bonferroni", data = all_last_days_NYC)