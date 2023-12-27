library(tidyverse)
library(xlsx)

load(here("Output", "Processed Files", "last_day_asy_all.Rdata"))
load(here("Output", "Processed Files", "last_day_re_all.Rdata"))
load(here("Output", "Processed Files", "last_day_vw_all.Rdata"))

## By parameter AND city

stats_last_day_as_asp = last_days_asy_all %>%
  group_by(as_value, city) %>%
  summarise(mean = mean(last_day),
            median = median(last_day),
            P25 = quantile(last_day, probs = 0.25),
            P75 = quantile(last_day, probs = 0.75),
            sd = sd(last_day))%>%
  ungroup()

stats_last_day_as_inf = last_days_asy_all %>%
  group_by(inf_value, city) %>%
  summarise(mean = mean(last_day),
            median = median(last_day),
            P25 = quantile(last_day, probs = 0.25),
            P75 = quantile(last_day, probs = 0.75),
            sd = sd(last_day))%>%
  ungroup()

stats_last_day_vw_wPwF = last_days_ww_all %>%
  group_by(wP_wF, city) %>%
  summarise(mean = mean(last_day),
            median = median(last_day),
            P25 = quantile(last_day, probs = 0.25),
            P75 = quantile(last_day, probs = 0.75),
            sd = sd(last_day))%>%
  ungroup()

stats_last_day_vw_inf = last_days_ww_all %>%
  group_by(inf_value, city) %>%
  summarise(mean = mean(last_day),
            median = median(last_day),
            P25 = quantile(last_day, probs = 0.25),
            P75 = quantile(last_day, probs = 0.75),
            sd = sd(last_day))%>%
  ungroup()

stats_last_day_re_waning = last_days_re_all %>%
  group_by(waning_months, city) %>%
  summarise(mean = mean(last_day),
            median = median(last_day),
            P25 = quantile(last_day, probs = 0.25),
            P75 = quantile(last_day, probs = 0.75),
            sd = sd(last_day)) %>%
  ungroup()

stats_last_day_re_inf = last_days_re_all %>%
  group_by(inf_value, city) %>%
  summarise(mean = mean(last_day),
            median = median(last_day),
            P25 = quantile(last_day, probs = 0.25),
            P75 = quantile(last_day, probs = 0.75),
            sd = sd(last_day)) %>%
  ungroup()


write.xlsx(stats_last_day_as_asp,
           file=here("Output", "Processed Files", "last_day_stats.xlsx"),
           sheetName="Asymp - Asymp")

write.xlsx(stats_last_day_as_inf,
           file=here("Output", "Processed Files", "last_day_stats.xlsx"),
           sheetName="Asymp - Inf", append=TRUE)

write.xlsx(stats_last_day_vw_wPwF,
           file=here("Output", "Processed Files", "last_day_stats.xlsx"),
           sheetName="Vax Wane - Vax", append=TRUE)

write.xlsx(stats_last_day_vw_inf,
           file=here("Output", "Processed Files", "last_day_stats.xlsx"),
           sheetName="Vax Wane - Inf", append=TRUE)

write.xlsx(stats_last_day_re_waning,
           file=here("Output", "Processed Files", "last_day_stats.xlsx"),
           sheetName="Reinfection - Waning", append=TRUE)

write.xlsx(stats_last_day_re_inf,
           file=here("Output", "Processed Files", "last_day_stats.xlsx"),
           sheetName="Reinfection - Inf", append=TRUE)

## Only by parameter

stats_last_day_as_asp = last_days_asy_all %>%
  group_by(as_value) %>%
  summarise(mean = mean(last_day),
            median = median(last_day),
            P25 = quantile(last_day, probs = 0.25),
            P75 = quantile(last_day, probs = 0.75),
            sd = sd(last_day))%>%
  ungroup()

last_days_asy_all = last_days_asy_all %>%
  mutate(inf_as = paste0(as_value, "_", inf_value))

ggplot(last_days_asy_all, aes(x=last_day_date, y=as_value))+
  geom_bar()+
  facet_grid(cols = vars(city), scales = "free_y", space = "free_y") +
  theme_minimal()

stats_last_day_as_asp = last_days_asy_all %>%
  group_by(inf_as) %>%
  summarise(mean = mean(last_day),
            median = median(last_day),
            P25 = quantile(last_day, probs = 0.25),
            P75 = quantile(last_day, probs = 0.75),
            sd = sd(last_day))%>%
  ungroup()

stats_last_day_as_inf = last_days_asy_all %>%
  group_by(inf_value) %>%
  summarise(mean = mean(last_day),
            median = median(last_day),
            P25 = quantile(last_day, probs = 0.25),
            P75 = quantile(last_day, probs = 0.75),
            sd = sd(last_day))%>%
  ungroup()

stats_last_day_vw_wPwF = last_days_ww_all %>%
  group_by(wP_wF) %>%
  summarise(mean = mean(last_day),
            median = median(last_day),
            P25 = quantile(last_day, probs = 0.25),
            P75 = quantile(last_day, probs = 0.75),
            sd = sd(last_day))%>%
  ungroup()

stats_last_day_vw_inf = last_days_ww_all %>%
  group_by(inf_value) %>%
  summarise(mean = mean(last_day),
            median = median(last_day),
            P25 = quantile(last_day, probs = 0.25),
            P75 = quantile(last_day, probs = 0.75),
            sd = sd(last_day))%>%
  ungroup()

stats_last_day_re_waning = last_days_re_all %>%
  group_by(waning_months) %>%
  summarise(mean = mean(last_day),
            median = median(last_day),
            P25 = quantile(last_day, probs = 0.25),
            P75 = quantile(last_day, probs = 0.75),
            sd = sd(last_day)) %>%
  ungroup()

stats_last_day_re_inf = last_days_re_all %>%
  group_by(inf_value) %>%
  summarise(mean = mean(last_day),
            median = median(last_day),
            P25 = quantile(last_day, probs = 0.25),
            P75 = quantile(last_day, probs = 0.75),
            sd = sd(last_day)) %>%
  ungroup()


write.xlsx(stats_last_day_as_asp,
           file=here("Output", "Processed Files", "last_day_stats_grouped.xlsx"),
           sheetName="Asymp - Asymp")

write.xlsx(stats_last_day_as_inf,
           file=here("Output", "Processed Files", "last_day_stats_grouped.xlsx"),
           sheetName="Asymp - Inf", append=TRUE)

write.xlsx(stats_last_day_vw_wPwF,
           file=here("Output", "Processed Files", "last_day_stats_grouped.xlsx"),
           sheetName="Vax Wane - Vax", append=TRUE)

write.xlsx(stats_last_day_vw_inf,
           file=here("Output", "Processed Files", "last_day_stats_grouped.xlsx"),
           sheetName="Vax Wane - Inf", append=TRUE)

write.xlsx(stats_last_day_re_waning,
           file=here("Output", "Processed Files", "last_day_stats_grouped.xlsx"),
           sheetName="Reinfection - Waning", append=TRUE)

write.xlsx(stats_last_day_re_inf,
           file=here("Output", "Processed Files", "last_day_stats_grouped.xlsx"),
           sheetName="Reinfection - Inf", append=TRUE)