## Boxplots
library(here)
library(tidyverse)
library(RColorBrewer)

## Boxplot for Length of Outbreak for Asymp. Transmission Mechanisms -------
load(here("Output", "Processed Files", "last_day_asy_all.Rdata"))

my_cols = brewer.pal(9, "PuRd")

ggplot(last_days_asy_all, aes(x=as_value, y=last_day_date, group=interaction(as_value, city))) +
  geom_violin(aes(fill=city), color = "gray", linewidth=0.25, bw=20, alpha=0.5)+
  stat_summary(fun.min = function(z) { quantile(z,0.25) },
               fun.max = function(z) { quantile(z,0.75) },
               fun = "median", color = "black", size=0.25) +
  stat_summary(fun = "median",
               geom = "point", shape = 23, size = 5, 
               color = "black", aes(fill = city)) +
  scale_fill_manual(values=c("Chicago" = my_cols[3], 
                             "New York City" = my_cols[5], 
                             "San Francisco" = my_cols[7]))+
  #scale_x_continuous(breaks=c(6,9,12)) +
  scale_y_date(limits=c(as.Date("2022-05-01"), as.Date("2023-09-01")), 
               breaks = seq(as.Date("2022-04-01"), as.Date("2023-09-01"), by="2 months"),
               date_labels = "%b %Y") +
  coord_flip() +
  facet_grid(cols = vars(city), scales = "free_y", space = "free_y") +
  labs(x = NULL, y = NULL) +
  theme(text = element_text(family = "Helvetica", size = 18, color = "#000000"),
        strip.background = element_blank(),
        strip.text = element_blank(),
        legend.position = "none",
        legend.justification = c(1, 1),
        legend.background = element_blank(),
        legend.key = element_blank(),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.grid = element_blank(),
        panel.grid.major.x = element_line(color="gray", linewidth=0.25),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank())

ggsave(here("Output", "Figures", "outbreak_length_as_as.svg"),
       height = 700,
       width = 800,
       units = "px",
       dpi = 72)

my_cols = brewer.pal(9, "Reds")

ggplot(last_days_asy_all, aes(x=inf_value, y=last_day_date, group=interaction(inf_value, city))) +
  geom_violin(aes(fill=city), color = "gray", linewidth=0.25, bw=20, alpha=0.5)+
  stat_summary(fun.min = function(z) { quantile(z,0.25) },
               fun.max = function(z) { quantile(z,0.75) },
               fun = "median", color = "black", size=0.25) +
  stat_summary(fun = "median",
               geom = "point", shape = 23, size = 5, 
               color = "black", aes(fill = city)) +
  scale_fill_manual(values=c("Chicago" = my_cols[3], 
                             "New York City" = my_cols[5], 
                             "San Francisco" = my_cols[7]))+
  scale_x_continuous(breaks=seq(0.3,0.9,0.1)) +
  scale_y_date(limits=c(as.Date("2022-05-01"), as.Date("2023-09-01")), 
               breaks = seq(as.Date("2022-04-01"), as.Date("2023-09-01"), by="2 months"),
               date_labels = "%b %Y") +
  coord_flip() +
  facet_grid(cols = vars(city), scales = "free_y", space = "free_y") +
  labs(x = NULL, y = NULL) +
  theme(text = element_text(family = "Helvetica", size = 18, color = "#000000"),
        strip.background = element_blank(),
        strip.text = element_blank(),
        legend.position = "none",
        legend.justification = c(1, 1),
        legend.background = element_blank(),
        legend.key = element_blank(),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.grid = element_blank(),
        panel.grid.major.x = element_line(color="gray", linewidth=0.25),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank())

ggsave(here("Output", "Figures", "outbreak_length_as_inf.svg"),
       height = 500,
       width = 800,
       units = "px",
       dpi = 72)

## Boxplot for Length of Outbreak for Vaccine-Waning Mechanisms -------
load(here("Output", "Processed Files", "last_day_vw_all.Rdata"))

last_days_ww_all = last_days_ww_all %>%
  mutate(wP_wF = paste0(wP, " / ", wF))

last_days_ww_all = last_days_ww_all %>%
  mutate(wP_wF = factor(wP_wF,
                        levels = c("6 / 9", "6 / 12", "6 / 15", "9 / 12", "9 / 15", "12 / 15"),
                        labels = c("6 / 9", "6 / 12", "6 / 15", "9 / 12", "9 / 15", "12 / 15")))

my_cols = brewer.pal(9, "Blues")

ggplot(last_days_ww_all, aes(x=wP_wF, y=last_day_date, group=interaction(wP_wF, city))) +
  geom_violin(aes(fill=city), color = "gray", linewidth=0.25, bw=20, alpha=0.5)+
  stat_summary(fun.min = function(z) { quantile(z,0.25) },
               fun.max = function(z) { quantile(z,0.75) },
               fun = "median", color = "black", size=0.25) +
  stat_summary(fun = "median",
               geom = "point", shape = 23, size = 5, 
               color = "black", aes(fill = city)) +
  scale_fill_manual(values=c("Chicago" = my_cols[3], 
                             "New York City" = my_cols[5], 
                             "San Francisco" = my_cols[7]))+
  #scale_x_continuous(breaks=c(6,9,12)) +
  scale_y_date(limits=c(as.Date("2022-05-01"), as.Date("2023-09-01")), 
               breaks = seq(as.Date("2022-04-01"), as.Date("2023-09-01"), by="2 months"),
               date_labels = "%b %Y") +
  coord_flip() +
  facet_grid(cols = vars(city), scales = "free_y", space = "free_y") +
  labs(x = NULL, y = NULL) +
  theme(text = element_text(family = "Helvetica", size = 18, color = "#000000"),
        strip.background = element_blank(),
        strip.text = element_blank(),
        legend.position = "none",
        legend.justification = c(1, 1),
        legend.background = element_blank(),
        legend.key = element_blank(),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.grid = element_blank(),
        panel.grid.major.x = element_line(color="gray", linewidth=0.25),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank())

ggsave(here("Output", "Figures", "outbreak_length_as_vw_vaccines.svg"),
       height = 500,
       width = 800,
       units = "px",
       dpi = 72)

my_cols = brewer.pal(9, "Reds")

ggplot(last_days_ww_all, aes(x=inf_value, y=last_day_date, group=interaction(inf_value, city))) +
  geom_violin(aes(fill=city), color = "gray", linewidth=0.25, bw=20, alpha=0.5)+
  stat_summary(fun.min = function(z) { quantile(z,0.25) },
               fun.max = function(z) { quantile(z,0.75) },
               fun = "median", color = "black", size=0.25) +
  stat_summary(fun = "median",
               geom = "point", shape = 23, size = 5, 
               color = "black", aes(fill = city)) +
  scale_fill_manual(values=c("Chicago" = my_cols[3], 
                             "New York City" = my_cols[5], 
                             "San Francisco" = my_cols[7]))+
  scale_x_continuous(breaks=seq(0.3,0.9,0.1)) +
  scale_y_date(limits=c(as.Date("2022-05-01"), as.Date("2023-09-01")), 
               breaks = seq(as.Date("2022-04-01"), as.Date("2023-09-01"), by="2 months"),
               date_labels = "%b %Y") +
  coord_flip() +
  facet_grid(cols = vars(city), scales = "free_y", space = "free_y") +
  labs(x = NULL, y = NULL) +
  theme(text = element_text(family = "Helvetica", size = 18, color = "#000000"),
        strip.background = element_blank(),
        strip.text = element_blank(),
        legend.position = "none",
        legend.justification = c(1, 1),
        legend.background = element_blank(),
        legend.key = element_blank(),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.grid = element_blank(),
        panel.grid.major.x = element_line(color="gray", linewidth=0.25),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank())

ggsave(here("Output", "Figures", "outbreak_length_as_vw_inf.svg"),
       height = 500,
       width = 800,
       units = "px",
       dpi = 72)

## Boxplot for Length of Outbreak for Infection-Waning Mechanisms -------
load(here("Output", "Processed Files", "last_day_re_all.Rdata"))

last_days_re_all = last_days_re_all %>%
  mutate(waning_months = round(1/(reinfect*30),0))

my_cols = brewer.pal(9, "Greens")

ggplot(last_days_re_all, aes(x=waning_months, y=last_day_date, group=interaction(waning_months, city))) +
  geom_violin(aes(fill=city), color = "gray", linewidth=0.25, bw=20, alpha=0.5)+
  stat_summary(fun.min = function(z) { quantile(z,0.25) },
               fun.max = function(z) { quantile(z,0.75) },
               fun = "median", color = "black", size=0.25) +
  stat_summary(fun = "median",
               geom = "point", shape = 23, size = 5, 
               color = "black", aes(fill = city)) +
  scale_fill_manual(values=c("Chicago" = my_cols[3], 
                             "New York City" = my_cols[5], 
                             "San Francisco" = my_cols[7]))+
  scale_x_continuous(breaks=seq(6,16,1)) +
  scale_y_date(limits=c(as.Date("2022-05-01"), as.Date("2023-09-01")), 
               breaks = seq(as.Date("2022-04-01"), as.Date("2023-09-01"), by="2 months"),
               date_labels = "%b %Y") +
  coord_flip() +
  facet_grid(cols = vars(city), scales = "free_y", space = "free_y") +
  labs(x = NULL, y = NULL) +
  theme(text = element_text(family = "Helvetica", size = 18, color = "#000000"),
        strip.background = element_blank(),
        strip.text = element_blank(),
        legend.position = "none",
        legend.justification = c(1, 1),
        legend.background = element_blank(),
        legend.key = element_blank(),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.grid = element_blank(),
        panel.grid.major.x = element_line(color="gray", linewidth=0.25),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank())

ggsave(here("Output", "Figures", "outbreak_length_re_months.svg"),
       height = 700,
       width = 800,
       units = "px",
       dpi = 72)


my_cols = brewer.pal(9, "Reds")


ggplot(last_days_re_all, aes(x=inf_value, y=last_day_date, group=interaction(inf_value, city))) +
  geom_violin(aes(fill=city), color = "gray", linewidth=0.25, bw=20, alpha=0.5)+
  stat_summary(fun.min = function(z) { quantile(z,0.25) },
               fun.max = function(z) { quantile(z,0.75) },
               fun = "median", color = "black", size=0.25) +
  stat_summary(fun = "median",
               geom = "point", shape = 23, size = 5, 
               color = "black", aes(fill = city)) +
  scale_fill_manual(values=c("Chicago" = my_cols[3], 
                             "New York City" = my_cols[5], 
                             "San Francisco" = my_cols[7]))+
  scale_x_continuous(breaks=seq(0.3,0.9,0.1)) +
  scale_y_date(limits=c(as.Date("2022-05-01"), as.Date("2023-09-01")), 
               breaks = seq(as.Date("2022-04-01"), as.Date("2023-09-01"), by="2 months"),
               date_labels = "%b %Y") +
  coord_flip() +
  facet_grid(cols = vars(city), scales = "free_y", space = "free_y") +
  labs(x = NULL, y = NULL) +
  theme(text = element_text(family = "Helvetica", size = 18, color = "#000000"),
        strip.background = element_blank(),
        strip.text = element_blank(),
        legend.position = "none",
        legend.justification = c(1, 1),
        legend.background = element_blank(),
        legend.key = element_blank(),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.grid = element_blank(),
        panel.grid.major.x = element_line(color="gray", linewidth=0.25),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank())

ggsave(here("Output", "Figures", "outbreak_length_re_inf.svg"),
       height = 500,
       width = 800,
       units = "px",
       dpi = 72)