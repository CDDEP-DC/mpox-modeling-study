load(here("Output", "Processed Files", "reinfect_chicago_stats_daily.Rdata"))
load(here("Output", "Processed Files", "reinfect_nyc_stats_daily.Rdata"))
load(here("Output", "Processed Files", "reinfect_sf_stats_daily.Rdata"))

load(here("Output", "Processed Files", "scaled_cases_nyc.Rdata"))
load(here("Output", "Processed Files", "scaled_cases_chicago.Rdata"))
load(here("Output", "Processed Files", "scaled_cases_sf.Rdata"))

load(here("Output", "Processed Files", "last_day_best_fit_models.Rdata"))

mypal <- brewer.pal(9, "Reds")
mypal_blue <- brewer.pal(9, "Blues")

combined_last_day = combined_last_day %>%
  filter(scene == "reinfection") %>%
  mutate(last_day_date = as.POSIXct(last_day_date))

last_day_chi = combined_last_day %>%
  filter(city == "Chicago")

ggplot(reinfection_chicago_stats$reinfect_0.002564_inf_0.8, aes(x=date, y=median))+
  geom_ribbon(aes(ymin=perc2.5, ymax=perc97.5, fill="Model_95"), alpha=0.50)+
  geom_ribbon(aes(ymin=perc25, ymax=perc75, fill="Model_IQR"), alpha=0.50)+
  geom_line(aes(col="Model"),size=0.5)+
  geom_line(aes(x=date, y=mean, col="Model"), linetype = "dotted",size=0.5)+
  geom_point(data = CHI_data_cases, aes(x=week, y =scaled_weekly_cases, col="Observed"), size=0.5, shape = 3)+
  geom_boxplot(data = last_day_chi, aes(x=last_day_date, y =50), width=5, size=0.25, col='black', fill=mypal_blue[7], outlier.shape = NA, coef=0, alpha=0.75)+
  scale_x_datetime(date_breaks = "2 months", 
                   date_labels = "%b %Y", 
                   limits = c(as.POSIXct("2022-05-07"), as.POSIXct("2023-07-23")),
                   expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0), limits=c(0,100), breaks = seq(5,95,10)) +
  scale_color_manual(name = "Legend", values = c("Model" = mypal[7], "Observed" = "black")) +
  scale_fill_manual(name = "Legend", values = c("Model" = mypal[7],
                                                "Model_IQR" = mypal[6],
                                                "Model_95" = mypal[4])) +
  theme(
    text = element_text(family = "Helvetica", size = 10, color="black"),
    legend.position = "NONE",
    legend.justification = c(1, 1),
    legend.background = element_blank(),
    legend.key = element_blank(),
    panel.background = element_blank(),
    plot.background = element_blank(),
    panel.grid.major.x = element_line(color="gray",linewidth=0.25),
    panel.border = element_rect(colour = "black", fill = NA),
    axis.text.x = element_blank()) + 
  guides(fill = "none", color = guide_legend(title = NULL)) +
  labs(x = NULL, y = NULL)

ggsave(here("Output", "Figures", "rei_chi_new.svg"),
       height = 128.2711,
       width = 195.849,
       units = "px",
       dpi = 72)


last_day_nyc = combined_last_day %>%
  filter(city == "New York City")

last_day_nyc = last_days_re_nyc  %>%
  filter(reinfect == 0.005556 & inf_value == 0.8)

last_day_nyc$last_day_date <- as.Date("2022-05-01") + as.numeric(last_day_nyc$last_day)

last_day_nyc = last_day_nyc %>%
  mutate(last_day_date = as.POSIXct(last_day_date))

ggplot(reinfection_nyc_stats$reinfect_0.005556_inf_0.8, aes(x=date, y=median))+
  geom_ribbon(aes(ymin=perc2.5, ymax=perc97.5, fill="Model_95"), alpha=0.50)+
  geom_ribbon(aes(ymin=perc25, ymax=perc75, fill="Model_IQR"), alpha=0.50)+
  geom_line(aes(col="Model"),size=0.5)+
  geom_line(aes(x=date, y=mean, col="Model"), linetype = "dotted",size=0.5)+
  geom_point(data = NYC_weekly_data, aes(x=week_starting, y =weekly_scaled_cases, col="Observed"), size=0.5, shape = 3)+
  geom_boxplot(data = last_day_nyc, aes(x=last_day_date, y =50), width=5, size=0.25, col='black', fill=mypal_blue[7], outlier.shape = NA, coef=0, alpha=0.75)+
  scale_x_datetime(date_breaks = "2 months", 
                   date_labels = "%b %Y", 
                   limits = c(as.POSIXct("2022-05-07"), as.POSIXct("2023-07-23")),
                   expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0), limits=c(0,95), breaks = seq(5,95,10)) +
  scale_color_manual(name = "Legend", values = c("Model" = mypal[7], "Observed" = "black")) +
  scale_fill_manual(name = "Legend", values = c("Model" = mypal[7],
                                                "Model_IQR" = mypal[6],
                                                "Model_95" = mypal[4])) +
  theme(
    text = element_text(family = "Helvetica", size = 10, color="black"),
    legend.position = "NONE",
    legend.justification = c(1, 1),
    legend.background = element_blank(),
    legend.key = element_blank(),
    panel.background = element_blank(),
    plot.background = element_blank(),
    panel.grid.major.x = element_line(color="gray",linewidth=0.25),
    panel.border = element_rect(colour = "black", fill = NA),
    axis.text.x = element_blank()) + 
  guides(fill = "none", color = guide_legend(title = NULL)) +
  labs(x = NULL, y = NULL)

ggsave(here("Output", "Figures", "rei_nyc_new.svg"),
       height = 128.2711,
       width = 195.849,
       units = "px",
       dpi = 72)

last_day_sf = combined_last_day %>%
  filter(city == "San Francisco")

ggplot(reinfection_sf_stats$reinfect_0.003333_inf_0.8, aes(x=date, y=median))+
  geom_ribbon(aes(ymin=perc2.5, ymax=perc97.5, fill="Model_95"), alpha=0.50)+
  geom_ribbon(aes(ymin=perc25, ymax=perc75, fill="Model_IQR"), alpha=0.50)+
  geom_line(aes(col="Model"),size=0.5)+
  geom_line(aes(x=date, y=mean, col="Model"), linetype = "dotted",size=0.5)+
  geom_point(data = SF_weekly_data, aes(x=week_starting, y =weekly_scaled_cases, col="Observed"), size=0.5, shape = 3)+
  geom_boxplot(data = last_day_sf, aes(x=last_day_date, y =50), width=5, size=0.25, col='black', fill=mypal_blue[7], outlier.shape = NA, coef=0, alpha=0.75)+
  scale_x_datetime(date_breaks = "2 months", 
                   date_labels = "%b %Y", 
                   limits = c(as.POSIXct("2022-05-07"), as.POSIXct("2023-07-23")),
                   expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0), limits=c(0,100), breaks = seq(5,95,10)) +
  scale_color_manual(name = "Legend", values = c("Model" = mypal[7], "Observed" = "black")) +
  scale_fill_manual(name = "Legend", values = c("Model" = mypal[7],
                                                "Model_IQR" = mypal[6],
                                                "Model_95" = mypal[4])) +
  theme(
    text = element_text(family = "Helvetica", size = 10, color="black"),
    legend.position = "NONE",
    legend.justification = c(1, 1),
    legend.background = element_blank(),
    legend.key = element_blank(),
    panel.background = element_blank(),
    plot.background = element_blank(),
    panel.grid.major.x = element_line(color="gray",linewidth=0.25),
    panel.border = element_rect(colour = "black", fill = NA),
    axis.text.x = element_blank()) + 
  guides(fill = "none", color = guide_legend(title = NULL)) +
  labs(x = NULL, y = NULL)

ggsave(here("Output", "Figures", "rei_sf_new.svg"),
       height = 128.2711,
       width = 195.849,
       units = "px",
       dpi = 72)

