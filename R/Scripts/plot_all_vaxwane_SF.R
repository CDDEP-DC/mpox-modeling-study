plot1 = ggplot()+
  geom_line(data = vaxwane_sf_stats$wP_6_wF_9_inf_0.9,aes(x=date, y=median, col="6 / 9"),size=0.5)+
  geom_line(data = vaxwane_sf_stats$wP_6_wF_12_inf_0.9,aes(x=date, y=median, col="6 / 12"),size=0.5)+
  geom_line(data = vaxwane_sf_stats$wP_6_wF_15_inf_0.9,aes(x=date, y=median, col="6 / 15"),size=0.5)+
  geom_line(data = vaxwane_sf_stats$wP_9_wF_12_inf_0.9,aes(x=date, y=median, col="9 /12"),size=0.5)+
  geom_line(data = vaxwane_sf_stats$wP_9_wF_15_inf_0.9,aes(x=date, y=median, col="9 / 15"),size=0.5)+
  geom_line(data = vaxwane_sf_stats$wP_12_wF_15_inf_0.9,aes(x=date, y=median, col="12 / 15"),size=0.5)+
  geom_point(data = SF_weekly_data, aes(x=week_starting, y =weekly_scaled_cases), col="black", size=0.5, shape = 3)+
  scale_x_datetime(date_breaks = "2 months", 
                   date_labels = "%b %Y", 
                   limits = c(as.POSIXct("2022-05-07"), as.POSIXct("2023-07-23")),
                   expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0), limits=c(0,100), breaks=seq(0,100,10)) +
  scale_color_viridis_d(option="turbo") +
  theme(
    text = element_text(family = "Helvetica", size = 12, color = "#000000"),
    legend.position = "right",
    legend.justification = c(1, 1),
    legend.background = element_blank(),
    legend.key = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA),
    axis.text.x = element_text(angle = 45, hjust = 1)) + 
  guides(fill = "none", color = guide_legend(title = NULL)) +
  labs(x = NULL, y = NULL)

legend_only <- get_legend(plot1)
plot1 <- plot1 + theme(legend.position = "none")

ggsave(plot = plot1,
       filename = here("Output", "Figures", "vaxwane_sf_all_inf_0.9.svg"),
       height = 168.5938,
       width = 218.5357,
       units = "px",
       dpi = 72)

ggsave(filename = here("Output", "Figures", paste0("legend", "_vaxwane_comparing trends", ".svg")), legend_only, width = 200, height = 400, units="px", dpi=72)

ggplot()+
  geom_line(data = vaxwane_sf_stats$wP_6_wF_9_inf_0.8,aes(x=date, y=median, col="6 / 9"),size=0.5)+
  geom_line(data = vaxwane_sf_stats$wP_6_wF_12_inf_0.8,aes(x=date, y=median, col="6 / 12"),size=0.5)+
  geom_line(data = vaxwane_sf_stats$wP_6_wF_15_inf_0.8,aes(x=date, y=median, col="6 / 15"),size=0.5)+
  geom_line(data = vaxwane_sf_stats$wP_9_wF_12_inf_0.8,aes(x=date, y=median, col="9 /12"),size=0.5)+
  geom_line(data = vaxwane_sf_stats$wP_9_wF_15_inf_0.8,aes(x=date, y=median, col="9 / 15"),size=0.5)+
  geom_line(data = vaxwane_sf_stats$wP_12_wF_15_inf_0.8,aes(x=date, y=median, col="12 / 15"),size=0.5)+
  geom_point(data = SF_weekly_data, aes(x=week_starting, y =weekly_scaled_cases), col="black", size=0.5, shape = 3)+
  scale_x_datetime(date_breaks = "2 months", 
                   date_labels = "%b %Y", 
                   limits = c(as.POSIXct("2022-05-07"), as.POSIXct("2023-07-23")),
                   expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0), limits=c(0,100), breaks=seq(0,100,10)) +
  scale_color_viridis_d(option="turbo") +
  theme(
    text = element_text(family = "Helvetica", size = 12, color = "#000000"),
    legend.position = "none",
    legend.justification = c(1, 1),
    legend.background = element_blank(),
    legend.key = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA),
    axis.text.x = element_text(angle = 45, hjust = 1)) + 
  guides(fill = "none", color = guide_legend(title = NULL)) +
  labs(x = NULL, y = NULL)

ggsave(here("Output", "Figures", "vaxwane_sf_all_inf_0.8.svg"),
       height = 168.5938,
       width = 218.5357,
       units = "px",
       dpi = 72)

ggplot()+
  geom_line(data = vaxwane_sf_stats$wP_6_wF_9_inf_0.7,aes(x=date, y=median, col="6 / 9"),size=0.5)+
  geom_line(data = vaxwane_sf_stats$wP_6_wF_12_inf_0.7,aes(x=date, y=median, col="6 / 12"),size=0.5)+
  geom_line(data = vaxwane_sf_stats$wP_6_wF_15_inf_0.7,aes(x=date, y=median, col="6 / 15"),size=0.5)+
  geom_line(data = vaxwane_sf_stats$wP_9_wF_12_inf_0.7,aes(x=date, y=median, col="9 /12"),size=0.5)+
  geom_line(data = vaxwane_sf_stats$wP_9_wF_15_inf_0.7,aes(x=date, y=median, col="9 / 15"),size=0.5)+
  geom_line(data = vaxwane_sf_stats$wP_12_wF_15_inf_0.7,aes(x=date, y=median, col="12 / 15"),size=0.5)+
  geom_point(data = SF_weekly_data, aes(x=week_starting, y =weekly_scaled_cases), col="black", size=0.5, shape = 3)+
  scale_x_datetime(date_breaks = "2 months", 
                   date_labels = "%b %Y", 
                   limits = c(as.POSIXct("2022-05-07"), as.POSIXct("2023-07-23")),
                   expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0), limits=c(0,100), breaks=seq(0,100,10)) +
  scale_color_viridis_d(option="turbo") +
  theme(
    text = element_text(family = "Helvetica", size = 12, color = "#000000"),
    legend.position = "none",
    legend.justification = c(1, 1),
    legend.background = element_blank(),
    legend.key = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA),
    axis.text.x = element_text(angle = 45, hjust = 1)) + 
  guides(fill = "none", color = guide_legend(title = NULL)) +
  labs(x = NULL, y = NULL)

ggsave(here("Output", "Figures", "vaxwane_sf_all_inf_0.7.svg"),
       height = 168.5938,
       width = 218.5357,
       units = "px",
       dpi = 72)