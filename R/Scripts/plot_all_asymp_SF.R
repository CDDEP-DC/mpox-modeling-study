plot1 = ggplot()+
  geom_line(data = asymptomatic_sf_stats$as_0.1_inf_0.9,aes(x=date, y=median, col="10%"),size=0.5)+
  geom_line(data = asymptomatic_sf_stats$as_0.2_inf_0.9,aes(x=date, y=median, col="20%"),size=0.5)+
  geom_line(data = asymptomatic_sf_stats$as_0.3_inf_0.9,aes(x=date, y=median, col="30%"),size=0.5)+
  geom_line(data = asymptomatic_sf_stats$as_0.4_inf_0.9,aes(x=date, y=median, col="40%"),size=0.5)+
  geom_line(data = asymptomatic_sf_stats$as_0.5_inf_0.9,aes(x=date, y=median, col="50%"),size=0.5)+
  geom_line(data = asymptomatic_sf_stats$as_0.6_inf_0.9,aes(x=date, y=median, col="60%"),size=0.5)+
  geom_line(data = asymptomatic_sf_stats$as_0.7_inf_0.9,aes(x=date, y=median, col="70%"),size=0.5)+
  geom_line(data = asymptomatic_sf_stats$as_0.8_inf_0.9,aes(x=date, y=median, col="80%"),size=0.5)+
  geom_line(data = asymptomatic_sf_stats$as_0.9_inf_0.9,aes(x=date, y=median, col="90%"),size=0.5)+
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
       filename = here("Output", "Figures", "asymp_sf_all_inf_0.9.svg"),
       height = 168.5938,
       width = 218.5357,
       units = "px",
       dpi = 72)

ggsave(filename = here("Output", "Figures", paste0("legend", "_vaxwane_comparing trends", ".svg")), legend_only, width = 200, height = 400, units="px", dpi=72)

ggplot()+
  geom_line(data = asymptomatic_sf_stats$as_0.1_inf_0.8,aes(x=date, y=median, col="10%"),size=0.5)+
  geom_line(data = asymptomatic_sf_stats$as_0.2_inf_0.8,aes(x=date, y=median, col="20%"),size=0.5)+
  geom_line(data = asymptomatic_sf_stats$as_0.3_inf_0.8,aes(x=date, y=median, col="30%"),size=0.5)+
  geom_line(data = asymptomatic_sf_stats$as_0.4_inf_0.8,aes(x=date, y=median, col="40%"),size=0.5)+
  geom_line(data = asymptomatic_sf_stats$as_0.5_inf_0.8,aes(x=date, y=median, col="50%"),size=0.5)+
  geom_line(data = asymptomatic_sf_stats$as_0.6_inf_0.8,aes(x=date, y=median, col="60%"),size=0.5)+
  geom_line(data = asymptomatic_sf_stats$as_0.7_inf_0.8,aes(x=date, y=median, col="70%"),size=0.5)+
  geom_line(data = asymptomatic_sf_stats$as_0.8_inf_0.8,aes(x=date, y=median, col="80%"),size=0.5)+
  geom_line(data = asymptomatic_sf_stats$as_0.9_inf_0.8,aes(x=date, y=median, col="90%"),size=0.5)+
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

ggsave(here("Output", "Figures", "asymp_sf_all_inf_0.8.svg"),
       height = 168.5938,
       width = 218.5357,
       units = "px",
       dpi = 72)

ggplot()+
  geom_line(data = asymptomatic_sf_stats$as_0.1_inf_0.7,aes(x=date, y=median, col="10%"),size=0.5)+
  geom_line(data = asymptomatic_sf_stats$as_0.2_inf_0.7,aes(x=date, y=median, col="20%"),size=0.5)+
  geom_line(data = asymptomatic_sf_stats$as_0.3_inf_0.7,aes(x=date, y=median, col="30%"),size=0.5)+
  geom_line(data = asymptomatic_sf_stats$as_0.4_inf_0.7,aes(x=date, y=median, col="40%"),size=0.5)+
  geom_line(data = asymptomatic_sf_stats$as_0.5_inf_0.7,aes(x=date, y=median, col="50%"),size=0.5)+
  geom_line(data = asymptomatic_sf_stats$as_0.6_inf_0.7,aes(x=date, y=median, col="60%"),size=0.5)+
  geom_line(data = asymptomatic_sf_stats$as_0.7_inf_0.7,aes(x=date, y=median, col="70%"),size=0.5)+
  geom_line(data = asymptomatic_sf_stats$as_0.8_inf_0.7,aes(x=date, y=median, col="80%"),size=0.5)+
  geom_line(data = asymptomatic_sf_stats$as_0.9_inf_0.7,aes(x=date, y=median, col="90%"),size=0.5)+
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

ggsave(here("Output", "Figures", "asymp_sf_all_inf_0.7.svg"),
       height = 168.5938,
       width = 218.5357,
       units = "px",
       dpi = 72)