plot1 = ggplot()+
  geom_line(data = reinfection_chicago_stats$reinfect_0.005556_inf_0.9,aes(x=date, y=median, col="06"),size=0.5)+
  geom_line(data = reinfection_chicago_stats$reinfect_0.004762_inf_0.9,aes(x=date, y=median, col="07"),size=0.5)+
  geom_line(data = reinfection_chicago_stats$reinfect_0.004167_inf_0.9,aes(x=date, y=median, col="08"),size=0.5)+
  geom_line(data = reinfection_chicago_stats$reinfect_0.003704_inf_0.9,aes(x=date, y=median, col="09"),size=0.5)+
  geom_line(data = reinfection_chicago_stats$reinfect_0.003333_inf_0.9,aes(x=date, y=median, col="10"),size=0.5)+
  geom_line(data = reinfection_chicago_stats$reinfect_0.00303_inf_0.9,aes(x=date, y=median, col="11"),size=0.5)+
  geom_line(data = reinfection_chicago_stats$reinfect_0.002778_inf_0.9,aes(x=date, y=median, col="12"),size=0.5)+
  geom_line(data = reinfection_chicago_stats$reinfect_0.002564_inf_0.9,aes(x=date, y=median, col="13"),size=0.5)+
  geom_line(data = reinfection_chicago_stats$reinfect_0.002381_inf_0.9,aes(x=date, y=median, col="14"),size=0.5)+
  geom_line(data = reinfection_chicago_stats$reinfect_0.002222_inf_0.9,aes(x=date, y=median, col="15"),size=0.5)+
  geom_line(data = reinfection_chicago_stats$reinfect_0.002083_inf_0.9,aes(x=date, y=median, col="16"),size=0.5)+
  geom_point(data = CHI_data_cases, aes(x=week, y =scaled_weekly_cases), col="black", size=0.5, shape = 3)+
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
       filename = here("Output", "Figures", "rei_chi_all_inf_0.9.svg"),
       height = 168.5938,
       width = 218.5357,
       units = "px",
       dpi = 72)

ggsave(filename = here("Output", "Figures", paste0("legend", "_vaxwane_comparing trends", ".svg")), legend_only, width = 200, height = 400, units="px", dpi=72)

ggplot()+
  geom_line(data = reinfection_chicago_stats$reinfect_0.005556_inf_0.8,aes(x=date, y=median, col="06"),size=0.5)+
  geom_line(data = reinfection_chicago_stats$reinfect_0.004762_inf_0.8,aes(x=date, y=median, col="07"),size=0.5)+
  geom_line(data = reinfection_chicago_stats$reinfect_0.004167_inf_0.8,aes(x=date, y=median, col="08"),size=0.5)+
  geom_line(data = reinfection_chicago_stats$reinfect_0.003704_inf_0.8,aes(x=date, y=median, col="09"),size=0.5)+
  geom_line(data = reinfection_chicago_stats$reinfect_0.003333_inf_0.8,aes(x=date, y=median, col="10"),size=0.5)+
  geom_line(data = reinfection_chicago_stats$reinfect_0.00303_inf_0.8,aes(x=date, y=median, col="11"),size=0.5)+
  geom_line(data = reinfection_chicago_stats$reinfect_0.002778_inf_0.8,aes(x=date, y=median, col="12"),size=0.5)+
  geom_line(data = reinfection_chicago_stats$reinfect_0.002564_inf_0.8,aes(x=date, y=median, col="13"),size=0.5)+
  geom_line(data = reinfection_chicago_stats$reinfect_0.002381_inf_0.8,aes(x=date, y=median, col="14"),size=0.5)+
  geom_line(data = reinfection_chicago_stats$reinfect_0.002222_inf_0.8,aes(x=date, y=median, col="15"),size=0.5)+
  geom_line(data = reinfection_chicago_stats$reinfect_0.002083_inf_0.8,aes(x=date, y=median, col="16"),size=0.5)+
  geom_point(data = CHI_data_cases, aes(x=week, y =scaled_weekly_cases), col="black", size=0.5, shape = 3)+
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

ggsave(here("Output", "Figures", "rei_chi_all_inf_0.8.svg"),
       height = 168.5938,
       width = 218.5357,
       units = "px",
       dpi = 72)

ggplot()+
  geom_line(data = reinfection_chicago_stats$reinfect_0.005556_inf_0.7,aes(x=date, y=median, col="06"),size=0.5)+
  geom_line(data = reinfection_chicago_stats$reinfect_0.004762_inf_0.7,aes(x=date, y=median, col="07"),size=0.5)+
  geom_line(data = reinfection_chicago_stats$reinfect_0.004167_inf_0.7,aes(x=date, y=median, col="08"),size=0.5)+
  geom_line(data = reinfection_chicago_stats$reinfect_0.003704_inf_0.7,aes(x=date, y=median, col="09"),size=0.5)+
  geom_line(data = reinfection_chicago_stats$reinfect_0.003333_inf_0.7,aes(x=date, y=median, col="10"),size=0.5)+
  geom_line(data = reinfection_chicago_stats$reinfect_0.00303_inf_0.7,aes(x=date, y=median, col="11"),size=0.5)+
  geom_line(data = reinfection_chicago_stats$reinfect_0.002778_inf_0.7,aes(x=date, y=median, col="12"),size=0.5)+
  geom_line(data = reinfection_chicago_stats$reinfect_0.002564_inf_0.7,aes(x=date, y=median, col="13"),size=0.5)+
  geom_line(data = reinfection_chicago_stats$reinfect_0.002381_inf_0.7,aes(x=date, y=median, col="14"),size=0.5)+
  geom_line(data = reinfection_chicago_stats$reinfect_0.002222_inf_0.7,aes(x=date, y=median, col="15"),size=0.5)+
  geom_line(data = reinfection_chicago_stats$reinfect_0.002083_inf_0.7,aes(x=date, y=median, col="16"),size=0.5)+
  geom_point(data = CHI_data_cases, aes(x=week, y =scaled_weekly_cases), col="black", size=0.5, shape = 3)+
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

ggsave(here("Output", "Figures", "rei_chi_all_inf_0.7.svg"),
       height = 168.5938,
       width = 218.5357,
       units = "px",
       dpi = 72)