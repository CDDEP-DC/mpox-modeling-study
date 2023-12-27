library(here)
library(ggplot2)

load(here("Output","Processed Files", "prop_asymp_NYC_stats.Rdata"))
load(here("Output", "Processed Files", "as_nyc_stats_daily.Rdata"))
load(here("Output", "Processed Files", "Rt_NYC.Rdata"))

ggplot(Rt_summary_NYC, aes(x = Day, y = Median)) +
  geom_ribbon(aes(ymin=Q2_5, ymax=Q97_5), fill="#4575b4", alpha=0.2) +
  geom_ribbon(aes(ymin=Q25, ymax=Q75), fill="#4575b4", alpha=0.5) +
  geom_line(color = "#4575b4",size=0.5) +
  geom_line(data = asymptomatic_nyc_stats$as_0.7_inf_0.7, aes(x=as.Date(date), y=mean*0.09), col="#a50026",linetype = "solid",size=0.5)+
  geom_line(data = asymptomatic_nyc_num.a_stats$as_0.7_inf_0.7, aes(x=as.Date(date), y=mean*0.09), col="#a50026",linetype = "dashed",size=0.5)+
  geom_point(data=NULL,shape=1,aes(x=as.Date("2022-06-14"),y=1.49),size=4,color="blue",alpha=1) +
  geom_hline(yintercept = 1,col="black",size=0.5)+
  labs(x =NULL, y = expression(R[e])) +
  scale_y_continuous(breaks=seq(0,6,by=1), limits=c(0,6), expand=c(0,0), sec.axis = sec_axis(~ . / 0.09, name="Weekly Incidence")) +
  scale_x_date(limits=c(as.Date("2022-05-01"), as.Date("2023-08-01")), 
               breaks = seq(as.Date("2022-05-01"), as.Date("2023-09-01"), by="2 months"),
               date_labels = "%b %Y", expand=c(0,0)) +
  theme(
    text = element_text(family = "Helvetica", size =11, color = "#000000"),
    legend.position = "none",
    legend.justification = c(1, 1),
    legend.background = element_blank(),
    legend.key = element_blank(),
    panel.background = element_blank(),
    panel.grid.major = element_line(color="gray", linewidth=0.25),
    plot.margin = unit(c(0.1,0,0.1,0), "in"),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA))

ggsave(here("Output", "Figures", "NYC_rt_with_incidence.svg"), dpi=300, height=2.5, width=6.1856, units="in",limitsize = FALSE)


load(here("Output","Processed Files", "prop_asymp_CHI_stats.Rdata"))
load(here("Output", "Processed Files", "as_chi_stats_daily.Rdata"))
load(here("Output", "Processed Files", "Rt_CHI.Rdata"))

ggplot(Rt_summary_CHI, aes(x = Day, y = Median)) +
  geom_ribbon(aes(ymin=Q2_5, ymax=Q97_5), fill="#4575b4", alpha=0.2) +
  geom_ribbon(aes(ymin=Q25, ymax=Q75), fill="#4575b4", alpha=0.5) +
  geom_line(color = "#4575b4",size=0.5) +
  geom_line(data = asymptomatic_chi_stats$as_0.9_inf_0.7, aes(x=as.Date(date), y=mean*0.05), col="#a50026",linetype = "solid",size=0.5)+
  geom_line(data = asymptomatic_chi_num.a_stats$as_0.9_inf_0.7, aes(x=as.Date(date), y=mean*0.05), col="#a50026",linetype = "dashed",size=0.5)+
  geom_point(data=NULL,shape=1,aes(x=as.Date("2022-06-01"),y=1.95068),size=4,color="blue",alpha=1)+
  geom_hline(yintercept = 1,col="black",size=0.5)+
  labs(x =NULL, y = expression(R[e])) +
  scale_y_continuous(breaks=seq(0,6,by=1), limits=c(0,6), expand=c(0,0), sec.axis = sec_axis(~ . / 0.09, name="Weekly Incidence")) +
  scale_x_date(limits=c(as.Date("2022-05-01"), as.Date("2023-08-01")), 
               breaks = seq(as.Date("2022-05-01"), as.Date("2023-09-01"), by="2 months"),
               date_labels = "%b %Y", expand=c(0,0)) +
  theme(
    text = element_text(family = "Helvetica", size =11, color = "#000000"),
    legend.position = "none",
    legend.justification = c(1, 1),
    legend.background = element_blank(),
    legend.key = element_blank(),
    panel.background = element_blank(),
    panel.grid.major = element_line(color="gray", linewidth=0.25),
    plot.margin = unit(c(0.1,0,0.1,0), "in"),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA))

ggsave(here("Output", "Figures", "CHI_rt_with_incidence.svg"), dpi=300, height=2.5, width=6.1856, units="in",limitsize = FALSE)

load(here("Output","Processed Files", "prop_asymp_SF_stats.Rdata"))
load(here("Output", "Processed Files", "as_sf_stats_daily.Rdata"))
load(here("Output", "Processed Files", "Rt_SF.Rdata"))

ggplot(Rt_summary_SF, aes(x = Day, y = Median)) +
  geom_ribbon(aes(ymin=Q2_5, ymax=Q97_5), fill="#4575b4", alpha=0.2) +
  geom_ribbon(aes(ymin=Q25, ymax=Q75), fill="#4575b4", alpha=0.5) +
  geom_line(color = "#4575b4",size=0.5) +
  geom_line(data = asymptomatic_sf_stats$as_0.9_inf_0.9, aes(x=as.Date(date), y=mean*0.03), col="#a50026",linetype = "solid",size=0.5)+
  geom_line(data = asymptomatic_sf_num.a_stats$as_0.9_inf_0.9, aes(x=as.Date(date), y=mean*0.03), col="#a50026",linetype = "dashed",size=0.5)+
  geom_point(data=NULL,shape=1,aes(x=as.Date("2022-06-18"),y=1.695635),size=4,color="blue",alpha=1) + #SF
  geom_hline(yintercept = 1,col="black",size=0.5)+
  labs(x =NULL, y = expression(R[e])) +
  scale_y_continuous(breaks=seq(0,6,by=1), limits=c(0,6), expand=c(0,0), sec.axis = sec_axis(~ . / 0.09, name="Weekly Incidence")) +
  scale_x_date(limits=c(as.Date("2022-05-01"), as.Date("2023-08-01")), 
               breaks = seq(as.Date("2022-05-01"), as.Date("2023-09-01"), by="2 months"),
               date_labels = "%b %Y", expand=c(0,0)) +
  theme(
    text = element_text(family = "Helvetica", size =11, color = "#000000"),
    legend.position = "none",
    legend.justification = c(1, 1),
    legend.background = element_blank(),
    legend.key = element_blank(),
    panel.background = element_blank(),
    panel.grid.major = element_line(color="gray", linewidth=0.25),
    plot.margin = unit(c(0.1,0,0.1,0), "in"),
    axis.text.x = element_text(angle = 45, hjust = 1),    # If bottom part of panel, then enable this and make sure to use right dimensions on export
    panel.border = element_rect(colour = "black", fill = NA))

ggsave(here("Output", "Figures", "SF_rt_with_incidence.svg"), dpi=300, height=2.7819, width=6.1856, units="in",limitsize = FALSE)
