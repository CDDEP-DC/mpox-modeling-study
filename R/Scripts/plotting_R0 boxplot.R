## Boxplots for each R0

load_R0_estimates = list.files(here("Output", "R0 Estimates", "All Runs R Data Files"))

all_R0_estimates = data.frame()

for(file in load_R0_estimates){
  load(here("Output", "R0 Estimates", "All Runs R Data Files", paste0(file)))
  all_R0_estimates <- rbind(all_R0_estimates, R0_full)
}

all_R0_estimates_filtered = all_R0_estimates %>%
  filter(generation <=1 & scene == 'as') %>%
  mutate(city = factor(city, levels = rev(c("sf","nyc","chi")), labels = rev(c("San Francisco","New York City","Chicago"))))

summary_stats_R0_gen2 = all_R0_estimates %>%
  filter(generation <=2 & scene == 'as') %>%
  mutate(city = factor(city, levels = c("sf","nyc","chi"), labels = c("San Francisco","New York City","Chicago"))) %>%
  group_by(city) %>%
  summarise(Median = median(mean_sec_inf, na.rm = TRUE),
            Mean = mean(mean_sec_inf, na.rm = TRUE),
            Q2_5 = quantile(mean_sec_inf, 0.025, na.rm = TRUE),
            Q25 = quantile(mean_sec_inf, 0.25, na.rm = TRUE),
            Q75 = quantile(mean_sec_inf, 0.75, na.rm = TRUE),
            Q97_5 = quantile(mean_sec_inf, 0.975, na.rm = TRUE),
            max_recovery = max(max_recovery, na.rm = TRUE)) %>%
  mutate(gen = 2)

summary_stats_R0_gen1 = all_R0_estimates %>%
  filter(generation <=1 & scene == 'as') %>%
  mutate(city = factor(city, levels = c("sf","nyc","chi"), labels = c("San Francisco","New York City","Chicago"))) %>%
  group_by(city) %>%
  summarise(Median = median(mean_sec_inf, na.rm = TRUE),
            Mean = mean(mean_sec_inf, na.rm = TRUE),
            Q2_5 = quantile(mean_sec_inf, 0.025, na.rm = TRUE),
            Q25 = quantile(mean_sec_inf, 0.25, na.rm = TRUE),
            Q75 = quantile(mean_sec_inf, 0.75, na.rm = TRUE),
            Q97_5 = quantile(mean_sec_inf, 0.975, na.rm = TRUE),
            max_recovery = max(max_recovery, na.rm = TRUE)) %>%
  mutate(gen = 1)

summary_stats_R0_gen3 = all_R0_estimates %>%
  filter(generation <=3 & scene == 'as') %>%
  mutate(city = factor(city, levels = c("sf","nyc","chi"), labels = c("San Francisco","New York City","Chicago"))) %>%
  group_by(city) %>%
  summarise(Median = median(mean_sec_inf, na.rm = TRUE),
            Mean = mean(mean_sec_inf, na.rm = TRUE),
            Q2_5 = quantile(mean_sec_inf, 0.025, na.rm = TRUE),
            Q25 = quantile(mean_sec_inf, 0.25, na.rm = TRUE),
            Q75 = quantile(mean_sec_inf, 0.75, na.rm = TRUE),
            Q97_5 = quantile(mean_sec_inf, 0.975, na.rm = TRUE),
            max_recovery = max(max_recovery, na.rm = TRUE)) %>%
  mutate(gen = 3)

summary_stats_R0_all_gens = rbind(summary_stats_R0_gen1,summary_stats_R0_gen2,summary_stats_R0_gen3)
summary_stats_R0_all_gens = summary_stats_R0_all_gens %>%
  arrange(city, gen) %>%
  mutate(max_recovery = as.Date(as.Date("2022-05-01")+max_recovery))

write.csv(summary_stats_R0_all_gens, file=here("Output","R0 Estimates","R0_sense.csv"), row.names = FALSE)

ggplot(all_R0_estimates_filtered, aes(y=mean_sec_inf, x=city))+
  geom_violin(aes(fill=interaction(scene, city)),alpha=0.4,width=0.4)+
  geom_boxplot(outlier.colour = 'black', outlier.shape = 4, outlier.alpha=1, outlier.size=3, aes(fill=interaction(scene, city)), alpha=0, width=0.1, size=0.5)+
  geom_point(data=R0_proxy_estimates,shape=21,aes(x=city,y=estimate),size=4,color="blue",fill="blue",alpha=0.5)+
  geom_point(data=R0_proxy_estimates,shape=1,aes(x=city,y=estimate),size=4,color="blue",alpha=1)+
  geom_text_repel(data = R0_proxy_estimates, aes(label=paste0("Proxy Estimate, median (95% CI):\n",round(estimate,2)," (",round(P2.5,2),", ",round(P97.5, 2), ")"),x=city,y=estimate),size=3,
                  nudge_x      = -0.175,
                  nudge_y      = 2,
                  direction    = "x",
                  hjust        = 0,
                  segment.size = 0.2)+
  geom_text_repel(data = summary_stats_R0_gen1, aes(label=paste0("Median (95% CI):\n",round(Median,2)," (",round(Q2_5,2),", ",round(Q97_5,2), ")"),x=city,y=Median),size=3,
                  nudge_x      = -0.145,
                  nudge_y      = -1.1,
                  hjust        = 1,
                  segment.size = 0.2)+
  stat_summary(fun="mean",size=0.5,shape=18)+
  geom_hline(yintercept = 1.0, col="red") +
  scale_y_continuous(limits=c(-0.5,5), breaks=seq(0,5,by=0.2),expand=c(0,0))+
  scale_fill_brewer(palette="Blues") +
  theme_bw()+
  theme(text = element_text(family = "Helvetica", size =11, color = "#000000"),
       legend.position = "none",
       panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(y=expression(R[0]), x="City")

ggsave(here("Output", "Figures", "R0_boxplots.svg"), dpi=300, height=8.008, width=4.6117, units="in",limitsize = FALSE)

save(plot_R0, file=(here("Output", "Figures", "plot_R0.RData")))


ggplot(all_R0_estimates_filtered, aes(y=mean_sec_inf, x=city)) +
  geom_boxplot(outlier.colour = 'black', outlier.shape = 4, outlier.alpha=1, aes(fill=interaction(scene, city)), alpha=0.4, width=0.1, size=0.5) +
  geom_point(data=R0_proxy_estimates, shape=21, aes(x=city, y=estimate), size=4, color="blue", fill="blue", alpha=0.5) +
  geom_point(data=R0_proxy_estimates, shape=1, aes(x=city, y=estimate), size=4, color="blue", alpha=1) +
  # Use geom_text() with manual adjustment to the x position and right justification
  geom_text(data = R0_proxy_estimates, aes(label=paste0("Proxy Estimate: ",round(estimate,2)), x=4, y=estimate), size=3, hjust=0, vjust=0.5) +
  geom_text(data = summary_stats_R0, aes(label=paste0("Mean (95% Cl):", round(Mean,2), " (", round(Q2_5,2), ", ", round(Q97_5,2), ")"), x=4, y=Mean), size=3, hjust=0, vjust=-0.5) +
  stat_summary(fun="mean", size=0.5, shape=18) +
  geom_hline(yintercept = 1.0, col="red") +
  scale_y_continuous(limits=c(0,12), breaks=seq(0,12.5,by=0.5), expand=c(0,0)) +
  scale_fill_brewer(palette="Blues") +
  theme_bw() +
  theme(text = element_text(family = "Helvetica", size =11, color = "#000000"),
        legend.position = "none",
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank()) +  # Adjust the right margin to make space for text
  labs(y=expression(R[0]), x="City") +
  coord_cartesian(xlim=c(1,5))  # Apply the new x limits to the plot

ggsave(here("Output", "Figures", "R0_boxplots_opt2.svg"), dpi=300, height=8.008, width=4.6117, units="in",limitsize = FALSE)

