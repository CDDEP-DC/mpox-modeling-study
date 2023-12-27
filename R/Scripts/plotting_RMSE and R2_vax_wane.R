library(ggplot2)
library(cowplot)
library(tidyverse)

rmse_df <- rmse_values_vaxwane_nyc %>%
  separate(ColumnName, into = c("wP", "wF", "inf"), sep = "_wF_|_inf_") %>%
  mutate(wP = as.numeric(gsub("wP_", "", wP)),
         wF = as.numeric(gsub("wF_", "", wF)),
         inf = as.numeric(inf))

rmse_df <- rmse_df %>%
  mutate(wP_wF_combination = paste("wP", wP, "wF", wF, sep="_"))

highlight_data <- rmse_df[rmse_df$wP == 6 & rmse_df$wF == 9 & rmse_df$inf == 0.80, ]

plot_r2_v_rmse = ggplot(rmse_df, aes(y=R2, x=RMSE, col=wP_wF_combination, shape=as.factor(inf)))+
  geom_point(size=1.5) +
  geom_segment(data=highlight_data, aes(xend=RMSE + 2, yend=R2 + 0.1), size=0.25, color="black") +
  geom_text(data=highlight_data, aes(label="Selected Combination"), size=3, nudge_x=2, nudge_y=0.11, hjust=0.2, col="black") +
  scale_shape_manual(values=c(3, 2, 1, 0, 4, 8, 5)) +
  scale_y_continuous(breaks=seq(0,1,by=0.1))+
  scale_x_continuous(breaks=seq(0,max(rmse_df$RMSE),by=2))+
  scale_color_viridis_d(option="turbo")+
  theme_bw() +
  theme(
    text = element_text(family = "Helvetica", size = 8, color = "#000000"),
    legend.position = "right",
    legend.justification = c(1, 1),
    legend.background = element_blank(),
    legend.key = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    panel.grid.major.x = element_line(color="gray", linewidth=0.25),
    panel.grid.major.y = element_line(color="gray", linewidth=0.25),
    panel.border = element_rect(colour = "black", fill = NA)) +
  labs(x=NULL, y=NULL, shape="Probabiliy of\nInfection", col="Months Waning\nPartial / Full\nVaccination")

#legend_only <- get_legend(plot_r2_v_rmse)

plot_r2_v_rmse <- plot_r2_v_rmse + theme(legend.position = "none")

ggsave(plot = plot_r2_v_rmse,
       filename = here("Output", "Figures", paste0("RMSE", "_vaxWane", "_nyc", ".svg")),
       height = 140.1581,
       width = 233.5968,
       # height = 245.961,     # If bottom of panel
       # width = 446.85,        # If bottom of panel
       units = "px",
       dpi = 72)

#ggsave(filename = here("Output", "Figures", paste0("legend", "_vaxwane", ".svg")), legend_only, width = 200, height = 400, units="px", dpi=72)

