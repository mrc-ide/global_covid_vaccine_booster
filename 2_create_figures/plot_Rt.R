
name <- "rq1_hic_abmodel_omicron"
df1o <- readRDS(paste0("processed_outputs/df_summarise_", name, ".rds")) %>%
  filter(vaccine_doses == 2,
         t_d3 == 180,
         vfr == 3.5) %>%
  select(date, Rt, max_Rt_omicron) %>%
  mutate(max_Rt_omicron = factor(max_Rt_omicron, levels = c(7.5, 5), labels = c("max Rt = 7.5", "max Rt = 5"), ordered = TRUE))

name <- "rq3_hic_abmodel_omicron_zerocovid"
df2o <- readRDS(paste0("processed_outputs/df_summarise_", name, ".rds")) %>%
  filter(vaccine_doses == 3,
         t_d3 == 180,
         strategy_name == "10y+ 2 doses, booster 10y+",
         max_Rt_omicron == 7.5) %>%
  select(date, Rt, max_Rt_omicron, Rt_lift_t) %>%
  mutate(max_Rt_omicron = factor(max_Rt_omicron, levels = c(7.5, 5), labels = c("max Rt = 7.5", "max Rt = 5"), ordered = TRUE)) %>%
  mutate(Rt_lift_t = factor(Rt_lift_t, levels = c("Sept '21 lift", "Nov '21 lift", "April '22 lift", "Slow April '22 lift"), ordered = TRUE))

p_Rt1 <- ggplot(data = df1o, aes(x = as.Date(date), y = Rt, linetype = factor(max_Rt_omicron))) +
  geom_line(size = 0.8) +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0,
        legend.position = "bottom") +
  labs(x = "Time", y = "Rt", col = "", title = "Substantial prior transmission", linetype = "")

p_Rt1

p_Rt2 <- ggplot(data = df2o, aes(x = as.Date(date), y = Rt, col = Rt_lift_t)) +
  geom_line(size = 0.8) +
  facet_wrap(~Rt_lift_t, nrow = 4) +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0,
        legend.position = "right") +
  labs(x = "Time", y = "Rt", col = "", title = "Minimal prior transmission", linetype = "")

p_Rt2

ggsave("plots/p_Rt_substantial.png", p_Rt1, height = 3, width = 4)
ggsave("plots/p_Rt_minimal.png", p_Rt2, height = 3, width = 4)

library(patchwork)
layout <- "
AB
CB
"
combined <- p_Rt1 + p_Rt2 +
  plot_annotation(tag_levels = "A") + 
  #plot_layout(guides = "collect") + 
  plot_layout(ncol = 2, design = layout)

combined
ggsave("plots/p_Rt.png", combined, height = 5, width = 8)
