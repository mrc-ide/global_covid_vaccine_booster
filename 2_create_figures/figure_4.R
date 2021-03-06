name <- "rq3_hic_abmodel_omicron_zerocovid"

df_summarise <- readRDS(paste0("processed_outputs/df_summarise_", name, ".rds"))
df_summarise_totals <- readRDS(paste0("processed_outputs/df_summarise_totals_", name, ".rds"))

m <- unique(df_summarise$strategy_name)
m
df_summarise <- df_summarise %>%
  mutate(strategy_name = factor(strategy_name, levels = m, ordered = TRUE))
df_summarise_totals <- df_summarise_totals %>%
  mutate(strategy_name = factor(strategy_name, levels = m, ordered = TRUE))

df1_omicron <- filter(df_summarise, vacc_per_week == 0.05) %>%
  filter(strategy_name %in% c("10y+ 2 doses, no booster", "10y+ 2 doses, booster 60y+", "10y+ 2 doses, booster 10y+")) %>%
  mutate(Rt_lift_t = factor(Rt_lift_t, levels = c("Sept '21 lift", "Nov '21 lift", "April '22 lift", "Slow April '22 lift"))) %>%
  filter(Rt_lift_t == "Sept '21 lift" | (Rt_lift_t == "Nov '21 lift" & (strategy_name %in% c("10y+ 2 doses, booster 60y+", "10y+ 2 doses, booster 10y+"))) | (Rt_lift_t == "April '22 lift" & strategy_name == "10y+ 2 doses, booster 10y+") | (Rt_lift_t == "Slow April '22 lift" & strategy_name == "10y+ 2 doses, booster 10y+"))

df2_omicron <- filter(df_summarise_totals, vacc_per_week == 0.05) %>%
  filter(strategy_name %in% c("10y+ 2 doses, no booster", "10y+ 2 doses, booster 60y+", "10y+ 2 doses, booster 10y+")) %>%
  mutate(Rt_lift_t = factor(Rt_lift_t, levels = c("Sept '21 lift", "Nov '21 lift", "April '22 lift", "Slow April '22 lift"))) %>%
  filter(Rt_lift_t == "Sept '21 lift" | (Rt_lift_t == "Nov '21 lift" & (strategy_name %in% c("10y+ 2 doses, booster 60y+", "10y+ 2 doses, booster 10y+"))) | (Rt_lift_t == "April '22 lift" & strategy_name == "10y+ 2 doses, booster 10y+") | (Rt_lift_t == "Slow April '22 lift" & strategy_name == "10y+ 2 doses, booster 10y+"))


# plot outputs: deaths
deaths_omicron <- ggplot(data = filter(df1_omicron, t_d3 == 180, max_Rt_omicron == 7.5), aes(x = as.Date(date), y = deaths_t/target_pop * 1e6, col = strategy_name)) +
  geom_ribbon(aes(ymin = deaths_tmin/target_pop * 1e6, ymax = deaths_tmax/target_pop * 1e6, fill = strategy_name), alpha = 0.5, col = NA) +
  geom_line() +
  facet_wrap(~ Rt_lift_t, nrow = 4) +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0,
        legend.position = "none") +
  scale_color_viridis_d(option = "E", begin = 0.2, end = 0.9) +
  scale_fill_viridis_d(option = "E", begin = 0.2, end = 0.9) +
  labs(x = "Year", y = "Daily deaths per million", col = "Dose scenario", fill = "Dose scenario")

deaths_omicron

deaths_summary_omicron <- ggplot(data = filter(df2_omicron, t_d3 == 180, max_Rt_omicron == 7.5), aes(x = Rt_lift_t, y = deaths_med / target_pop * 1e6, fill = strategy_name)) +
  geom_col(position = position_dodge2(width = 0.9, preserve = "single"))+
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0) +
  scale_fill_viridis_d(option = "E", begin = 0.2, end = 0.9) +
  labs(x = "Rt lift scenario", y = "Total deaths per million", fill = "Dose scenario")

deaths_summary_omicron

###########################

# plot outputs: infections
infections_omicron <- ggplot(data = filter(df1_omicron, t_d3 == 180, max_Rt_omicron == 7.5), aes(x = as.Date(date), y = inc_t/target_pop * 1e6, col = strategy_name)) +
  geom_ribbon(aes(ymin = inc_tmin/target_pop * 1e6, ymax = inc_tmax/target_pop * 1e6, fill = strategy_name), alpha = 0.5, col = NA) +
  geom_line() +
  facet_wrap(~ Rt_lift_t, nrow = 4) +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0,
        legend.position = "none") +
  scale_color_viridis_d(option = "E", begin = 0.2, end = 0.9) +
  scale_fill_viridis_d(option = "E", begin = 0.2, end = 0.9) +
  labs(x = "Year", y = "Daily incidence per million", col = "Dose scenario", fill = "Dose scenario")

infections_omicron

infections_summary_omicron <- ggplot(data = filter(df2_omicron, t_d3 == 180, max_Rt_omicron == 7.5), aes(x = Rt_lift_t, y = inc_med / target_pop * 1e6, fill = strategy_name)) +
  geom_col(position = position_dodge2(width = 0.9, preserve = "single"))+
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0) +
  scale_fill_viridis_d(option = "E", begin = 0.2, end = 0.9) +
  labs(x = "Rt lift scenario", y = "Total incidence per million", fill = "Dose scenario")

infections_summary_omicron

###########################
library(patchwork)
layout <- "
AB
AC
AD
"
combined <- deaths_omicron + deaths_summary_omicron + infections_summary_omicron + guide_area() + 
  plot_annotation(tag_levels = "A") + 
  plot_layout(guides = "collect", design = layout, widths = c(1.2,1)) 
combined

ggsave("plots/fig4.png", combined, height = 8, width = 12)

