##############################################
# AGE PLOT
name <- "rq1_hic_abmodel_omicron_age"
df_age <- readRDS(paste0("processed_outputs/df_summarise_", name, ".rds")) %>%
  unique()

x <- seq(0,80,5)
y <- seq(4,84,5)
z <- paste0(x,"-",y)
z[17] <- "80+"

age_plot <- ggplot(data = df_age, aes(x = age, y= value_med, fill =strategy_name)) +
  geom_col(position = "dodge", alpha = 0.8) +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0,
        axis.text.x = element_text(angle = 335, vjust = 0.5, hjust=0),
        legend.position = "none") +
  labs(x = "Age", y = "Total deaths", fill = "Dose scenario") +
  scale_x_continuous(breaks = 1:17, labels = z) +
  scale_fill_manual(values = col_set)

age_plot


# plot deaths - omicron scenario
name <- "rq1_hic_abmodel_omicron"
df_summarise_om <- readRDS(paste0("processed_outputs/df_summarise_", name, ".rds"))
df_summarise_totals_om <- readRDS(paste0("processed_outputs/df_summarise_totals_", name, ".rds"))

#############################################

# numbers for text
x <- df_summarise_totals_om %>% 
  filter(vacc_per_week == 0.05,
         t_d3 == 180,
         max_Rt_omicron == 7.5,
         strategy_name %in% c("10y+ 2 doses, no booster", "10y+ 2 doses, booster 60y+")) %>%
  select(target_pop, strategy_name, vfr, deaths_med, inc_med)
x
(5786-4210)/5786
(2547994-2496691)/2547994
y <- df_summarise_om %>%
  filter(date > as.Date("2021-01-01")) %>%
  filter(vacc_per_week == 0.05,
         t_d3 == 180,
         strategy_name %in% c("10y+ 2 doses, no booster", "10y+ 2 doses, booster 60y+",
                              "10y+ 2 doses, booster 10y+")) %>%
  group_by(target_pop, strategy_name, vfr, max_Rt_omicron) %>%
  summarise(deaths = max(deaths_t))
y

################################
m <- unique(df_summarise_om$strategy_name)
m

df_summarise_om <- df_summarise_om %>%
  mutate(strategy_name = factor(strategy_name, levels = m, ordered = TRUE))

df_summarise_totals_om <- df_summarise_totals_om %>%
  mutate(strategy_name = factor(strategy_name, levels = m, ordered = TRUE))

df_summarise_om <- df_summarise_om %>%
  mutate(dose_3_timing = factor(dose_3_timing, levels = c("6 months (default)", "3 months")))

df_summarise_totals_om <- df_summarise_totals_om %>%
  mutate(dose_3_timing = factor(dose_3_timing, levels = c("6 months (default)", "3 months")))

df_summarise_om <- df_summarise_om %>%
  mutate(vfr_lab = paste0("VFR = ", vfr))

p_deaths_omicron <- ggplot(data = filter(df_summarise_om,
                                 strategy_name %in% c("Pre-vaccine introduction", "10y+ 2 doses, no booster", "10y+ 2 doses, booster 60y+", "10y+ 2 doses, booster 10y+"),
                                 max_Rt == 4,
                                 t_d3 == 180,
                                 max_Rt_omicron == 7.5)
                   , aes(x = as.Date(date), y = deaths_t/target_pop * 1e6, col = strategy_name)) +
  geom_ribbon(aes(ymin =deaths_tmin/target_pop * 1e6, ymax = deaths_tmax/target_pop * 1e6, fill = strategy_name), alpha = 0.5, col = NA) +
  geom_line() +
  facet_wrap(~vfr_lab, nrow = 1) +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0) +
  scale_color_manual(values = c("grey20", col_set_3)) +
  scale_fill_manual(values = c("grey20", col_set_3)) +
  labs(x = "Time", y = "Daily deaths per million", col = "Dose scenario", fill = "Dose scenario")

p_deaths_omicron

p_inc_omicron <- ggplot(data = filter(df_summarise_om,
                                         strategy_name %in% c("Pre-vaccine introduction", "10y+ 2 doses, no booster", "10y+ 2 doses, booster 60y+", "10y+ 2 doses, booster 10y+"),
                                         max_Rt == 4,
                                         t_d3 == 180,
                                      max_Rt_omicron == 7.5,
                                          timestep < max(df_summarise$timestep))
                           , aes(x = as.Date(date), y = (inc_t/target_pop*1e6), col = strategy_name)) +
   geom_ribbon(aes(ymin =inc_tmin/target_pop * 1e6, ymax = inc_tmax/target_pop * 1e6, fill = strategy_name), alpha = 0.5, col = NA) +
  geom_line() +
  facet_wrap(~vfr_lab, nrow = 1) +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0) +
  scale_color_manual(values = c("grey20", col_set_3)) +
  scale_fill_manual(values = c("grey20", col_set_3)) +
  #scale_y_continuous(trans = "log10") +
  labs(x = "Time", y = "Daily incidence per million", col = "Dose scenario", fill = "Dose scenario")

p_inc_omicron

y1 <- df_summarise_om %>%
  filter(date > as.Date("2021-01-01")) %>%
  filter(vacc_per_week == 0.05,
         t_d3 == 180,
         strategy_name %in% c("Pre-vaccine introduction", "10y+ 2 doses, no booster", "10y+ 2 doses, booster 60y+", "10y+ 2 doses, booster 10y+"),
         max_Rt_omicron == 7.5) %>%
  group_by(target_pop, strategy_name, vfr, max_Rt_omicron) %>%
  summarise(deaths = max(deaths_t))
y1

#########################
# summary barchart
df_barchart <- df_summarise_totals_om %>%
  filter(t_d3 == 180) %>%
  select(deaths_med, inc_med, vfr, strategy_name, max_Rt_omicron, target_pop) %>%
  mutate(scenario = paste0("VFR = ", vfr))
df_barchart_90 <- df_summarise_totals_om %>%
  filter(t_d3 == 90) %>%
  filter(vfr == 3.5) %>%
  select(deaths_med, inc_med, vfr, strategy_name, max_Rt_omicron, target_pop) %>%
  mutate(scenario = paste0("VFR = ", vfr, "\n3 month boost"))

df_barchart <- rbind(df_barchart, df_barchart_90)
df_barchart

# barplot summary of deaths
p_deaths_summary <- ggplot(data = filter(df_barchart, max_Rt_omicron == 7.5), aes(x = scenario, y = deaths_med/target_pop * 1e6, fill = strategy_name)) +
  geom_bar(position = "dodge", stat = "identity", alpha = 0.8) +
  scale_fill_manual(values = col_set) +
  labs(x = "Variant scenario", y = "Total deaths per million", col = "Dose scenario", fill = "Dose scenario") +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0)

p_deaths_summary

# barplot summary of deaths by Rt
df_barchart_Rt <-
  df_barchart %>%
  mutate(max_Rt_omicron = paste0("Rt = ", max_Rt_omicron))

p_deaths_summary_Rt <- ggplot(data = filter(df_barchart_Rt, vfr == 3.5), aes(x = max_Rt_omicron, y = deaths_med/target_pop * 1e6, fill = strategy_name)) +
  geom_bar(position = "dodge", stat = "identity", alpha = 0.8) +
  scale_fill_manual(values = col_set) +
  labs(x = "Rt max", y = "Total deaths per million", col = "Dose scenario", fill = "Dose scenario") +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0)
p_deaths_summary_Rt

# barplot summary of incidence
p_inc_summary_180 <- ggplot(data = filter(df_barchart, max_Rt_omicron == 7.5), aes(x = scenario, y = inc_med/target_pop * 1e6, fill = strategy_name)) +
  geom_bar(position = "dodge", stat = "identity", alpha = 0.8) +
  scale_fill_manual(values = col_set) +
  labs(x = "Variant scenario", y = "Total incidence per million", col = "Dose scenario", fill = "Dose scenario") +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0)

p_inc_summary_180

# plot total doses over time
fig_doses_time <- ggplot(data = filter(df_summarise_om,
                                       strategy_name != "Pre-vaccine introduction",
                                       max_Rt == 4,
                                       vfr == 3.5,
                                       max_Rt_omicron == 7.5), aes(x = as.Date(date), y  = vaccines_t/target_pop, col = strategy_name, linetype = dose_3_timing)) +
  geom_line(size = 1) +
  lims(x = c(as.Date("2021-01-01"), as.Date("2022-06-30"))) +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0) +
  labs(x = "Year", y = "Cumulative doses per person", col = "Dose scenario", linetype = "Booster dose timing") +
  scale_color_manual(values = col_set)

fig_doses_time
ggsave("plots/rq1_fig_doses_time.png", fig_doses_time, height = 3.5, width = 7)


############################
library(patchwork)
layout <- "
AB
CC
DE
"
combined <-  fig_doses_time + p_deaths_summary + p_deaths_omicron +age_plot +p_deaths_summary_Rt +
  plot_annotation(tag_levels = "A") + 
  plot_layout(guides = "collect") + 
  plot_layout(ncol = 2, nrow = 3, design = layout)

combined
ggsave("plots/fig2.png",combined, height = 8, width = 11)

layout2 <- "
AA
BC
"
inc_combined <- p_inc_omicron +p_inc_summary_180 + 
  plot_annotation(tag_levels = "A") + 
  plot_layout(guides = "collect") + 
  plot_layout(ncol = 2, nrow = 2, design = layout2)
inc_combined
ggsave("plots/fig2_incidence.png",inc_combined, height = 6, width = 11)

