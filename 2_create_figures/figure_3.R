name <- "rq2_lmic_abmodel_omicron"
ages_covered <- 9
dose_3_t <- 180

df_summarise <- readRDS(paste0("processed_outputs/df_summarise_", name, ".rds")) %>%
  filter(age_groups_covered == ages_covered,
         t_d3 == dose_3_t) %>%
  mutate(vaccine_doses = factor(vaccine_doses, levels = c("Expand 2 doses", "2 doses + booster", "Pre-vaccine introduction"), ordered = TRUE)) %>%
  mutate(vfr_lab = paste0("VFR = ", round(vfr,2))) %>%
  mutate(vfr_lab = factor(vfr_lab)) %>%
  mutate(max_Rt_lab = factor(max_Rt_omicron, levels = c(6,7.5), labels = c("Rt = 6", "Rt = 7.5")))

df_summarise_totals <- readRDS(paste0("processed_outputs/df_summarise_totals_", name, ".rds")) %>%
  filter(age_groups_covered == ages_covered)%>%
  mutate(vaccine_doses = factor(vaccine_doses, levels = c("Expand 2 doses", "2 doses + booster"), ordered = TRUE))

#################################################
# plot total doses over time
ggplot(data = df_summarise, aes(x = as.Date(date), y = vaccines_t/target_pop, col = vaccine_doses)) +
  geom_line() +
  facet_wrap(~rollout_rate)

df_summarise %>%
  select(rollout_rate, vaccines_t, target_pop, t_d3, vaccine_doses, date) %>%
  group_by(rollout_rate, target_pop, t_d3, vaccine_doses) %>%
  summarise(vaccines_t = max(vaccines_t)/max(target_pop))

#cumsum(rev(pop$n/1e6))*0.8


#################################################
# blue-green doses barplot
df_doses <- df_summarise %>%
  filter(vaccine_doses != "Pre-vaccine introduction",
         t_d3 == 180,
         max_Rt_omicron == 7.5,
         vfr == unique(df_summarise$vfr)[1]) %>%
  rename("Dose 1" = "dose1_t", "Dose 2" = "dose2_t", "Booster" = "dose3_t") %>%
  filter(rollout_rate == "Default") %>%
  pivot_longer(cols = c("Dose 1", "Dose 2", "Booster"), names_to = "dose") %>%
  mutate(dose = factor(dose, levels = c("Dose 1", "Dose 2", "Booster"), ordered = TRUE))

df1_doses_month <- df_doses %>%
  # filter to last date of each month
  mutate(year = lubridate::year(date),
         month = lubridate::month(date),
         day = lubridate::day(date),
         date = lubridate::floor_date(date, "month")) %>%
  group_by(income_group, target_pop, age_groups_covered, vaccine_doses, dose, year, month) %>% 
  mutate(max_day = max(day)) %>%
  ungroup() %>%
  filter(day == max_day)

plot_doses <- ggplot(data = df1_doses_month, aes(x = as.Date(date), y = value/target_pop*100, fill = dose)) +
  geom_bar(stat = "identity") +
  facet_grid( ~ vaccine_doses) +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0,
        axis.text.x = element_text(angle = 335, vjust = 0.3, hjust=0.2)) +
  scale_fill_brewer(palette = "Paired") +
  labs(x = "Time", y = "Vaccinated (%)", fill = "Dose number")
plot_doses



df1 <- df_summarise %>%
  mutate(vfr_lab = paste0("VFR = ", round(vfr,2))) %>%
  mutate(vfr_lab = factor(vfr_lab)) %>%
  mutate(max_Rt_lab = factor(max_Rt_omicron, levels = c(6,7.5), labels = c("Rt = 6", "Rt = 7.5")))

df_summarise_totals_om <- readRDS(paste0("processed_outputs/df_summarise_totals_", name, ".rds")) %>%
  filter(age_groups_covered == ages_covered) 

deaths_omicron <- ggplot(data = filter(df1, vacc_per_week == 0.02, max_Rt_omicron == 7.5), aes(x = as.Date(date), y = deaths_t/target_pop * 1e6, col = vaccine_doses)) +
  geom_ribbon(aes(ymin = deaths_tmin/target_pop * 1e6, ymax = deaths_tmax/target_pop * 1e6, fill = vaccine_doses), alpha = 0.5, col = NA) +
  geom_line() +
  facet_wrap(~vfr_lab, nrow = 1) +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0) +
  scale_color_manual(values = c(col6, col8, "black")) +
  scale_fill_manual(values = c(col6, col8, "grey")) +
  labs(x = "Time", y = "Daily deaths per million", col = "Dose scenario", fill = "Dose scenario")

deaths_omicron

#########################
# summary barchart
df_om <- df_summarise_totals_om %>%
  select(deaths_med, inc_med, vfr, vaccine_doses, rollout_rate, dose_3_timing, max_Rt_omicron, target_pop) %>%
  mutate(scenario = paste0("VFR = ", vfr)) %>%
  filter(max_Rt_omicron == 7.5)

df_barchart <-df_om %>%
  filter(!(dose_3_timing == "12 months" & rollout_rate == "Slower rollout")) %>%
  mutate(sensitivity_scenario = if_else(dose_3_timing == "6 months (default)" & rollout_rate == "Default", "Default", if_else(dose_3_timing == "6 months (default)" & rollout_rate == "Slower rollout", "Slower rollout", if_else(dose_3_timing == "12 months" & rollout_rate == "Default", "12 months to booster", "NA")))) %>%
  filter(sensitivity_scenario != "NA") %>%
  mutate(sensitivity_scenario = factor(sensitivity_scenario, levels = c("Default", "Slower rollout", "12 months to booster")))

df_barchart_d <- df_barchart %>%
  select(-inc_med) %>%
  pivot_wider(names_from = vaccine_doses, values_from = deaths_med) %>%
  mutate(`2 doses + booster` = `2 doses + booster` / `Expand 2 doses`) %>%
  pivot_longer(cols = c("2 doses + booster", "Expand 2 doses")) %>%
  filter(name == "2 doses + booster")

x <- df_barchart %>%
  filter(sensitivity_scenario == "Default") %>%
  select(-sensitivity_scenario, -rollout_rate, -dose_3_timing)
x
(3376-2838)/3376

# barplot summary of deaths
p_deaths_summary <- ggplot(data = df_barchart, aes(x = scenario, y = deaths_med/target_pop * 1e6, fill = vaccine_doses)) +
  geom_bar(position = "dodge", stat = "identity", alpha = 0.8) +
  labs(x = "Variant scenario", y = "Total deaths per million", col = "Dose scenario", fill = "Dose scenario") +
  facet_wrap(~sensitivity_scenario) +
  scale_fill_manual(values = c(col6, col8)) +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0)

p_deaths_summary

p_deaths_summary_v2 <- ggplot(data = df_barchart_d, aes(x = scenario, y = value, fill = name)) +
  geom_bar(position = "dodge", stat = "identity", alpha = 0.8) +
  labs(x = "Variant scenario", y = "Relative deaths", col = "Dose scenario", fill = "Dose scenario") +
  facet_wrap(~sensitivity_scenario) +
  scale_fill_manual(values = c("grey40")) +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0)

p_deaths_summary_v2

# barplot summary of incidence
p_inc_summary <- ggplot(data = df_barchart, aes(x = scenario, y = inc_med/target_pop * 1e6, fill = vaccine_doses)) +
  geom_bar(position = "dodge", stat = "identity", alpha = 0.8) +
  labs(x = "Variant scenario", y = "Total incidence per million", col = "Dose scenario", fill = "Dose scenario") +
  facet_wrap(~sensitivity_scenario) +
  scale_fill_manual(values = c(col6, col8)) +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0)

p_inc_summary
ggsave(paste0("plots/fig3_inc_age_groups_covered_", ages_covered, ".png"),p_inc_summary, height = 4, width = 11)

#############################################################
#########################
# summary transmission
df_om_2 <- df_summarise_totals_om %>%
  select(deaths_med, inc_med, vfr, vaccine_doses, vacc_per_week, t_d3, max_Rt_omicron, target_pop) %>%
  filter(vfr == 3.5,
         t_d3 == 180,
         vacc_per_week == 0.02) %>%
  mutate(max_Rt_omicron = paste0("Rt = ", max_Rt_omicron))

# barplot summary of deaths
deaths_summary_Rt <- ggplot(data = df_om_2, aes(x = max_Rt_omicron, y = deaths_med/target_pop * 1e6, fill = vaccine_doses)) +
  geom_bar(position = "dodge", stat = "identity", alpha = 0.8) +
  labs(x = "Variant scenario", y = "Total deaths per million", col = "Dose scenario", fill = "Dose scenario") +
  scale_fill_manual(values = c(col6, col8)) +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0)

deaths_summary_Rt

# combine plots
library(patchwork)
layout <- "
AB
CC
DD
"
combined <- plot_doses + deaths_summary_Rt + deaths_omicron+ p_deaths_summary  +
  plot_annotation(tag_levels = "A") + 
  plot_layout(guides = "collect") + 
  plot_layout(ncol = 2, nrow = 3, design = layout)

combined
ggsave(paste0("plots/fig3_age_groups_covered_", ages_covered, ".png"),combined, height = 10, width = 11)

