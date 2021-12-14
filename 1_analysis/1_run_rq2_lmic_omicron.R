
name <- "rq2_lmic_abmodel_omicron"

#### Get vaccine parameters  ##############################################
vaccine <- "Oxford-AstraZeneca"
t_d3_set <- c(180, 360)

vacc_names <- data.frame(vaccine = c("Pfizer", "Oxford-AstraZeneca"), vacc = c("PF", "AZ"))
vaccine_set <- vaccine
vacc_params <- readRDS("data/param_list_out_Imperial.rds") %>%
  rename(vacc = vaccine) %>%
  left_join(vacc_names, by = "vacc") %>%
  filter(vaccine == vaccine_set) %>%
  mutate(std10 = 0.44) %>%
  filter(vfr > 1,
         t_d3 %in% t_d3_set) %>%
  select(-c(boost_d2, mu_ab_d3, boost_d3, vacc))

#### Set up other simulation parameters  ##############################################
target_pop <- 1e6
income_group <- "LMIC"
hs_constraints <- "Present"
dt <- 0.25
repetition <- 1:10
vacc_start <- "1/1/2021"
vaccine_doses <- c(2,3)
max_coverage <- 0.8
age_groups_covered <- c(5, 9)
seeding_cases <- 10
vacc_per_week <- c(0.02, 0.01)
ab_model_infection <- TRUE
strategy <- "same_doses"
t_d3 <- t_d3_set
max_Rt <- 4
max_Rt_omicron <- c(5,7.5)
vfr_time1 <- "11/27/2021"
vfr_time2 <- "12/31/2021"
vfr <- unique(vacc_params$vfr)

#### Create scenarios ##########################################################

scenarios <- expand_grid(income_group = income_group,
                         target_pop = target_pop,
                         hs_constraints = hs_constraints,
                         vaccine_doses = vaccine_doses,
                         vaccine = vaccine,
                         max_coverage = max_coverage,
                         age_groups_covered = age_groups_covered,
                         vacc_start = vacc_start,
                         dt = dt,
                         repetition = repetition,
                         seeding_cases = seeding_cases,
                         vacc_per_week = vacc_per_week,
                         ab_model_infection = ab_model_infection,
                         t_d3 = t_d3,
                         max_Rt = max_Rt,
                         max_Rt_omicron = max_Rt_omicron,
                         vfr = vfr,
                         vfr_time1 = vfr_time1,
                         vfr_time2 = vfr_time2) %>%
  filter((vacc_per_week == 0.02) | (max_Rt_omicron == 7.5) | (vacc_per_week == 0.02 & vfr == unique(vacc_params$vfr)[1] & t_d3 == 180 & max_Rt_omicron == 5)) %>%
  unique()

scenarios$scenario <- 1:nrow(scenarios)
scenarios$name <- name
scenarios$strategy <- strategy

scenarios <- left_join(scenarios, vacc_params, by = c("vaccine", "t_d3", "vfr"))

nrow(scenarios)

write_csv(scenarios, paste0("scenarios_", name, ".csv"))

#### Run the model on cluster ###############################################
# Load functions
sources <- c("R/run_function_abmodel_omicron_v2.R", "R/utils.R", "R/vaccine_strategy.R")
src <- conan::conan_sources(c("mrc-ide/safir", "mrc-ide/squire", "mrc-ide/nimue"))
ctx <- context::context_save("context",
                             sources = sources,
                             packages = c("tibble", "dplyr", "tidyr", "countrycode", "safir", "nimue", "squire", "data.table"),
                             package_sources = src)

config <- didehpc::didehpc_config(use_rrq = FALSE, use_workers = FALSE, cluster="fi--didemrchnb")
#config <- didehpc::didehpc_config(use_rrq = FALSE, use_workers = FALSE, cluster="fi--dideclusthn")

# Create the queue
run <- didehpc::queue_didehpc(ctx, config = config)
# Summary of all available clusters
# run$cluster_load(nodes = FALSE)
# Run
runs <- run$enqueue_bulk(scenarios, run_scenario, do_call = TRUE, progress = TRUE)
runs$status()
