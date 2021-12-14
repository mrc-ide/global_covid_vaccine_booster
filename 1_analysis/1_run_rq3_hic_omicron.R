
name <- "rq3_hic_abmodel_omicron_zerocovid"
data <- "Imperial"

#### Get vaccine parameters  ##############################################
vaccine <- "Pfizer"
t_d3_set <- 180

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
income_group <- "HIC"
hs_constraints <- "Present"
dt <- 0.25
repetition <- 1:10
vacc_start <- "1/1/2021"
vaccine_doses <- c(2,3)
max_coverage <- 0.9
age_groups_covered <- 15
age_groups_covered_d3 <- c(5, 15)
seeding_cases <- 10
vacc_per_week <- 0.05
ab_model_infection <- TRUE
strategy <- "realistic"
t_d3 <- t_d3_set
max_Rt <- 4
max_Rt_omicron <- c(5,7.5)
vfr_time1 <- "11/27/2021"
vfr_time2 <- "12/31/2021"
vfr <- unique(vacc_params$vfr)[1]

# how many days to reach 80% vaccinated with 2 doses, at 5% vacc per dose per week?
days <- floor(0.8 / (vacc_per_week/7))*2
d1 <- as.Date(x = (as.Date("1/1/2021", format = "%m/%d/%Y") + days), format = "%m/%d/%Y")

R0_t3_in <- c("8/1/2021", "10/1/2021", "3/1/2022", "3/2/2022")

#### Create scenarios ##########################################################

scenarios <- expand_grid(income_group = income_group,
                         target_pop = target_pop,
                         hs_constraints = hs_constraints,
                         vaccine_doses = vaccine_doses,
                         vaccine = vaccine,
                         max_coverage = max_coverage,
                         age_groups_covered = age_groups_covered,
                         age_groups_covered_d3 = age_groups_covered_d3,
                         vacc_start = vacc_start,
                         dt = dt,
                         repetition = repetition,
                         seeding_cases = seeding_cases,
                         vacc_per_week = vacc_per_week,
                         ab_model_infection = ab_model_infection,
                         t_d3 = t_d3,
                         R0_t3_in = R0_t3_in,
                         max_Rt = max_Rt,
                         max_Rt_omicron = max_Rt_omicron,
                         vfr = vfr,
                         vfr_time1 = vfr_time1,
                         vfr_time2 = vfr_time2)  %>%
  filter((vaccine_doses == 2 & age_groups_covered_d3 == 15 ) | (vaccine_doses == 3) ) %>%
  filter(R0_t3_in == "8/1/2021" | (R0_t3_in == "10/1/2021" & (age_groups_covered_d3 %in% c(5,15))) | (R0_t3_in == "3/1/2022" & age_groups_covered_d3 == 15) | (R0_t3_in == "3/2/2022" & age_groups_covered_d3 == 15)) %>%
  unique()

scenarios$scenario <- 1:nrow(scenarios)
scenarios$name <- name
scenarios$strategy <- strategy

scenarios <- left_join(scenarios, vacc_params, by = c("vaccine", "t_d3", "vfr"))

nrow(scenarios)

write_csv(scenarios, paste0("scenarios_", name, ".csv"))

#### Run the model on cluster ###############################################
# Load functions
sources <- c("R/run_function_abmodel_zerocovid_omicron_v2.R", "R/utils.R", "R/vaccine_strategy.R")
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
runs <- run$enqueue_bulk(scenarios, run_scenario_zerocovid, do_call = TRUE, progress = TRUE)
runs$status()

