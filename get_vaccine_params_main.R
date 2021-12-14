library(tidyverse)
library(patchwork)
library(dplyr)
library(purrr)
library(drjacoby)

source("R/vaccine_profile.R")

##################################
##### LOAD THE PARAMETERS AND DATA
##################################

if(data=="Imperial") {
  load("Z:/Azra/covid_vaccine/Ab model fitting/Model Fits/imp_ve_mcmc_om_5a.Rdata")
}

if(data=="PHE") {
  load("Z:/Azra/covid_vaccine/Ab model fitting/Model Fits/phe_ve_mcmc_om_5a.Rdata")
}

chain <- mcmc$output %>%
  filter(phase == "sampling") %>%
  subset(select=-c(chain,phase,iteration,logprior,loglikelihood))

chain <- sample_chains(mcmc, 10000)

posterior_median <- chain %>%
  summarise( 
    across(where(is.numeric), median)
  )

posterior_upper <- chain %>%
  summarise( 
    across(where(is.numeric), quantile, 0.975)
  )

posterior_lower <- chain %>%
  summarise( 
    across(where(is.numeric), quantile, 0.025)
  )

vaccine <- c("AZ", "PF")

#############################################
# assemble df of new params

log10_d2_AZ <- log10(32/59)- posterior_median$fold_red_AZ
log10_d2_PF <- log10(223/94) - posterior_median$fold_red_PF

d1_AZ       <- 10^( log10_d2_AZ + posterior_median$d1_AZ)
d1_PF       <- 10^( log10_d2_PF + posterior_median$d1_PF)
fold_red_AZ <- 10^( posterior_median$fold_red_AZ) 
fold_red_PF <- 10^( posterior_median$fold_red_PF)

#d3_AZ       <- 10^(log10_d2_AZ + posterior_median$bst_AZ)## CHANGE 1 - everyone gets the PF boost
d3_PF       <- 10^(log10_d2_PF + posterior_median$bst_PF)

ab_50_PF       <- 10^( log10_d2_PF + posterior_median$ni50) 
ab_50_severe_PF <- 10^( log10_d2_PF + posterior_median$ns50)
ab_50_death_PF  <- 10^( log10_d2_PF + posterior_median$nd50)

ab_50_AZ       <- 10^( log10_d2_PF + posterior_median$ni50 ) 
ab_50_severe_AZ <- 10^( log10_d2_PF + posterior_median$ns50 ) 
ab_50_death_AZ  <- 10^( log10_d2_PF + posterior_median$nd50 ) 

ab_50 <-c(ab_50_AZ, ab_50_PF)
ab_50_severe <-c(ab_50_severe_AZ, ab_50_severe_PF)
ab_50_death <-c(ab_50_death_AZ, ab_50_death_PF)

# note we are going to use the death efficacy estimates for our hospitalisations efficacy
ab_50_severe <- ab_50_death

k           <- posterior_median$k
hl_s        <- posterior_median$hl_s
hl_l        <- posterior_median$hl_l
period_s    <- posterior_median$period_s
period_l  <- posterior_median$period_l

om_red <- 10^(posterior_median$om_red)
om_red_lower <- 10^(posterior_lower$om_red)
om_red_upper <- 10^(posterior_upper$om_red)

vfr <- c(1,round(om_red, 1), round(om_red_lower,1),round(om_red_upper,1))

mu_ab_d1 <- c(d1_AZ,d1_PF)
mu_ab_d2 <- c(32/59, 223/94)/c(fold_red_AZ,fold_red_PF)

# For our LMIC analysis only (AZ vaccine), we want to simulate the impact of an AZ boost rather than a PF boost, so that we are modelling the same vaccine product as in the primary series (to compare like with like)
# To do this, apply the fold increase of a PF to PF dose 2 to 3, to the AZ dose 2.
dose_3_fold_increase_PF <- d3_PF/mu_ab_d2[2]
d3_AZ <- mu_ab_d2[1] * dose_3_fold_increase_PF

mu_ab_d3 <- c(d3_AZ,d3_PF)   ####

dose_3_fold_increase <- mu_ab_d3/mu_ab_d2

param_list <- data.frame(vaccine,mu_ab_d1,mu_ab_d2, k, hl_s, hl_l, period_s, period_l, dose_3_fold_increase, ab_50, ab_50_severe) 

#############################################
# calculate efficacies over time

r1 <- 
  # Create input options
  expand_grid(
    vfr = vfr,
    vaccine = c("AZ", "PF")) %>%
  # Join with MCMC samples
  left_join(param_list, by = "vaccine") %>%
  # Apply vaccine profile function to each row
  mutate(profile = pmap(., vaccine_profile)) %>%
  # Format
  unnest(cols = c(profile)) %>%
  pivot_longer(cols = c(Titre_d1, Titre_d2, Titre_d3, Efficacy_d1, Efficacy_d2, Efficacy_d3, Severe_Efficacy_d1,Severe_Efficacy_d2, Severe_Efficacy_d3), names_to = "group", values_to = "Value") %>% 
  mutate(dose = case_when(group == "Titre_d1" | group == "Efficacy_d1" | group == "Severe_Efficacy_d1" ~ 1,
                          group == "Titre_d2" | group == "Efficacy_d2" | group == "Severe_Efficacy_d2" ~ 2,
                          group == "Titre_d3" | group == "Efficacy_d3" | group == "Severe_Efficacy_d3" ~ 3 )) %>%
  mutate(group = substr(group, 1, nchar(group) - 3))

#############################################
# Create dose adjustment for safir inputs (need to subtract decayed titre from previous dose)

t_d2 <- 28
t_d3 <- c(t_d2 + 180, t_d2 + 90, t_d2 + 360)

nat_scaling_d2 <- r1 %>%
  filter(t==t_d2 ) %>%
  filter(group == "Titre") %>%
  filter(dose==1) %>%
  mutate(boost_d2 = mu_ab_d2 - Value) %>%
  mutate(scale_d2 = boost_d2/mu_ab_d2)
nat_scaling_d2 <- subset(nat_scaling_d2, select=c(vaccine, vfr, mu_ab_d2, boost_d2, scale_d2))

nat_scaling_d3 <- r1 %>%
  filter(t %in% t_d3 ) %>%
  filter(group == "Titre") %>%
  filter(dose==2) %>%
  mutate(mu_ab_d3 = mu_ab_d2 * dose_3_fold_increase,
         boost_d3 = mu_ab_d3 - Value) %>%
  mutate(scale_d3 = boost_d3/mu_ab_d3) %>%
  mutate(t_d3 = t - 28) 
nat_scaling_d3 <- subset(nat_scaling_d3, select=c(vaccine, vfr, mu_ab_d3, boost_d3, scale_d3, t_d3))

nat_scaling <- left_join(nat_scaling_d2,nat_scaling_d3, by = c("vaccine", "vfr"))

####################
# save outputs for reading into safir runs
param_list_out <- param_list %>%
  left_join(nat_scaling, by = c("vaccine", "mu_ab_d2"))

saveRDS(param_list_out, paste0("data/param_list_out_", data, ".rds"))
