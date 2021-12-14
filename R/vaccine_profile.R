vaccine_profile <- function(mu_ab_d2,
                       fold_red,
                       vfr,
                       mu_ab_d1,
                       hl_s,
                       hl_l,
                       period_s,
                       period_l,
                       k,
                       ab_50,
                       ab_50_severe,
                       dose_3_fold_increase,
                       vaccine) {
  mu_ab_d2 <- mu_ab_d2/vfr
  mu_ab_d1 <- mu_ab_d1/vfr
  
  t <- 0:730  #vaccinated on day 0
  dr_s <- -log(2)/hl_s  # Corresponding decay rate in days for half life above
  dr_l <- -log(2)/hl_l 
  period_s <- period_s
  period_l <- period_l
  dr_vec <- c(0, rep(dr_s, round(period_s) - 1),
              seq(dr_s, dr_l,length.out=round(period_l) - round(period_s) + 1),
              rep(dr_l, 1000 - round(period_l)))
  dr_vec <- dr_vec[1:length(t)]
  
  n1 = mu_ab_d1*exp(cumsum(dr_vec))
  
  efficacy_dose1_infection <- 1/(1+exp(-k*(log10(n1) -log10(ab_50))))
  efficacy_dose1_severe <- 1/(1+exp(-k*(log10(n1) -log10(ab_50_severe))))
  
  n2=mu_ab_d2*exp(cumsum(dr_vec))
  efficacy_dose2_infection <- 1/(1+exp(-k*(log10(n2) -log10(ab_50))))
  efficacy_dose2_severe <- 1/(1+exp(-k*(log10(n2) -log10(ab_50_severe))))
  
  n3=dose_3_fold_increase*mu_ab_d2*exp(cumsum(dr_vec))
  efficacy_dose3_infection <- 1/(1+exp(-k*(log10(n3) -log10(ab_50))))
  efficacy_dose3_severe <- 1/(1+exp(-k*(log10(n3) -log10(ab_50_severe))))
  
  sub <- data.frame(t=t,
                    Titre_d1=n1, Efficacy_d1=efficacy_dose1_infection*100, Severe_Efficacy_d1=efficacy_dose1_severe*100,
                    Titre_d2=n2, Efficacy_d2=efficacy_dose2_infection*100, Severe_Efficacy_d2=efficacy_dose2_severe*100,
                    Titre_d3=n3, Efficacy_d3=efficacy_dose3_infection*100, Severe_Efficacy_d3=efficacy_dose3_severe*100)
  return(sub)
}
