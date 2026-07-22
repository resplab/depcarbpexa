library(tidyverse)
library(heemod

set.seed(1)
parameters <- define_parameters(
  p_subrec_epi2_base = 0.25,
  p_sub_epi1 = rate_to_prob(0.0158, to = 1/4), # de Graaf
  p_subrec_epi2 = rescale_prob(p = p_subrec_epi2_base, 1/4), #blackburn
  or_recurrence = 1.16, 
  p_subrec_epi3 = or_to_prob(or = or_recurrence, p = p_subrec_epi2), # solomon (+16% par épisode, relatif)
  p_subrec_epi4 = or_to_prob(or = or_recurrence^2, p = p_subrec_epi2), # solomon (+16% par épisode, relatif)
  p_subrec_epi5 = or_to_prob(or = or_recurrence^3, p = p_subrec_epi2), # solomon (+16% par épisode, relatif)
  p_remi_drug = 65/190, # NICE
  p_remi_com = 79/159, # NICE
  p_remi_cbt = 65/190,
  
  #p_chr_chr = NA, pas utile, ce sera 1- toutes les autres probas
  
  p_chr_dth_base = 28.6,
  p_sub_dth_base = 19.2,
  
  p_chr_dth = rate_to_prob(p_chr_dth_base, 1/4, 1000),
  p_sub_dth = rate_to_prob(p_sub_dth_base, 1/4, 1000),
  p_subrec_dth = rate_to_prob(p_sub_dth_base, 1/4, 1000),
  p_epi_dth = rate_to_prob(p_chr_dth_base, 1/4, 1000), 
  
  cost_epi_cbt = 463,
  cost_epi_dru = 324,
  cost_epi_com = 502,
  cost_chr_cbt_base = 612,
  cost_chr_cbt = cost_chr_cbt_base/4,
  cost_chr_dru_base = 423,
  cost_chr_dru = cost_chr_dru_base/4,
  cost_chr_com_base = 672,
  cost_chr_com = cost_chr_com_base/4,
  
  
  ut_epi_base = 0.63,
  ut_epi = ut_epi_base/4,
  ut_sub_base = 0.86,
  ut_sub = ut_sub_base/4,
  ut_chr_base = 0.8,
  ut_chr = ut_chr_base/4,
  carbon_chr_cbt_base = 338,
  carbon_chr_cbt = carbon_chr_cbt_base/4,
  carbon_chr_dru_base = 319,
  carbon_chr_dru = carbon_chr_dru_base/4,
  carbon_chr_com_base = 373,
  carbon_chr_com = carbon_chr_com_base/4,
  carbon_epi_cbt_base = 177,
  carbon_epi_cbt = carbon_epi_cbt_base/4,
  carbon_epi_dru_base = 104,
  carbon_epi_dru = carbon_epi_dru_base/4,
  carbon_epi_com_base = 225,
  carbon_epi_com = carbon_epi_com_base/4,
  scc = 90,
  dr = 0.025,
  dru_duration = 8,
  dru_duration_com = dru_duration
)

transition_dru <- define_transition(
  0, 0, 0, 0, 0, C, 0, 0, 0, 0, 0, p_sub_dth, # sub
  0, C, 0, 0, 0, 0, p_subrec_epi2, 0, 0, 0, 0, p_sub_dth, #subrec1
  0, 0, C, 0, 0, 0, 0, p_subrec_epi3, 0, 0, 0, p_sub_dth, #subrec2
  0, 0, 0, C, 0, 0, 0, 0, p_subrec_epi4, 0, 0, p_sub_dth, #subrec3
  0, 0, 0, 0, C, 0, 0, 0, 0, p_subrec_epi5, 0, p_sub_dth, #subrec4
  0, p_remi_drug, 0, 0, 0, C, 0, 0, 0, 0, 0, p_epi_dth, # 1st episode
  0, 0, p_remi_drug, 0, 0, 0, C, 0, 0, 0, 0, p_epi_dth, # 2nd episode
  0, 0, 0, p_remi_drug, 0, 0, 0, C, 0, 0, 0, p_epi_dth, # 3rd episode
  0, 0, 0, 0, p_remi_drug, 0, 0, 0, C, 0, 0, p_epi_dth, # 4th episode
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, C, p_epi_dth, # 5th episode
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, C, p_chr_dth, # chronic
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, # death
  state_names = c("subthreshold", "subthreshold recovery after 1st episode",
                  "subthreshold recovery after 2nd episode", "subthreshold recovery after 3rd episode",
                  "subthreshold recovery after 4th episode", 
                  "1st episode", "2nd episode", "3rd episode", "4th episode", "5th episode",
                  "chronic state", "death")
)
transition_cbt <- define_transition(
  0, 0, 0, 0, 0, C, 0, 0, 0, 0, 0, p_sub_dth, # sub
  0, C, 0, 0, 0, 0, p_subrec_epi2, 0, 0, 0, 0, p_sub_dth, #subrec1
  0, 0, C, 0, 0, 0, 0, p_subrec_epi3, 0, 0, 0, p_sub_dth, #subrec2
  0, 0, 0, C, 0, 0, 0, 0, p_subrec_epi4, 0, 0, p_sub_dth, #subrec3
  0, 0, 0, 0, C, 0, 0, 0, 0, p_subrec_epi5, 0, p_sub_dth, #subrec4
  0, p_remi_cbt, 0, 0, 0, C, 0, 0, 0, 0, 0, p_epi_dth, # 1st episode
  0, 0, p_remi_cbt, 0, 0, 0, C, 0, 0, 0, 0, p_epi_dth, # 2nd episode
  0, 0, 0, p_remi_cbt, 0, 0, 0, C, 0, 0, 0, p_epi_dth, # 3rd episode
  0, 0, 0, 0, p_remi_cbt, 0, 0, 0, C, 0, 0, p_epi_dth, # 4th episode
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, C, p_epi_dth, # 5th episode
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, C, p_chr_dth, # chronic
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, # death
  state_names = c("subthreshold", "subthreshold recovery after 1st episode",
                  "subthreshold recovery after 2nd episode", "subthreshold recovery after 3rd episode",
                  "subthreshold recovery after 4th episode", 
                  "1st episode", "2nd episode", "3rd episode", "4th episode", "5th episode",
                  "chronic state", "death")
)

transition_com <- define_transition(
    0, 0, 0, 0, 0, C, 0, 0, 0, 0, 0, p_sub_dth, # sub
    0, C, 0, 0, 0, 0, p_subrec_epi2, 0, 0, 0, 0, p_sub_dth, #subrec1
    0, 0, C, 0, 0, 0, 0, p_subrec_epi3, 0, 0, 0, p_sub_dth, #subrec2
    0, 0, 0, C, 0, 0, 0, 0, p_subrec_epi4, 0, 0, p_sub_dth, #subrec3
    0, 0, 0, 0, C, 0, 0, 0, 0, p_subrec_epi5, 0, p_sub_dth, #subrec4
    0, p_remi_com, 0, 0, 0, C, 0, 0, 0, 0, 0, p_epi_dth, # 1st episode
    0, 0, p_remi_com, 0, 0, 0, C, 0, 0, 0, 0, p_epi_dth, # 2nd episode
    0, 0, 0, p_remi_com, 0, 0, 0, C, 0, 0, 0, p_epi_dth, # 3rd episode
    0, 0, 0, 0, p_remi_com, 0, 0, 0, C, 0, 0, p_epi_dth, # 4th episode
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, C, p_epi_dth, # 5th episode
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, C, p_chr_dth, # chronic
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, # death
    state_names = c("subthreshold", "subthreshold recovery after 1st episode",
                    "subthreshold recovery after 2nd episode", "subthreshold recovery after 3rd episode",
                    "subthreshold recovery after 4th episode", 
                    "1st episode", "2nd episode", "3rd episode", "4th episode", "5th episode",
                    "chronic state", "death")
  )

sub_state <- define_state(
  utility = ut_sub,
  cost = dispatch_strategy(
    cbt = 0,
    drug = discount(ifelse(state_time <= dru_duration,
                           cost_epi_dru, 0), dr, period = 4, linear = TRUE),
    combined = discount(ifelse(state_time <= dru_duration_com,
                               cost_epi_dru, 0), dr, period = 4, linear = TRUE),
  ),
  carbon = dispatch_strategy(
    cbt = 0,
    drug = ifelse(state_time <= dru_duration, carbon_epi_dru, 0),
    combined = ifelse(state_time <= dru_duration_com, carbon_epi_dru, 0),
  ),
  cost_with_carbon = carbon * scc/1000 + cost
)

epi_state <- define_state(
  utility = ut_epi,
  cost = dispatch_strategy(
    cbt = discount(cost_epi_cbt, dr, period = 4, linear = TRUE),
    drug = discount(cost_epi_dru, dr, period = 4, linear = TRUE),
    combined = discount(cost_epi_com, dr, period = 4, linear = TRUE)
  ),
  carbon = dispatch_strategy(
    cbt = carbon_epi_cbt,
    drug = carbon_epi_dru,
    combined = carbon_epi_com
  ),
  cost_with_carbon = carbon * scc/1000 + cost
)

chr_state <- define_state(
  utility = ut_chr,
  cost = dispatch_strategy(
    cbt = discount(cost_chr_cbt,dr, period = 4, linear = TRUE),
    drug = discount(cost_chr_dru,dr, period = 4, linear = TRUE),
    combined = discount(cost_chr_com, dr,period = 4, linear = TRUE),
  ),
  carbon = dispatch_strategy(
    cbt = carbon_chr_cbt,
    drug = carbon_chr_dru,
    combined = carbon_chr_com
  ),
  cost_with_carbon = carbon * scc/1000 + cost
)


death_state = define_state(
  utility = 0,
  cost = 0,
  carbon = 0,
  cost_with_carbon = 0
)

strats_com <- define_strategy(
  "subthreshold" = sub_state,
  "subthreshold recovery after 1st episode" = sub_state,
  "subthreshold recovery after 2nd episode" = sub_state, 
  "subthreshold recovery after 3rd episode" = sub_state,
  "subthreshold recovery after 4th episode" = sub_state,
  "1st episode" = epi_state, 
  "2nd episode" = epi_state, 
  "3rd episode" = epi_state, 
  "4th episode" = epi_state, 
  "5th episode" = epi_state,
  "chronic state" = chr_state, 
  "death" = death_state,
  transition = transition_com
)
strats_dru <- define_strategy(
  "subthreshold" = sub_state,
  "subthreshold recovery after 1st episode" = sub_state,
  "subthreshold recovery after 2nd episode" = sub_state, 
  "subthreshold recovery after 3rd episode" = sub_state,
  "subthreshold recovery after 4th episode" = sub_state,
  "1st episode" = epi_state, 
  "2nd episode" = epi_state, 
  "3rd episode" = epi_state, 
  "4th episode" = epi_state, 
  "5th episode" = epi_state,
  "chronic state" = chr_state, 
  "death" = death_state,
  transition = transition_dru
)

strats_cbt <- define_strategy(
  "subthreshold" = sub_state,
  "subthreshold recovery after 1st episode" = sub_state,
  "subthreshold recovery after 2nd episode" = sub_state, 
  "subthreshold recovery after 3rd episode" = sub_state,
  "subthreshold recovery after 4th episode" = sub_state,
  "1st episode" = epi_state, 
  "2nd episode" = epi_state, 
  "3rd episode" = epi_state, 
  "4th episode" = epi_state, 
  "5th episode" = epi_state,
  "chronic state" = chr_state, 
  "death" = death_state,
  transition = transition_cbt
)

res_mod <- run_model(
  cbt = strats_cbt,
  drug = strats_dru,
  combined = strats_com,
  parameters = parameters,
  cycles = 20,
  cost = cost,
  effect = utility,
  central_strategy = "drug",
  state_time_limit = 20
)

psa <- define_psa(
  p_chr_dth_base ~ beta(40, 1402-40), 
  p_sub_dth_base ~ beta(27, 1402-27), 
  or_recurrence ~ lognormal(1.16, 0.07),
  p_remi_drug ~ beta(65, 190-65),
  p_remi_cbt ~ beta(65, 190-65),
  p_remi_com ~ beta(79, 159-79),
  p_subrec_epi2_base ~ beta(5, 15),
  carbon_chr_cbt_base ~ lognormal(338, 50),
  carbon_chr_dru_base ~ lognormal(319, 50),
  carbon_chr_com_base ~ lognormal(373, 50),
  carbon_epi_cbt_base ~ lognormal(177, 50),
  carbon_epi_dru_base ~ lognormal(104, 50),
  carbon_epi_com_base ~ lognormal(225, 50),
  cost_chr_cbt_base ~ gamma(612, 20),
  cost_chr_dru_base ~ gamma(423, 20),
  cost_chr_com_base ~ gamma(672, 20),
  cost_epi_cbt ~ gamma(463, 20), 
  cost_epi_dru ~ gamma(324, 20), 
  cost_epi_com ~ gamma(502, 20), 
  ut_epi_base ~ logitnormal(0.55, 0.28),
  ut_sub_base ~ logitnormal(1.8, 0.19),
  ut_chr_base ~ logitnormal(1.5, 0.34),
  scc ~ gamma(90, 20),
  dru_duration ~ define_distribution(
    function(x) stats::qbinom(p = x, size = 10, prob = 0.8)
  )
)

res_psa <- run_psa(res_mod, psa, 100)

plot(res_psa) + theme_bw() + geom_hline(yintercept = 0, linetype = 2) + 
  geom_vline(xintercept = 0, linetype = 2) + 
  stat_ellipse(data = heemod:::scale.psa(res_psa) %>% filter(.strategy_names != "drug"), mapping=aes(.effect, .cost),
                                                            linetype = 2, linewidth = 1) 

res_carbon_mod <- run_model(
  cbt = strats_cbt,
  drug = strats_dru,
  combined = strats_com,
  parameters = parameters,
  cycles = 20,
  cost = cost_with_carbon,
  effect = utility,
  central_strategy = "drug",
  state_time_limit = 20
)


res_carbon_psa <- run_psa(res_carbon_mod, psa, 100)

plot(res_carbon_psa) + theme_bw() + geom_hline(yintercept = 0, linetype = 2) + 
  geom_vline(xintercept = 0, linetype = 2) + 
  stat_ellipse(data = heemod:::scale.psa(res_carbon_psa) %>% filter(.strategy_names != "drug"), mapping=aes(.effect, .cost),
               linetype = 2, linewidth = 1)


res_dsa <- define_dsa(scc, 100, 300,
             dru_duration, 4, 12,
             dru_duration_com, 1, 8
           )

dsa <- run_dsa(res_carbon_mod,res_dsa)