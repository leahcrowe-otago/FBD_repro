library(RPresence)
library(jagsUI)
library(rjags)
library(dplyr)
library(ggplot2)

## Results ----
#Doubtful = 1, Dusky = 2

#date = "2025-01-19" # 25k iterations
#date = "2025-01-22" # 50k but without both gammas  
#date = "2025-01-23" # 20k
#date = "2025-01-30" # 20k, N correctly
#date = "2025-01-31" # 50k, N correctly
#date = "2025-03-20" # 50k, SA
date = "2025-03-21" # 50k, SA adjusted sigmas
#date = "2025-03-23_70k_half-t13" # 70k, SA adjusted sigmas, females ordered wrong?
#date = "2025-03-24" #20k, SA fixed order of female_ch?, 3 phis
#date = "2025-03-25" #20k, SA 5 phis
date = "2025-03-25_50k5phi" #50k, SA 5 phis
date = "2025-03-28" #100k, SA 5 phis
results_in_repro<-readRDS(paste0("./data/multi-event_repro_",date,".rds"))
female_ch<-readRDS("./data/female_ch_SA.RDS")%>%ungroup()
ID_per_day_SA<-readRDS("./data/ID_per_day_SA.RDS")
long_samp_ch_SA<-readRDS("./data/long_samp_SA.RDS")
n_occ = 56
date
library(posterior)

## summary ----
results_repro<-as.data.frame(summary(results_in_repro))

saveRDS(results_repro, file = paste0("./data/summ_results_repro_",date,".rds"))
results_repro<-readRDS(paste0("./data/summ_results_repro_",date,".rds"))

#alpha1 capture pod, alpha2 capture sex (adults only), beta1 survival pod, beta2 survival sex (adults only), gamma pod transition
# pod = j, 1 = Doubtful, 2 = Dusky
#traceplots
bayesplot::mcmc_trace(results_in_repro, pars = c("alpha1[1]","alpha1[2]","beta1[1]","beta1[2]","gamma1[1]","gamma1[2]")) 
#capture: alpha1 = non-adults, alpha2 = breeding age w/o offspring, alpha3 = breeding age/status with offspring
bayesplot::mcmc_areas(results_in_repro, pars = c("alpha1[1]","alpha1[2]","alpha2[1]","alpha2[2]","alpha3[1]","alpha3[2]"),prob = 0.9)
#survival: beta1 = non-adults, beta2 = breeding age w/o offspring, alpha3 = breeding age/status with offspring
bayesplot::mcmc_areas(results_in_repro, pars = c("beta1[1]","beta1[2]", "beta2[1]","beta2[2]", "beta3[1]","beta3[2]"),prob = 0.9)
#transition: gamma1: non-adult to breeding age, gamma2: non-adult to with offspring, gamma3: breeding age no offspring to breeding age offspring, gamma4: with offspring to no offspring
bayesplot::mcmc_areas(results_in_repro, pars = c("gamma1[1]","gamma1[2]","gamma2[1]","gamma2[2]","gamma3[1]","gamma3[2]","gamma4[1]","gamma4[2]"),prob = 0.9)
# 
bayesplot::mcmc_trace(results_in_repro, pars = c("sigma2[1]","sigma2[2]","sigma2[3]","sigma2[4]","sigma2[5]"))
bayesplot::mcmc_trace(results_in_repro, pars = c("sigma2[1]","sigma2[2]","sigma2[3]"))

bayesplot::mcmc_areas(results_in_repro, pars = c("sigma2[1]","sigma2[2]","sigma2[3]"))

# results_all
min(results_repro$ess_bulk, na.rm = T)
max(results_repro$rhat, na.rm = T)

hist(results_repro$rhat)
results_repro%>%
  filter(grepl("sigma2", variable))
results_repro%>%
  filter(grepl("tau", variable))

 #capture
alpha<-results_repro%>%
  filter(grepl("alpha", variable))
#survival
beta<-results_repro%>%
  filter(grepl("beta", variable))
results_repro%>%
  filter(grepl("gamma", variable))

#phi[sex,pod,time]
phi<-results_repro%>%
  filter(grepl("phi", variable))
#p[sex,pod,time]
p<-results_repro%>%
  filter(grepl("pN.est", variable) | grepl("pB.est", variable) | grepl("pW.est", variable))

psi<-results_repro%>%
  filter(grepl("psi", variable))

N<-results_repro%>%
  filter(grepl("N", variable))

##

sampling_periods_long<-ID_per_day_SA%>%
  filter(IDperDay > 0)%>%
  distinct(year_season_code)%>%
  arrange(year_season_code)%>%
  mutate(occasion = 1:(n()))

N<-results_repro%>%
  filter(grepl("_N_", variable))%>%
  #     i = as.numeric(stringr::str_extract(variable,
  #                                         pattern = "(?<=\\[).*(?=\\,)")),
  mutate(t = as.numeric(stringr::str_extract(variable,pattern = "(?<=\\[).*(?=\\])")),
         Pod = toupper(gsub("\\_.*", "", variable)),
         Repro_stage = stringr::str_extract(variable,"(?<=N_).+(?=\\[)"))%>%
  left_join(sampling_periods_long, by = c("t" = "occasion"))%>%
  mutate(Repro_stage = case_when(
    Repro_stage == "N" ~ "Non-adult",
    Repro_stage == "A" ~ "Adult w/o offspring",
    Repro_stage == "W" ~ "Adult with offspring"))%>%
  mutate(Season = as.factor(case_when(
    grepl(".33", year_season_code) ~ "Summer",
    grepl(".67", year_season_code) ~ "Winter",
    TRUE ~ "Spring")))

occasions<-names(long_samp_ch_SA)

ID_per_day_SA$year_season_code<-as.character(ID_per_day_SA$year_season_code)

phi_p_col <- c("Female: Adult with offspring" = "#FDE725FF", "Female: Adult w/o offspring" = "#73D055FF", "Female: Non-adult" = "#414487FF", "Non-adult" = "black", "Adult" = "black")

## capture prob # not identifiable at first occasion

results_p_in_repro<-p%>%
  mutate(calfyr_season = rep(rep(names(long_samp_ch_SA)[3:(n_occ+1)], each = 2),3), # skip 2006.07
         Pod = rep(rep(c("DOUBTFUL","DUSKY"), each = 1), (n_occ-1)*3))%>%
  mutate(Season = as.factor(case_when(
    grepl(".33", calfyr_season) ~ "Summer",
    grepl(".67", calfyr_season) ~ "Winter",
    TRUE ~ "Spring")),
    Repro_stage = case_when(
      grepl("pN",variable) == TRUE ~ "Female: Non-adult",
      grepl("pB",variable) == TRUE ~ "Female: Adult w/o offspring",
      grepl("pW",variable) == TRUE ~ "Female: Adult with offspring"
    ))%>%
  left_join(ID_per_day_SA, by = c("calfyr_season" = "year_season_code", "Pod" = "POD","Season" = "season"))%>%
  mutate(eff = case_when(
    IDperDay != 0 ~ "effort",
    TRUE ~ "no effort"))%>%
  filter(eff != "no effort")%>%
  mutate(area = "All areas")%>%
  filter(!(Pod == "DUSKY" & calfyr_season == 2007))# need to manually remove first period for Dusky

results_p_in_repro%>%
  group_by(Pod, Repro_stage)%>%
  dplyr::summarise(o_median = median(median), o_q5 = median(q5), o_q95 = median(q95))

results_phi_in_repro<-phi%>%
  mutate(calfyr_season = c(rep(names(long_samp_ch_SA)[2:(n_occ)], each = 2),rep(names(long_samp_ch_SA)[2:(n_occ)], each = 2),rep(names(long_samp_ch_SA)[2:(n_occ)], each = 2)), # skip 2006.07
         Pod = rep(rep(c("DOUBTFUL","DUSKY"), each = 1), (n_occ-1)*3))%>%
  mutate(Season = as.factor(case_when(
    grepl(".33", calfyr_season) ~ "Summer",
    grepl(".67", calfyr_season) ~ "Winter",
    TRUE ~ "Spring")),
    Repro_stage = case_when(
      grepl("phiN",variable) == TRUE ~ "Female: Non-adult",
      grepl("phiB",variable) == TRUE ~ "Female: Adult w/o offspring",
      grepl("phiW",variable) == TRUE ~ "Female: Adult with offspring"
    ))%>%
  left_join(ID_per_day_SA, by = c("calfyr_season" = "year_season_code", "Pod" = "POD","Season" = "season"))%>%
  mutate(eff = case_when(
    IDperDay != 0 ~ "effort",
    TRUE ~ "no effort"))%>%
  filter(eff != "no effort")%>%
  mutate(area = "All areas")

results_phi_in_repro%>%
  group_by(Pod, Repro_stage)%>%
  dplyr::summarise(o_median = median(median), o_q5 = median(q5), o_q95 = median(q95))

## surival prob # not identifiable at last occasion
multi_p<-ggplot(results_p_in_repro)+
  geom_errorbar(aes(ymin = q5, ymax = q95, x = as.numeric(calfyr_season), color = Repro_stage), width = 0.5, linewidth = 0.5, alpha = 0.8)+
  geom_point(aes(x = as.numeric(calfyr_season), y = median, color = Repro_stage, shape = Season))+
  facet_wrap(~Pod, ncol = 2)+
  theme_bw()+
  scale_color_manual(values = phi_p_col)+
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        axis.text.x=element_text(angle=90, vjust=0.5))+
  ylab("")+
  xlab(expression('Capture probability, ' *p))+
  scale_x_continuous(breaks = c(2005:2024))

multi_phi<-ggplot(results_phi_in_repro)+
  geom_errorbar(aes(ymin = q5, ymax = q95, x = as.numeric(calfyr_season), color = Repro_stage), width = 0.5, linewidth = 0.5, alpha = 0.8)+
  geom_point(aes(x = as.numeric(calfyr_season), y = median, color = Repro_stage, shape = Season))+
  facet_wrap(~Pod, ncol = 2)+
  theme_bw()+
  scale_color_manual(values = phi_p_col)+
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        axis.text.x=element_text(angle=90, vjust=0.5))+
  ylab("")+
  xlab(expression('Survival probability, ' *phi))+
  scale_x_continuous(breaks = c(2005:2024))

multi_p_phi_SA<-ggpubr::ggarrange(multi_p,multi_phi, common.legend = T, legend = "bottom", nrow = 2, labels = "auto")

ggsave(paste0('./figures/multi_p_phi_SA',date,'.png'), multi_p_phi_SA, dpi = 300, width = 300, height = 200, units = "mm")

library(ggridges)

alpha%>%
  mutate(param = "p",
    Repro_stage = case_when(
      grepl("alpha1",variable) == TRUE ~ "Female: Non-adult",
      grepl("alpha2",variable) == TRUE ~ "Female: Adult w/o offspring",
      grepl("alpha3",variable) == TRUE ~ "Female: Adult with offspring"
), Pod = rep(rep(c("DOUBTFUL","DUSKY"), each = 1),3))
  
library(tidybayes)

comp_results_repro<-compose_data(results_in_repro)

alpha1_Doubtful<-comp_results_repro$`alpha1[1]`
alpha2_Doubtful<-comp_results_repro$`alpha2[1]`
alpha3_Doubtful<-comp_results_repro$`alpha3[1]`
alpha1_Dusky<-comp_results_repro$`alpha1[2]`
alpha2_Dusky<-comp_results_repro$`alpha2[2]`
alpha3_Dusky<-comp_results_repro$`alpha3[2]`

alpha_den<-ggplot()+
  geom_density_ridges(mapping = aes(x = alpha1_Doubtful, y = "Female: Non-adult", fill = "Doubtful", color = "Doubtful"), scale = 1, alpha = 0.4, quantile_lines = TRUE, quantiles = 2, linewidth = 1)+
  geom_density_ridges(mapping = aes(x = alpha1_Dusky, y = "Female: Non-adult", fill = "Dusky", color = "Dusky"), scale = 1, alpha = 0.4, quantile_lines = TRUE, quantiles = 2, linewidth = 1)+
  geom_density_ridges(mapping = aes(x = alpha2_Doubtful, y = "Female: Adult w/o offspring", fill = "Doubtful", color = "Doubtful"), scale = 1, alpha = 0.4, quantile_lines = TRUE, quantiles = 2, linewidth = 1)+
  geom_density_ridges(mapping = aes(x = alpha2_Dusky, y = "Female: Adult w/o offspring", fill = "Dusky", color = "Dusky"), scale = 1, alpha = 0.4, quantile_lines = TRUE, quantiles = 2, linewidth = 1)+
  geom_density_ridges(mapping = aes(x = alpha3_Doubtful, y = "Female: Adult with offspring", fill = "Doubtful", color = "Doubtful"), scale = 1, alpha = 0.4, quantile_lines = TRUE, quantiles = 2, linewidth = 1)+
  geom_density_ridges(mapping = aes(x = alpha3_Dusky, y = "Female: Adult with offspring", fill = "Dusky", color = "Dusky"), scale = 1, alpha = 0.4, quantile_lines = TRUE, quantiles = 2, linewidth = 1)+
  theme_bw()+
  theme(legend.position = "bottom",
        legend.title = element_blank())+
  xlab(expression(''*alpha))+
  ylab("")+
  xlim(c(2,12))+
  guides(color = guide_legend(override.aes = list(fill = c("#F8766D", "#00BFC4"))),
         fill = "none")

beta1_Doubtful<-comp_results_repro$`beta1[1]`
beta2_Doubtful<-comp_results_repro$`beta2[1]`
beta3_Doubtful<-comp_results_repro$`beta3[1]`
beta1_Dusky<-comp_results_repro$`beta1[2]`
beta2_Dusky<-comp_results_repro$`beta2[2]`
beta3_Dusky<-comp_results_repro$`beta3[2]`

beta_den<-ggplot()+
  geom_density_ridges(mapping = aes(x = beta1_Doubtful, y = "Female: Non-adult", fill = "Doubtful", color = "Doubtful"), scale = 1, alpha = 0.4, quantile_lines = TRUE, quantiles = 2, linewidth = 1)+
  geom_density_ridges(mapping = aes(x = beta1_Dusky, y = "Female: Non-adult", fill = "Dusky", color = "Dusky"), scale = 1, alpha = 0.4, quantile_lines = TRUE, quantiles = 2, linewidth = 1)+
  geom_density_ridges(mapping = aes(x = beta2_Doubtful, y = "Female: Adult w/o offspring", fill = "Doubtful", color = "Doubtful"), scale = 1, alpha = 0.4, quantile_lines = TRUE, quantiles = 2, linewidth = 1)+
  geom_density_ridges(mapping = aes(x = beta2_Dusky, y = "Female: Adult w/o offspring", fill = "Dusky", color = "Dusky"), scale = 1, alpha = 0.4, quantile_lines = TRUE, quantiles = 2, linewidth = 1)+
  geom_density_ridges(mapping = aes(x = beta3_Doubtful, y = "Female: Adult with offspring", fill = "Doubtful", color = "Doubtful"), scale = 1, alpha = 0.4, quantile_lines = TRUE, quantiles = 2, linewidth = 1)+
  geom_density_ridges(mapping = aes(x = beta3_Dusky, y = "Female: Adult with offspring", fill = "Dusky", color = "Dusky"), scale = 1, alpha = 0.4, quantile_lines = TRUE, quantiles = 2, linewidth = 1)+
  theme_bw()+
  theme(legend.position = "bottom",
        legend.title = element_blank())+
  xlab(expression(''*beta))+
  ylab("")+
  xlim(c(2,12))+
  guides(color = guide_legend(override.aes = list(fill = c("#F8766D", "#00BFC4"))),
         fill = "none")

ab_den_SA<-ggpubr::ggarrange(alpha_den, beta_den, ncol = 1, labels = "auto", common.legend = T, legend = "bottom")
ab_den_SA

results_repro%>%
  filter(grepl("alpha", variable))%>%
  mutate(inv_logit = 1/(1+exp(-median)),
         inv_logit2 = 1/(1+exp(-mean)))%>%
  arrange(inv_logit)

results_repro%>%
  filter(grepl("beta", variable))%>%
  mutate(inv_logit = 1/(1+exp(-median)),
         inv_logit2 = 1/(1+exp(-mean)))%>%
  arrange(inv_logit)

ggsave(paste0('./figures/ab_den_SA',date,'.png'), ab_den_SA, dpi = 300, width = 200, height = 150, units = "mm")

gamma1_Doubtful<-comp_results_repro$`gamma1[1]`
gamma2_Doubtful<-comp_results_repro$`gamma2[1]`
gamma3_Doubtful<-comp_results_repro$`gamma3[1]`
gamma4_Doubtful<-comp_results_repro$`gamma4[1]`
gamma1_Dusky<-comp_results_repro$`gamma1[2]`
gamma2_Dusky<-comp_results_repro$`gamma2[2]`
gamma3_Dusky<-comp_results_repro$`gamma3[2]`
gamma4_Dusky<-comp_results_repro$`gamma4[2]`

gamma<-ggplot()+
  geom_density_ridges(mapping = aes(x = gamma1_Doubtful, y = "Female: Non-adult to Adult w/o offspring", fill = "Doubtful", color = "Doubtful"), scale = 1, alpha = 0.4, quantile_lines = TRUE, quantiles = 2, linewidth = 1)+
  geom_density_ridges(mapping = aes(x = gamma1_Dusky, y = "Female: Non-adult to Adult w/o offspring", fill = "Dusky", color = "Dusky"), scale = 1, alpha = 0.4, quantile_lines = TRUE, quantiles = 2, linewidth = 1)+
  geom_density_ridges(mapping = aes(x = gamma2_Doubtful, y = "Female: Non-adult to Adult with offspring", fill = "Doubtful", color = "Doubtful"), scale = 1, alpha = 0.4, quantile_lines = TRUE, quantiles = 2, linewidth = 1)+
  geom_density_ridges(mapping = aes(x = gamma2_Dusky, y = "Female: Non-adult to Adult with offspring", fill = "Dusky", color = "Dusky"), scale = 1, alpha = 0.4, quantile_lines = TRUE, quantiles = 2, linewidth = 1)+
  geom_density_ridges(mapping = aes(x = gamma3_Doubtful, y = "Female: Adult w/o offspring to Adult with offspring", fill = "Doubtful", color = "Doubtful"), scale = 1, alpha = 0.4, quantile_lines = TRUE, quantiles = 2, linewidth = 1)+
  geom_density_ridges(mapping = aes(x = gamma3_Dusky, y = "Female: Adult w/o offspring to Adult with offspring", fill = "Dusky", color = "Dusky"), scale = 1, alpha = 0.4, quantile_lines = TRUE, quantiles = 2, linewidth = 1)+
  geom_density_ridges(mapping = aes(x = gamma4_Doubtful, y = "Female: Adult with offspring to Adult w/o offspring", fill = "Doubtful", color = "Doubtful"), scale = 1, alpha = 0.4, quantile_lines = TRUE, quantiles = 2, linewidth = 1)+
  geom_density_ridges(mapping = aes(x = gamma4_Dusky, y = "Female: Adult with offspring to Adult w/o offspring", fill = "Dusky", color = "Dusky"), scale = 1, alpha = 0.4, quantile_lines = TRUE, quantiles = 2, linewidth = 1)+
  theme_bw()+
  theme(legend.position = "bottom",
        legend.title = element_blank())+
  xlab(expression(''*gamma))+
  ylab("")+
  guides(color = guide_legend(override.aes = list(fill = c("#F8766D", "#00BFC4"))),
         fill = "none")

## transition probability ----
results_psi_in_repro<-psi%>%
  mutate(calfyr_season = rep(rep(names(long_samp_ch_SA)[2:(n_occ)], each = 2),4), # skip 2006.07
         Pod = rep(rep(c("DOUBTFUL","DUSKY"), each = 1), (n_occ-1)*4))%>%
  mutate(Season = as.factor(case_when(
    grepl(".33", calfyr_season) ~ "Summer",
    grepl(".67", calfyr_season) ~ "Winter",
    TRUE ~ "Spring")),
    Repro_stage = case_when(
      grepl("psiNB",variable) == TRUE ~ "Non-adult to adult w/o offspring",
      grepl("psiNW",variable) == TRUE ~ "Non-adult to adult with offspring",
      grepl("psiBW",variable) == TRUE ~ "Adult w/o offspring to adult with offspring",
      grepl("psiWB",variable) == TRUE ~ "Adult with offspring to adult w/o offspring"
    ))%>%
  left_join(ID_per_day_SA, by = c("calfyr_season" = "year_season_code", "Pod" = "POD", "Season" = "season"))%>%
  mutate(eff = case_when(
    IDperDay != 0 ~ "effort",
    TRUE ~ "no effort"))%>%
  filter(eff != "no effort")%>%
  mutate(area = "All areas")%>%
  filter(!(Pod == "DUSKY" & calfyr_season == 2007))# need to manually remove first period for Dusky

results_psi_in_repro%>%
  group_by(Pod, Repro_stage)%>%
  mutate(median_stage = median(median))%>%
  distinct(Pod, Repro_stage, median_stage)%>%
  mutate(opposite = 1 - median_stage)

unique(results_psi_in_repro$Repro_stage)

opposite_psi<-results_psi_in_repro%>%
  mutate(median = 1 - median,
         q5 = 1 - q5,
         q95 = 1 - q95)%>%
  dplyr::select(-variable,-mean,-sd,-mad,-rhat,-ess_bulk,-ess_tail)%>%
  mutate(Repro_stage = case_when(
    Repro_stage == "Non-adult to adult w/o offspring" ~ "Non-adult to non-adult",
    Repro_stage == "Non-adult to adult with offspring" ~ "Non-adult to non-adult?",
    Repro_stage == "Adult w/o offspring to adult with offspring" ~ "Adult w/o offspring to adult w/o offspring",
    Repro_stage == "Adult with offspring to adult w/o offspring" ~ "Adult with offspring to adult with offspring"
  ))

NANA2<-opposite_psi%>%
  filter(Repro_stage == "Non-adult to non-adult?")%>%
  mutate(Repro_stage = "Non-adult to non-adult",
         median2 = median,
         q5_2 = q5,
         q95_2 = q5)%>%
  dplyr::select(-median,-q5,-q95)

opposite_psi2<-opposite_psi%>%
  filter(Repro_stage != "Non-adult to non-adult?")%>%
  left_join(NANA2, by = c("calfyr_season","Pod","Season","Repro_stage","CALFYEAR","season_code","n.x","n.y","IDperDay","area","eff"))%>%
  mutate(median = case_when(
    !is.na(median2) ~ median*median2,
    TRUE ~ median))%>%
  mutate(q5 = case_when(
    !is.na(q5_2) ~ q5*q5_2,
    TRUE ~ q5))%>%
  mutate(q95 = case_when(
    !is.na(q95_2) ~ q95*q95_2,
    TRUE ~ q95))

psi_stages<-opposite_psi2%>%
   bind_rows(results_psi_in_repro)%>%
   mutate(repro_group = case_when(
     grepl("Non", Repro_stage) ~ "From non-adult",
     grepl("Adult with", Repro_stage) ~ "From adult with offspring",
     grepl("Adult w/o", Repro_stage) ~ "From adult w/o offspring"
   ))

psi_stages$repro_group<-factor(psi_stages$repro_group, c("From adult with offspring","From adult w/o offspring","From non-adult"))
fill_col <- c("From adult with offspring" = "#FDE725FF", "From adult w/o offspring" = "#73D055FF", "From non-adult" = "#414487FF")

psi_stages%>%group_by(Repro_stage, Pod)%>%dplyr::summarise(p5 = quantile(median, 0.05), p50 = median(median), p95 = quantile(median, 0.95))

ggplot(psi_stages)+
  geom_density_ridges(aes(x = median, y = Repro_stage, fill = repro_group), scale = 2, quantile_lines = TRUE, quantiles = 2, alpha = 0.5)+
  facet_wrap(~Pod, ncol = 2)+
  theme_bw()+
  scale_fill_manual(values = fill_col)+
  theme(legend.title = element_blank(),
        legend.position = "bottom")+
  ylab("")+
  xlab(expression('Transition probability, ' *psi))

ggsave(paste0('./figures/repro_transitions_SA_',date,'.png'), dpi = 300, width = 200, height = 100, units = "mm")

# ggplot(psi_stages)+
#   geom_point(aes(x = calfyr_season, y = median, color = Repro_stage))+
#   geom_errorbar(aes(ymin = q5, ymax = q95, x = calfyr_season, color = Repro_stage), width = 0.5, size = 0.8, alpha = 0.8)+
#   facet_wrap(~Pod, ncol = 1)+
#   theme_bw()+
#   scale_fill_manual(values = fill_col)+
#   theme(legend.title = element_blank(),
#         legend.position = "bottom")+
#   ylab("")+
#   xlab(expression('Transition probability, ' *psi))
# Abundance

N$Repro_stage<-factor(N$Repro_stage, c("Adult with offspring","Adult w/o offspring","Non-adult"))
library(viridis)
viridis(4, begin = 1, end = 0.2)

#fill_col <- c("Dead" = "#21908CFF", "Adult with offspring" = "#FDE725FF", "Adult age w/o offspring" = "#73D055FF", "Non-adult age" = "#414487FF")
fill_col <- c("Adult with offspring" = "#FDE725FF", "Adult w/o offspring" = "#73D055FF", "Non-adult" = "#414487FF")

N<-N%>%
  arrange(Pod,t,Repro_stage)%>%
  group_by(Pod,t)%>%
  mutate(median2 = cumsum(median))%>%
  mutate(q5_2 = median2 - (median-q5), q95_2 = median2 + (q95-median))

N$Repro_stage<-factor(N$Repro_stage, c("Non-adult","Adult w/o offspring","Adult with offspring"))

alive_repro<-ggplot(N%>%filter(median > 0), mapping = aes(x = as.numeric(year_season_code), y = median, fill = Repro_stage))+
  geom_col(alpha = 0.5, color = "grey30")+
  facet_wrap(~Pod, ncol = 1, scales = "free_y")+
  geom_errorbar(aes(ymin = q5_2, ymax = q95_2, x = as.numeric(year_season_code), color = Repro_stage), width = 0.2, size = 1, alpha = 0.8)+
  geom_errorbar(aes(ymin = q5_2, ymax = q95_2, x = as.numeric(year_season_code)), width = 0.1, alpha = 0.8)+
  #geom_point(ID_per_day_all%>%dplyr::rename("pod" = "POD"), mapping = aes(x = as.numeric(year_season_code), y = n.y, shape = season), color = "red")+
  #geom_line(ID_per_day_all%>%dplyr::rename("pod" = "POD"), mapping = aes(x = as.numeric(year_season_code), y = n.y), color = "red")+
  scale_color_manual(values = fill_col)+
  scale_fill_manual(values = fill_col)+
  scale_x_continuous(breaks = c(2005:2024))+
  theme_bw()+
  theme(axis.text.x=element_text(angle=90, vjust=0.5),
        legend.position = "bottom",
        legend.title = element_blank())+
  xlab(expression("Dolphin year (01Sep"[y-1]~"–31Aug"[y]~")"))+
  ylab("# individuals")
# geom_rect(data = data.frame(POD = "DOUBTFUL"), aes(xmin = 2004.83, xmax = 2013.16, ymin = 0, ymax = 70), 
#           linewidth = 1, linetype = "dashed", color="black", fill = NA, alpha = 0.2, inherit.aes = FALSE)+
# geom_rect(data = data.frame(POD = "DUSKY"), aes(xmin = 2006.83, xmax = 2015.83, ymin = 0, ymax = 140), 
#           linewidth = 1, linetype = "dashed", color="black", fill = NA, alpha = 0.2, inherit.aes = FALSE)

alive_repro
#ggsave(paste0('./figures/alive_repro_',date,'.png'), alive_repro, dpi = 300, width = 300, height = 175, units = "mm")

alive_trend<-ggplot(N%>%filter(median > 0), mapping = aes(x = as.numeric(year_season_code), y = median, fill = Repro_stage, color = Repro_stage))+
  geom_smooth(aes(x = as.numeric(year_season_code), y = median, color = Repro_stage), method = "loess", span = 0.2, alpha = 0.3)+
  geom_smooth(aes(x = as.numeric(year_season_code), y = median), color = "black", se = F, method = "loess", span = 0.2, alpha = 0.1, linetype = "dashed")+
  #geom_errorbar(aes(ymin = q5, ymax = q95, x = as.numeric(year_season_code), color = Repro_stage), width = 0.3, size = 1, alpha = 0.8)+
  #geom_point(size = 3, aes(shape = Season))+
  facet_wrap(~Pod, ncol = 1, scales = "free_y")+
  scale_color_manual(values = fill_col)+
  scale_fill_manual(values = fill_col)+
  scale_x_continuous(breaks = c(2005:2024))+
  theme_bw()+
  theme(axis.text.x=element_text(angle=90, vjust=0.5),
        legend.position = "bottom")+
  xlab(expression("Dolphin year (01Sep"[y-1]~"–31Aug"[y]~")"))+
  ylab("# individuals")

### prop N
prop<-N%>%
  group_by(Pod, year_season_code)%>%
  filter(Repro_stage != "Non-adult")%>%
  mutate(prop = median/sum(median))

prop_trend<-ggplot(prop%>%filter(median > 0), mapping = aes(x = as.numeric(year_season_code), y = median, fill = Repro_stage, color = Repro_stage))+
  #geom_smooth(aes(x = as.numeric(year_season_code), y = prop, color = Repro_stage), method = "loess", span = 0.2, alpha = 0.3)+
  #geom_smooth(aes(x = as.numeric(year_season_code), y = prop), color = "black", se = F, method = "loess", span = 0.2, alpha = 0.1, linetype = "dashed")+
  geom_path()+
  geom_ribbon(aes(ymin = q5, ymax = q95, x = as.numeric(year_season_code), color = Repro_stage), alpha = 0.4)+
  geom_point(size = 2, aes(shape = Season))+
  facet_wrap(~Pod, ncol = 1, scale = "free_y")+
  scale_color_manual(values = fill_col)+
  scale_fill_manual(values = fill_col)+
  scale_x_continuous(breaks = c(2005:2024))+
  theme_bw()+
  theme(axis.text.x=element_text(angle=90, vjust=0.5),
        legend.position = "bottom")+
  xlab(expression("Dolphin year (01Sep"[y-1]~"–31Aug"[y]~")"))+
  ylab("Number of individuals")+
  guides(fill = "none")+
  guides(color = "none")

prop_trend

alive_abund<-ggpubr::ggarrange(alive_repro,prop_trend, labels = "auto", common.legend = FALSE, legend = "bottom")

ggsave(paste0('./figures/alive_repro_abund_SA_ribbon_',date,'.png'), alive_abund, dpi = 300, width = 300, height = 175, units = "mm")

prop%>%filter(Pod == "DOUBTFUL")%>%
  group_by(Repro_stage)%>%
  dplyr::summarise(median = median(prop, na.rm = T), q5 = quantile(prop, 0.05, na.rm = T), q95 = quantile(prop, 0.95, na.rm = T))

prop%>%filter(Pod == "DUSKY")%>%# & year_season_code >= 2015.67)%>%
  group_by(Repro_stage)%>%
  dplyr::summarise(median = round(median(prop, na.rm = T),2), q5 = quantile(prop, 0.05, na.rm = T), q95 = quantile(prop, 0.95, na.rm = T))

prop$Repro_stage<-factor(prop$Repro_stage, c("Adult age w/o offspring","Adult with offspring"))

## sum of all adult females
results_AF<-N%>%filter(Repro_stage != "Non-adult")%>%
  filter(median > 0)%>%
  group_by(Pod, Repro_stage)%>%
  mutate(median_stage = median(median), q5 = quantile(median, 0.05), q95 = quantile(median, 0.95))%>%
  group_by(Pod, t)%>%
  mutate(sum = sum(median))%>%
  group_by(Pod)%>%
  mutate(min = min(sum), max = max(sum))%>%
  ungroup()#%>%
  #filter(!(POD == "DUSKY" & year_season_code < 2008))%>% # remove first and second occasion
  #filter(!(POD == "DOUBTFUL" & year_season_code == 2005)) # remove first occasion

results_AF%>%distinct(Pod, Repro_stage, median_stage, q5, q95)
results_AF%>%distinct(Pod, min, max)
N%>%filter(year_season_code == "2023.67")

## fecundity ########
# need to run up to row 43 in repro.ss to get "calves" variable

N_adult<-N%>%filter(Repro_stage != "Non-adult")%>%
  mutate(year = stringr::str_sub(year_season_code, 1,4))

calves_per_year<-calves%>%group_by(BIRTH_YEAR,POD)%>%tally()

fecundity<-N_adult%>%
  filter(!(Pod == "DUSKY" & year_season_code < 2008))%>% # remove first and second occasion
  filter(!(Pod == "DOUBTFUL" & year_season_code == 2005))%>%
  arrange(Pod, year_season_code)%>%
  group_by(Pod, year_season_code)%>%
  mutate(total_avail = sum(median))%>% # remove first occasion
  left_join(calves_per_year, by = c("Pod" = "POD", "year" = "BIRTH_YEAR"))%>%
  replace(is.na(.), 0)%>%
  group_by(year,Pod)%>%
  mutate(fecundity_mean = n/mean(total_avail),
         fec_min = n/min(total_avail),
         fec_max = n/max(total_avail))%>%
  group_by(Pod)%>%
  mutate(fec_mean = mean(fecundity_mean))%>%
  mutate(IBI = 1/fecundity_mean,
         IBI_mean = 1/fec_mean)

fecundity%>%distinct(Pod, fec_mean, IBI_mean)
fecundity%>%group_by(Pod)%>%dplyr::summarise(
  max_fec = max(fecundity_mean), min_fec = min(fecundity_mean),
  max_IBI = max(IBI), min_IBI = min(IBI))

# fecundity
fec_plot<-ggplot(fecundity%>%distinct(year_season_code, fecundity_mean, fec_min, fec_max,Pod,year, fec_mean))+
  geom_point(aes(x = as.numeric(year), y = fecundity_mean))+
  geom_hline(aes(yintercept = fec_mean), linetype = "dotted")+
  #geom_errorbar(aes(x = as.numeric(year), ymax = fec_min, ymin = fec_max), color = "red")+
  geom_path(aes(x = as.numeric(year), y = fecundity_mean))+
  facet_wrap(~Pod)+
  scale_x_continuous(breaks = c(2005:2024))+
  theme_bw()+
  theme(axis.text.x=element_text(angle=90, vjust=0.5),
        panel.grid.minor = element_blank())+
  ylab("Fecundity")+
  xlab("Year")

ggsave(paste0('./figures/fecundity_',date,'.png'), fec_plot, dpi = 300, width = 200, height = 100, units = "mm")

## IBI
ggplot(fecundity%>%distinct(year_season_code, IBI, IBI_mean, fecundity_mean, fec_min, fec_max,Pod,year, fec_mean))+
  geom_point(aes(x = as.numeric(year), y = IBI))+
  geom_hline(aes(yintercept = IBI_mean), linetype = "dotted")+
  #geom_errorbar(aes(x = as.numeric(year), ymax = fec_min, ymin = fec_max), color = "red")+
  geom_path(aes(x = as.numeric(year), y = IBI))+
  facet_wrap(~Pod)+
  scale_x_continuous(breaks = c(2005:2024))+
  theme_bw()+
  theme(axis.text.x=element_text(angle=90, vjust=0.5),
        panel.grid.minor = element_blank())+
  ylab("IBI")+
  xlab("Year")

fecundity%>%group_by(Pod)%>%dplyr::summarise(min = min(fecundity_mean), max = max(fecundity_mean))

fecundity%>%distinct(year, fecundity_mean, fec_min, fec_max,Pod,year, fec_mean)%>%
  filter(as.numeric(year) >= 2019)
