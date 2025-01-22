library(RPresence)
library(jagsUI)
library(rjags)
library(dplyr)

## Results ----
#Doubtful = 1, Dusky = 2

date = "2025-01-11" # older z for each pod/ageclass (too much output)
date = "2025-01-18" # pod dependent, two alive states with just z output
date = "2025-01-20"
results_in_age<-readRDS(paste0("./data/multi-event_ageclass_",date,".rds"))
everyone<-readRDS(everyone, "./data/everyone.RDS")

library(posterior)
#subset because z params are for every individual and occasion
#this takes ### to run
Sys.time()
#z_est<-subset_draws(results_in_age, c("Doubtful_A","Dusky_A","Doubtful_PA","Dusky_PA"))
z_est<-subset_draws(results_in_age, c("z"))
results_z<-as.data.frame(summary(z_est))
Sys.time()

saveRDS(results_z, file = paste0("./data/multi-event_ageclass_z_results",date,".rds"))

results_z<-readRDS(paste0("./data/multi-event_ageclass_z_results",date,".rds"))

results_z_it<-results_z%>%
  mutate(
    i = as.numeric(stringr::str_extract(variable,
                                        pattern = "(?<=\\[).*(?=\\,)")),
    t = as.numeric(stringr::str_extract(variable,
                                        pattern = "(?<=\\,).*(?=\\])")))%>%
  dplyr::select(i,t,median, q5,q95)%>%  
  mutate(pod = case_when(
    i > n_doubtful ~"DUSKY",
    TRUE ~ "DOUBTFUL"
  ))%>%
  filter(!(pod == "DOUBTFUL" & t == 1))%>%
  filter(!(pod == "DUSKY" & t == 7))

results_z_N<-results_z_it%>%
  group_by(pod,t,median)%>%
  tally()%>%
  group_by(pod,t)%>%
  mutate(sum = sum(n))

results_alive_sum<-results_z_it%>%
  filter(median != 3)%>%
  group_by(pod,t,median)%>%
  tally()%>%
  group_by(pod,t)%>%
  mutate(sum = sum(n))

results_z_q5<-results_z_it%>%
  filter(median != 3)%>%
  group_by(pod,t,q5)%>%
  tally()%>%
  group_by(pod,t)%>%
  mutate(q5sum = sum(n))

results_z_q95<-results_z_it%>%
  filter(median != 3)%>%
  group_by(pod,t,q95)%>%
  tally()%>%
  group_by(pod,t)%>%
  mutate(q95sum = sum(n))

ggplot()+
  geom_line(results_z_N%>%filter(median != 3), mapping = aes(x = t, y = sum))+
  geom_line(results_z_q5, mapping = aes(x = t, y = q5sum), color = "red")+
  geom_line(results_z_q95, mapping = aes(x = t, y = q95sum), color = "green")+
  facet_wrap(~pod, scales = "free")

sampling_periods_long<-ID_per_day_all%>%
  filter(IDperDay > 0)%>%
  distinct(year_season_code)%>%
  arrange(year_season_code)%>%
  mutate(occasion = 1:(n()))

results_states<-results_z_N%>%
  left_join(sampling_periods_long, by = c("t" = "occasion"))%>%
  mutate(age = case_when(
    median == 1 ~ "Non-adult",
    median == 2 ~ "Adult",
    median == 3 ~ "Dead"))

dead_t<-results_states%>%
  filter(median == 3)%>%
  group_by(pod)%>%
  mutate(dead_num = n - lag(n))%>%
  mutate(dead_num = case_when(
    is.na(dead_num) ~ n,
    TRUE ~ dead_num
  ))%>%
  mutate(n = dead_num)%>%
  dplyr::select(-dead_num)

results_states_2<-results_states%>%
  filter(median != 3)%>%
  bind_rows(dead_t)%>%
  arrange(pod,t,age)

alive_est<-results_states_2%>%
  ungroup()%>%
  filter(age != "Dead")%>%
  group_by(pod,t)%>%
  mutate(sum = sum(n))

## doesn't include zeros
observed<-everyone%>%
  ungroup()%>%
  mutate(ageclass = case_when(
    ageclass != "A" ~ "Non-adult",
    TRUE ~ "Adult"
  ))%>%
  #distinct(POD, year_season_code, ageclass)%>%
  group_by(POD, year_season_code, ageclass)%>%
  tally()%>%
  group_by(POD, year_season_code)%>%
  mutate(sum = sum(n),
         Pod = POD)

alive_est$year_season_code<-as.numeric(alive_est$year_season_code)

est_obs<-alive_est%>%
  left_join(observed, by = c("pod" = "Pod", "year_season_code","age"= "ageclass"))%>%
  ungroup()%>%
  dplyr::select("Samnpling occasion" = year_season_code, "Pod" = pod, "Ageclass" = age, "# obs" = n.y, "# est" = n.x, "Total obs" = sum.y, "Total est" = sum.x)

n_density<-alive_est%>%distinct(pod,t,sum)%>%group_by(pod)%>%
  mutate(median_pod = median(sum),
         q5 = quantile(sum, 0.05),
         q95 = quantile(sum, 0.95))

age_n_density<-results_states_2%>%ungroup()%>%distinct(pod,age,year_season_code,n)%>%group_by(pod,age)%>%
  filter(!(pod == "DOUBTFUL" & year_season_code <= 2013))%>%
  filter(!(pod == "DUSKY" & year_season_code <= 2015.67))%>%
  mutate(median_age_pod = median(n),
         q5 = quantile(n, 0.05),
         q95 = quantile(n, 0.95))


age_n_density%>%distinct(pod,age,median_age_pod,q5,q95)
unique(n_density$median_pod)
alive_est%>%distinct(pod,t,sum)%>%group_by(pod)%>%
  dplyr::summarise(median = median(sum), q5 = quantile(sum, 0.05), q95 = quantile(sum, 0.95))

n_dens_plot<-ggplot(n_density)+
  geom_density(aes(x = sum, fill = pod), color = "grey30", alpha = 0.4)+
  geom_vline(aes(xintercept = median_pod, color = pod), linewidth = 1)+
  geom_vline(aes(xintercept = q5, color = pod), linetype = "dashed")+
  geom_vline(aes(xintercept = q95, color = pod), linetype = "dashed")+
  theme_bw()+
  theme(legend.position = "bottom")+
  xlim(c(45,135))+
  xlab("Median N/sampling occasion")

age_n_density$age<-factor(age_n_density$age, c("Dead","Adult","Non-adult"))
fill_col <- c("Dead" = "#21908CFF", "Adult" = "#FDE725FF", "Non-adult" = "#414487FF")

age_n_dens_plot<-ggplot(age_n_density%>%filter(age != "Dead"))+
  geom_density(aes(x = n, fill = age), color = "grey30", alpha = 0.4)+
  geom_vline(aes(xintercept = median_age_pod, color = age), linewidth = 1)+
  geom_vline(aes(xintercept = q5, color = age), linetype = "dotted")+
  geom_vline(aes(xintercept = q95, color = age), linetype = "dashed")+
  theme_bw()+
  facet_wrap(~pod, scales = "free_x")+
  theme(legend.position = "bottom")+
  scale_fill_manual(values = fill_col)+
  scale_color_manual(values = fill_col)+
  #scale_fill_viridis_d(begin = 1, end = 0.2)+
  #scale_color_viridis_d(begin = 1, end = 0.2)+
  xlab("Median N/sampling occasion")

age_density<-ggpubr::ggarrange(n_dens_plot, age_n_dens_plot, ncol = 2, labels = c("b","c"), widths = c(1,2))
#ggsave(paste0('./figures/n_density_',date,'.png'), age_density, dpi = 300, width = 200, height = 75, units = "mm")

observed%>%
  group_by(POD, year_season_code)%>%
  mutate(total = sum(n))%>%
  group_by(POD)%>%
  #filter(total > 50)%>% #doubtful
  #filter(total > 100)%>% #dusky
  mutate(q5 = quantile(total, 0.05), q95 = quantile(total, 0.95), median = median(total))%>%
  distinct(POD, q5, median, q95)

results_states_2$age<-factor(results_states_2$age, c("Dead","Adult","Non-adult"))

alive<-ggplot()+
  geom_col(results_states_2, mapping = aes(x = as.numeric(year_season_code), y = n, fill =  age), alpha = 0.5, color = "grey30")+
  facet_wrap(~pod, scales = "free", ncol = 1)+
  geom_point(ID_per_day_all%>%dplyr::rename("pod" = "POD"), mapping = aes(x = as.numeric(year_season_code), y = n.y, shape = season), color = "red")+
  geom_line(ID_per_day_all%>%dplyr::rename("pod" = "POD"), mapping = aes(x = as.numeric(year_season_code), y = n.y), color = "red")+
  scale_fill_manual(values = fill_col)+
  scale_x_continuous(breaks = c(2005:2024))+
  theme_bw()+
  theme(axis.text.x=element_text(angle=90, vjust=0.5),
        legend.position = "bottom")+
  xlab(expression("Dolphin year (01Sep"[y-1]~"–31Aug"[y]~")"))+
  ylab("# individuals")+
  geom_rect(data = data.frame(pod = "DOUBTFUL"), aes(xmin = 2004.83, xmax = 2013.16, ymin = 0, ymax = 70), 
            linewidth = 1.5, linetype = "dashed", color="black", fill = NA, alpha = 0.2, inherit.aes = FALSE)+
  geom_rect(data = data.frame(pod = "DUSKY"), aes(xmin = 2006.83, xmax = 2015.83, ymin = 0, ymax = 140), 
            linewidth = 1.5, linetype = "dashed", color="black", fill = NA, alpha = 0.2, inherit.aes = FALSE)

alive
ggsave(paste0('./figures/alive_',date,'.png'), alive, dpi = 300, width = 200, height = 175, units = "mm")


prop<-results_states%>%filter(median != 3)%>%
  group_by(pod, year_season_code)%>%
  mutate(prop = n/sum(n))

prop%>%filter(pod == "DOUBTFUL" & year_season_code >= 2013)%>%
  group_by(age)%>%
  dplyr::summarise(median = median(prop), min = min(prop), max = max(prop))

prop%>%filter(pod == "DUSKY" & year_season_code >= 2015.67)%>%
  group_by(age)%>%
  dplyr::summarise(median = round(median(prop),2), min = min(prop), max = max(prop))

prop_plot<-ggplot()+
  geom_col(prop, mapping = aes(x = as.numeric(year_season_code), y = prop, fill = age), alpha = 0.5, color = "grey30", linewidth = 0.5)+
  facet_wrap(~pod)+
  scale_fill_viridis_d(begin = 1, end = 0.2)+
  theme_bw()+
  theme(axis.text.x=element_text(angle=90, vjust=0.5),
        legend.position = "none")+
  xlab(expression("Dolphin year (01Sep"[y-1]~"–31Aug"[y]~")"))+
  ylab("Proportion of individuals")+
  geom_rect(data = data.frame(pod = "DOUBTFUL"), aes(xmin = 2004.83, xmax = 2013.16, ymin = 0, ymax = 1), 
            linewidth = 1.5, linetype = "dashed", color="black", fill = NA, alpha = 0.2, inherit.aes = FALSE)+
  geom_rect(data = data.frame(pod = "DUSKY"), aes(xmin = 2006.83, xmax = 2015.83, ymin = 0, ymax = 1), 
            linewidth = 1.5, linetype = "dashed", color="black", fill = NA, alpha = 0.2, inherit.aes = FALSE)

ggpubr::ggarrange(prop_plot, age_density, nrow = 2, labels = c("a",""))
ggsave(paste0('./figures/prop_density_',date,'.png'), dpi = 300, width = 200, height = 150, units = "mm")
## compare ----

Doubtful_z<-results_states%>%
  filter(pod == "DOUBTFUL" & median != 3)%>%
  group_by(pod, year_season_code)%>%
  dplyr::summarise(sum = sum(n))%>%
  ungroup()

Dusky_z<-results_states%>%
  filter(pod == "DUSKY"& median != 3)%>%
  group_by(pod, year_season_code)%>%
  dplyr::summarise(sum = sum(n))%>%
  ungroup()

compare_a<-Doubtful_z%>%
  left_join(Dusky_z, by = c("year_season_code"))

# ggplot(compare_a, aes(x = sum.y, y = sum.x, color = year_season_code))+
#   geom_point()+
#   xlim(c(100,130))+
#   ylim(c(45,75))+
#   geom_smooth(method = "glm")

####

doubtful_dead<-dead_t%>%
  filter(pod == "DOUBTFUL")

dusky_dead<-dead_t%>%
  filter(pod == "DUSKY")

compare_d<-doubtful_dead%>%
  left_join(dusky_dead, by = "year_season_code")

ggplot(compare_d)+
  geom_point(aes(x = n.x, y = n.y, color = year_season_code))

ggplot(dead_t)+
  geom_point(aes(x = year_season_code, y = n, color = pod))+
  geom_line(aes(x = year_season_code, y = n, color = pod))

#####
#names(results_in_age)
main_params<-subset_draws(results_in_age, c("pPA.est","pA.est","phiPA.est","phiA.est","psiPAA.est","alpha1","alpha2","beta1","beta2","gamma","sigma2"))
results_age<-as.data.frame(summary(main_params))
Sys.time()

#alpha1 capture pod, alpha2 capture sex (adults only), beta1 survival pod, beta2 survival sex (adults only), gamma pod transition
# pod = j
#traceplots
bayesplot::mcmc_trace(main_params, pars = c("alpha1[1]","alpha1[2]","beta1[1]","beta1[2]","gamma[1]","gamma[2]")) #"alpha2[1]","alpha2[2]","beta2[1]","beta2[2]",
#alpha1 = capture between adults, alpha2 = Non-adults
bayesplot::mcmc_areas(main_params, pars = c("alpha1[1]","alpha1[2]","alpha2[1]","alpha2[2]"),prob = 0.9)
#beta1 = survival adults, beta2 = Non-adults
bayesplot::mcmc_areas(main_params, pars = c("beta1[1]","beta1[2]", "beta2[1]","beta2[2]"),prob = 0.9)
#transition between pods
bayesplot::mcmc_areas(main_params, pars = c("gamma[1]","gamma[2]"),prob = 0.9)
# 
bayesplot::mcmc_areas(main_params, pars = c("sigma2[1]","sigma2[2]","sigma2[3]"))

# results_all
min(results_age$ess_bulk)
max(results_age$rhat)

hist(results_age$rhat)
results_age%>%
  filter(grepl("sigma2", variable))

results_age%>%
  filter(grepl("alpha", variable))
results_age%>%
  filter(grepl("beta", variable))
results_age%>%
  filter(grepl("gamma", variable))

#phi[sex,pod,time]
phi<-results_age%>%
  filter(grepl("phi", variable))
#p[sex,pod,time]
p<-results_age%>%
  filter(grepl("pPA.est", variable) | grepl("pA.est", variable))

psi<-results_age%>%
  filter(grepl("psiPAA", variable))

## surival prob # not identifiable at last occasion
occasions<-names(long_samp_ch_all)

ID_per_day_all$year_season_code<-as.character(ID_per_day_all$year_season_code)

results_phi_in_age<-phi%>%
  mutate(calfyr_season = c(rep(names(long_samp_ch_all)[2:(n_occ)], each = 2),rep(names(long_samp_ch_all)[2:(n_occ)], each = 2)), # skip 2006.07
         Pod = rep(rep(c("DOUBTFUL","DUSKY"), each = 1), (n_occ-1)*2))%>%
  mutate(Season = as.factor(case_when(
    grepl(".33", calfyr_season) ~ "Summer",
    grepl(".67", calfyr_season) ~ "Winter",
    TRUE ~ "Spring")),
    Ageclass = case_when(
      grepl("phiA",variable) == TRUE ~ "Adult",
      grepl("phiPA",variable) == TRUE ~ "Non-adult"
    ))%>%
  left_join(ID_per_day_all, by = c("calfyr_season" = "year_season_code", "Pod" = "POD","Season" = "season"))%>%
  mutate(eff = case_when(
    IDperDay != 0 ~ "effort",
    TRUE ~ "no effort"))%>%
  filter(eff != "no effort")%>%
  mutate(area = "All areas")

NAdults_phi<-results_phi_in_age%>%filter(Ageclass == "Non-adult")

Adults_phi<-results_phi_in_age%>%filter(Ageclass == "Adult")

NAdults_phi%>%
  left_join(Adults_phi, by = c("Pod", "calfyr_season"))%>%
  mutate(med_diff = median.y-median.x)%>%
  dplyr::summarise(min = min(med_diff), max = max(med_diff))

library(ggplot2)

phi_ageclass_plot<-ggplot()+
  geom_errorbar(results_phi_in_age, mapping = aes(x = as.numeric(calfyr_season),ymin = q5, ymax = q95, color = Ageclass), position = position_dodge(width = 0.2), linewidth = 0.5, alpha = 0.8)+
  geom_point(results_phi_in_age, mapping = aes(x = as.numeric(calfyr_season), y = median, shape = Season, color = Ageclass), position = position_dodge(width = 0.2), size = 2, alpha = 0.8)+
  facet_wrap(~Pod)+
  theme_bw()+
  theme(legend.position = "bottom")+
  scale_x_continuous(breaks = c(2005:2024))+
  theme(axis.text.x=element_text(angle=90, vjust=0.5))+
  xlab(expression("Dolphin year (01Sep"[y-1]~"–31Aug"[y]~")"))+
  ylab(expression('Survival probability,' *phi))

ggsave(paste0('./figures/age_phi_pod_',date,'.png'), dpi = 300, width = 300, height = 100, units = "mm")

## capture prob # not identifiable at first occasion

results_p_in_age<-p%>%
  mutate(calfyr_season = rep(rep(names(long_samp_ch_all)[3:(n_occ+1)], each = 2),2), # skip 2006.07
         Pod = rep(rep(c("DOUBTFUL","DUSKY"), each = 1), (n_occ-1)*2))%>%
  mutate(Season = as.factor(case_when(
    grepl(".33", calfyr_season) ~ "Summer",
    grepl(".67", calfyr_season) ~ "Winter",
    TRUE ~ "Spring")),
    Ageclass = case_when(
      grepl("pA",variable) == TRUE ~ "Adult",
      grepl("pPA",variable) == TRUE ~ "Non-adult"
    ))%>%
  left_join(ID_per_day_all, by = c("calfyr_season" = "year_season_code", "Pod" = "POD","Season" = "season"))%>%
  mutate(eff = case_when(
    IDperDay != 0 ~ "effort",
    TRUE ~ "no effort"))%>%
  filter(eff != "no effort")%>%
  mutate(area = "All areas")%>%
  filter(!(Pod == "DUSKY" & calfyr_season == 2007))# need to manually remove first period for Dusky

NAdults_p<-results_p_in_age%>%filter(Ageclass == "Non-adult")

Adults_p<-results_p_in_age%>%filter(Ageclass == "Adult")

NAdults_p%>%
  left_join(Adults_p, by = c("Pod", "calfyr_season"))%>%
  mutate(med_diff = median.y-median.x)%>%
  dplyr::summarise(min = min(med_diff), max = max(med_diff))

p_ageclass_plot<-ggplot()+
  geom_errorbar(results_p_in_age, mapping = aes(x = as.numeric(calfyr_season),ymin = q5, ymax = q95, color = Ageclass), position = position_dodge(width = 0.2), linewidth = 0.5, alpha = 0.8)+
  geom_point(results_p_in_age, mapping = aes(x = as.numeric(calfyr_season), y = median, shape = Season, color = Ageclass), position = position_dodge(width = 0.2), size = 2, alpha = 0.8)+
  facet_wrap(~Pod)+
  theme_bw()+
  scale_x_continuous(breaks = c(2005:2024))+
  theme(axis.text.x=element_text(angle=90, vjust=0.5))+
  theme(legend.position = "bottom")+
  xlab(expression("Dolphin year (01Sep"[y-1]~"–31Aug"[y]~")"))+
  ylab(expression('Capture probability, p'))

ggsave(paste0('./figures/age_p_pod_',date,'.png'), dpi = 300, width = 300, height = 100, units = "mm")

## transition probability ----
results_psi_in_age<-psi%>%
  mutate(calfyr_season = rep(names(long_samp_ch_all)[3:(n_occ+1)], each = 2), # skip 2006.07
         #Sex = c(rep(rep(c("Female","Male"), each = 1), (n_occ-1)*2), rep(NA,(n_occ-1)*2)),
         Pod = rep(rep(c("DOUBTFUL","DUSKY"), each = 1), (n_occ-1)))%>%
  mutate(Season = as.factor(case_when(
    grepl(".33", calfyr_season) ~ "Summer",
    grepl(".67", calfyr_season) ~ "Winter",
    TRUE ~ "Spring")))%>%
  left_join(ID_per_day_all, by = c("calfyr_season" = "year_season_code", "Pod" = "POD", "Season" = "season"))%>%
  mutate(eff = case_when(
    IDperDay != 0 ~ "effort",
    TRUE ~ "no effort"))%>%
  filter(eff != "no effort")%>%
  mutate(area = "All areas")%>%
  filter(!(Pod == "DUSKY" & calfyr_season == 2007))# need to manually remove first period for Dusky

psi_ageclasse_plot<-ggplot(results_psi_in_age, aes(x = as.numeric(calfyr_season), y = median))+
  geom_errorbar(aes(ymin = q5, ymax = q95), position = position_dodge(width = 0.2), linewidth = 0.5, alpha = 0.8)+
  geom_point(aes(shape = Season), position = position_dodge(width = 0.2), size = 2, alpha = 0.8)+
  facet_wrap(~Pod)+
  theme_bw()+
  scale_x_continuous(breaks = c(2005:2024))+
  theme(axis.text.x=element_text(angle=90, vjust=0.5))+
  theme(legend.position = "bottom")+
  xlab(expression("Dolphin year (01Sep"[y-1]~"–31Aug"[y]~")"))+
  ylab(expression('Transition probability, ' *psi))+
  geom_rect(data = data.frame(Pod = "DOUBTFUL"), aes(xmin = 2004.83, xmax = 2013.49, ymin = 0, ymax = 0.6), 
            linewidth = 0.5, linetype = "dashed", color="black", fill = NA, alpha = 0.2, inherit.aes = FALSE)+
  geom_rect(data = data.frame(Pod = "DUSKY"), aes(xmin = 2006.83, xmax = 2016.16, ymin = 0, ymax = 0.6), 
            linewidth = 0.5, linetype = "dashed", color="black", fill = NA, alpha = 0.2, inherit.aes = FALSE)

ggsave(paste0('./figures/age_psi_pod_',date,'.png'), dpi = 300, width = 300, height = 100, units = "mm")

ggpubr::ggarrange(p_ageclass_plot, phi_ageclass_plot, psi_ageclasse_plot, labels = "auto", ncol = 1, common.legend = TRUE, legend = "bottom")

ggsave(paste0('./figures/ageclass_params_',date,'.png'), dpi = 300, width = 250, height = 250, units = "mm")
