library(RPresence)
library(jagsUI)
library(rjags)
library(dplyr)


## Results ----
#Doubtful = 1, Dusky = 2

date = "2025-01-19" 
results_in_repro<-readRDS(paste0("./data/multi-event_repro_",date,".rds"))

library(posterior)
#subset because z params are for every individual and occasion
Sys.time()
z_est<-subset_draws(results_in_repro, c("z"))
results_z<-as.data.frame(summary(z_est))
Sys.time()

saveRDS(results_z, file = paste0("./data/multi-event_repro_z_results_",date,".rds"))

results_z<-readRDS(paste0("./data/multi-event_repro_z_results_",date,".rds"))

pod_i<-female_ch%>%dplyr::select(NAME,POD,pod_ch)%>%mutate(i = 1:n())

results_z_sum<-results_z%>%
  mutate(
    i = as.numeric(stringr::str_extract(variable,
                                        pattern = "(?<=\\[).*(?=\\,)")),
    t = as.numeric(stringr::str_extract(variable,
                                        pattern = "(?<=\\,).*(?=\\])")))%>%
  dplyr::select(i,t,median)%>%
  # mutate(pod = case_when(
  #   i > n_doubtful ~"DUSKY",
  #   TRUE ~ "DOUBTFUL"
  # ))%>%
  left_join(pod_i, by = "i")%>%
  group_by(POD,t,median)%>%
  tally()

sampling_periods_long<-ID_per_day_all%>%
  filter(IDperDay > 0)%>%
  distinct(year_season_code)%>%
  arrange(year_season_code)%>%
  mutate(occasion = 1:(n()))

results_states<-results_z_sum%>%
  left_join(sampling_periods_long, by = c("t" = "occasion"))%>%
  mutate(Repro_stage = case_when(
    median == 1 ~ "Non-breeding age",
    median == 2 ~ "Breeding age, w/o offspring",
    median == 3 ~ "Breeding age, with offspring",
    median == 4 ~ "Dead"))

dead_t<-results_states%>%
  filter(median == 4)%>%
  group_by(POD)%>%
  mutate(dead_num = n - lag(n))%>%
  mutate(dead_num = case_when(
    is.na(dead_num) ~ n,
    TRUE ~ dead_num
  ))%>%
  mutate(n = dead_num)%>%
  dplyr::select(-dead_num)

results_states_2<-results_states%>%
  filter(median != 4)%>%
  bind_rows(dead_t)%>%
  arrange(POD,t,Repro_stage)

library(ggplot2)

results_states_2$Repro_stage<-factor(results_states_2$Repro_stage, c("Dead","Non-breeding age","Breeding age, w/o offspring","Breeding age, with offspring"))
library(viridis)
viridis(2, begin = 1, end = 0.2)

fill_col <- c("Dead" = "#21908CFF", "Adult" = "#FDE725FF", "Pre-adult" = "#414487FF")

alive_repro<-ggplot()+
  geom_col(results_states_2, mapping = aes(x = as.numeric(year_season_code), y = n, fill =  Repro_stage), alpha = 0.5, color = "grey30")+
  facet_wrap(~POD, ncol = 1)+
  #geom_point(ID_per_day_all%>%dplyr::rename("pod" = "POD"), mapping = aes(x = as.numeric(year_season_code), y = n.y, shape = season), color = "red")+
  #geom_line(ID_per_day_all%>%dplyr::rename("pod" = "POD"), mapping = aes(x = as.numeric(year_season_code), y = n.y), color = "red")+
  #scale_fill_manual(values = fill_col)+
  scale_fill_viridis_d()+
  scale_x_continuous(breaks = c(2005:2024))+
  theme_bw()+
  theme(axis.text.x=element_text(angle=90, vjust=0.5),
        legend.position = "bottom")+
  xlab(expression("Dolphin year (01Sep"[y-1]~"–31Aug"[y]~")"))+
  ylab("# individuals")#+
# geom_rect(data = data.frame(POD = "DOUBTFUL"), aes(xmin = 2004.83, xmax = 2013.16, ymin = 0, ymax = 70), 
#           linewidth = 1, linetype = "dashed", color="black", fill = NA, alpha = 0.2, inherit.aes = FALSE)+
# geom_rect(data = data.frame(POD = "DUSKY"), aes(xmin = 2006.83, xmax = 2015.83, ymin = 0, ymax = 140), 
#           linewidth = 1, linetype = "dashed", color="black", fill = NA, alpha = 0.2, inherit.aes = FALSE)

alive_repro
ggsave(paste0('./figures/alive_repro_',date,'.png'), alive_repro, dpi = 300, width = 300, height = 175, units = "mm")

ggplot()+
  geom_point(results_states_2, mapping = aes(x = as.numeric(year_season_code), y = n, color =  Repro_stage))+
  geom_line(results_states_2, mapping = aes(x = as.numeric(year_season_code), y = n, color =  Repro_stage))+
  facet_wrap(~POD, ncol = 1)+
  #geom_point(ID_per_day_all%>%dplyr::rename("pod" = "POD"), mapping = aes(x = as.numeric(year_season_code), y = n.y, shape = season), color = "red")+
  #geom_line(ID_per_day_all%>%dplyr::rename("pod" = "POD"), mapping = aes(x = as.numeric(year_season_code), y = n.y), color = "red")+
  #scale_fill_manual(values = fill_col)+
  scale_fill_viridis_d()+
  scale_x_continuous(breaks = c(2005:2024))+
  theme_bw()+
  theme(axis.text.x=element_text(angle=90, vjust=0.5),
        legend.position = "bottom")+
  xlab(expression("Dolphin year (01Sep"[y-1]~"–31Aug"[y]~")"))+
  ylab("# individuals")


prop<-results_states%>%filter(median != 4)%>%
  group_by(POD, year_season_code)%>%
  mutate(prop = n/sum(n))

prop%>%filter(POD == "DOUBTFUL" & year_season_code >= 2013)%>%
  group_by(Repro_stage)%>%
  dplyr::summarise(median = median(prop), min = min(prop), max = max(prop))

prop%>%filter(POD == "DUSKY" & year_season_code >= 2015.67)%>%
  group_by(Repro_stage)%>%
  dplyr::summarise(median = median(prop), min = min(prop), max = max(prop))

prop$Repro_stage<-factor(prop$Repro_stage, c("Non-breeding age","Breeding age, w/o offspring","Breeding age, with offspring"))

ggplot()+
  geom_col(prop, mapping = aes(x = year_season_code, y = prop, fill = Repro_stage), alpha = 0.5, color = "grey30")+
  facet_wrap(~POD)+
  scale_fill_viridis_d()+
  theme_bw()+
  theme(axis.text.x=element_text(angle=90, vjust=0.5))+
  xlab(expression("Dolphin year (01Sep"[y-1]~"–31Aug"[y]~")"))+
  ylab("# individuals")
ggsave(paste0('./figures/repro_prop_',date,'.png'), dpi = 300, width = 300, height = 100, units = "mm")

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

ggplot(compare_a, aes(x = sum.y, y = sum.x, color = year_season_code))+
  geom_point()+
  xlim(c(100,130))+
  ylim(c(45,75))+
  geom_smooth(method = "glm")

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

##### not z -----
#names(results_in_age)
main_repro_params<-subset_draws(results_in_repro, c("pN.est","pB.est","pW.est","phiN.est","phiB.est","phiW.est",
                                                    "psiNB.est","psiNW.est","psiBW.est","psiWB.est",
                                                    "alpha1","alpha2","alpha3","beta1","beta2","beta3",
                                                    "gamma1","gamma2","gamma3","gamma4","sigma2"))
results_repro<-as.data.frame(summary(main_repro_params))
Sys.time()

#alpha1 capture pod, alpha2 capture sex (adults only), beta1 survival pod, beta2 survival sex (adults only), gamma pod transition
# pod = j
#traceplots
bayesplot::mcmc_trace(main_repro_params, pars = c("alpha1[1]","alpha1[2]","beta1[1]","beta1[2]","gamma[1]","gamma[2]")) #"alpha2[1]","alpha2[2]","beta2[1]","beta2[2]",
#alpha1 = capture between adults, alpha2 = pre-adults
bayesplot::mcmc_areas(main_repro_params, pars = c("alpha1[1]","alpha1[2]","alpha2[1]","alpha2[2]"),prob = 0.9)
#beta1 = survival adults, beta2 = pre-adults
bayesplot::mcmc_areas(main_repro_params, pars = c("beta1[1]","beta1[2]", "beta2[1]","beta2[2]"),prob = 0.9)
#transition between pods
bayesplot::mcmc_areas(main_repro_params, pars = c("gamma1[1]","gamma1[2]","gamma2[1]","gamma2[2]","gamma3[1]","gamma3[2]","gamma4[1]","gamma4[2]"),prob = 0.9)
# 
bayesplot::mcmc_areas(main_repro_params, pars = c("sigma2[1]","sigma2[2]","sigma2[3]"))


# results_all
min(results_repro$ess_bulk)
max(results_repro$rhat)

hist(results_repro$rhat)
results_repro%>%
  filter(grepl("sigma2", variable))

results_repro%>%
  filter(grepl("alpha", variable))
results_repro%>%
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

## surival prob # not identifiable at last occasion
occasions<-names(long_samp_ch_all)

ID_per_day_all$year_season_code<-as.character(ID_per_day_all$year_season_code)

results_phi_in_repro<-phi%>%
  mutate(calfyr_season = c(rep(names(long_samp_ch_all)[2:(n_occ)], each = 2),rep(names(long_samp_ch_all)[2:(n_occ)], each = 2),rep(names(long_samp_ch_all)[2:(n_occ)], each = 2)), # skip 2006.07
         Pod = rep(rep(c("DOUBTFUL","DUSKY"), each = 1), (n_occ-1)*3))%>%
  mutate(Season = as.factor(case_when(
    grepl(".33", calfyr_season) ~ "Summer",
    grepl(".67", calfyr_season) ~ "Winter",
    TRUE ~ "Spring")),
    Repro_stage = case_when(
      grepl("phiN",variable) == TRUE ~ "Non-breeding",
      grepl("phiB",variable) == TRUE ~ "Breeding age",
      grepl("phiW",variable) == TRUE ~ "With calf"
    ))%>%
  left_join(ID_per_day_all, by = c("calfyr_season" = "year_season_code", "Pod" = "POD","Season" = "season"))%>%
  mutate(eff = case_when(
    IDperDay != 0 ~ "effort",
    TRUE ~ "no effort"))%>%
  filter(eff != "no effort")%>%
  mutate(area = "All areas")

library(ggplot2)

ggplot()+
  geom_linerange(results_phi_in_repro, mapping = aes(x = as.numeric(calfyr_season),ymin = q5, ymax = q95, color = Repro_stage), position = position_dodge(width = 0.2), size = 1, alpha = 0.8)+
  geom_point(results_phi_in_repro, mapping = aes(x = as.numeric(calfyr_season), y = median, shape = Season, color = Repro_stage), position = position_dodge(width = 0.2), size = 3, alpha = 0.8)+
  facet_wrap(~Pod, ncol = 1)+
  theme_bw()+
  theme(legend.position = "bottom")+
  scale_x_continuous(breaks = c(2005:2024))+
  theme(axis.text.x=element_text(angle=90, vjust=0.5))+
  xlab(expression("Dolphin year (01Sep"[y-1]~"–31Aug"[y]~")"))+
  ylab(expression('Survival probability,' *phi))

ggsave(paste0('./figures/age_phi_pod_',date,'.png'), dpi = 300, width = 300, height = 175, units = "mm")

## capture prob # not identifiable at first occasion

results_p_in_repro<-p%>%
  mutate(calfyr_season = rep(rep(names(long_samp_ch_all)[3:(n_occ+1)], each = 2),3), # skip 2006.07
         Pod = rep(rep(c("DOUBTFUL","DUSKY"), each = 1), (n_occ-1)*3))%>%
  mutate(Season = as.factor(case_when(
    grepl(".33", calfyr_season) ~ "Summer",
    grepl(".67", calfyr_season) ~ "Winter",
    TRUE ~ "Spring")),
    Repro_stage = case_when(
      grepl("pN",variable) == TRUE ~ "Non-breeding",
      grepl("pB",variable) == TRUE ~ "Breeding age",
      grepl("pW",variable) == TRUE ~ "With calf"
    ))%>%
  left_join(ID_per_day_all, by = c("calfyr_season" = "year_season_code", "Pod" = "POD","Season" = "season"))%>%
  mutate(eff = case_when(
    IDperDay != 0 ~ "effort",
    TRUE ~ "no effort"))%>%
  filter(eff != "no effort")%>%
  mutate(area = "All areas")

ggplot()+
  geom_linerange(results_p_in_repro, mapping = aes(x = as.numeric(calfyr_season),ymin = q5, ymax = q95, color = Repro_stage), position = position_dodge(width = 0.2), size = 1, alpha = 0.8)+
  geom_point(results_p_in_repro, mapping = aes(x = as.numeric(calfyr_season), y = median, shape = Season, color = Repro_stage), position = position_dodge(width = 0.2), size = 3, alpha = 0.8)+
  facet_wrap(~Pod, ncol = 1)+
  theme_bw()+
  scale_x_continuous(breaks = c(2005:2024))+
  theme(axis.text.x=element_text(angle=90, vjust=0.5))+
  theme(legend.position = "bottom")+
  xlab(expression("Dolphin year (01Sep"[y-1]~"–31Aug"[y]~")"))+
  ylab(expression('Capture probability, p'))

ggsave(paste0('./figures/age_p_pod_',date,'.png'), dpi = 300, width = 300, height = 175, units = "mm")

## transition probability ----
results_psi_in_repro<-psi%>%
  mutate(calfyr_season = rep(rep(names(long_samp_ch_all)[2:(n_occ)], each = 2),4), # skip 2006.07
         Pod = rep(rep(c("DOUBTFUL","DUSKY"), each = 1), (n_occ-1)*4))%>%
  mutate(Season = as.factor(case_when(
    grepl(".33", calfyr_season) ~ "Summer",
    grepl(".67", calfyr_season) ~ "Winter",
    TRUE ~ "Spring")),
    Repro_stage = case_when(
      grepl("psiNB",variable) == TRUE ~ "Non-breeding to breeding age",
      grepl("psiNW",variable) == TRUE ~ "Non-breeding to with calf",
      grepl("psiBW",variable) == TRUE ~ "Breeding age to with calf",
      grepl("psiWB",variable) == TRUE ~ "With calf to breeding age"
    ))%>%
  left_join(ID_per_day_all, by = c("calfyr_season" = "year_season_code", "Pod" = "POD", "Season" = "season"))%>%
  mutate(eff = case_when(
    IDperDay != 0 ~ "effort",
    TRUE ~ "no effort"))%>%
  filter(eff != "no effort")%>%
  mutate(area = "All areas")

results_psi_in_repro%>%
  group_by(Pod, Repro_stage)%>%
  mutate(median_stage = median(median))%>%
  distinct(Pod, Repro_stage, median_stage)


psi_plot<-ggplot(results_psi_in_repro, aes(x = as.numeric(calfyr_season), y = median, color = Repro_stage))+
  geom_linerange(aes(ymin = q5, ymax = q95), position = position_dodge(width = 0.2), size = 1, alpha = 0.8)+
  geom_point(aes(shape = Season), position = position_dodge(width = 0.2), size = 3, alpha = 0.8)+
  facet_wrap(~Pod, ncol = 1)+
  theme_bw()+
  scale_x_continuous(breaks = c(2005:2024))+
  theme(axis.text.x=element_text(angle=90, vjust=0.5))+
  theme(legend.position = "bottom")+
  xlab(expression("Dolphin year (01Sep"[y-1]~"–31Aug"[y]~")"))+
  ylab(expression('Transition probability, ' *psi))+
  geom_rect(data = data.frame(Pod = "DOUBTFUL"), aes(xmin = 2004.83, xmax = 2013.16, ymin = 0, ymax = 1), 
            linewidth = 0.5, linetype = "dashed", color="black", fill = NA, alpha = 0.2, inherit.aes = FALSE)+
  geom_rect(data = data.frame(Pod = "DUSKY"), aes(xmin = 2006.83, xmax = 2015.83, ymin = 0, ymax = 1), 
            linewidth = 0.5, linetype = "dashed", color="black", fill = NA, alpha = 0.2, inherit.aes = FALSE)

psi_plot
ggsave(paste0('./figures/age_psi_pod_',date,'.png'), dpi = 300, width = 300, height = 175, units = "mm")
