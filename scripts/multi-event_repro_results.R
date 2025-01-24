library(RPresence)
library(jagsUI)
library(rjags)
library(dplyr)
library(ggplot2)

## Results ----
#Doubtful = 1, Dusky = 2

date = "2025-01-19" # 25k iterations
date = "2025-01-22" # 50k but without both gammas  
date = "2025-01-23" # 20k
results_in_repro<-readRDS(paste0("./data/multi-event_repro_",date,".rds"))
female_ch<-readRDS("./data/female_ch.RDS")%>%ungroup()#%
n_occ = 56

library(posterior)
#subset because z params are for every individual and occasion
Sys.time()
z_est<-subset_draws(results_in_repro, c("z"))
results_z<-as.data.frame(summary(z_est))
Sys.time()

saveRDS(results_z, file = paste0("./data/multi-event_repro_z_results_",date,".rds"))
###

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
    median == 1 ~ "Non-adult age",
    median == 2 ~ "Adult age w/o offspring",
    median == 3 ~ "Adult with offspring",
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
  arrange(POD,t,Repro_stage)%>%
  filter(!(POD == "DUSKY" & year_season_code < 2008))%>%
  filter(!(POD == "DOUBTFUL" & year_season_code == 2005))

library(ggplot2)

results_states_2$Repro_stage<-factor(results_states_2$Repro_stage, c("Dead","Non-adult age","Adult age w/o offspring","Adult with offspring"))
library(viridis)
viridis(4, begin = 1, end = 0.2)

fill_col <- c("Dead" = "#21908CFF", "Adult with offspring" = "#FDE725FF", "Adult age w/o offspring" = "#73D055FF", "Non-adult age" = "#414487FF")

alive_repro<-ggplot()+
  geom_col(results_states_2, mapping = aes(x = as.numeric(year_season_code), y = n, fill =  Repro_stage), alpha = 0.5, color = "grey30")+
  facet_wrap(~POD, ncol = 1, scales = "free_y")+
  #geom_point(ID_per_day_all%>%dplyr::rename("pod" = "POD"), mapping = aes(x = as.numeric(year_season_code), y = n.y, shape = season), color = "red")+
  #geom_line(ID_per_day_all%>%dplyr::rename("pod" = "POD"), mapping = aes(x = as.numeric(year_season_code), y = n.y), color = "red")+
  #scale_fill_manual(values = fill_col)+
  scale_fill_manual(values = fill_col)+
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

# ggplot()+
#   geom_point(results_states_2, mapping = aes(x = as.numeric(year_season_code), y = n, color =  Repro_stage))+
#   geom_line(results_states_2, mapping = aes(x = as.numeric(year_season_code), y = n, color =  Repro_stage))+
#   facet_wrap(~POD, ncol = 1)+
#   #geom_point(ID_per_day_all%>%dplyr::rename("pod" = "POD"), mapping = aes(x = as.numeric(year_season_code), y = n.y, shape = season), color = "red")+
#   #geom_line(ID_per_day_all%>%dplyr::rename("pod" = "POD"), mapping = aes(x = as.numeric(year_season_code), y = n.y), color = "red")+
#   #scale_fill_manual(values = fill_col)+
#   scale_fill_viridis_d()+
#   scale_x_continuous(breaks = c(2005:2024))+
#   theme_bw()+
#   theme(axis.text.x=element_text(angle=90, vjust=0.5),
#         legend.position = "bottom")+
#   xlab(expression("Dolphin year (01Sep"[y-1]~"–31Aug"[y]~")"))+
#   ylab("# individuals")


prop<-results_states%>%filter(median != 4)%>%
  group_by(POD, year_season_code)%>%
  mutate(prop = n/sum(n))

prop%>%filter(POD == "DOUBTFUL" & year_season_code >= 2013)%>%
  group_by(Repro_stage)%>%
  dplyr::summarise(median = median(prop), q5 = quantile(prop, 0.05), q95 = quantile(prop, 0.95))

prop%>%filter(POD == "DUSKY" & year_season_code >= 2015.67)%>%
  group_by(Repro_stage)%>%
  dplyr::summarise(median = round(median(prop),2), q5 = quantile(prop, 0.05), q95 = quantile(prop, 0.95))

prop$Repro_stage<-factor(prop$Repro_stage, c("Non-breeding age","Breeding age, w/o offspring","Breeding age/status, with offspring"))
## fix colors
prop_female<-ggplot()+
  geom_col(prop, mapping = aes(x = year_season_code, y = prop, fill = Repro_stage), alpha = 0.5, color = "grey30")+
  facet_wrap(~POD)+
  scale_fill_manual(values = fill_col)+
  scale_x_continuous(breaks = c(2005:2024))+
  theme_bw()+
  theme(axis.text.x=element_text(angle=90, vjust=0.5),
        legend.position = "bottom")+
  xlab(expression("Dolphin year (01Sep"[y-1]~"–31Aug"[y]~")"))+
  ylab("# individuals")
#ggsave(paste0('./figures/repro_prop_',date,'.png'), dpi = 300, width = 300, height = 100, units = "mm")

results_AF<-results_states%>%filter(median != 4 & median != 1)%>%
  group_by(POD, Repro_stage)%>%
  mutate(median_stage = median(n), q5 = quantile(n, 0.05), q95 = quantile(n, 0.95))%>%
  group_by(POD, t)%>%
  mutate(sum = sum(n))%>%
  group_by(POD)%>%
  mutate(min = min(sum), max = max(sum))%>%
  ungroup()%>%
  filter(!(POD == "DUSKY" & year_season_code < 2008))%>% # remove first and second occasion
  filter(!(POD == "DOUBTFUL" & year_season_code == 2005)) # remove first occasion

results_AF%>%distinct(POD, Repro_stage, median_stage, q5, q95)
results_AF%>%distinct(POD, min, max)

adult_distr<-ggplot(results_AF)+
  geom_density(aes(x = n, fill = Repro_stage, linetype = Repro_stage), color = "grey30", alpha = 0.4)+
  geom_vline(mapping = aes(xintercept = median_stage, color = Repro_stage), linewidth = 1)+
  geom_vline(aes(xintercept = q5, color = Repro_stage), linetype = "dashed", linewidth = 1)+
  geom_vline(aes(xintercept = q95, color = Repro_stage), linetype = "dashed", linewidth = 1)+
  facet_wrap(~POD, scales = "free_x")+
  scale_fill_manual(values = fill_col)+
  scale_color_manual(values = fill_col)+
  theme_bw()+
  theme(legend.position = "bottom",
        legend.title = element_blank())


ggpubr::ggarrange(alive_repro, adult_distr, ncol = 1, labels = "auto", common.legend = TRUE, legend = "bottom", heights = c(2,1.5))

ggsave(paste0('./figures/repro_abund_prop_',date,'.png'), dpi = 300, width = 200, height = 200, units = "mm")
## compare ----

Doubtful_z<-results_states%>%
  filter(POD == "DOUBTFUL" & median != 3)%>%
  group_by(POD, year_season_code)%>%
  dplyr::summarise(sum = sum(n))%>%
  ungroup()

Dusky_z<-results_states%>%
  filter(POD == "DUSKY"& median != 3)%>%
  group_by(POD, year_season_code)%>%
  dplyr::summarise(sum = sum(n))%>%
  ungroup()

compare_a<-Doubtful_z%>%
  left_join(Dusky_z, by = c("year_season_code"))

ggplot(compare_a, aes(x = sum.y, y = sum.x, color = as.numeric(year_season_code)))+
  geom_point()+
  #xlim(c(100,130))+
  #ylim(c(45,75))+
  geom_smooth(method = "glm")

####

doubtful_dead<-dead_t%>%
  filter(POD == "DOUBTFUL")

dusky_dead<-dead_t%>%
  filter(POD == "DUSKY")

compare_d<-doubtful_dead%>%
  left_join(dusky_dead, by = "year_season_code")

ggplot(compare_d)+
  geom_point(aes(x = n.x, y = n.y, color = as.numeric(year_season_code)))

ggplot(dead_t)+
  geom_point(aes(x = year_season_code, y = n, color = POD))+
  geom_line(aes(x = year_season_code, y = n, color = POD))

##### not z -----
#names(results_in_age)
main_repro_params<-subset_draws(results_in_repro, c("pN.est","pB.est","pW.est","phiN.est","phiB.est","phiW.est",
                                                    "psiNB.est","psiNW.est","psiBW.est","psiWB.est",
                                                    "alpha1","alpha2","alpha3","beta1","beta2","beta3",
                                                    "gamma1","gamma2",
                                                    "gamma3","gamma4","sigma2"))
results_repro<-as.data.frame(summary(main_repro_params))

Sys.time()

saveRDS(results_repro, file = paste0("./data/multi-event_results_repro_",date,".rds"))

#alpha1 capture pod, alpha2 capture sex (adults only), beta1 survival pod, beta2 survival sex (adults only), gamma pod transition
# pod = j, 1 = Doubtful, 2 = Dusky
#traceplots
bayesplot::mcmc_trace(main_repro_params, pars = c("alpha1[1]","alpha1[2]","beta1[1]","beta1[2]","gamma1[1]","gamma1[2]")) 
#capture: alpha1 = non-adults, alpha2 = breeding age w/o offspring, alpha3 = breeding age/status with offspring
bayesplot::mcmc_areas(main_repro_params, pars = c("alpha1[1]","alpha1[2]","alpha2[1]","alpha2[2]","alpha3[1]","alpha3[2]"),prob = 0.9)
#survival: beta1 = non-adults, beta2 = breeding age w/o offspring, alpha3 = breeding age/status with offspring
bayesplot::mcmc_areas(main_repro_params, pars = c("beta1[1]","beta1[2]", "beta2[1]","beta2[2]", "beta3[1]","beta3[2]"),prob = 0.9)
#transition: gamma1: non-adult to breeding age, gamma2: non-adult to with offspring, gamma3: breeding age no offspring to breeding age offspring, gamma4: with offspring to no offspring
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
      grepl("phiN",variable) == TRUE ~ "Female: Non-adult age",
      grepl("phiB",variable) == TRUE ~ "Female: Adult age w/o offspring",
      grepl("phiW",variable) == TRUE ~ "Female: Adult with offspring"
    ))%>%
  left_join(ID_per_day_all, by = c("calfyr_season" = "year_season_code", "Pod" = "POD","Season" = "season"))%>%
  mutate(eff = case_when(
    IDperDay != 0 ~ "effort",
    TRUE ~ "no effort"))%>%
  filter(eff != "no effort")%>%
  mutate(area = "All areas")

phi_p_col <- c("Female: Adult with offspring" = "#FDE725FF", "Female: Adult age w/o offspring" = "#73D055FF", "Female: Non-adult age" = "#414487FF", "Non-adult" = "black", "Adult" = "black")

phi_age<-readRDS("./data/results_phi_in_age_2025-01-20.RDS")

phi_repro_box<-ggplot()+
    geom_boxplot(results_phi_in_repro, mapping = aes(x = median, y = Repro_stage, fill = Repro_stage), alpha = 0.5, linewidth = 0.2)+
  geom_boxplot(phi_age, mapping = aes(x = median, y = Ageclass, fill = Ageclass), alpha = 0.5,  linewidth = 0.2)+
  scale_fill_manual(values = phi_p_col)+
  facet_wrap(~Pod)+
  theme_bw()+
  theme(legend.position = "none")+
  xlab(expression('Survival probability,' *phi))+
  ylab("")

ggsave(paste0('./figures/phi_repro_box_',date,'.png'), phi_repro_box, dpi = 300, width = 200, height = 100, units = "mm")

# phi_repro<-ggplot()+
#   geom_linerange(results_phi_in_repro, mapping = aes(x = as.numeric(calfyr_season),ymin = q5, ymax = q95, color = Repro_stage), position = position_dodge(width = 0.2), size = 1, alpha = 0.8)+
#   geom_point(results_phi_in_repro, mapping = aes(x = as.numeric(calfyr_season), y = median, shape = Season, color = Repro_stage), position = position_dodge(width = 0.2), size = 3, alpha = 0.8)+
#   facet_wrap(~Pod, ncol = 1)+
#   theme_bw()+
#   theme(legend.position = "bottom")+
#   scale_x_continuous(breaks = c(2005:2024))+
#   theme(axis.text.x=element_text(angle=90, vjust=0.5))+
#   xlab(expression("Dolphin year (01Sep"[y-1]~"–31Aug"[y]~")"))+
#   ylab(expression('Survival probability,' *phi))

#ggsave(paste0('./figures/age_phi_pod_',date,'.png'), dpi = 300, width = 300, height = 175, units = "mm")

## capture prob # not identifiable at first occasion

results_p_in_repro<-p%>%
  mutate(calfyr_season = rep(rep(names(long_samp_ch_all)[3:(n_occ+1)], each = 2),3), # skip 2006.07
         Pod = rep(rep(c("DOUBTFUL","DUSKY"), each = 1), (n_occ-1)*3))%>%
  mutate(Season = as.factor(case_when(
    grepl(".33", calfyr_season) ~ "Summer",
    grepl(".67", calfyr_season) ~ "Winter",
    TRUE ~ "Spring")),
    Repro_stage = case_when(
      grepl("pN",variable) == TRUE ~ "Female: Non-adult age",
      grepl("pB",variable) == TRUE ~ "Female: Adult age w/o offspring",
      grepl("pW",variable) == TRUE ~ "Female: Adult with offspring"
    ))%>%
  left_join(ID_per_day_all, by = c("calfyr_season" = "year_season_code", "Pod" = "POD","Season" = "season"))%>%
  mutate(eff = case_when(
    IDperDay != 0 ~ "effort",
    TRUE ~ "no effort"))%>%
  filter(eff != "no effort")%>%
  mutate(area = "All areas")%>%
  filter(!(Pod == "DUSKY" & calfyr_season == 2007))# need to manually remove first period for Dusky

p_age<-readRDS("./data/results_p_in_age_2025-01-20.RDS")

p_repro_box<-ggplot()+
  geom_boxplot(results_p_in_repro, mapping = aes(x = median, y = Repro_stage, fill = Repro_stage), alpha = 0.5, linewidth = 0.2)+
  geom_boxplot(p_age, mapping = aes(x = median, y = Ageclass, fill = Ageclass), alpha = 0.5,  linewidth = 0.2)+
  scale_fill_manual(values = phi_p_col)+
  facet_wrap(~Pod)+
  theme_bw()+
  theme(legend.position = "none")+
  xlab(expression('Survival probability,' *phi))+
  ylab("")

p_repro_box

ggsave(paste0('./figures/p_repro_box_',date,'.png'), p_repro_box, dpi = 300, width = 200, height = 100, units = "mm")

# p_repro<-ggplot()+
#   geom_linerange(results_p_in_repro, mapping = aes(x = as.numeric(calfyr_season),ymin = q5, ymax = q95, color = Repro_stage), position = position_dodge(width = 0.2), size = 1, alpha = 0.8)+
#   geom_point(results_p_in_repro, mapping = aes(x = as.numeric(calfyr_season), y = median, shape = Season, color = Repro_stage), position = position_dodge(width = 0.2), size = 3, alpha = 0.8)+
#   facet_wrap(~Pod, ncol = 1)+
#   theme_bw()+
#   scale_x_continuous(breaks = c(2005:2024))+
#   theme(axis.text.x=element_text(angle=90, vjust=0.5))+
#   theme(legend.position = "bottom")+
#   xlab(expression("Dolphin year (01Sep"[y-1]~"–31Aug"[y]~")"))+
#   ylab(expression('Capture probability, p'))

#ggsave(paste0('./figures/age_p_pod_',date,'.png'), dpi = 300, width = 300, height = 175, units = "mm")

## transition probability ----
results_psi_in_repro<-psi%>%
  mutate(calfyr_season = rep(rep(names(long_samp_ch_all)[2:(n_occ)], each = 2),4), # skip 2006.07
         Pod = rep(rep(c("DOUBTFUL","DUSKY"), each = 1), (n_occ-1)*4))%>%
  mutate(Season = as.factor(case_when(
    grepl(".33", calfyr_season) ~ "Summer",
    grepl(".67", calfyr_season) ~ "Winter",
    TRUE ~ "Spring")),
    Repro_stage = case_when(
      grepl("psiNB",variable) == TRUE ~ "Non-adult to adult age w/o offspring",
      grepl("psiNW",variable) == TRUE ~ "Non-adult to adult with offspring",
      grepl("psiBW",variable) == TRUE ~ "Adult age w/o offspring to adult with offspring",
      grepl("psiWB",variable) == TRUE ~ "Adult with offspring to adult w/o offspring"
    ))%>%
  left_join(ID_per_day_all, by = c("calfyr_season" = "year_season_code", "Pod" = "POD", "Season" = "season"))%>%
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
    Repro_stage == "Non-adult to adult age w/o offspring" ~ "Non-adult to non-adult",
    Repro_stage == "Non-adult to adult with offspring" ~ "Non-adult to non-adult?",
    Repro_stage == "Adult age w/o offspring to adult with offspring" ~ "Adult age w/o offspring to adult age w/o offspring",
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
     grepl("Adult age", Repro_stage) ~ "From adult age w/o offspring"
   ))

psi_stages$repro_group<-factor(psi_stages$repro_group, c("From adult with offspring","From adult age w/o offspring","From non-adult"))
fill_col <- c("From adult with offspring" = "#FDE725FF", "From adult age w/o offspring" = "#73D055FF", "From non-adult" = "#414487FF")

library(ggridges)

psi_stages%>%group_by(Repro_stage, Pod)%>%dplyr::summarise(p5 = quantile(median, 0.05), p50 = median(median), p95 = quantile(median, 0.95))

ggplot(psi_stages)+
  geom_density_ridges(aes(x = median, y = Repro_stage, fill = repro_group), scale = 2, quantile_lines = TRUE, quantiles = 2, alpha = 0.5)+
  facet_wrap(~Pod)+
  theme_bw()+
  scale_fill_manual(values = fill_col)+
  theme(legend.title = element_blank(),
        legend.position = "bottom")+
  ylab("")+
  xlab(expression('Transition probability, ' *psi))

ggsave(paste0('./figures/repro_transitions_',date,'.png'), dpi = 300, width = 300, height = 100, units = "mm")

# psi_repro<-ggplot(results_psi_in_repro, aes(x = as.numeric(calfyr_season), y = median, color = Repro_stage))+
#   geom_linerange(aes(ymin = q5, ymax = q95), position = position_dodge(width = 0.2), size = 1, alpha = 0.8)+
#   geom_point(aes(shape = Season), position = position_dodge(width = 0.2), size = 3, alpha = 0.8)+
#   facet_wrap(~Pod, ncol = 1, scales = "free_y")+
#   theme_bw()+
#   scale_x_continuous(breaks = c(2005:2024))+
#   theme(axis.text.x=element_text(angle=90, vjust=0.5))+
#   theme(legend.position = "bottom")+
#   xlab(expression("Dolphin year (01Sep"[y-1]~"–31Aug"[y]~")"))+
#   ylab(expression('Transition probability, ' *psi))+
#   geom_rect(data = data.frame(Pod = "DOUBTFUL"), aes(xmin = 2004.83, xmax = 2013.16, ymin = 0, ymax = 1), 
#             linewidth = 0.5, linetype = "dashed", color="black", fill = NA, alpha = 0.2, inherit.aes = FALSE)+
#   geom_rect(data = data.frame(Pod = "DUSKY"), aes(xmin = 2006.83, xmax = 2015.83, ymin = 0, ymax = 1), 
#             linewidth = 0.5, linetype = "dashed", color="black", fill = NA, alpha = 0.2, inherit.aes = FALSE)
# 
# ab<-ggpubr::ggarrange(p_repro, phi_repro, ncol = 1, labels = "auto", common.legend = TRUE, legend = "bottom")
# ggpubr::ggarrange(ab, psi_repro, ncol = 1, labels = c("","c"), heights = c(2,1))

#ggsave(paste0('./figures/repro_params_',date,'.png'), dpi = 300, width = 200, height = 250, units = "mm")
