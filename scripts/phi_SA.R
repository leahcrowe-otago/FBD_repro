
library(RPresence)
library(jagsUI)
library(rjags)
library(dplyr)

# data ----

# only data collected in Doubtful or Dusky complexes

everyone_ch_SA<-readRDS("./data/everyone_SA.RDS")
long_samp_ch_SA<-readRDS("./data/long_samp_SA.RDS") #skip 2007.67
ID_per_day_SA<-readRDS("./data/ID_per_day_SA.RDS") 

ID_ch<-everyone_ch_SA%>%arrange(POD, NAME)%>%dplyr::select(ind,NAME,POD,pod_ch,SEX,sex_ch)

# observation data
obs_it<-everyone_ch_SA%>%dplyr::select(-ind,-POD,-pod_ch,-NAME,-SEX,-sex_ch)%>%
  as.matrix()

# effort
eff_it<-long_samp_ch_SA%>%
  dplyr::select(-POD)%>%
  as.matrix()

eff_it[eff_it > 0]<-1
eff_mat<-unname(eff_it)

# occasions
occ<-names(everyone_ch_SA)[5:(ncol(everyone_ch_SA)-3)]

obs_ch_mat<-unname(obs_it)
obs_ch_mat[is.na(obs_ch_mat)]<-0
obs_ch_mat[1,]

# number of individuals 
n_ind <- nrow(obs_ch_mat) 

# number of capture occasions
n_occ <- ncol(obs_ch_mat)

# number Doubtful
doubtful_n<-everyone_ch_SA%>%filter(POD == "DOUBTFUL")%>%nrow()
doubtful_mat<-obs_ch_mat[1:doubtful_n,]

# number Dusky
dusky_mat<-obs_ch_mat[(doubtful_n+1):n_ind,]

doubtful_sum<-NULL

for (j in 1:n_occ){
  doubtful_sum[j]<-sum(doubtful_mat[j,])
  
}

dusky_sum<-NULL

for (j in 1:n_occ){
  dusky_sum[j]<-sum(dusky_mat[j,])
  
}

# first capture
get.first<- function(x) min(which(x!=0))
f<-apply(obs_ch_mat, 1, get.first)
f

# model ----
# model built in phi_model.R
source('~/git-otago/FBD_repro/scripts/phi_model.R', local = TRUE)$value

## data ----
mcmc.data<-list(
  eff = eff_mat,
  y = obs_ch_mat,
  n_ind = nrow(obs_ch_mat),
  f = f,
  doubtful_n = doubtful_n,
  n_occ = ncol(obs_ch_mat),
  pod = ID_ch$pod_ch) 

## run model ----
rjags::load.module("glm")
m1 = rjags::jags.model("pod_surv_cap.txt", data = mcmc.data, inits = mcmc.inits, n.chains = 3, n.adapt = 5000)
update(m1) # another burn in
out1 = coda.samples(model = m1, variable.names = mcmc.params, n.iter = 20000)

## draws ----
out1_df = posterior::as_draws_df(out1)
## save results ----
saveRDS(out1_df, file = paste0("./data/survival&cap_SA",Sys.Date(),".rds"))

# Load results ----
date = "2025-01-29"
results_in_SA<-readRDS(paste0("./data/survival&cap_SA",date,".rds"))

#everyone_ch_SA<-readRDS("./data/everyone_SA.RDS")
long_samp_ch_SA<-readRDS("./data/long_samp_SA.RDS") #skip 2007.67
ID_per_day_SA<-readRDS("./data/ID_per_day_SA.RDS") 

results_SA<-as.data.frame(summary(results_in_SA))
results_SA
min(results_SA$ess_bulk)
max(results_SA$rhat)

results_SA%>%
  filter(grepl("sigma", variable))%>%
  mutate(sigma2 = median^2)

results_SA%>%
  filter(grepl("alpha", variable))%>%
  mutate(sigma2 = median^2)

results_SA%>%
  filter(grepl("phi.est", variable))

results_SA%>%
  filter(grepl("epsilon", variable))

## surival prob # not identifiable at last occasion
occasions_SA<-names(long_samp_ch_SA)

ID_per_day_SA$year_season_code<-as.character(ID_per_day_SA$year_season_code)

results_phi_SA<-results_SA%>%
  filter(grepl("phi.est", variable))%>%
  mutate(calfyr_season = (rep(names(long_samp_ch_SA)[2:(n_occ)], each = 2)), # skip 2006.07
         pod = rep(rep(c("DOUBTFUL","DUSKY"), each = 1), (n_occ-1)))%>%
  mutate(Season = case_when(
    grepl(".33", calfyr_season) ~ "Summer",
    grepl(".67", calfyr_season) ~ "Winter",
    TRUE ~ "Spring"
  ))%>%
  left_join(ID_per_day_SA, by = c("calfyr_season" = "year_season_code", "pod" = "POD","Season" = "season"))%>%
  mutate(eff = case_when(
    IDperDay != 0 ~ "effort",
    TRUE ~ "no effort"))%>%
  mutate(area = "Complexes only")

summary(results_phi_SA)
  
library(ggplot2)

ggplot(results_phi_SA, aes(x = as.numeric(calfyr_season), y = median))+
  geom_errorbar(aes(ymin = q5, ymax = q95, color = eff), size = 1, alpha = 0.8)+
  geom_point(aes(shape = as.factor(Season), color = eff), size = 3, alpha = 0.8)+
  facet_wrap(~pod)+
  theme_bw()+
  theme(legend.position = "bottom")+
  scale_x_continuous(breaks = c(2005:2024))+
  theme(axis.text.x=element_text(angle=90, vjust=0.5))+
  xlab(expression('Dolphin year (01Sep_{year-1}–31Aug_{year})'))+
  ylab(expression('Survival probability,' *phi))

ggsave(paste0('./figures/SA_phi_pod_',date,'.png'), dpi = 300, width = 300, height = 175, units = "mm")

## capture prob # not identifiable at first occasion

results_p_SA<-results_SA%>%
  filter(grepl("p.est", variable))%>%
  mutate(calfyr_season = rep(names(long_samp_ch_SA)[3:(n_occ+1)], each = 2),
         pod = rep(rep(c("DOUBTFUL","DUSKY"), each = 1), (n_occ-1)))%>%
  mutate(Season = case_when(
    grepl(".33", calfyr_season) ~ "Summer",
    grepl(".67", calfyr_season) ~ "Winter",
    TRUE ~ "Spring"
  ))%>%
  left_join(ID_per_day_SA, by = c("calfyr_season" = "year_season_code", "pod" = "POD","Season" = "season"))%>%
  mutate(eff = case_when(
    IDperDay != 0 ~ "effort",
    TRUE ~ "no effort"))%>%
  mutate(area = "Complexes only")

ggplot(results_p_SA, aes(x = as.numeric(calfyr_season), y = median))+
  geom_errorbar(aes(ymin = q5, ymax = q95, color = eff), size = 1, alpha = 0.8)+
  geom_point(aes(shape = as.factor(Season), color = eff),size = 3, alpha = 0.8)+
  #geom_line(aes(linetype = pod))+
  ylim(c(0.0,1.0))+
  facet_wrap(~pod)+
  theme_bw()+
  scale_x_continuous(breaks = c(2005:2024))+
  theme(axis.text.x=element_text(angle=90, vjust=0.5))+
  theme(legend.position = "bottom")+
  xlab(expression('Dolphin year (01Sep_{year-1}–31Aug_{year})'))+
  ylab(expression('Capture probability, p'))

ggsave(paste0('./figures/SA_p_pod_',date,'.png'), dpi = 300, width = 300, height = 175, units = "mm")

library(ggridges)
ggplot(results_p_SA%>%filter(eff == "effort"), mapping = aes(fill = as.factor(Season), y = 1, x = median))+
  geom_density_ridges(alpha = 0.5)+
  facet_wrap(~pod)+
  theme_bw()

ggplot(results_p_SA, aes(x = as.factor(Season), y = median))+
  geom_violin()+
  geom_jitter(aes(color = Season))+
  facet_wrap(~pod)+
  theme_bw()

ggplot(results_p_SA%>%filter(eff != "no effort"), aes(x = as.numeric(calfyr_season), y = median))+
  geom_errorbar(aes(ymin = q5, ymax = q95, color = pod), size = 1, alpha = 0.8)+
  geom_point(aes(shape = Season, color = pod),size = 3, alpha = 0.8)+
  #geom_path(aes(color = pod),alpha = 0.8)+
  facet_wrap(~Season)+
  theme_bw()+
  scale_x_continuous(breaks = c(2005:2024))+
  theme(axis.text.x=element_text(angle=90, vjust=0.5))+
  theme(legend.position = "bottom")+
  xlab(expression("Dolphin year (01Sep"[y-1]~"–31Aug"[y]~")"))+
  ylab(expression('Capture probability, p'))

## epsilon

results_ep_SA<-results_SA%>%
  filter(grepl("epsilon", variable))%>%
  mutate(parameter = rep(rep(c("p","phi")), (n_occ-1)*2),
         pod = rep(rep(c("DOUBTFUL","DUSKY"), each = 2), (n_occ-1)))

results_ep_SA_p<-results_ep_SA%>%
  filter(parameter == "p")%>%
  mutate(calfyr_season = (rep(names(long_samp_ch_SA)[3:(n_occ+1)], each = 2)), # skip 2006.07%>%
  Season = case_when(
    grepl(".33", calfyr_season) ~ "Summer",
    grepl(".67", calfyr_season) ~ "Winter",
    TRUE ~ "Spring"
  ))%>%
  left_join(ID_per_day_SA, by = c("calfyr_season" = "year_season_code", "pod" = "POD","Season" = "season"))%>%
  mutate(eff = case_when(
    IDperDay != 0 ~ "effort",
    TRUE ~ "no effort"))%>%
  mutate(area = "Complexes only")

ggplot(results_ep_SA_p%>%filter(eff != "no effort"), aes(x = as.numeric(calfyr_season), y = median))+
  geom_errorbar(aes(ymin = q5, ymax = q95, color = pod), size = 1, alpha = 0.8)+
  geom_point(aes(shape = Season, color = pod),size = 3, alpha = 0.8)+
  #geom_path(aes(color = pod),alpha = 0.8)+
  facet_wrap(~Season)+
  theme_bw()+
  scale_x_continuous(breaks = c(2005:2024))+
  theme(axis.text.x=element_text(angle=90, vjust=0.5))+
  theme(legend.position = "bottom")+
  xlab(expression("Dolphin year (01Sep"[y-1]~"–31Aug"[y]~")"))+
  ylab(expression('Random effect on p'))

N_SA<-results_SA%>%
  filter(grepl("Doubtful_N", variable) | grepl("Dusky_N", variable))%>%
  mutate(calfyr_season = rep(rep(names(long_samp_ch_all)[2:(n_occ+1)], each = 1),2),
         Pod = c(rep("DOUBTFUL",56),rep("DUSKY",56)))%>%
  mutate(Season = case_when(
    grepl(".33", calfyr_season) ~ "Summer",
    grepl(".67", calfyr_season) ~ "Winter",
    TRUE ~ "Spring"
  ))%>%
  left_join(ID_per_day_all, by = c("calfyr_season" = "year_season_code", "Pod" = "POD","Season" = "season"))%>%
  mutate(eff = case_when(
    IDperDay != 0 ~ "effort",
    TRUE ~ "no effort"))%>%
  mutate(area = "All areas")

sumstats_n_SA<-N_SA%>%
  filter(median > 0)%>%
  group_by(Pod)%>%
  dplyr::summarise(q5_N = quantile(median, 0.05), med_N = median(median), q95_N = quantile(median, 0.95), min_census = min(n.y), med_census = median(n.y), max_census = max(n.y), min_q5 = min(q5), max_q95 = max(q95))

ggplot(N_SA)+
  geom_col(aes(x = calfyr_season, y = median), alpha = 0.5)+
  geom_errorbar(aes(ymin = q5, ymax = q95, x = calfyr_season), size = 1, alpha = 0.8)+
  geom_path(aes(x = calfyr_season, y = n.y, group = 1), color = "red")+
  geom_point(aes(x = calfyr_season, y = n.y), color = "red")+
  facet_wrap(~Pod)

ggplot(N_SA%>%filter(median > 0))+
  geom_hline(data = sumstats_n_SA, mapping = aes(yintercept = med_N, color = Pod), linetype = "dashed")+
  geom_rect(data = sumstats_n_SA, mapping = aes(ymin = q5_N, ymax = q95_N, xmin = 2005.33, xmax = 2023.67, fill = Pod), alpha = 0.2)+
  #geom_path(aes(x = as.numeric(calfyr_season), y = median, color = pod))+
  geom_point(aes(x = as.numeric(calfyr_season), y = median, color = Pod, shape = Season),size = 3, alpha = 0.8)+
  geom_errorbar(aes(ymin = q5, ymax = q95, x = as.numeric(calfyr_season), color = Pod), size = 1, alpha = 0.8)+
  geom_smooth(aes(x = as.numeric(calfyr_season), y = median, color = Pod), method = "loess", span = 0.75)+
  theme_bw()+
  xlab("Year")+
  ylab("Abundance")+
  theme(legend.position = "bottom")

