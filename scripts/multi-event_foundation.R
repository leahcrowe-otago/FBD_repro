
library(RPresence)
library(jagsUI)
library(rjags)
library(dplyr)

# data ----

ID_per_day_all<-readRDS("./data/ID_per_day_all.RDS") 

long_samp_ch_all<-readRDS("./data/long_samp.RDS") #skip 2007.67
eff_it<-long_samp_ch_all%>%
  dplyr::select(-POD)%>%
  as.matrix()

eff_it[eff_it > 0]<-1
eff_mat<-unname(eff_it)

everyone_ch_age<-readRDS("./data/everyone_ageclass_ch.RDS")%>%ungroup()#%>%group_by(POD)%>%slice_head(n = 40)
everyone_ch_age%>%dplyr::select(NAME,POD,pod_ch,SEX,sex_ch)

everyone_ch_age$pod_ch
obs_it<-everyone_ch_age%>%dplyr::select(-ind,-POD,-pod_ch,-NAME,-SEX,-sex_ch)%>%
  as.matrix()
obs_ch_mat<-unname(obs_it)
obs_ch_mat[is.na(obs_ch_mat)]<-0

#mydata <- obs_ch_mat

pod_ch<-everyone_ch_age$pod_ch
sex_ch<-everyone_ch_age$sex_ch

#mydata <- read.table('dipper.txt')
head(obs_ch_mat)
dim(obs_ch_mat)

# number of individuals 
n_ind <- nrow(obs_ch_mat) 

# number of capture occasions
n_occ <- ncol(obs_ch_mat)

# Compute the date of first capture for each individual:
f <- NULL
for (i in 1:n_ind){
  temp <- 1:n_occ
  f <- c(f,min(temp[obs_ch_mat[i,]>=1], na.rm = T))}

f

#NAs for non observed occasions
for (i in 1:n_ind){
for (t in 1:n_occ){
  for (j in 1:2){
  if (eff_mat[j,t] == 0 & pod_ch[i] == j){
    obs_ch_mat[i,t] <- NA

  }}}}

obs_ch_mat


## set up z values ----
#remove NA values from age data by replacing with inferred age values OR 0 (1 gets added later because 0 is not a latent state)

z_data <- obs_ch_mat

for (i in 1:n_ind){
  if(is.na(z_data[i,11]) && is.na(z_data[i,12]) && z_data[i,10] == z_data[i,13]){
    z_data[i,11] <- z_data[i,10]
    z_data[i,12] <- z_data[i,10]
    print(i)
  }} 

for (i in 1:n_ind){  
  for (j in 1:n_occ){
    if (is.na(z_data[i,j])){
      if (!is.na(z_data[i,(j-1)]) && !is.na(z_data[i,(j+1)]) && (z_data[i,(j-1)] == z_data[i,(j+1)])){
        z_data[i,j] <- z_data[i,j+1]
        print('1')
        print(c(i,j))
      } else if (!is.na(z_data[i,(j-1)]) && !is.na(z_data[i,(j+2)]) && (z_data[i,(j-1)] == z_data[i,(j+2)])){
        z_data[i,j] <- z_data[i,j+2]
        print('2')
        print(c(i,j))
      } else if (i > f[i]){
        z_data[i,j] <- 0
        print('3')
        print(f[i])
        print(c(i,j))
      }
    }
  }
}

z_data

# model -----
model <- function() {
  
  # OBSERVATIONS ----
  # 1 = non-detected
  # 2 = detected as pre adult
  # 3 = detected as adult 
  
  # STATES -----
  # 1 = pre-adult alive
  # 2 = adult alive
  # 3 = dead
  
  # PARAMETERS ----
  # phiPA  survival prob of pre-adults
  # phiA  survival prob of adults
  
  # pPA detectiondetection prob. of pre-adults 
  # pA  detection prob. of adults
  
  # psiPAA transition prob. from pre-adult to adult
  
  # piPA prob of being in initial state pre-adult
  
# constraints ----
  for (i in 1:n_ind){
    
    for (t in 1:(n_occ-1)){
      
      logit(pA[i,t]) <-  alpha1[pod[i]] + alpha2[sex[i]] + epsilon_p[pod[i],t]
      logit(phiA[i,t]) <- beta1[pod[i]] + beta2[sex[i]] + epsilon_phi[pod[i],t]
      
      logit(pPA[i,t]) <-  alpha1[pod[i]] + epsilon_p[pod[i],t]
      logit(phiPA[i,t]) <- beta1[pod[i]] + epsilon_phi[pod[i],t]

      logit(psiPAA[i,t]) <- gamma[pod[i]] + epsilon_psi[pod[i],t]
    }}
  
for (t in 1:(n_occ-1)){
  for (j in 1:2){ # pod
    for (k in 1:2){ #sex
      logit(pA.est[k,j,t]) <- alpha1[j] + alpha2[k] + epsilon_p[j,t]
      logit(phiA.est[k,j,t]) <- beta1[j] + beta2[k] + epsilon_phi[j,t]
    }
      logit(pPA.est[j,t]) <- alpha1[j] + epsilon_p[j,t]
      logit(phiPA.est[j,t]) <- beta1[j] + epsilon_phi[j,t]
      
      logit(psiPAA.est[j,t]) <- gamma[j] + epsilon_psi[j,t]
    }}
  
# PARAMETERS	----
  # probabilities for each initial state
  ps.init[1] <- piPA # prob. of being in initial state alive pre-adult
  ps.init[2] <- 1 - piPA # probs of bring initial state alive adult 
  ps.init[3] <- 0 # prob. of being in initial state dead
  
  for (i in 1:n_ind){
    
  # probabilities for each initial observation ----  
    po.init[1,i,1] <- 0
    po.init[1,i,2] <- 1
    po.init[1,i,3] <- 0
    
    po.init[2,i,1] <- 0
    po.init[2,i,2] <- 0
    po.init[2,i,3] <- 1
    
    po.init[3,i,1] <- 1
    po.init[3,i,2] <- 0
    po.init[3,i,3] <- 0
    
    for (t in 1:(n_occ-1)){
  # OBSERVATION PROCESS: prob of y(t) given z(t) [z(t),i,t,y(t)]----
  # detection
  po[1,i,t,1] <- 1 - (pPA[i,t] * eff[pod[i],t+1]) # prob of nondetection given PA
  po[1,i,t,2] <- pPA[i,t] * eff[pod[i],t+1] # prob of PA given detected as PA
  po[1,i,t,3] <- 0
  
  po[2,i,t,1] <- 1 - (pA[i,t] * eff[pod[i],t+1]) # prob of nondetection given A
  po[2,i,t,2] <- 0 # prob of A given detected as PA (is 0 now, but should add delta variable)
  po[2,i,t,3] <- pA[i,t] * eff[pod[i],t+1] # prob detected as adult given it is an adult
  
  po[3,i,t,1] <- 1 # prob of nondetection given it is dead
  po[3,i,t,2] <- 0 # prob of PA given it is dead
  po[3,i,t,3] <- 0 # prob of PA given it is dead
  
  # STATE PROCESS: prob of z(t+1) given z(t), [z(t),i,t,z(t+1)] ----
  # survival
  ps[1,i,t,1] <- phiPA[i,t] * (1 - psiPAA[i,t])
  ps[1,i,t,2] <- phiPA[i,t] * psiPAA[i,t]
  ps[1,i,t,3] <- 1 - phiPA[i,t]
  
  ps[2,i,t,1] <- 0
  ps[2,i,t,2] <- phiA[i,t]
  ps[2,i,t,3] <- 1 - phiA[i,t]
  
  ps[3,i,t,1] <- 0
  ps[3,i,t,2] <- 0
  ps[3,i,t,3] <- 1
  
  }
  }

  
  for (i in 1:n_ind){  # for each indiv
  
    # estimated probabilities of initial states are the proportions in each state at first capture occasion
    z[i,f[i]] ~ dcat(ps.init[1:3])
    y[i,f[i]] ~ dcat(po.init[z[i,f[i]],i,1:3])
    #z[i,f[i]] ~ y[i,f[i]]
    
    for (t in (f[i]+1):n_occ){  # loop over time
      
      ## STATE EQUATIONS ##
      # draw states at t given states at t-1
      z[i,t] ~ dcat(ps[z[i,t-1],i, t-1,1:3])
      
      ## OBSERVATION EQUATIONS ##
      # draw observations at t given states at t
      y[i,t] ~ dcat(po[z[i,t],i, t-1,1:3])
      
    }
    
  }
  
  # PRIORS  ----
  piPA ~ dunif(0,1)

  for (j in 1:2){
    for (t in 1:(n_occ-1)){
        epsilon_p[j,t] ~ dnorm(0, tau[1])
        epsilon_phi[j,t] ~ dnorm(0, tau[2])
        epsilon_psi[j,t] ~ dnorm(0, tau[3])
      }}
  
  for (j in 1:2){
    gamma[j] ~ dt(0,1,3)
    beta1[j] ~ dt(0,1,3)
    alpha1[j] ~ dt(0,1,3)
    beta2[j] ~ dt(0,1,3)
    alpha2[j] ~ dt(0,1,3)
  }
   
 for (u in 1:3){   
  sigma[u] = 1/sqrt(tau[u])
  tau[u] ~ dscaled.gamma(1,3)
  sigma2[u] <- pow(sigma[u], 2)
 }
}

# mcmc.data ----
mcmc.data <- list(n_ind=n_ind,
                n_occ=n_occ,
                y=as.matrix(obs_ch_mat+1),
                f=f, 
                pod = pod_ch, 
                eff = eff_mat,
                sex = sex_ch)

# Generate inits for the latent states
x.init <- z_data
for (i in 1:n_ind){
  if (f[i] == 1) next
  if (f[i] > 1) x.init[i,1:(f[i]-1)] <- NA
}

for (i in 1:n_ind){
  for (t in 1:n_occ){
    if (!is.na(x.init[i,t]) && !is.na(x.init[i,t-1]) && x.init[i,t] == 0){
      print(c(i,t)) 
      x.init[i,t] = x.init[i,t-1]
    }  else if (!is.na(x.init[i,t]) & x.init[i,t] == 0 && !is.na(x.init[i,t-2])){
      print(c(i,t)) 
      x.init[i,t] = x.init[i,t-2]
    } else if (!is.na(x.init[i,t]) & x.init[i,t] == 0 && !is.na(x.init[i,t-3])){
      print(c(i,t)) 
      x.init[i,t] = x.init[i,t-3]
    } 
  }
}


z <- as.matrix(x.init)
z

inits<-function(){list(z = z,
                      beta1 = rnorm(2, 0, 1),
                      alpha1 = rnorm(2, 0, 1),
                      beta2 = rnorm(2, 0, 1),
                      alpha2 = rnorm(2, 0, 1),
                      gamma = rnorm(2, 0, 1),
                      tau = runif(3, 1, 5)
                      #tau2 = matrix(runif(4, 1, 5), ncol = 2)
)}

# Specify the parameters to be monitored
parameters <- c("pPA.est","pA.est","phiPA.est","phiA.est","psiPAA.est","gamma","beta1","alpha1","beta2","alpha2","sigma2")
rjags::load.module("glm")
# fit the model:
R2OpenBUGS::write.model(model,con="multievent.txt") # write JAGS model code to file
m1 = rjags::jags.model("multievent.txt", data = mcmc.data, inits = inits, n.chains = 3, n.adapt = 5000) #5000
update(m1) # another burn in
out1 = coda.samples(model = m1, variable.names = parameters, n.iter = 25000) #20000
out1_df = posterior::as_draws_df(out1)
Sys.time()
saveRDS(out1_df, file = paste0("./data/multi-event_age_",Sys.Date(),".rds"))
Sys.time()

## Results ----
# F = 1, M = 2
#Doubtful = 1, Dusky = 2

date = "2024-12-24" # before adding sex variable to adults
date = "2024-12-26" # sex variable to adults, next try is time varying transition prob
date = "2024-12-28" # sex variable with time-varying psi

results_in_age<-readRDS(paste0("./data/multi-event_age_",date,".rds"))

#alpha1 capture pod, alpha2 capture sex (adults only), beta1 survival pod, beta2 survival sex (adults only), gamma pod transition
# pod = j, sex = k

#traceplots
bayesplot::mcmc_trace(results_in_age, pars = c("alpha1[1]","alpha1[2]","beta1[1]","beta1[2]","alpha2[1]","alpha2[2]","beta2[1]","beta2[2]","gamma[1]","gamma[2]"))
#sex
bayesplot::mcmc_areas(results_in_age, pars = c("alpha2[1]","alpha2[2]","beta2[1]","beta2[2]"),prob = 0.9)
#pod
bayesplot::mcmc_areas(results_in_age, pars = c("alpha1[1]","alpha1[2]","beta1[1]","beta1[2]","gamma[1]","gamma[2]"),prob = 0.9)
# 
bayesplot::mcmc_areas(results_in_age, pars = c("sigma2[1]","sigma2[2]","sigma2[3]"))
  
results_age<-as.data.frame(summary(results_in_age))
subset_draws(results_in_age, c("alpha1","alpha2"))
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
  mutate(calfyr_season = c(rep(names(long_samp_ch_all)[2:(n_occ)], each = 4),rep(names(long_samp_ch_all)[2:(n_occ)], each = 2)), # skip 2006.07
         #pod = rep(rep(c("DOUBTFUL","DUSKY"), each = 1), (n_occ-1)*2))%>%
         Sex = c(rep(rep(c("Female","Male"), each = 1), (n_occ-1)*2), rep(NA,(n_occ-1)*2)),
         Pod = c(rep(rep(c("DOUBTFUL","DUSKY"), each = 2), (n_occ-1)), rep(rep(c("DOUBTFUL","DUSKY"), each = 1), (n_occ-1))))%>%
  mutate(season = case_when(
    grepl(".33", calfyr_season) ~ "Summer",
    grepl(".67", calfyr_season) ~ "Winter",
    TRUE ~ "Spring"),
    phi = case_when(
      grepl("phiA",variable) == TRUE ~ "Adult",
      grepl("phiPA",variable) == TRUE ~ "Pre-adult"
    ))%>%
  left_join(ID_per_day_all, by = c("calfyr_season" = "year_season_code", "Pod" = "POD","season"))%>%
  mutate(eff = case_when(
    IDperDay != 0 ~ "effort",
    TRUE ~ "no effort"))%>%
  filter(eff != "no effort")%>%
  mutate(area = "All areas")

library(ggplot2)

ggplot()+
  geom_point(results_phi_in_age%>%filter(phi == "Adult"), mapping = aes(x = as.numeric(calfyr_season), y = median, shape = as.factor(season), color = Sex), size = 3, alpha = 0.8)+
  geom_point(results_phi_in_age%>%filter(phi != "Adult"), mapping = aes(x = as.numeric(calfyr_season), y = median, shape = as.factor(season)), color = "black", size = 3, alpha = 0.8)+
  geom_errorbar(results_phi_in_age%>%filter(phi == "Adult"), mapping = aes(x = as.numeric(calfyr_season),ymin = q5, ymax = q95, color = Sex), size = 1, alpha = 0.8)+
  geom_errorbar(results_phi_in_age%>%filter(phi != "Adult"), mapping = aes(x = as.numeric(calfyr_season),ymin = q5, ymax = q95), color = "black", size = 1, alpha = 0.8)+
  facet_wrap(~Pod)+
  theme_bw()+
  theme(legend.position = "bottom")+
  scale_x_continuous(breaks = c(2005:2024))+
  theme(axis.text.x=element_text(angle=90, vjust=0.5))+
  xlab(expression('Dolphin year (01Sep_{year-1}–31Aug_{year})'))+
  ylab(expression('Survival probability,' *phi))

ggsave('./figures/sex_age_phi_pod.png', dpi = 300, width = 300, height = 175, units = "mm")

## capture prob # not identifiable at first occasion

results_p_in_age<-p%>%
  mutate(calfyr_season = c(rep(names(long_samp_ch_all)[3:(n_occ+1)], each = 4),rep(names(long_samp_ch_all)[3:(n_occ+1)], each = 2)), # skip 2006.07
         Sex = c(rep(rep(c("Female","Male"), each = 1), (n_occ-1)*2), rep(NA,(n_occ-1)*2)),
         Pod = c(rep(rep(c("DOUBTFUL","DUSKY"), each = 2), (n_occ-1)), rep(rep(c("DOUBTFUL","DUSKY"), each = 1), (n_occ-1))))%>%
  mutate(Season = case_when(
    grepl(".33", calfyr_season) ~ "Summer",
    grepl(".67", calfyr_season) ~ "Winter",
    TRUE ~ "Spring"),
  p = case_when(
    grepl("pA",variable) == TRUE ~ "Adult",
    grepl("pPA",variable) == TRUE ~ "Pre-adult"
    ))%>%
  left_join(ID_per_day_all, by = c("calfyr_season" = "year_season_code", "Pod" = "POD","Season" = "season"))%>%
  mutate(eff = case_when(
    IDperDay != 0 ~ "effort",
    TRUE ~ "no effort"))%>%
  filter(eff != "no effort")%>%
  mutate(area = "All areas")

ggplot()+
  geom_point(results_p_in_age%>%filter(p == "Adult"), mapping = aes(x = as.numeric(calfyr_season), y = median, shape = as.factor(Season), color = Sex), size = 3, alpha = 0.8)+
  geom_point(results_p_in_age%>%filter(p != "Adult"), mapping = aes(x = as.numeric(calfyr_season), y = median, shape = as.factor(Season)), color = "black", size = 3, alpha = 0.8)+
  geom_errorbar(results_p_in_age%>%filter(p == "Adult"), mapping = aes(x = as.numeric(calfyr_season),ymin = q5, ymax = q95, color = Sex), size = 1, alpha = 0.8)+
  geom_errorbar(results_p_in_age%>%filter(p != "Adult"), mapping = aes(x = as.numeric(calfyr_season),ymin = q5, ymax = q95), color = "black", size = 1, alpha = 0.8)+
  facet_wrap(~Pod)+
  theme_bw()+
  scale_x_continuous(breaks = c(2005:2024))+
  theme(axis.text.x=element_text(angle=90, vjust=0.5))+
  theme(legend.position = "bottom")+
  xlab(expression('Dolphin year (01Sep_{year-1}–31Aug_{year})'))+
  ylab(expression('Capture probability, p'))

ggsave('./figures/sex_age_p_pod.png', dpi = 300, width = 300, height = 175, units = "mm")

ggplot(results_p_in_age%>%filter(p == "Adult"), aes(x = as.numeric(calfyr_season), y = median))+
  geom_errorbar(aes(ymin = q5, ymax = q95, color = Sex), size = 1, alpha = 0.8)+
  geom_point(aes(shape = as.factor(season), color = Sex),size = 3, alpha = 0.8)+
  #geom_line(aes(linetype = pod))+
  #ylim(c(0.0,1.0))+
  facet_wrap(~Pod)+
  theme_bw()+
  scale_x_continuous(breaks = c(2005:2024))+
  theme(axis.text.x=element_text(angle=90, vjust=0.5))+
  theme(legend.position = "bottom")+
  xlab(expression('Dolphin year (01Sep_{year-1}–31Aug_{year})'))+
  ylab(expression('Capture probability, p'))

ggsave('./figures/sex_adult_p_pod.png', dpi = 300, width = 300, height = 175, units = "mm")

results_psi_in_age<-psi%>%
  mutate(calfyr_season = rep(names(long_samp_ch_all)[3:(n_occ+1)], each = 2), # skip 2006.07
         #pod = rep(rep(c("DOUBTFUL","DUSKY"), each = 1), (n_occ-1)*2))%>%
         #Sex = c(rep(rep(c("Female","Male"), each = 1), (n_occ-1)*2), rep(NA,(n_occ-1)*2)),
         Pod = rep(rep(c("DOUBTFUL","DUSKY"), each = 1), (n_occ-1)))%>%
  mutate(season = case_when(
    grepl(".33", calfyr_season) ~ "Summer",
    grepl(".67", calfyr_season) ~ "Winter",
    TRUE ~ "Spring"))%>%
  left_join(ID_per_day_all, by = c("calfyr_season" = "year_season_code", "Pod" = "POD","season"))%>%
  mutate(eff = case_when(
    IDperDay != 0 ~ "effort",
    TRUE ~ "no effort"))%>%
  filter(eff != "no effort")%>%
  mutate(area = "All areas")

ggplot(results_psi_in_age, aes(x = as.numeric(calfyr_season), y = median))+
  geom_errorbar(aes(ymin = q5, ymax = q95), size = 1, alpha = 0.8)+
  geom_point(aes(shape = as.factor(season)),size = 3, alpha = 0.8)+
  #geom_line(aes(linetype = pod))+
  #ylim(c(0.0,1.0))+
  facet_wrap(~Pod)+
  theme_bw()+
  scale_x_continuous(breaks = c(2005:2024))+
  theme(axis.text.x=element_text(angle=90, vjust=0.5))+
  theme(legend.position = "bottom")+
  xlab(expression('Dolphin year (01Sep_{year-1}–31Aug_{year})'))+
  ylab(expression('Transition probability, ' *psi))
