
library(RPresence)
library(jagsUI)
library(rjags)
library(dplyr)

# data ----

# all data

everyone_ch_ageclass<-readRDS("./data/everyone_ageclass_ch.RDS")
long_samp_ch_all<-readRDS("./data/long_samp.RDS") #skip 2007.67
ID_per_day_all<-readRDS("./data/ID_per_day_all.RDS") 

ID_ch<-everyone_ch_ageclass%>%arrange(POD, NAME)%>%dplyr::select(ind,NAME,POD,pod_ch,SEX,sex_ch)

obs_it<-everyone_ch_ageclass%>%ungroup()%>%dplyr::select(-ind,-POD,-pod_ch,-NAME,-SEX,-sex_ch)%>%
  as.matrix()

eff_it<-long_samp_ch_all%>%
  dplyr::select(-POD)%>%
  as.matrix()

eff_it[eff_it > 0]<-1
#eff_it[eff_it == 0]<-0.00001
eff_mat<-unname(eff_it)

occ<-names(everyone_ch_ageclass)[5:(ncol(everyone_ch_ageclass)-3)]
#scaled
occ<-names(everyone_ch_ageclass)[4:(ncol(everyone_ch_ageclass)-3)]

obs_ch_mat<-unname(obs_it)
obs_ch_mat[is.na(obs_ch_mat)]<-4
#obs_ch_mat[obs_ch_mat==2]<-1 #undo weaning info for now
obs_ch_mat[1,]

# number of individuals 
n_ind <- nrow(obs_ch_mat) 

# number of capture occasions
n_occ <- ncol(obs_ch_mat)

doubtful_n<-everyone_ch_ageclass%>%filter(POD == "DOUBTFUL")%>%
  nrow()

# number Doubtful
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
get.first<- function(x) min(which(x!=5))
f<-apply(obs_ch_mat, 1, get.first)
f

## model ----

model<-function(){
  
  # observations ----
  # 1 calf seen alive
  # 2 Pre-adult age seen alive
  # 3 Adult alive
  # 4 not seen
  ### could consider adding # 5 calf known dead
  
  # states ----
  # 1 Calf alive
  # 2 Pre-adult alive
  # 3 Adult alive
  # 4 Dead, not recovered (consider adding # 5 specific dead for recovered calves)
  
  # parameters -----
  # phiC  survival prob. of calves
  # phiPA  survival prob. of pre-adult
  # phiA  survival prob. of adults
  
  # psiCPA transition prob. from calf to pre-adult
  # psiPAA transition prob. from pre-adult to adult
  # 1 - psiCPA - psiPAA transition from unk to adult
  
  # pC  detection prob. of calves (dependent on mom)
  # pPA detection prob. of pre-adults 
  # pU  detection prob. of unknown
  # pA  detection prob. of adults

  # # piR prob of being in initial state resting
  # 
  # # dR prob. to assign resting individuals correctly
  # # dA prob. to assign active breedings correctly

# state transition and observation matrices -----

  for (i in 1:n_ind){
    for (t in 1:(n_occ-1)){
    # prob of S_t+1 give S_t, [S_t,i,t,S_t+1] ----
    # omega matrix
    ps[1,i,t,1] = phiC[pod[i],t] * (1 - psiCPA[pod[i],t])# being a calf given was a calf 
    ps[1,i,t,2] = phiC[pod[i],t] * psiCPA[pod[i],t] # being a pre-adult given was a calf 
    ps[1,i,t,3] = 0 # being an adult given was a calf
    ps[1,i,t,4] = 1-phiC[pod[i],t] # being dead given was a calf
    
    ps[2,i,t,1] = 0 # being a calf given was a pre-adult
    ps[2,i,t,2] = phiPA[pod[i],t] * (1 - psiPAA[pod[i],t])# being a pre-adult given was a pre-adult
    ps[2,i,t,3] = phiPA[pod[i],t] * psiPAA[pod[i],t] # being an adult given was a pre-adult
    ps[2,i,t,4] = 1 - phiPA[pod[i],t] # being dead given was a pre-adult
    
    ps[3,i,t,1] = 0 # being a calf given was an adult
    ps[3,i,t,2] = 0 # being a pre-adult given was an adult
    ps[3,i,t,3] = phiA[pod[i],t] # being an adult given was an adult
    ps[3,i,t,4] = 1 - phiA[pod[i],t] # being dead given was an adult
    
    ps[4,i,t,1] = 0 # being a calf given was dead
    ps[4,i,t,2] = 0 # being a pre-adult given was dead
    ps[4,i,t,3] = 0 # being an adult given was dead
    ps[4,i,t,4] = 1 # being dead given was dead
    
    # prob of O(t) given S(t) [S_t,i,t,O_t]----
    # theta matrix
    po[1,i,t,1] <- pC[pod[i],t] # detecting as alive calf given it is alive calf
    po[1,i,t,2] <- 0 # detecting as alive pre-adult given it is alive calf
    po[1,i,t,3] <- 0 # detecting as alive adult given it is alive calf
    po[1,i,t,4] <- 1 - pC[pod[i],t] # not seen

    po[2,i,t,1] <- 0 # detecting as alive calf given it is alive pre-adult 
    po[2,i,t,2] <- pPA[pod[i],t] # detecting as alive pre-adult given it is alive pre-adult
    po[2,i,t,3] <- 0 # detecting as alive adult given it is alive pre-adult / 0 for now, but could be a delta
    po[2,i,t,4] <- 1 - pPA[pod[i],t]# not seen

    po[3,i,t,1] <- 0 # detecting as alive calf given it is an adult
    po[3,i,t,2] <- 0 # detecting as alive pre-adult given it is an adult / 0 for now, but could be a delta
    po[3,i,t,3] <- pA[pod[i],t] # detecting as alive adult given it is an alive adult
    po[3,i,t,4] <- 1 - pA[pod[i],t] # not seen
    
    po[4,i,t,1] <- 0 # detecting as alive calf given it is dead
    po[4,i,t,2] <- 0 # detecting as alive pre-adult given it is dead
    po[4,i,t,3] <- 0 # detecting as alive adult given it is dead
    po[4,i,t,4] <- 1 # not detecting it because it's dead
    
  }  
  }
  
# constraints -----
  for (i in 1:n_ind){
    for (t in 1:(n_occ-1)){
      
      #survival at each state
      logit(phiC[i,t]) <- phi_mu1[pod[i]] + epsilon[1,pod[i],t]
      logit(phiPA[i,t]) <- phi_mu2[pod[i]] + epsilon[1,pod[i],t] 
      logit(phiA[i,t]) <- phi_mu3[pod[i]] + epsilon[1,pod[i],t] 
      
      logit(pC[i,t]) <-  p_mu1[pod[i]] + epsilon[2,pod[i],t]
      logit(pPA[i,t]) <-  p_mu2[pod[i]] + epsilon[2,pod[i],t]
      logit(pA[i,t]) <-  p_mu3[pod[i]] + epsilon[2,pod[i],t]
      
      #transition between states
      logit(psiCPA[i,t]) <- psi_mu1[pod[i]] + epsilon[3,pod[i],t]
      logit(psiPAA[i,t]) <- psi_mu2[pod[i]] + epsilon[3,pod[i],t]
      
      }
  }
  
for (t in 1:(n_occ-1)){
    for (j in 1:2){

      # survival for each group
      logit(phiC.est[j,t]) <- phi_mu1[j] + epsilon[1,j,t]
      logit(phiPA.est[j,t]) <- phi_mu2[j] + epsilon[1,j,t]
      logit(phiA.est[j,t]) <- phi_mu3[j] + epsilon[1,j,t]
      # capture for each group
      logit(pC.est[j,t]) <-  p_mu1[j] + epsilon[2,j,t]
      logit(pPA.est[j,t]) <-  p_mu2[j] + epsilon[2,j,t]
      logit(pA.est[j,t]) <-  p_mu3[j] + epsilon[2,j,t]
      # #transition between states      
      logit(psiCPA.est[j,t]) <- psi_mu1[j] + epsilon[3,j,t]
      logit(psiPAA.est[j,t]) <- psi_mu2[j] + epsilon[3,j,t]
      
      }

  }

  # priors ----
  for (j in 1:2){
    # survival  
      phi_mu1[j] ~ dt(0,1,3)
      phi_mu2[j] ~ dt(0,1,3)
      phi_mu3[j] ~ dt(0,1,3)
    # capture               
      p_mu1[j] ~ dt(0,1,3)
      p_mu2[j] ~ dt(0,1,3)
      p_mu3[j] ~ dt(0,1,3)
    # transition
      psi_mu1[j] ~ dt(0,1,3)
      psi_mu2[j] ~ dt(0,1,3)

    }
    # # fixed effects
    # for (j in 1:2){
    #   beta2[j] ~ dt(0,1,3)
    #   alpha2[j] ~ dt(0,1,3)
    #   gamma2[j] ~ dt(0,1,3)
    # }
    
    # random effects
    for (t in 1:(n_occ-1)){
      for (j in 1:2){
      epsilon[1,j,t] ~ dnorm(0, tau[1])
      epsilon[2,j,t] ~ dnorm(0, tau[2])
      epsilon[3,j,t] ~ dnorm(0, tau[3])
    }}

      sigma[1] = 1/sqrt(tau[1])
      sigma[2] = 1/sqrt(tau[2])
      sigma[3] = 1/sqrt(tau[3])
      tau[1] ~ dscaled.gamma(1,3)
      tau[2] ~ dscaled.gamma(1,3)
      tau[3] ~ dscaled.gamma(1,3)
      sigma2[1] <- pow(sigma[1], 2)
      sigma2[2] <- pow(sigma[2], 2)
      sigma2[3] <- pow(sigma[3], 2)

    
# likelihood ----
  for (i in 1:n_ind){
    # define latent state at first capture
    z[i,f[i]] <- y[i,f[i]]
    
    for (t in (f[i]+1):n_occ){
      # state process
      z[i,t] ~ dcat(ps[z[i,t-1], i, t-1,]) 
      #dbern(phi[i,t-1] * z[i,t-1])
      
      # observation process
      y[i,t] ~ dcat(po[z[i,t], i, t-1,])
      #dbern(p[i,t-1] * eff[pod[i],t] * z[i,t])
    }
  }
  
} 

## data ----
mcmc.data<-list(
  #eff = eff_mat,
  y = obs_ch_mat,
  n_ind = nrow(obs_ch_mat),
  f = f,
  n_occ = ncol(obs_ch_mat),
  pod = ID_ch$pod_ch) 

mcmc.params<-c("phi.est","p.est","psi.est","sigma") #"beta2","alpha2","gamma2",

z_inits<-matrix(c(rep(rep(3,n_ind),n_occ/2),rep(rep(3,n_ind),ceiling(n_occ/2))),ncol = n_occ)

z_inits = obs_ch_mat

rand<-sample(c(3, 4), n_ind*n_occ, prob=c(.8, .2), replace=TRUE)
z_inits<-matrix(data = rand, nrow=n_ind,ncol=n_occ)
for (i in 1:nrow(z_inits)){z_inits[i,1:f[i]] <- NA}

mcmc.inits<-function(){list(z = z_inits,
                            #beta1 = runif(2, 0, 1),
                            #alpha1 = runif(2, 0, 1),
                            #beta1 = matrix(rep(runif(2*(n_occ-1), 0, 1)), ncol =n_occ-1),
                            #beta2 = runif(2, 0, 1),
                            #alpha2 = runif(2, 0, 1),
                            #gamma2 = runif(2, 0, 1),
                            #phi_mu = matrix(rep(runif(3, 0, 1),2), ncol = 2),
                            #p_mu = matrix(rep(runif(3, 0, 1),2), ncol = 2),
                            phi_mu1 = rnorm(2, 0, 1),
                            phi_mu2 = rnorm(2, 0, 1),
                            phi_mu3 = rnorm(2, 0, 1),
                            p_mu1 = rnorm(2, 0, 1),
                            p_mu2 = rnorm(2, 0, 1),
                            p_mu3 = rnorm(2, 0, 1),
                            psi_mu1 = rnorm(2, 0, 1),
                            psi_mu2 = rnorm(2, 0, 1),
                            tau = runif(3, 1, 5)
                            )}

## run model ----
R2OpenBUGS::write.model(model,con="F_repro_model.txt") # write JAGS model code to file
rjags::load.module("glm")
m1 = rjags::jags.model("F_repro_model.txt", data = mcmc.data, inits = mcmc.inits, n.chains = 1, n.adapt = 1000)
m1 = rjags::jags.model("F_repro_model.txt", data = mcmc.data, inits = mcmc.inits, n.chains = 3, n.adapt = 5000)
update(m1) # another burn in
out1 = coda.samples(model = m1, variable.names = mcmc.params, n.iter = 20000)
out1_df = posterior::as_draws_df(out1)

saveRDS(out1_df, file = paste0("./data/survival&cap_ageclass",Sys.Date(),".rds"))

## Results ----
date = "2024-12-19"
results_in_ageclass<-readRDS(paste0("./data/survival&cap_ageclass",date,".rds"))

results_ageclass<-as.data.frame(summary(results_in_ageclass))
results_ageclass
min(results_ageclass$ess_bulk)

results_ageclass%>%
  filter(grepl("sigma", variable))%>%
  mutate(sigma2 = median^2)

results_ageclass%>%
  filter(grepl("phi.est", variable))

## surival prob # not identifiable at last occasion
occasions<-names(long_samp_ch_all)

ID_per_day_all$year_season_code<-as.character(ID_per_day_all$year_season_code)

results_phi_ageclass<- results_ageclass%>%
  filter(grepl("phi.est", variable))%>%
  mutate(calfyr_season = (rep(names(long_samp_ch_all)[2:(n_occ)], each = 2)), # skip 2006.07
         pod = rep(rep(c("DOUBTFUL","DUSKY"), each = 1), (n_occ-1)))%>%
  mutate(season = case_when(
    grepl(".33", calfyr_season) ~ "Summer",
    grepl(".67", calfyr_season) ~ "Winter",
    TRUE ~ "Spring"
  ))%>%
  left_join(ID_per_day_all, by = c("calfyr_season" = "year_season_code", "pod" = "POD","season"))%>%
  mutate(eff = case_when(
    IDperDay != 0 ~ "effort",
    TRUE ~ "no effort"))%>%
  mutate(area = "All areas")
  
library(ggplot2)

ggplot(results_phi_ageclass, aes(x = as.numeric(calfyr_season), y = median))+
  geom_errorbar(aes(ymin = q5, ymax = q95, color = eff), size = 1, alpha = 0.8)+
  geom_point(aes(shape = as.factor(season), color = eff), size = 3, alpha = 0.8)+
  facet_wrap(~pod)+
  theme_bw()+
  theme(legend.position = "bottom")+
  scale_x_continuous(breaks = c(2005:2024))+
  theme(axis.text.x=element_text(angle=90, vjust=0.5))+
  xlab(expression('Dolphin year (01Sep_{year-1}–31Aug_{year})'))+
  ylab(expression('Survival probability,' *phi))

ggsave('./figures/all_phi_pod.png', dpi = 300, width = 300, height = 175, units = "mm")

## capture prob # not identifiable at first occasion

results_p_ageclass<-results_ageclass%>%
  filter(grepl("p.est", variable))%>%
  mutate(calfyr_season = rep(names(long_samp_ch_all)[3:(n_occ+1)], each = 2),
         pod = rep(rep(c("DOUBTFUL","DUSKY"), each = 1), (n_occ-1)))%>%
  mutate(season = case_when(
    grepl(".33", calfyr_season) ~ "Summer",
    grepl(".67", calfyr_season) ~ "Winter",
    TRUE ~ "Spring"
  ))%>%
  left_join(ID_per_day_all, by = c("calfyr_season" = "year_season_code", "pod" = "POD","season"))%>%
  mutate(eff = case_when(
    IDperDay != 0 ~ "effort",
    TRUE ~ "no effort"))%>%
  mutate(area = "All areas")

ggplot(results_p_ageclass, aes(x = as.numeric(calfyr_season), y = median))+
  geom_errorbar(aes(ymin = q5, ymax = q95, color = eff), size = 1, alpha = 0.8)+
  geom_point(aes(shape = as.factor(season), color = eff),size = 3, alpha = 0.8)+
  #geom_line(aes(linetype = pod))+
  ylim(c(0.0,1.0))+
  facet_wrap(~pod)+
  theme_bw()+
  scale_x_continuous(breaks = c(2005:2024))+
  theme(axis.text.x=element_text(angle=90, vjust=0.5))+
  theme(legend.position = "bottom")+
  xlab(expression('Dolphin year (01Sep_{year-1}–31Aug_{year})'))+
  ylab(expression('Capture probability, p'))

ggsave('./figures/all_p_pod.png', dpi = 300, width = 300, height = 175, units = "mm")

