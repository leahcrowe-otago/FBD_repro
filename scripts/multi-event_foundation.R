
library(RPresence)
library(jagsUI)
library(rjags)
library(dplyr)

# Read in the data: 

long_samp_ch_all<-readRDS("./data/long_samp.RDS") #skip 2007.67
eff_it<-long_samp_ch_all%>%
  dplyr::select(-POD)%>%
  as.matrix()

eff_it[eff_it > 0]<-1
eff_mat<-unname(eff_it)

everyone_ch_all<-readRDS("./data/everyone_everywhere.RDS")%>%group_by(POD)%>%slice_head(n = 40)%>%ungroup()
everyone_ch_all$pod_ch
obs_it<-everyone_ch_all%>%dplyr::select(-ind,-POD,-pod_ch,-NAME,-SEX,-sex_ch)%>%
  as.matrix()
obs_ch_mat<-unname(obs_it)
obs_ch_mat[is.na(obs_ch_mat)]<-0
obs_ch_mat[(obs_ch_mat>0)]<-1
mydata <- obs_ch_mat

#mydata <- read.table('dipper.txt')
head(mydata)
dim(mydata)
# remove counts
#mydata <- mydata[,-8]
N <- dim(mydata)[1]
K <- dim(mydata)[2]

# Compute the date of first capture for each individual:
e <- NULL
for (i in 1:N){
  temp <- 1:K
  e <- c(e,min(temp[mydata[i,]>=1]))}

e

# model -----
model <- function() {
  
  # OBSERVATIONS
  # 1 = non-detected
  # 2 = detected
  
  # STATES
  # 1 = alive
  # 2 = dead
  
  # PARAMETERS
  # phi  survival
  # p detection
  
# constraints ----
  for (i in 1:N){
    for (t in 1:(K-1)){
      logit(phi[i,t]) <- beta2[pod[i]] + epsilon[1,pod[i],t]
      logit(p[i,t]) <-  alpha2[pod[i]] + epsilon[2,pod[i],t]
    }}
  
  for (t in 1:(K-1)){
    for (j in 1:2){ # pod
      logit(phi.est[j,t]) <- beta2[j] + epsilon[1,j,t]
      logit(p.est[j,t]) <- alpha2[j] + epsilon[2,j,t]
    }}
  
# PARAMETERS	----
  # probabilities for each initial state
  px0[1] <- 1 # prob. of being in initial state alive
  px0[2] <- 0 # prob. of being in initial state dead
  
  for (i in 1:N){
    
  # probabilities for each initial observation  
    po.init[1,i,1] <- 0
    po.init[1,i,2] <- 1
    po.init[2,i,1] <- 1
    po.init[2,i,2] <- 0
    
    for (t in 1:(K-1)){
  # OBSERVATION PROCESS: prob of z(t+1) given z(t), [z(t),i,t,z(t+1)] ----
  # detection
  po[1,i,t,1] <- 1 - (p[i,t] * eff[pod[i],t+1])
  po[1,i,t,2] <- p[i,t] * eff[pod[i],t+1]
  
  po[2,i,t,1] <- 1
  po[2,i,t,2] <- 0
  
  # STATE PROCESS: prob of y(t) given z(t) [z(t),i,t,y(t)]----
  # survival
  px[1,i,t,1] <- phi[i,t]
  px[1,i,t,2] <- 1 - phi[i,t]
  
  px[2,i,t,1] <- 0
  px[2,i,t,2] <- 1
  
  }
  }

  
  for (i in 1:N){  # for each indiv
  
    # estimated probabilities of initial states are the proportions in each state at first capture occasion
    z[i,f[i]] ~ dcat(px0[1:2])
    y[i,f[i]] ~ dcat(po.init[z[i,f[i]],i,1:2])
    
    for (t in (f[i]+1):K){  # loop over time
      
      ## STATE EQUATIONS ##
      # draw states at t given states at t-1
      z[i,t] ~ dcat(px[z[i,t-1],i, t-1,1:2])
      
      ## OBSERVATION EQUATIONS ##
      # draw observations at t given states at t
      y[i,t] ~ dcat(po[z[i,t],i, t-1,1:2])
      
    }
    
  }
  
  # PRIORS  ----
  #phi ~ dunif(0, 1)
  #p ~ dunif(0, 1)
  
  for (t in 1:(K-1)){
    for (j in 1:2){
      epsilon[1,j,t] ~ dnorm(0, tau[1])
      epsilon[2,j,t] ~ dnorm(0, tau[2])
    }}
  
  for (j in 1:2){
    beta2[j] ~ dt(0,1,3)
    alpha2[j] ~ dt(0,1,3)
  }
  
  sigma[1] = 1/sqrt(tau[1])
  sigma[2] = 1/sqrt(tau[2])
  tau[1] ~ dscaled.gamma(1,3)
  tau[2] ~ dscaled.gamma(1,3)
  sigma2[1] <- pow(sigma[1], 2)
  sigma2[2] <- pow(sigma[2], 2)
  
}

# Form the list of data
mydatax <- list(N=N,K=K,y=as.matrix(mydata+1),f=e, pod = everyone_ch_all$pod_ch, eff = eff_mat)

# Generate inits for the latent states
x.init <- mydata
for (i in 1:N){
  if (e[i] == 1) next
  if (e[i] > 1) x.init[i,1:(e[i]-1)] <- NA
}
x.init[x.init==0] <- 1
z <- as.matrix(x.init)


inits<-function(){list(z = z,
                            #beta1 = runif(2, 0, 1),
                            #alpha1 = runif(2, 0, 1),
                            #beta1 = matrix(rep(runif(2*(n_occ-1), 0, 1)), ncol =n_occ-1),
                            beta2 = runif(2, 0, 1),
                            alpha2 = runif(2, 0, 1),
                            #mean.p = runif(1, 0, 1),
                            tau = runif(2, 1, 5)
)}

# Specify the parameters to be monitored
parameters <- c("p.est","phi.est","beta2","alpha2","sigma")
rjags::load.module("glm")
# fit the model:
R2OpenBUGS::write.model(model,con="multievent.txt") # write JAGS model code to file
m1 = rjags::jags.model("multievent.txt", data = mydatax, inits = inits, n.chains = 3, n.adapt = 1000)
update(m1) # another burn in
out1 = coda.samples(model = m1, variable.names = parameters, n.iter = 5000)
out1_df = posterior::as_draws_df(out1)
results_me<-as.data.frame(summary(out1_df))

#bayesplot::mcmc_trace(out1)

#####

## Results ----
# date = "2024-12-19"
# results_in_all<-readRDS(paste0("./data/survival&cap_all",date,".rds"))
# 
# results_all<-as.data.frame(summary(results_in_all))
# results_all
min(results_me$ess_bulk)

results_me%>%
  filter(grepl("sigma", variable))%>%
  mutate(sigma2 = median^2)

results_me%>%
  filter(grepl("phi.est", variable))

## surival prob # not identifiable at last occasion
occasions<-names(long_samp_ch_all)

ID_per_day_all$year_season_code<-as.character(ID_per_day_all$year_season_code)

results_phi_me<- results_me%>%
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

ggplot(results_phi_me, aes(x = as.numeric(calfyr_season), y = median))+
  geom_errorbar(aes(ymin = q5, ymax = q95, color = eff), size = 1, alpha = 0.8)+
  geom_point(aes(shape = as.factor(season), color = eff), size = 3, alpha = 0.8)+
  facet_wrap(~pod)+
  theme_bw()+
  theme(legend.position = "bottom")+
  scale_x_continuous(breaks = c(2005:2024))+
  theme(axis.text.x=element_text(angle=90, vjust=0.5))+
  xlab(expression('Dolphin year (01Sep_{year-1}–31Aug_{year})'))+
  ylab(expression('Survival probability,' *phi))

ggsave('./figures/me_phi_pod.png', dpi = 300, width = 300, height = 175, units = "mm")

## capture prob # not identifiable at first occasion

results_p_me<-results_me%>%
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

ggplot(results_p_me, aes(x = as.numeric(calfyr_season), y = median))+
  geom_errorbar(aes(ymin = q5, ymax = q95, color = eff), size = 1, alpha = 0.8)+
  geom_point(aes(shape = as.factor(season), color = eff),size = 3, alpha = 0.8)+
  #geom_line(aes(linetype = pod))+
  #ylim(c(0.0,1.0))+
  facet_wrap(~pod)+
  theme_bw()+
  scale_x_continuous(breaks = c(2005:2024))+
  theme(axis.text.x=element_text(angle=90, vjust=0.5))+
  theme(legend.position = "bottom")+
  xlab(expression('Dolphin year (01Sep_{year-1}–31Aug_{year})'))+
  ylab(expression('Capture probability, p'))

ggsave('./figures/me_p_pod.png', dpi = 300, width = 300, height = 175, units = "mm")


