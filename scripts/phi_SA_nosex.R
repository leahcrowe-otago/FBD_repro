
library(RPresence)
library(jagsUI)
library(rjags)
library(dplyr)

# data ----

# all ages
# sex == X grouped with males since they have no calving history and only looking at adults here. 
### should estimate this instead of brute force?

everyone_ch_SA<-readRDS("./data/everyone_SA.RDS")
long_samp_ch_SA<-readRDS("./data/long_samp_SA.RDS") #skip 2007.67
ID_per_day_SA<-readRDS("./data/ID_per_day_SA.RDS") 

ID_ch<-everyone_ch_SA%>%arrange(POD, NAME)%>%dplyr::select(ind,NAME,POD,pod_ch,SEX,sex_ch)

obs_it<-everyone_ch_SA%>%dplyr::select(-ind,-POD,-pod_ch,-NAME,-SEX,-sex_ch)%>%
  as.matrix()

eff_it<-long_samp_ch_SA%>%
  dplyr::select(-POD)%>%
  as.matrix()

eff_it[eff_it > 0]<-1
#eff_it[eff_it == 0]<-0.00001
eff_mat<-unname(eff_it)

occ<-names(everyone_ch_SA)[5:(ncol(everyone_ch_SA)-3)]

obs_ch_mat<-unname(obs_it)
obs_ch_mat[is.na(obs_ch_mat)]<-0
#obs_ch_mat[obs_ch_mat==2]<-1 #undo weaning info for now
obs_ch_mat[1,]

# number of individuals 
n_ind <- nrow(obs_ch_mat) 

# number of capture occasions
n_occ <- ncol(obs_ch_mat)

doubtful_n<-everyone_ch_SA%>%filter(POD == "DOUBTFUL")%>%
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
get.first<- function(x) min(which(x!=0))
f<-apply(obs_ch_mat, 1, get.first)
f

## model ----

model<-function(){
  
# constraints
  for (i in 1:n_ind){
    for (t in 1:(n_occ-1)){
      
      logit(phi[i,t]) <- beta2[pod[i]] + epsilon[1,pod[i],t]
      logit(p[i,t]) <-  alpha2[pod[i]] + epsilon[2,pod[i],t]
      
      }
    }
  
  for (t in 1:(n_occ-1)){
      for (j in 1:2){ # pod
    logit(phi.est[j,t]) <- beta2[j] + epsilon[1,j,t]
    logit(p.est[j,t]) <- alpha2[j] + epsilon[2,j,t]
  }}

  # priors
  
  for (t in 1:(n_occ-1)){
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
  
# likelihood
  for (i in 1:n_ind){
    # latent state at first capture
    z[i,f[i]] <- y[i,f[i]]
    
    for (t in (f[i]+1):n_occ){
      # state process
      z[i,t] ~ dbern(phi[i,t-1] * z[i,t-1])
      # observation process
      y[i,t] ~ dbern(p[i,t-1] * eff[pod[i],t] * z[i,t])
    }
  }
  
} 

## data ----
mcmc.data<-list(
  eff = eff_mat,
  y = obs_ch_mat,
  n_ind = nrow(obs_ch_mat),
  f = f,
  n_occ = ncol(obs_ch_mat),
  pod = ID_ch$pod_ch) 

mcmc.params<-c("p.est","phi.est","beta2","alpha2","sigma")

z.inits <- function(ch){
  state <- ch
  state[state==0] <- 1
  get.first <- function(x) min(which(x!=0))
  f <- apply(ch, 1, get.first)
  for (i in 1:nrow(ch)){
    state[i,1:f[i]] <- NA
  }
  return(state)
}

mcmc.inits<-function(){list(z = z.inits(obs_ch_mat),
                            #beta1 = runif(2, 0, 1),
                            #alpha1 = runif(2, 0, 1),
                            #beta1 = matrix(rep(runif(2*(n_occ-1), 0, 1)), ncol =n_occ-1),
                            beta2 = runif(2, 0, 1),
                            alpha2 = runif(2, 0, 1),
                            #mean.p = runif(1, 0, 1),
                            tau = runif(2, 1, 5)
                            )}

## run model ----
R2OpenBUGS::write.model(model,con="F_repro_model.txt") # write JAGS model code to file
rjags::load.module("glm")
m1 = rjags::jags.model("F_repro_model.txt", data = mcmc.data, inits = mcmc.inits, n.chains = 3, n.adapt = 5000)
update(m1) # another burn in
out1 = coda.samples(model = m1, variable.names = mcmc.params, n.iter = 20000)
out1_df = posterior::as_draws_df(out1)

saveRDS(out1_df, file = paste0("./data/survival&cap_SA",Sys.Date(),".rds"))

## Results ----
date = "2024-12-19"
results_in_SA<-readRDS(paste0("./data/survival&cap_SA",date,".rds"))

results_SA<-as.data.frame(summary(results_in_SA))
results_SA
min(results_SA$ess_bulk)

results_SA%>%
  filter(grepl("sigma", variable))%>%
  mutate(sigma2 = median^2)

results_SA%>%
  filter(grepl("phi.est", variable))

## surival prob # not identifiable at last occasion
occasions_SA<-names(long_samp_ch_SA)

ID_per_day_SA$year_season_code<-as.character(ID_per_day_SA$year_season_code)

results_phi_SA<-results_SA%>%
  filter(grepl("phi.est", variable))%>%
  mutate(calfyr_season = (rep(names(long_samp_ch_SA)[2:(n_occ)], each = 2)), # skip 2006.07
         pod = rep(rep(c("DOUBTFUL","DUSKY"), each = 1), (n_occ-1)))%>%
  mutate(season = case_when(
    grepl(".33", calfyr_season) ~ "Summer",
    grepl(".67", calfyr_season) ~ "Winter",
    TRUE ~ "Spring"
  ))%>%
  left_join(ID_per_day_SA, by = c("calfyr_season" = "year_season_code", "pod" = "POD","season"))%>%
  mutate(eff = case_when(
    IDperDay != 0 ~ "effort",
    TRUE ~ "no effort"))%>%
  mutate(area = "Complexes only")
  
library(ggplot2)

ggplot(results_phi_SA, aes(x = as.numeric(calfyr_season), y = median))+
  geom_errorbar(aes(ymin = q5, ymax = q95, color = eff), size = 1, alpha = 0.8)+
  geom_point(aes(shape = as.factor(season), color = eff), size = 3, alpha = 0.8)+
  facet_wrap(~pod)+
  theme_bw()+
  theme(legend.position = "bottom")+
  scale_x_continuous(breaks = c(2005:2024))+
  theme(axis.text.x=element_text(angle=90, vjust=0.5))+
  xlab(expression('Dolphin year (01Sep_{year-1}–31Aug_{year})'))+
  ylab(expression('Survival probability,' *phi))

ggsave('./figures/SA_phi_pod.png', dpi = 300, width = 300, height = 175, units = "mm")

## capture prob # not identifiable at first occasion

results_p_SA<-results_SA%>%
  filter(grepl("p.est", variable))%>%
  mutate(calfyr_season = rep(names(long_samp_ch_SA)[3:(n_occ+1)], each = 2),
         pod = rep(rep(c("DOUBTFUL","DUSKY"), each = 1), (n_occ-1)))%>%
  mutate(season = case_when(
    grepl(".33", calfyr_season) ~ "Summer",
    grepl(".67", calfyr_season) ~ "Winter",
    TRUE ~ "Spring"
  ))%>%
  left_join(ID_per_day_SA, by = c("calfyr_season" = "year_season_code", "pod" = "POD","season"))%>%
  mutate(eff = case_when(
    IDperDay != 0 ~ "effort",
    TRUE ~ "no effort"))%>%
  mutate(area = "Complexes only")

ggplot(results_p_SA, aes(x = as.numeric(calfyr_season), y = median))+
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

ggsave('./figures/SA_p_pod.png', dpi = 300, width = 300, height = 175, units = "mm")

