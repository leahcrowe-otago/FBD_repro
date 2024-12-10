
library(RPresence)
library(jagsUI)
library(rjags)
library(dplyr)

# data ----

# all ages
# sex == X grouped with males since they have no calving history and only looking at adults here. 
### should estimate this instead of brute force?

everyone_ch<-readRDS("./data/everyone_everywhere.RDS")

ID_ch<-everyone_ch%>%arrange(POD, NAME)%>%dplyr::select(ind,NAME,POD,pod_ch,SEX,sex_ch)

obs_it<-everyone_ch%>%dplyr::select(-ind,-POD,-pod_ch,-NAME,-SEX,-sex_ch,-ageclass)%>%
  as.matrix()

occ<-names(everyone_ch)[5:(ncol(everyone_ch)-4)]

obs_ch_mat<-unname(obs_it)
obs_ch_mat[is.na(obs_ch_mat)]<-0
#obs_ch_mat[obs_ch_mat==2]<-1 #undo weaning info for now
obs_ch_mat[1,]

doubtful_mat<-obs_ch_mat[1:166,]
dusky_mat<-obs_ch_mat[167:n_ind,]

doubtful_sum<-NULL

for (j in 1:n_occ){
  doubtful_sum[j]<-sum(doubtful_mat[j,])
  
}

dusky_sum<-NULL

for (j in 1:n_occ){
  dusky_sum[j]<-sum(dusky_mat[j,])
  
}

# number of individuals 
n_ind <- nrow(obs_ch_mat) 

# number of capture occasions
n_occ <- ncol(obs_ch_mat)

# first capture
get.first<- function(x) min(which(x!=0))
f<-apply(obs_ch_mat, 1, get.first)
f

## model ----

model<-function(){
  
# constraints
  for (i in 1:n_ind){
    for (t in 1:(n_occ-1)){

      logit(phi[i,t]) <- beta1[sex[i]] + beta2[pod[i]] + epsilon[1,sex[i],pod[i],t]
      logit(p[i,t]) <- alpha1[sex[i]] + alpha2[pod[i]] + epsilon[2,sex[i],pod[i],t]
    
    }
  }
  
  for (t in 1:(n_occ-1)){
    for (k in 1:2){
      for (j in 1:2){
    logit(phi.est[k,j,t]) <- beta1[k] + beta2[j] + epsilon[1,k,j,t]
    logit(p.est[k,j,t]) <- alpha1[k] + alpha2[j] + epsilon[2,k,j,t]
  }}}

  # priors
  
  for (t in 1:(n_occ-1)){
    for (k in 1:2){
      for (j in 1:2){
    epsilon[1,k,j,t] ~ dnorm(0, tau[1])
    epsilon[2,k,j,t] ~ dnorm(0, tau[2])
  }}}
  
    for (k in 1:2){
      beta1[k] ~ dnorm(0,1)
      alpha1[k] ~ dnorm(0,1)
    }
  
    for (j in 1:2){
      beta2[j] ~ dnorm(0,1)
      alpha2[j] ~ dnorm(0,1)
    }
# priors

  sigma[1] = 1/sqrt(tau[1])
  sigma[2] = 1/sqrt(tau[2])
  tau[1] ~ dgamma(1,2)
  tau[2] ~ dgamma(1,2)
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
      y[i,t] ~ dbern(p[i,t-1] * z[i,t])
    }
  }
  
} 

## data ----
mcmc.data<-list(
  y = obs_ch_mat,
  n_ind = nrow(obs_ch_mat),
  f = f,
  n_occ = ncol(obs_ch_mat),
  pod = ID_ch$pod_ch,
  sex = ID_ch$sex_ch) 

mcmc.params<-c("p.est","phi.est","beta1","beta2","alpha1","alpha2","sigma")

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
                            beta1 = runif(2, 0, 1),
                            alpha1 = runif(2, 0, 1),
                            #beta1 = matrix(rep(runif(2*(n_occ-1), 0, 1)), ncol =n_occ-1),
                            beta2 = runif(2, 0, 1),
                            alpha2 = runif(2, 0, 1),
                            #mean.p = runif(1, 0, 1),
                            tau = runif(2, 1, 5)
                            )}

## run model ----
R2OpenBUGS::write.model(model,con="F_repro_model.txt") # write JAGS model code to file
m1 = rjags::jags.model("F_repro_model.txt", data = mcmc.data, inits = mcmc.inits, n.chains = 3, n.adapt = 5000)
out1 = coda.samples(model = m1, variable.names = mcmc.params, n.iter = 20000)
out1_df = posterior::as_draws_df(out1)
# survival&cap2 is only doubtful and dusky survey areas
saveRDS(out1_df, file = paste0("./data/survival&cap_",Sys.Date(),".rds"))
#saveRDS(out1_df, file = paste0("./data/all_survival&cap_",Sys.Date(),".rds")) # all age classes together

## Results ----
date = "2024-12-10"
results_in<-readRDS(paste0("./data/survival&cap_",date,".rds"))

results<-as.data.frame(summary(results_in))
results
min(results$ess_bulk)

results%>%
  filter(grepl("sigma", variable))%>%
  mutate(sigma2 = median^2)

results%>%
  filter(grepl("phi.est", variable))


param_filter<-function(x){
  results%>%
    filter(grepl(x, variable))%>%
    mutate(calfyr_season = c(rep(occ, each = 4)),
           #sex = rep(rep(c("F","M","X"), each = 1), (n_occ-1)*2), 
           sex = rep(rep(c("F","M"), each = 1), (n_occ-1)*2),
           pod = rep(rep(c("Doubtful","Dusky"), each = 2), (n_occ-1)))%>%
    mutate(season = c(rep(rep(1:3, each = 4), 15),rep(1,4),rep(2,4)))
}



## surival prob

results_phi<-param_filter("phi.est")
  
library(ggplot2)

ggplot(results_phi, aes(x = as.numeric(calfyr_season), y = median, color = sex))+
  geom_errorbar(aes(ymin = q5, ymax = q95), size = 1, alpha = 0.8)+
  geom_point(aes(shape = as.factor(season)), size = 3, alpha = 0.8)+
  #geom_line(aes(linetype = pod))+
  #ylim(c(0.7,1.0))+
  facet_wrap(~pod)+
  theme_bw()+
  theme(legend.position = "bottom")+
  scale_x_continuous(breaks = c(2008:2024))+
  theme(axis.text.x=element_text(angle=90, vjust=0.5))+
  xlab(expression('Dolphin year (01Sep_{year-1}–31Aug_{year})'))+
  ylab(expression('Survival probability,' *phi))

#ggsave('./figures/phi_pod_sex.png', dpi = 300, width = 300, height = 175, units = "mm")
ggsave('./figures/all_phi_pod_sex.png', dpi = 300, width = 300, height = 175, units = "mm")
#ggsave('./figures/phi_pod_sex_SA.png', dpi = 300, width = 300, height = 175, units = "mm")

## capture prob

results_p<-param_filter("p.est")

ggplot(results_p, aes(x = as.numeric(calfyr_season), y = median, color = sex))+
  geom_errorbar(aes(ymin = q5, ymax = q95), size = 1, alpha = 0.8)+
  geom_point(aes(shape = as.factor(season)),size = 3, alpha = 0.8)+
  #geom_line(aes(linetype = pod))+
  ylim(c(0.0,1.0))+
  facet_wrap(~pod)+
  theme_bw()+
  scale_x_continuous(breaks = c(2009:2024))+
  theme(axis.text.x=element_text(angle=90, vjust=0.5))+
  theme(legend.position = "bottom")+
  xlab(expression('Dolphin year (01Sep_{year-1}–31Aug_{year})'))+
  ylab(expression('Capture probability, p'))

#ggsave('./figures/p_pod_sex.png', dpi = 300, width = 300, height = 175, units = "mm")
ggsave('./figures/all_p_pod_sex.png', dpi = 300, width = 300, height = 175, units = "mm")
#ggsave('./figures/p_pod_sex_SA.png', dpi = 300, width = 300, height = 175, units = "mm")
