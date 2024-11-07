
library(RPresence)
library(jagsUI)
library(rjags)
library(dplyr)

# data ----

# mostly adults, some unk adults at the beginning
# sex == X grouped with males since they have no calving history and only looking at adults here. 
### should estimate this instead of brute force?
data_date = "2024-11-06"

obs_ch<-readRDS(paste0("./data/obs_ch_",data_date,".rds"))

ID_ch<-obs_ch%>%arrange(POD, ID_NAME)%>%dplyr::select(ind,ID_NAME,POD,pod_ch,SEX,sex_ch)
ID_ch[is.na(ID_ch)]<-2 #unknown adults to males

obs_it<-obs_ch%>%dplyr::select(-ind,-POD,-pod_ch,-ID_NAME,-SEX,-sex_ch)%>%
  as.matrix()

obs_ch_mat<-unname(obs_it)
obs_ch_mat[is.na(obs_ch_mat)]<-0
obs_ch_mat[obs_ch_mat==2]<-1 #undo weaning info for now
obs_ch_mat[1,]
sum(obs_ch_mat[1,])
#fill in ch = 1 in years between sightings (not many)
# for (i in 1:nrow(obs_it_mat)){
#   for (t in 2:(ncol(obs_it_mat)-1)){
#     if (obs_it_mat[i,t] == 0 & obs_it_mat[i,t-1] == 1 & obs_it_mat[i,t+1] == 1){
#       obs_it_mat[i,t] = 1
#     }
#   }
# 
# }

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
      beta1[k] ~ dunif(0,1)
      alpha1[k] ~ dunif(0,1)
    }
  
    for (j in 1:2){
      beta2[j] ~ dunif(0,1)
      alpha2[j] ~ dunif(0,1)
    }
# priors

  #mean.p ~ dunif(0,1)
  sigma[1] ~ dunif(0,5)
  sigma[2] ~ dunif(0,5)
  tau[1] <- pow(sigma[1], -2)
  tau[2] <- pow(sigma[2], -2)
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
                            sigma = runif(2, 1, 5)
                            )}

## run model ----
R2OpenBUGS::write.model(model,con="F_repro_model.txt") # write JAGS model code to file
m1 = rjags::jags.model("F_repro_model.txt", data = mcmc.data, inits = mcmc.inits, n.chains = 3, n.adapt = 5000)
out1 = coda.samples(model = m1, variable.names = mcmc.params, n.iter = 5000)
out1_df = posterior::as_draws_df(out1)
#2 is only doubtful and dusky survey areas
saveRDS(out1_df, file = paste0("./data/survival&cap2_",Sys.Date(),".rds"))

#bayesplot::mcmc_trace(out1_df)
results<-as.data.frame(summary(out1_df))
results

results%>%
  filter(grepl("sigma", variable))%>%
  mutate(sigma2 = median^2)

## surival prob

results_phi<-results%>%
  filter(grepl("phi.est", variable))%>%
  mutate(CALFYEAR = c(rep(2008:2022, each = 8), rep(2023, each = 4)),
         season = c(rep(rep(c(0,5),each = 4),(n_occ-1)/2), rep(0, each = 4)),
         #sex = rep(rep(c("F","M","X"), each = 1), (n_occ-1)*2), 
         sex = rep(rep(c("F","M"), each = 1), (n_occ-1)*2),
         pod = rep(rep(c("Doubtful","Dusky"), each = 2), (n_occ-1)))%>%
  mutate(calfyr_season = as.numeric(CALFYEAR) + (as.numeric(season)/10))
  
ggplot(results_phi, aes(x = calfyr_season, y = median, color = sex))+
  geom_errorbar(aes(ymin = q5, ymax = q95), size = 1, alpha = 0.8)+
  geom_point(aes(shape = as.factor(season)), size = 3, alpha = 0.8)+
  #geom_line(aes(linetype = pod))+
  ylim(c(0.7,1.0))+
  facet_wrap(~pod)+
  theme_bw()+
  #theme(legend.position = "bottom")+
  ylab(expression('Survival probability,' *phi))

ggsave('./figures/phi_pod_sex.png', dpi = 300, width = 300, height = 175, units = "mm")

## capture prob

results_p<-results%>%
  filter(grepl("p.est", variable))%>%
  mutate(CALFYEAR = c(rep(2008:2022, each = 8), rep(2023, each = 4)),
         season = c(rep(rep(c(0,5),each = 4),(n_occ-1)/2), rep(0, each = 4)),
         #sex = rep(rep(c("F","M","X"), each = 1), (n_occ-1)*2), 
         sex = rep(rep(c("F","M"), each = 1), (n_occ-1)*2),
         pod = rep(rep(c("Doubtful","Dusky"), each = 2), (n_occ-1)))%>%
  mutate(calfyr_season = as.numeric(CALFYEAR) + (as.numeric(season)/10))

ggplot(results_p, aes(x = calfyr_season, y = median, color = sex))+
  geom_errorbar(aes(ymin = q5, ymax = q95), size = 1, alpha = 0.8)+
  geom_point(aes(shape = as.factor(season)),size = 3, alpha = 0.8)+
  #geom_line(aes(linetype = pod))+
  ylim(c(0.0,1.0))+
  facet_wrap(~pod)+
  theme_bw()+
  #theme(legend.position = "bottom")+
  ylab(expression('Capture probability, p'))

ggsave('./figures/p_pod_sex.png', dpi = 300, width = 300, height = 175, units = "mm")
