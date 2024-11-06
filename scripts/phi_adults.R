
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

      logit(phi[i,t]) <- beta1[sex[i],t] + beta2[pod[i]]
      p[i,t] <- mean.p
    
    }
  }
  
  for (t in 1:(n_occ-1)){
    for (k in 1:2){
      for (j in 1:2){
    logit(phi.est[k,j,t]) <- beta1[k,t] + beta2[j]
  }}}

  # priors
  
  for (t in 1:(n_occ-1)){
    
    for (k in 1:2){
      beta1[k,t] ~ dunif(0,1)}
    }
  
    for (j in 1:2){
      beta2[j] ~ dunif(0,1)
    }
# priors

  mean.p ~ dunif(0,1)
  # sigma ~ dunif(0,5)
  # tau<- pow(sigma, -2)
  # sigma2<- pow(sigma, 2)
  
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

mcmc.params<-c("mean.p","phi.est","beta2")

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
                            beta1 = matrix(rep(runif(2*(n_occ-1), 0, 1)), ncol =n_occ-1),
                            beta2 = runif(2, 0, 1),
                            mean.p = runif(1, 0, 1)
                            #sigma = runif(1, 1, 5)
                            )}

## run model ----
R2OpenBUGS::write.model(model,con="F_repro_model.txt") # write JAGS model code to file
m1 = rjags::jags.model("F_repro_model.txt", data = mcmc.data, inits = mcmc.inits, n.chains = 3, n.adapt = 5000)
out1 = coda.samples(model = m1, variable.names = mcmc.params, n.iter = 5000)
out1_df = posterior::as_draws_df(out1)

saveRDS(out1_df, file = paste0("./data/survival&cap_",Sys.Date(),".rds"))

bayesplot::mcmc_trace(out1_df)
results<-as.data.frame(summary(out1_df))
results

results_year<-results%>%
  filter(grepl("phi.est", variable))%>%
  mutate(Year = rep(2008:2022, each = 4),
         #sex = rep(rep(c("F","M","X"), each = 1), (n_occ-1)*2), 
         sex = rep(rep(c("F","M"), each = 1), (n_occ-1)*2),
         pod = rep(rep(c("Doubtful","Dusky"), each = 2), (n_occ-1)))
  

ggplot(results_year, aes(x = Year, y = median, color = sex))+
  geom_errorbar(aes(ymin = q5, ymax = q95), size = 1, alpha = 0.8)+
  geom_point(size = 3, alpha = 0.8)+
  #geom_line(aes(linetype = pod))+
  ylim(c(0.7,1.0))+
  facet_wrap(~pod)+
  theme_bw()+
  #theme(legend.position = "bottom")+
  ylab(expression('Survival probability,' *phi))

ggsave('./figures/surv_pod_sex.png', dpi = 300, width = 300, height = 175, units = "mm")
