
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

female_ch<-readRDS("./data/female_ch.RDS")%>%ungroup()#%>%group_by(POD)%>%slice_head(n = 40)
female_ch%>%dplyr::select(NAME,POD,pod_ch)

female_ch$pod_ch
obs_it<-female_ch%>%dplyr::select(-ind,-POD,-pod_ch,-NAME)%>%
  as.matrix()
obs_ch_mat<-unname(obs_it)
obs_ch_mat[is.na(obs_ch_mat)]<-0

pod_ch<-female_ch$pod_ch
n_doubtful<-sum(pod_ch == 1)

head(obs_ch_mat)
dim(obs_ch_mat)
sum(obs_ch_mat[1:n_doubtful,1])

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

#getting rid of NAs for z inits, but with logical values
z_data <- obs_ch_mat

for (i in 1:n_ind){
  if (is.na(z_data[i,11]) && is.na(z_data[i,12]) && z_data[i,10] == z_data[i,13]){
    z_data[i,11] <- z_data[i,10]
    z_data[i,12] <- z_data[i,10]
  }} 

for (i in 1:n_ind){  
  for (j in 1:n_occ){
    if (is.na(z_data[i,j])){
      if (!is.na(z_data[i,(j-1)]) && !is.na(z_data[i,(j+1)]) && (z_data[i,(j-1)] == z_data[i,(j+1)])){
        z_data[i,j] <- z_data[i,j+1]
      } else if (!is.na(z_data[i,(j-1)]) && !is.na(z_data[i,(j+2)]) && (z_data[i,(j-1)] == z_data[i,(j+2)])){
        z_data[i,j] <- z_data[i,j+2]
      } else if (i > f[i]){
        z_data[i,j] <- 0
      } 
    }
  }
}

for (j in 1:n_occ){
  if (is.na(z_data[1,j]) == TRUE){
    z_data[1,j] <-2
  }}

# model -----
model <- function() {
  
  # OBSERVATIONS ----
  # 1 = non-detected
  # 2 = detected as female of non-breeding age
  # 3 = detected as female of breeding age
  # 4 = detected as female in close association with a calf
  
  # STATES -----
  # 1 = female of non-breeding age alive
  # 2 = female of breeding age alive
  # 3 = female in close association with a calf
  # 4 = dead
  
  # PARAMETERS ----
  # phiN  survival non-breeding age females
  # phiB  survival breeding age females
  # phiW survival of female with calf
  
  # pN detection prob. non-breeding age females
  # pB  detection prob. breeding age females
  # pW detection prob. female with calf
  
  # psiNB transition prob. from non-breeding age to breeding age
  # psiNW transition prob. from non-breeding age to with calf
  # psiBW transition prob. from breeding age to with calf
  # psiWB transition prob. from with calf to breeding age
  
  
  # piPA prob of being in initial state pre-adult
  
# constraints ----
  for (i in 1:n_ind){
    for (t in 1:(n_occ-1)){
      
      logit(pN[i,t]) <-  alpha1[pod[i]] + epsilon_p[pod[i],t]
      logit(phiN[i,t]) <- beta1[pod[i]] + epsilon_phi[pod[i],t]
      
      logit(pB[i,t]) <-  alpha2[pod[i]] + epsilon_p[pod[i],t]
      logit(phiB[i,t]) <- beta2[pod[i]] + epsilon_phi[pod[i],t]
      
      logit(pW[i,t]) <-  alpha3[pod[i]] + epsilon_p[pod[i],t]
      logit(phiW[i,t]) <- beta3[pod[i]] + epsilon_phi[pod[i],t]

      logit(psiNB[i,t]) <- gamma1[pod[i]] + epsilon_psi[pod[i],t]
      logit(psiNW[i,t]) <- gamma2[pod[i]] + epsilon_psi[pod[i],t]
      logit(psiBW[i,t]) <- gamma3[pod[i]] + epsilon_psi[pod[i],t]
      logit(psiWB[i,t]) <- gamma4[pod[i]] + epsilon_psi[pod[i],t]
    }}

#parameter estimates  
for (t in 1:(n_occ-1)){
  for (j in 1:2){ # pod
      logit(pN.est[j,t]) <- alpha1[j] + epsilon_p[j,t]
      logit(phiN.est[j,t]) <- beta1[j] + epsilon_phi[j,t]
    
      logit(pB.est[j,t]) <- alpha2[j] + epsilon_p[j,t]
      logit(phiB.est[j,t]) <- beta2[j] + epsilon_phi[j,t]
      
      logit(pW.est[j,t]) <- alpha3[j] + epsilon_p[j,t]
      logit(phiW.est[j,t]) <- beta3[j] + epsilon_phi[j,t]
      
      logit(psiNB.est[j,t]) <- gamma1[j] + epsilon_psi[j,t]
      logit(psiNW.est[j,t]) <- gamma2[j] + epsilon_psi[j,t]
      logit(psiBW.est[j,t]) <- gamma3[j] + epsilon_psi[j,t]
      logit(psiWB.est[j,t]) <- gamma4[j] + epsilon_psi[j,t]
  }}
  
# PARAMETERS	----
  # probabilities for each initial state
  ps.init[1] <- pi[1] # prob initial state N
  ps.init[2] <- pi[2] # prob of initial state state B
  ps.init[3] <- pi[3] # prob of intial state W
  ps.init[4] <- 0 # prob. of being in initial state dead
  
  for (i in 1:n_ind){
    
  # probabilities for each initial observation ----  
    po.init[1,i,1] <- 0
    po.init[1,i,2] <- 1
    po.init[1,i,3] <- 0
    po.init[1,i,4] <- 0
    
    po.init[2,i,1] <- 0
    po.init[2,i,2] <- 0
    po.init[2,i,3] <- 1
    po.init[2,i,4] <- 0
    
    po.init[3,i,1] <- 0
    po.init[3,i,2] <- 0
    po.init[3,i,3] <- 0
    po.init[3,i,4] <- 1
    
    po.init[4,i,1] <- 1
    po.init[4,i,2] <- 0
    po.init[4,i,3] <- 0
    po.init[4,i,4] <- 0
    
    for (t in 1:(n_occ-1)){
  # OBSERVATION PROCESS: prob of y(t) given z(t) [z(t),i,t,y(t)]----
  # detection
  po[1,i,t,1] <- 1 - (pN[i,t] * eff[pod[i],t+1]) # prob of non-detection given PA
  po[1,i,t,2] <- pN[i,t] * eff[pod[i],t+1] # prob of PA given detected as PA
  po[1,i,t,3] <- 0
  po[1,i,t,4] <- 0
  
  po[2,i,t,1] <- 1 - (pB[i,t] * eff[pod[i],t+1]) # prob of nondetection given A
  po[2,i,t,2] <- 0 # prob of A given detected as PA
  po[2,i,t,3] <- pB[i,t] * eff[pod[i],t+1] # prob detected as adult given it is an adult
  po[2,i,t,4] <- 0
  
  po[3,i,t,1] <- 1 - (pW[i,t] * eff[pod[i],t+1])# prob of nondetection given with calf
  po[3,i,t,2] <- 0 # prob of PA given it is dead
  po[3,i,t,3] <- 0 # prob of A given it is dead
  po[3,i,t,4] <- pW[i,t] * eff[pod[i],t+1]
    
  po[4,i,t,1] <- 1 # prob of nondetection given it is dead
  po[4,i,t,2] <- 0 # prob of PA given it is dead
  po[4,i,t,3] <- 0 # prob of A given it is dead
  po[4,i,t,4] <- 0
  
  # STATE PROCESS: prob of z(t+1) given z(t), [z(t),i,t,z(t+1)] ----
  # survival
  ps[1,i,t,1] <- phiN[i,t] * (1 - psiNB[i,t]) * (1 - psiNW[i,t])
  ps[1,i,t,2] <- phiN[i,t] * psiNB[i,t]
  ps[1,i,t,3] <- phiN[i,t] * psiNW[i,t]
  ps[1,i,t,4] <- 1 - phiN[i,t]
  
  ps[2,i,t,1] <- 0
  ps[2,i,t,2] <- phiB[i,t] * (1 - psiBW[i,t])
  ps[2,i,t,3] <- phiB[i,t] * psiBW[i,t]
  ps[2,i,t,4] <- 1 - phiB[i,t]
  
  ps[3,i,t,1] <- 0
  ps[3,i,t,2] <- phiW[i,t] * psiWB[i,t]
  ps[3,i,t,3] <- phiW[i,t] * (1 - psiWB[i,t])
  ps[3,i,t,4] <- 1 - phiW[i,t]
  
  ps[4,i,t,1] <- 0
  ps[4,i,t,2] <- 0
  ps[4,i,t,3] <- 0
  ps[4,i,t,4] <- 1
  
  }
  }

  
  for (i in 1:n_ind){  # for each indiv
  
    # estimated probabilities of initial states are the proportions in each state at first capture occasion
    z[i,f[i]] ~ dcat(ps.init[1:4])
    y[i,f[i]] ~ dcat(po.init[z[i,f[i]],i,1:4])
    #z[i,f[i]] ~ y[i,f[i]]
    
    for (t in (f[i]+1):n_occ){  # loop over time
      
      ## STATE EQUATIONS ##
      # draw states at t given states at t-1
      z[i,t] ~ dcat(ps[z[i,t-1],i,t-1,1:4])
      
      ## OBSERVATION EQUATIONS ##
      # draw observations at t given states at t
      y[i,t] ~ dcat(po[z[i,t],i,t-1,1:4])
      
    }
    
  }
  
  # PRIORS  ----
  for (i in 1:3){
    #prior
    a[i] ~ dgamma(1,1)
    #restrict to 1
    pi[i] <- a[i] / sum(a[1:3])
  }
  
  for (j in 1:2){
    for (t in 1:(n_occ-1)){
        epsilon_p[j,t] ~ dnorm(0,tau[1])
        epsilon_phi[j,t] ~ dnorm(0,tau[2])
        epsilon_psi[j,t] ~ dnorm(0,tau[3])
    }}
  

  # for (i in 1:3){
  #     #prior
  #     b[i] ~ dgamma(1,1)
  #   for (j in 1:2){
  #     #restrict to 1
  #     gamma[i,j] <- b[i] / sum(b[1:3])
  #   }}
  
  for (j in 1:2){   
    gamma1[j] ~ dt(0,1,3)
    gamma2[j] ~ dt(0,1,3)
    gamma3[j] ~ dt(0,1,3)
    gamma4[j] ~ dt(0,1,3)
    beta1[j] ~ dt(0,1,3)
    alpha1[j] ~ dt(0,1,3)
    beta2[j] ~ dt(0,1,3)
    alpha2[j] ~ dt(0,1,3)
    beta3[j] ~ dt(0,1,3)
    alpha3[j] ~ dt(0,1,3)
  }
   
 for (u in 1:3){
  tau[u] ~ dscaled.gamma(1,3)
  sigma[u] <- 1/sqrt(tau[u])
  sigma2[u] <- pow(sigma[u],2)
 }
  
}

# mcmc.data ----
mcmc.data <- list(n_ind=n_ind,
                n_occ=n_occ,
                #n_doubtful = n_doubtful,
                y=as.matrix(obs_ch_mat+1),
                f=f, 
                pod = pod_ch, 
                eff = eff_mat)

# Generate inits for the latent states

for (i in 1:n_ind){
  if (f[i] == 1) next
  if (f[i] > 1) z_data[i,1:(f[i]-1)] <- NA
}

x.init <- z_data

for (i in 1:n_ind){
  for (t in 1:n_occ){
    if (!is.na(x.init[i,t]) && !is.na(x.init[i,t-1]) && x.init[i,t] == 0){
      #print(c(i,t))
      x.init[i,t] = x.init[i,t-1]
    }  else if (!is.na(x.init[i,t]) & x.init[i,t] == 0 && !is.na(x.init[i,t-2])){
      print(c(i,t))
      #x.init[i,t] = x.init[i,t-2]
    } else if (!is.na(x.init[i,t]) & x.init[i,t] == 0 && !is.na(x.init[i,t-3])){
      #print(c(i,t))
      x.init[i,t] = x.init[i,t-3]
    } 
  }
}


  for (t in 2:n_occ){
    for (i in 1:n_ind){
    #print(c(i,t))
    if ((!is.na(x.init[i,t])) && (!is.na(x.init[i,t-1])) && (x.init[i,t-1] == 3) && (x.init[i,t] == 1)){
      x.init[i,t] = 2
    } else if ((!is.na(x.init[i,t])) && (!is.na(x.init[i,t-1])) && (x.init[i,t-1] == 2) && (x.init[i,t] == 1)){
      x.init[i,t] = 2
    }else{
      x.init[i,t] = x.init[i,t]
    }
  }
}

z <- as.matrix(x.init)
z[2,50:53]
(obs_ch_mat+1)[44,28:30]

inits<-function(){list(z = z,
                      beta1 = rnorm(2, 0, 1),
                      alpha1 = rnorm(2, 0, 1),
                      beta2 = rnorm(2, 0, 1),
                      alpha2 = rnorm(2, 0, 1),
                      beta3 = rnorm(2, 0, 1),
                      alpha3 = rnorm(2, 0, 1),
                      gamma1 = rnorm(2, 0, 1),
                      gamma2 = rnorm(2, 0, 1),
                      gamma3 = rnorm(2, 0, 1),
                      gamma4 = rnorm(2, 0, 1),
                      tau = runif(3, 1, 5)
)}

#####
rjags::load.module("glm")
# fit the model:
R2OpenBUGS::write.model(model,con="multi-event_repro.txt") # write JAGS model code to file
Sys.time()
m1 = rjags::jags.model("multi-event_repro.txt", data = mcmc.data, inits = inits, n.chains = 3, n.adapt = 5000) #5000
update(m1) # another burn in
m1$state()
#### Specify the parameters to be monitored ----
parameters <- c("pN.est","pB.est","pW.est","phiN.est","phiB.est","phiW.est",
                "psiNB.est","psiNW.est","psiBW.est","psiWB.est",
                "alpha1","alpha2","alpha3","beta1","beta2","beta3",
                "gamma1","gamma2","gamma3","gamma4","sigma2","z")
                #"Doubtful_A","Dusky_A","Doubtful_PA","Dusky_PA")
                #"N_Doubtful_A","N_Dusky_A","N_Doubtful_PA","N_Dusky_PA")

out1 = coda.samples(model = m1, variable.names = parameters, n.iter = 25000) #20000
#####
out1_df = posterior::as_draws_df(out1)
Sys.time()
saveRDS(out1_df, file = paste0("./data/multi-event_repro_",Sys.Date(),".rds"))
Sys.time()

## Results ----
# F = 1, M = 2
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
