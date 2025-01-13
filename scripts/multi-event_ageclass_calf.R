
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

everyone_ch_age<-readRDS("./data/everyone_A_PA_C_ch.RDS")%>%ungroup()
everyone_ch_age%>%dplyr::select(NAME,POD,pod_ch,SEX,sex_ch)

everyone_ch_age$pod_ch
obs_it<-everyone_ch_age%>%dplyr::select(-ind,-POD,-pod_ch,-NAME,-SEX,-sex_ch)%>%
  as.matrix()
obs_ch_mat<-unname(obs_it)
obs_ch_mat[is.na(obs_ch_mat)]<-0

pod_ch<-everyone_ch_age$pod_ch
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

z_data <- obs_ch_mat

for (i in 1:n_ind){
  if(is.na(z_data[i,11]) && is.na(z_data[i,12]) && z_data[i,10] == z_data[i,13]){
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

z_data

# model -----
model <- function() {
  
  # OBSERVATIONS ----
  # 1 = non-detected
  # 2 = detected as calf
  # 3 = detected as pre adult/unk 
  # 4 = detected as adult
  
  # STATES -----
  # 1 = calf alive
  # 2 = pre-adult alive
  # 3 = adult alive
  # 4 = dead
  
  # PARAMETERS ----
  # phiC    survival prob of calf
  # phiPA   survival prob of pre-adults
  # phiA    survival prob of adults
  
  # pC  detection prob. of calves
  # pPA detection prob. of pre-adults 
  # pA  detection prob. of adults
  
  # psiCPA transition prob. from calf to pre-adult
  # psiPAA transition prob. from pre-adult to adult
  
  # piC prob of being in initial state calf
  # piPA prob of being in initial state pre-adult
  
# constraints ----
  for (i in 1:n_ind){
    for (t in 1:(n_occ-1)){
      
      logit(pA[i,t]) <-  alpha1[pod[i]] + epsilon_p[pod[i],t]
      logit(phiA[i,t]) <- beta1[pod[i]] + epsilon_phi[pod[i],t]
      
      logit(pPA[i,t]) <-  alpha2[pod[i]] + epsilon_p[pod[i],t]
      logit(phiPA[i,t]) <- beta2[pod[i]] + epsilon_phi[pod[i],t]
      
      logit(pC[i,t]) <-  alpha3[pod[i]] + epsilon_p[pod[i],t]
      logit(phiC[i,t]) <- beta3[pod[i]] + epsilon_phi[pod[i],t]

      logit(psiPAA[i,t]) <- gamma1[pod[i]] + epsilon_psi[pod[i],t]
      logit(psiCPA[i,t]) <- gamma2[pod[i]] + epsilon_psi[pod[i],t]
    }}

#parameter estimates  
for (t in 1:(n_occ-1)){
  for (j in 1:2){ # pod
      logit(pA.est[j,t]) <- alpha1[j] + epsilon_p[j,t]
      logit(phiA.est[j,t]) <- beta1[j] + epsilon_phi[j,t]
    
      logit(pPA.est[j,t]) <- alpha2[j] + epsilon_p[j,t]
      logit(phiPA.est[j,t]) <- beta2[j] + epsilon_phi[j,t]
      
      logit(pC.est[j,t]) <- alpha3[j] + epsilon_p[j,t]
      logit(phiC.est[j,t]) <- beta3[j] + epsilon_phi[j,t]
      
      logit(psiPAA.est[j,t]) <- gamma1[j] + epsilon_psi[j,t]
      logit(psiCPA.est[j,t]) <- gamma2[j] + epsilon_psi[j,t]
  }}
  
# for (i in 1:n_ind){
#   for (t in (f[i]+1):n_occ){
# 
#     a_PA[pod[i],i,t-1] <- equals(pod[i],z[i,t],1) #alive pre-adult matrix
#     a_A[pod[i],i,t-1] <- equals(pod[i],z[i,t],2) #alive adult matrix
#     
#     }
  # }

for (i in 1:n_doubtful){
  for (t in (f[i]+1):n_occ){
  Doubtful_C[i,t]<-equals(z[i,t],1) #alive pre-adult matrix
  Doubtful_PA[i,t]<-equals(z[i,t],2)
  Doubtful_A[i,t]<-equals(z[i,t],3)
  }}

for (i in (n_doubtful+1):n_ind){
  for (t in (f[i]+1):n_occ){
  Dusky_C[i,t]<-equals(z[i,t],1)
  Dusky_PA[i,t]<-equals(z[i,t],2)
  Dusky_A[i,t]<-equals(z[i,t],3)
    }}
  
# for (t in 1:n_occ-1){
#   N_Doubtful_PA[t] <- sum(Doubtful_PA[,t])
#   N_Doubtful_A[t] <- sum(Doubtful_A[,t])
#   N_Dusky_PA[t] <- sum(Dusky_PA[,t])
#   N_Dusky_A[t] <- sum(Dusky_A[,t])
# }
  
# PARAMETERS	----
  # probabilities for each initial state
  ps.init[1] <- piC # prob initial state C
  ps.init[2] <- (1 - piC)/2#piPA # prob. of being in initial state alive pre-adult
  ps.init[3] <- (1 - piC)/2#1 - piC - piPA # probs of being initial state alive adult 
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
    
    po.init[4,i,1] <- 1 #nondetected given dead
    po.init[4,i,2] <- 0
    po.init[4,i,3] <- 0
    po.init[4,i,4] <- 0
    
    for (t in 1:(n_occ-1)){
  # OBSERVATION PROCESS: prob of y(t) given z(t) [z(t),i,t,y(t)]----
  # detection
  po[1,i,t,1] <- 1 - (pC[i,t] * eff[pod[i],t+1]) # prob of nondetection given calf
  po[1,i,t,2] <- pC[i,t] * eff[pod[i],t+1] # prob of calf given detected as calf
  po[1,i,t,3] <- 0
  po[1,i,t,4] <- 0
  
  po[2,i,t,1] <- 1 - (pPA[i,t] * eff[pod[i],t+1]) # prob of nondetection given PA
  po[2,i,t,2] <- 0 # prob of PA given detected as calf
  po[2,i,t,3] <- pPA[i,t] * eff[pod[i],t+1] # prob detected as adult given it is an adult
  po[2,i,t,4] <- 0
  
  po[3,i,t,1] <- 1 - (pA[i,t] * eff[pod[i],t+1]) # prob of nondetection given adult
  po[3,i,t,2] <- 0 # prob of adult given detected as calf
  po[3,i,t,3] <- 0 # prob of adult given detected as pre-adult
  po[3,i,t,4] <- pA[i,t] * eff[pod[i],t+1] # prob adult given is adult
  
  po[4,i,t,1] <- 1 # prob of nondetection given it is dead
  po[4,i,t,2] <- 0 # prob of C given it is dead
  po[4,i,t,3] <- 0 # prob of PA given it is dead
  po[4,i,t,4] <- 0 # prob of A given is dead
  
  # STATE PROCESS: prob of z(t+1) given z(t), [z(t),i,t,z(t+1)] ----
  # survival
  ps[1,i,t,1] <- phiC[i,t] * (1 - psiCPA[i,t])
  ps[1,i,t,2] <- phiC[i,t] * psiCPA[i,t]
  ps[1,i,t,3] <- 0
  ps[1,i,t,4] <- 1 - phiC[i,t]
  
  ps[2,i,t,1] <- 0
  ps[2,i,t,2] <- phiPA[i,t] * (1 - psiPAA[i,t])
  ps[2,i,t,3] <- phiPA[i,t] * psiPAA[i,t]
  ps[2,i,t,4] <- 1 - phiPA[i,t]
  
  ps[3,i,t,1] <- 0
  ps[3,i,t,2] <- 0
  ps[3,i,t,3] <- phiA[i,t]
  ps[3,i,t,4] <- 1 - phiA[i,t]
    
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
  piC ~ dunif(0,1)
  #piPA ~ dbeta(1,1)

  for (j in 1:2){
    for (t in 1:(n_occ-1)){
        epsilon_p[j,t] ~ dnorm(0,tau[1])
        epsilon_phi[j,t] ~ dnorm(0,tau[2])
        epsilon_psi[j,t] ~ dnorm(0,tau[3])
    }}
  
  for (j in 1:2){
    gamma1[j] ~ dt(0,1,3)
    gamma2[j] ~ dt(0,1,3)
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
                n_doubtful = n_doubtful,
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

z <- as.matrix(x.init)
z[1,1]
(obs_ch_mat+1)[1,1]
###

inits<-function(){list(z = z,
                      beta1 = rnorm(2, 0, 1),
                      alpha1 = rnorm(2, 0, 1),
                      beta2 = rnorm(2, 0, 1),
                      alpha2 = rnorm(2, 0, 1),
                      beta3 = rnorm(2, 0, 1),
                      alpha3 = rnorm(2, 0, 1),
                      gamma1 = rnorm(2, 0, 1),
                      gamma2 = rnorm(2, 0, 1),
                      tau = runif(3, 1, 5)
)}

#####
rjags::load.module("glm")
# fit the model:
R2OpenBUGS::write.model(model,con="multievent_ageclass_calf.txt") # write JAGS model code to file
Sys.time()
m1 = rjags::jags.model("multievent_ageclass_calf.txt", data = mcmc.data, inits = inits, n.chains = 3, n.adapt = 5000) #5000
update(m1) # another burn in

#### Specify the parameters to be monitored ----
parameters <- c("pPA.est","pA.est","phiPA.est","phiA.est","psiPAA.est","alpha1","alpha2","beta1","beta2","gamma","sigma2","piC",
                "Doubtful_A","Dusky_A","Doubtful_PA","Dusky_PA")
                #"N_Doubtful_A","N_Dusky_A","N_Doubtful_PA","N_Dusky_PA")

out1 = coda.samples(model = m1, variable.names = parameters, n.iter = 25000) #20000
#####
out1_df = posterior::as_draws_df(out1)
Sys.time()
saveRDS(out1_df, file = paste0("./data/multi-event_ageclass_",Sys.Date(),".rds"))
Sys.time()

## Results ----
# F = 1, M = 2
#Doubtful = 1, Dusky = 2

date = "2025-01-09" # pod dependent, time varying random effects for p, phi, and psi

results_in_age<-readRDS(paste0("./data/multi-event_ageclass_",date,".rds"))

library(posterior)
#subset because z params are for every individual and occasion
#this takes ### to run
Sys.time()
z_est<-subset_draws(results_in_age, c("Doubtful_A","Dusky_A","Doubtful_PA","Dusky_PA"))
results_z<-as.data.frame(summary(z_est))
Sys.time()

saveRDS(results_z, file = paste0("./data/multi-event_ageclass_z_results",date,".rds"))

results_z<-readRDS(paste0("./data/multi-event_ageclass_z_results",date,".rds"))

sampling_periods_long<-ID_per_day_all%>%
  filter(IDperDay > 0)%>%
  distinct(year_season_code)%>%
  arrange(year_season_code)%>%
  mutate(occasion = 2:(n()+1))

results_z_age<-results_z%>%
  mutate(age = case_when(
    grepl("_PA", variable) ~ "Pre-adult",
    grepl("_A", variable) ~ "Adult"
  ),
  occasion = as.numeric(stringr::str_extract(variable,
                                  pattern = "(?<=\\,).*(?=\\])")),
  Pod = case_when(
    grepl("Doubtful", variable) ~ "DOUBTFUL",
    grepl("Dusky", variable) ~ "DUSKY"
  ))%>%
  left_join(sampling_periods_long, by = "occasion")%>%
  group_by(Pod, age, year_season_code)%>%
  mutate(n = sum(median))%>%
  distinct(Pod, age, year_season_code, n)%>%
  group_by(Pod, year_season_code)%>%
  mutate(sum = sum(n))

results_z_age%>%
  group_by(Pod, year_season_code)%>%
  mutate(total = sum(n))%>%
  group_by(Pod)%>%
  mutate(min = min(total), max = max(total), median = median(total), q5 = quantile(total, 0.05), q95 = quantile(total, 0.95))%>%
  distinct(Pod, min, q5, median, q95, max)

## doesn't include zeros
observed<-everyone%>%
  ungroup()%>%
  mutate(ageclass = case_when(
    ageclass != "A" ~ "Pre-adult",
    TRUE ~ "Adult"
  ))%>%
  #distinct(POD, year_season_code, ageclass)%>%
  group_by(POD, year_season_code, ageclass)%>%
  tally()%>%
  group_by(POD, year_season_code)%>%
  mutate(sum = sum(n),
         Pod = POD)

observed%>%
  group_by(POD, year_season_code)%>%
  mutate(total = sum(n))%>%
  group_by(POD)%>%
  mutate(min = min(total), max = max(total), median = median(total))%>%
  distinct(POD, min, max, median)

library(ggplot2)

ggplot()+
  #geom_col(results_z_age, mapping = aes(x = as.numeric(year_season_code), y = n, fill =  age))+
  geom_point(ID_per_day_all%>%dplyr::rename("Pod" = "POD"), mapping = aes(x = as.numeric(year_season_code), y = n.y), color = "red")+
  geom_line(ID_per_day_all%>%dplyr::rename("Pod" = "POD"), mapping = aes(x = as.numeric(year_season_code), y = n.y), color = "red")+
  geom_point(results_z_age, mapping = aes(x = as.numeric(year_season_code), y = sum), color = "blue")+
  geom_line(results_z_age, mapping = aes(x = as.numeric(year_season_code), y = sum), color = "blue")+
  facet_wrap(~Pod, scales = "free")+
  scale_x_continuous(breaks = c(2005:2024))+
  theme_bw()+
  theme(axis.text.x=element_text(angle=90, vjust=0.5))+
  xlab(expression("Dolphin year (01Sep"[y-1]~"–31Aug"[y]~")"))+
  ylab("# individuals")

prop<-results_z_age%>%
  group_by(Pod, year_season_code, age)%>%
  mutate(prop = n/sum)

prop%>%filter(Pod == "DOUBTFUL")%>%
  group_by(age)%>%
  dplyr::summarise(median = median(prop), min = min(prop), max = max(prop))

prop%>%filter(Pod == "DUSKY" & year_season_code >= 2016)%>%
  group_by(age)%>%
  dplyr::summarise(median = median(prop), min = min(prop), max = max(prop))

ggplot()+
  geom_col(prop, mapping = aes(x = year_season_code, y = prop, fill = age))+
  facet_wrap(~Pod)

Doubtful_z<-results_z_age%>%
  filter(Pod == "DOUBTFUL")

Dusky_z<-results_z_age%>%
  filter(Pod == "DUSKY")

compare<-Doubtful_z%>%
  left_join(Dusky_z, by = c("age","year_season_code"))

ggplot(compare, aes(x = sum.y, y = sum.x, color = year_season_code))+
  geom_point()+
  xlim(c(100,130))+
  ylim(c(45,75))+
  geom_smooth(method = "glm")

#####

main_params<-subset_draws(results_in_age, c("pPA.est","pA.est","phiPA.est","phiA.est","psiPAA.est","alpha1","alpha2","beta1","beta2","gamma","sigma2"))
results_age<-as.data.frame(summary(main_params))

Sys.time()

#alpha1 capture pod, alpha2 capture sex (adults only), beta1 survival pod, beta2 survival sex (adults only), gamma pod transition
# pod = j, sex = k
#traceplots
bayesplot::mcmc_trace(main_params, pars = c("alpha1[1]","alpha1[2]","beta1[1]","beta1[2]","gamma[1]","gamma[2]")) #"alpha2[1]","alpha2[2]","beta2[1]","beta2[2]",
#alpha1 = capture between adults, alpha2 = pre-adults
bayesplot::mcmc_areas(main_params, pars = c("alpha1[1]","alpha1[2]","alpha2[1]","alpha2[2]"),prob = 0.9)
#beta1 = survival adults, beta2 = pre-adults
bayesplot::mcmc_areas(main_params, pars = c("beta1[1]","beta1[2]", "beta2[1]","beta2[2]"),prob = 0.9)
#transition between pods
bayesplot::mcmc_areas(main_params, pars = c("gamma[1]","gamma[2]"),prob = 0.9)
# 
bayesplot::mcmc_areas(main_params, pars = c("sigma2[1]","sigma2[2]","sigma2[3]"))


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
  mutate(calfyr_season = c(rep(names(long_samp_ch_all)[2:(n_occ)], each = 2),rep(names(long_samp_ch_all)[2:(n_occ)], each = 2)), # skip 2006.07
         #pod = rep(rep(c("DOUBTFUL","DUSKY"), each = 1), (n_occ-1)*2))%>%
         #Sex = c(rep(rep(c("Female","Male"), each = 1), (n_occ-1)*2), rep(NA,(n_occ-1)*2)),
         Pod = rep(rep(c("DOUBTFUL","DUSKY"), each = 1), (n_occ-1)*2))%>%
  mutate(Season = as.factor(case_when(
    grepl(".33", calfyr_season) ~ "Summer",
    grepl(".67", calfyr_season) ~ "Winter",
    TRUE ~ "Spring")),
    Ageclass = case_when(
      grepl("phiA",variable) == TRUE ~ "Adult",
      grepl("phiPA",variable) == TRUE ~ "Pre-adult"
    ))%>%
  left_join(ID_per_day_all, by = c("calfyr_season" = "year_season_code", "Pod" = "POD","Season" = "season"))%>%
  mutate(eff = case_when(
    IDperDay != 0 ~ "effort",
    TRUE ~ "no effort"))%>%
  filter(eff != "no effort")%>%
  mutate(area = "All areas")

library(ggplot2)

ggplot()+
  geom_point(results_phi_in_age, mapping = aes(x = as.numeric(calfyr_season), y = median, shape = Season, color = Ageclass), size = 3, alpha = 0.8)+
  geom_errorbar(results_phi_in_age, mapping = aes(x = as.numeric(calfyr_season),ymin = q5, ymax = q95, color = Ageclass), size = 1, alpha = 0.8)+
  facet_wrap(~Pod)+
  theme_bw()+
  theme(legend.position = "bottom")+
  scale_x_continuous(breaks = c(2005:2024))+
  theme(axis.text.x=element_text(angle=90, vjust=0.5))+
  xlab(expression("Dolphin year (01Sep"[y-1]~"–31Aug"[y]~")"))+
  ylab(expression('Survival probability,' *phi))

ggsave(paste0('./figures/age_phi_pod_',date,'.png'), dpi = 300, width = 300, height = 175, units = "mm")

## capture prob # not identifiable at first occasion

results_p_in_age<-p%>%
  mutate(calfyr_season = rep(rep(names(long_samp_ch_all)[3:(n_occ+1)], each = 2),2), # skip 2006.07
         #Sex = c(rep(rep(c("Female","Male"), each = 1), (n_occ-1)*2), rep(NA,(n_occ-1)*2)),
         Pod = rep(rep(c("DOUBTFUL","DUSKY"), each = 1), (n_occ-1)*2))%>%
  mutate(Season = as.factor(case_when(
    grepl(".33", calfyr_season) ~ "Summer",
    grepl(".67", calfyr_season) ~ "Winter",
    TRUE ~ "Spring")),
  Ageclass = case_when(
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
  geom_point(results_p_in_age, mapping = aes(x = as.numeric(calfyr_season), y = median, shape = Season, color = Ageclass), size = 3, alpha = 0.8)+
  geom_errorbar(results_p_in_age, mapping = aes(x = as.numeric(calfyr_season),ymin = q5, ymax = q95, color = Ageclass), size = 1, alpha = 0.8)+
  facet_wrap(~Pod)+
  theme_bw()+
  scale_x_continuous(breaks = c(2005:2024))+
  theme(axis.text.x=element_text(angle=90, vjust=0.5))+
  theme(legend.position = "bottom")+
  xlab(expression("Dolphin year (01Sep"[y-1]~"–31Aug"[y]~")"))+
  ylab(expression('Capture probability, p'))

ggsave(paste0('./figures/age_p_pod_',date,'.png'), dpi = 300, width = 300, height = 175, units = "mm")

## transition probability ----
results_psi_in_age<-psi%>%
  mutate(calfyr_season = rep(names(long_samp_ch_all)[2:(n_occ)], each = 2), # skip 2006.07
         #Sex = c(rep(rep(c("Female","Male"), each = 1), (n_occ-1)*2), rep(NA,(n_occ-1)*2)),
         Pod = rep(rep(c("DOUBTFUL","DUSKY"), each = 1), (n_occ-1)))%>%
  mutate(Season = as.factor(case_when(
    grepl(".33", calfyr_season) ~ "Summer",
    grepl(".67", calfyr_season) ~ "Winter",
    TRUE ~ "Spring")))%>%
  left_join(ID_per_day_all, by = c("calfyr_season" = "year_season_code", "Pod" = "POD", "Season" = "season"))%>%
  mutate(eff = case_when(
    IDperDay != 0 ~ "effort",
    TRUE ~ "no effort"))%>%
  filter(eff != "no effort")%>%
  mutate(area = "All areas")

ggplot(results_psi_in_age, aes(x = as.numeric(calfyr_season), y = median))+
  geom_errorbar(aes(ymin = q5, ymax = q95), size = 1, alpha = 0.8)+
  geom_point(aes(shape = Season),size = 3, alpha = 0.8)+
  #geom_line(aes(linetype = pod))+
  #ylim(c(0.0,1.0))+
  facet_wrap(~Pod)+
  theme_bw()+
  scale_x_continuous(breaks = c(2005:2024))+
  theme(axis.text.x=element_text(angle=90, vjust=0.5))+
  theme(legend.position = "bottom")+
  xlab(expression("Dolphin year (01Sep"[y-1]~"–31Aug"[y]~")"))+
  ylab(expression('Transition probability, ' *psi))
