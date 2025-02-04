
library(RPresence)
library(jagsUI)
library(rjags)
library(dplyr)

# data ----
long_samp_ch_all<-readRDS("./data/long_samp.RDS") #skip 2007.67
eff_it<-long_samp_ch_all%>%
  dplyr::select(-POD)%>%
  as.matrix()

eff_it[eff_it > 0]<-1
eff_mat<-unname(eff_it)

age_ch<-readRDS("./data/age_ch.RDS")%>%arrange(ind)
age_ch$ind
age_it<-age_ch%>%dplyr::select(-ind,-POD,-NAME,-`2007.67`)%>%
  as.matrix()
age_ch_mat<-unname(age_it)

female_ch<-readRDS("./data/female_ch.RDS")%>%ungroup()%>%arrange(ind)#%>%group_by(POD)%>%slice_head(n = 40)
female_ch%>%dplyr::select(NAME,POD,pod_ch)

female_ch$pod_ch
female_ch$ind
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


obs_ch_mat[(n_doubtful+1):n_ind,1:6]<-0


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
  # 3 = detected as female of breeding age NOT with calf
  # 4 = detected as female in close association with a calf
  
  # STATES -----
  # 1 = female of non-breeding age alive
  # 2 = female of breeding age alive without a calf
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
      po[1,i,t,1] <- 1 - (pN[i,t] * eff[pod[i],t+1]) # prob of non-detection given non-breeding age
      po[1,i,t,2] <- pN[i,t] * eff[pod[i],t+1] # prob of non-breeding age given non-breeding age
      po[1,i,t,3] <- 0
      po[1,i,t,4] <- 0
      
      po[2,i,t,1] <- 1 - (pB[i,t] * eff[pod[i],t+1]) # prob of non detection given breeding age/adult
      po[2,i,t,2] <- 0 # prob of A given detected as PA
      po[2,i,t,3] <- pB[i,t] * eff[pod[i],t+1] # prob detected as breeding age/adult w/o calf given it is
      po[2,i,t,4] <- 0
      
      po[3,i,t,1] <- 1 - (pW[i,t] * eff[pod[i],t+1]) # prob of non detection given with calf
      po[3,i,t,2] <- 0 # prob of non-breeding age given it is with calf
      po[3,i,t,3] <- 0 # prob of breeding age/adult given it is with calf
      po[3,i,t,4] <- pW[i,t] * eff[pod[i],t+1]
      
      po[4,i,t,1] <- 1 # prob of non detection given it is dead
      po[4,i,t,2] <- 0 
      po[4,i,t,3] <- 0 
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
  
  #Abundance
    #Doubtful
  for (i in 1:n_doubtful){
    for (t in (f[i]+1):n_occ){
      Doubtful_aN[i,t]<-equals(z[i,t],1) #alive non-adult
      Doubtful_aA[i,t]<-equals(z[i,t],2) #alive adult
      Doubtful_aW[i,t]<-equals(z[i,t],3) #alive adult with calf
    }
    for (t in 1:f[i]){
      Doubtful_aN[i,t]<-0 # zeros for everything else
      Doubtful_aA[i,t]<-0
      Doubtful_aW[i,t]<-0 
    }}
    #Dusky
  for (i in (n_doubtful+1):n_ind){
    for (t in (f[i]+1):n_occ){
      Dusky_aN[i,t]<-equals(z[i,t],1) #alive non-adult
      Dusky_aA[i,t]<-equals(z[i,t],2) #alive adult
      Dusky_aW[i,t]<-equals(z[i,t],3) #alive adult with calf
    }
  for (t in 1:f[i]){
      Dusky_aN[i,t]<-0 # zeros for everything else
      Dusky_aA[i,t]<-0
      Dusky_aW[i,t]<-0 
    }}

  for (t in 1:n_occ){
    Doubtful_N_N[t] <- sum(Doubtful_aN[,t])
    Doubtful_N_A[t] <- sum(Doubtful_aA[,t])
    Doubtful_N_W[t] <- sum(Doubtful_aW[,t])
    Dusky_N_N[t] <- sum(Dusky_aN[(n_doubtful+1):n_ind,t])
    Dusky_N_A[t] <- sum(Dusky_aA[(n_doubtful+1):n_ind,t])
    Dusky_N_W[t] <- sum(Dusky_aW[(n_doubtful+1):n_ind,t])
  }

  
  for (i in 1:n_ind){  # for each indiv
    
    # estimated probabilities of initial states are the proportions in each state at first capture occasion
    z[i,f[i]] ~ dcat(ps.init[1:4])
    y[i,f[i]] ~ dcat(po.init[z[i,f[i]],i,1:4])
    
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
  
  # for (k in 1:2){
  #   kappa[k] ~ dt(0,1,3)
  # }
  # 
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
                  age = age_ch_mat,
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

#NAs for observed occasions
# for (i in 1:n_ind){
#   for (t in 1:n_occ){
#     for (j in 1:2){
#       if (eff_mat[j,t] == 1 & pod_ch[i] == j){
#         x.init[i,t] <- NA
#         
#       }}}}

z.inits <- as.matrix(x.init)

inits<-function(){list(z = z.inits,
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
                       #kappa = rnorm(2,0,1),
                       tau = runif(3, 1, 5)
)}

#####
rjags::load.module("glm")
# fit the model:
R2OpenBUGS::write.model(model,con="multi-event_repro.txt") # write JAGS model code to file
Sys.time()
m1 = rjags::jags.model("multi-event_repro.txt", data = mcmc.data, inits = inits, n.chains = 3, n.adapt = 5000)
update(m1) # another burn in
#m1$state()
#### Specify the parameters to be monitored ----
parameters <- c("pN.est","pB.est","pW.est","phiN.est","phiB.est","phiW.est",
                "psiNB.est","psiNW.est","psiBW.est","psiWB.est",
                "alpha1","alpha2","alpha3","beta1","beta2","beta3",
                "gamma1","gamma2","gamma3","gamma4","sigma2",
                "Doubtful_N_N", "Doubtful_N_A", "Doubtful_N_W",
                "Dusky_N_N", "Dusky_N_A", "Dusky_N_W")
                #"z")

out1 = coda.samples(model = m1, variable.names = parameters, n.iter = 50000)
#####
out1_df = posterior::as_draws_df(out1)
Sys.time()
saveRDS(out1_df, file = paste0("./data/multi-event_repro_",Sys.Date(),".rds"))
Sys.time()
