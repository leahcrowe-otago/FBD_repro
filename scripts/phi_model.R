## model ----

model<-function(){
  
  # constraints
  for (i in 1:n_ind){
    for (t in 1:(n_occ-1)){
      
      logit(p[i,t]) <-  alpha[pod[i]] + epsilon[2,pod[i],t]
      logit(phi[i,t]) <- beta[pod[i]] + epsilon[1,pod[i],t]
      
    }
  }
  
  for (t in 1:(n_occ-1)){
    for (j in 1:2){ # pod
      logit(p.est[j,t]) <- alpha[j] + epsilon[2,j,t]    
      logit(phi.est[j,t]) <- beta[j] + epsilon[1,j,t]
      
    }}
  
  # priors
  
  for (t in 1:(n_occ-1)){
    for (j in 1:2){
      epsilon[1,j,t] ~ dnorm(0, tau[1])
      epsilon[2,j,t] ~ dnorm(0, tau[2])
    }}
  
  for (j in 1:2){
    alpha[j] ~ dt(0,1,3)
    beta[j] ~ dt(0,1,3)
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

## global params ----
mcmc.params<-c("p.est","phi.est","beta","alpha","sigma")

## global inits ----
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
                            beta = runif(2, 0, 1),
                            alpha1 = runif(2, 0, 1),
                            alpha2 = rnorm(3, 0, 1),
                            tau = runif(2, 1, 5)
)}

## write model ----
R2OpenBUGS::write.model(model,con="pod_surv_cap.txt") # write JAGS model code to file
