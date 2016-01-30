falcon.data.2 <- list(y=falcon$wingspan,
  male=falcon$male,
  N=100)
  
  cat("
    model
    {
    # priors
    mu.f ~ dnorm(0, 0.001)
    delta ~ dnorm(0, 0.001)
    tau <- 1/(sigma*sigma)
    sigma ~ dunif(0,100)
    
    # likelihood
    for(i in 1:N)
    {
    y[i] ~ dnorm(mu[i], tau)
    mu[i] <- mu.f + delta*male[i]
    }
    
    # derived quantity
    mu.m <- mu.f + delta
    }    
    ", file="t-test2.bug")
  
  model.fit <- jags(data=falcon.data.2, 
    model.file="t-test2.bug",
    parameters.to.save=c("mu.f", "mu.m", "sigma", "delta"),
    n.chains=3,
    n.iter=2000,
    n.burnin=1000,
    DIC=FALSE)
  X11()
  plot(as.mcmc(model.fit))
  model.fit

  
#Rstan and others installation
Sys.setenv(MAKEFLAGS = "-j4") 
install.packages("rstan", dependencies = TRUE)  
install.packages("rstanarm")
library (rstan)
#set up processors and ram for use
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

library (rstanarm)
mm <- stan_glm (mpg ~ ., data=mtcars, prior=normal (0, 8))
mm  #===> Results
pp_check (mm, "dist", nreps=30)
summary (mm)

library(shinystan) # still do not know how to use this :)
