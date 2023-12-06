# # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# CLOSING THE SECTIONS PROVIDES AN OVERVIEW OF THE SCRIPT #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # #



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# THIS FILE BUILDS ON THE simulated_data.R AND estimation.R FILE#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #



# general prep ####
# clean workspace
rm(list = ls())


# clean garbage
gc()


# clear graphics device
# dev.off()


# set decimals to digits instead of scientific
options(scipen = 999)


# load packages
library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

library(bayesplot)
library(ggplot2)
library(rethinking)



# user-defined functions ####
# mean squared error
MSE <- function(est, sim, X, Y, I) {
  
  ASE <- 0
  
  for (y in 1:Y) {
    for (x in 1:X) {
      
      if (I[x,y] == 1) {
        ASE <- ASE + (as.numeric(est[x,y]) - as.numeric(sim[x,y]))^2
      }
      
    }
  }
  
  N <- sum(I)
  
  MSE <- round(ASE / N, digits = 4)
  
  return(MSE)
  
}


# 89% HPDI for mean squared error
HPDI_MSE <- function(est, sim, X, Y, I, iter) {
  
  MSE <- rep(0, times = iter)
  
  for(i in 1:iter) {
    MSE[iter] <- MSE(est[i,,], sim, X, Y, I)
  }
  
  HPDI_MSE <- round(HPDI(MSE), digits = 4)
  
  return(HPDI_MSE)
  
}



# evaluation prep ####
# load fit_model_sim
# load fit_model_sim
fit_model_sim <- readRDS("data/fit_m1_v1_sim_clean_data.rds")

# extract simulations
params_model_sim <- rstan::extract(fit_model_sim)


# load fit_model
fit_model <- readRDS("results/fit_m1_v1_clean_data.rds")

# extract samples
params_model <- rstan::extract(fit_model)


# number of total post-warmup iterations
iter <- iter_per_chain / 2 * 4



# fit - latent driver ability ####
# extract mu_D posterior mean
mu_D_pm <- matrix(data = NA, nrow = N, ncol = Q)

for (n in 1:N) {
  for (t in 1:Q) {
    
    mu_D_pm <-
      get_posterior_mean(fit_model,
                         pars = paste("mu_D[",n,",",t,"]", sep = ""))[5]
    
  }
}


# extract estimated mu_D
mu_D_est <- params_model$mu_D


# extract simulated mu_D
mu_D_sim_temp <- params_model_sim$mu_D

mu_D_sim <- mu_D_sim_temp[40,,]


# figures
# mu_D posterior mean ( violet ) vs simulated mu_D ( orange )
par(mfrow = c(5,2))
for (n in 1:N) {
  
  plot(mu_D_sim[n,],
       ylim = c(-5, 15),
       type="l",
       col = "orange",
       main = paste("driver", n),
       xlab = "qualifier",
       ylab = "mu_D",
       xaxt = "n",
       yaxt = "n")
  axis(side = 1, at = c(1,20,39,60,80,101,122,139,160))  # TODO first race
  axis(side = 2, at = c(-5, 5, 15), las = 1)
  
  lines(mu_D_pm[n,], col = "blueviolet")
  
}
par(mfrow = c(1,1))


# mean squared error
# mu_D posterior mean vs simulated mu_D
MSE(mu_D_pm, mu_D_sim, N, Q)


# 89% HPDI for mean squared error
# estimated mu_D vs simulated mu_D
HPDI_MSE(mu_D_est, mu_D_sim, N, Q, I_1, iter)


# 89% HPDI for mu_D
par(mfrow = c(5,2))
for (n in 1:N) {
  
  mu_D_U <- c()
  mu_D_L <- c()
  for (t in 1:Q) {
    
    mu_D_U_temp <- HPDI(as.numeric(mu_D_est[,n,t]))[2]
    mu_D_U <- c(mu_D_U, mu_D_U_temp)
    
    mu_D_L_temp <- HPDI(as.numeric(mu_D_est[,n,t]))[1]
    mu_D_L <- c(mu_D_L, mu_D_L_temp)
    
  }
  
  x <- 1:160
  
  plot(x = x,
       y = mu_D_U,
       ylim = c(-5, 15),
       type="l",
       col = "blueviolet",
       main = paste("driver", n),
       xlab = "qualifier",
       ylab = "89% HPDI mu_D",
       xaxt = "n",
       yaxt = "n")
  axis(side = 1, at = c(1,20,39,60,80,101,122,139,160))  # TODO first race
  axis(side = 2, at = c(-5, 5, 15), las = 1)
  
  lines(x = x, mu_D_L, col = "blueviolet")
  
  polygon(x = c(x, rev(x)),
          y = c(mu_D_U, rev(mu_D_L)),
          col = "blueviolet",
          lty = 0)
  
}
par(mfrow = c(1,1))



# fit - latent constructor ability ####
# extract mu_C posterior mean
mu_C_pm <- matrix(data = NA, nrow = K, ncol = Q)

for (k in 1:K) {
  for (t in 1:Q) {
    
    mu_C_pm <-
      get_posterior_mean(fit_model,
                         pars = paste("mu_C[",k,",",t,"]", sep = ""))[5]
    
  }
}


# extract estimated mu_D
mu_C_est <- params_model$mu_C


# extract simulated mu_D
mu_D_sim_temp <- params_model_sim$mu_D

mu_D_sim <- mu_D_sim_temp[40,,]


# figures
# mu_C posterior mean ( green ) vs simulated mu_C ( orange )
par(mfrow = c(5,2))
for (k in 1:K) {
  
  plot(mu_C_sim[k,],
       ylim = c(-5, 15),
       type="l",
       col = "orange",
       main = paste("constructor", k),
       xlab = "qualifier",
       ylab = "mu_C",
       xaxt = "n",
       yaxt = "n")
  axis(side = 1, at = c(1,20,39,60,80,101,122,139,160))  # TODO first race
  axis(side = 2, at = c(-5, 5, 15), las = 1)
  
  lines(mu_C[k,], col = "mediumspringgreen")
  
}
par(mfrow = c(1,1))


# mean squared error
# mu_C posterior mean vs simulated mu_C
MSE(mu_C_pm, mu_C_sim, K, Q)


# 89% HPDI for mean squared error
# estimated mu_C vs simulated mu_C
HPDI_MSE(mu_C_est, mu_C_sim, K, Q, I_3, iter)


# 89% HPDI for mu_C
par(mfrow = c(5,2))
for (k in 1:K) {
  
  mu_C_U <- c()
  mu_C_L <- c()
  for (t in 1:Q) {
    
    mu_C_U_temp <- HPDI(as.numeric(params_model_1$mu_C[,k,t]))[2]
    mu_C_U <- c(mu_C_U, mu_C_U_temp)
    
    mu_C_L_temp <- HPDI(as.numeric(params_model_1$mu_C[,k,t]))[1]
    mu_C_L <- c(mu_C_L, mu_C_L_temp)
    
  }
  
  x <- 1:160
  
  plot(x = x,
       y = mu_C_U,
       ylim = c(-5, 15),
       type="l",
       col = "mediumspringgreen",
       main = paste("constructor", k),
       xlab = "qualifier",
       ylab = "89% HPDI mu_C",
       xaxt = "n",
       yaxt = "n")
  axis(side = 1, at = c(1,21,41,61,81,101,121,141,160))
  axis(side = 2, at = c(-5, 5, 15), las = 1)
  
  lines(x = x, mu_C_L, col = "mediumspringgreen")
  
  polygon(x = c(x, rev(x)),
          y = c(mu_C_U, rev(mu_C_L)),
          col = "mediumspringgreen",
          lty = 0)
  
}
par(mfrow = c(1,1))



# insights - latent driver vs constructor ability
# placeholder







