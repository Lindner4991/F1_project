# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


# closing the sections provides an overview of the script


# this file builds on the simulated_data.R and estimation.R file


# required data files:
# fit_m___.rds
# fit_m___sim___.rds
# where ___ is a placeholder


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #



# general prep ####
# set working directory
setwd("C:/Users/Diiim/Documents/job_uni/Master/Thesis/F1_project")


# clean workspace
rm(list = ls())


# clean garbage
gc()


# clear graphics device
# dev.off()


# set decimals to digits instead of scientific
options(scipen = 999)


# load packages
library(todor)
# todor::todor_file("R/evaluation.R")

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


# 89% HDI for mean squared error
HDI_MSE <- function(est, sim, X, Y, I, iter) {
  
  mse <- rep(0, times = iter)
  
  for(i in 1:iter) {
    mse[i] <- MSE(est[i,,], sim, X, Y, I)
  }
  
  HDI_MSE <- round(HPDI(mse), digits = 4)

  return(HDI_MSE)
  
}



# evaluation prep ####
# load fit_model_sim
fit_model_sim <- readRDS("data/fit_m1_v1_sim_missing_data.rds")  # TODO data file

# extract simulations
params_model_sim <- rstan::extract(fit_model_sim)


# load fit_model
fit_model <- readRDS("results/fit_m1_v1_missing_data.rds")  # TODO data file

# extract samples
params_model <- rstan::extract(fit_model)


# number of total post-warmup iterations
iter <- iter_per_chain / 2 * 4



# fit - varsigma_D, varsigma_C, and cut points ####
# posterior mean and median
summary(fit_model,
        probs = c(0.5),
        pars = c("varsigma_D",
                 "varsigma_C",
                 "gamma[2]",
                 "gamma[3]",
                 "gamma[4]",
                 "gamma[5]",
                 "gamma[6]",
                 "gamma[7]",
                 "gamma[8]",
                 "gamma[9]",
                 "gamma[10]",
                 "gamma[11]",
                 "gamma[12]",
                 "gamma[13]",
                 "gamma[14]",
                 "gamma[15]",
                 "gamma[16]",
                 "gamma[17]",
                 "gamma[18]",
                 "gamma[19]",
                 "gamma[20]"))


# posterior density 89% HDI
# varsigma_D
round(HPDI(as.numeric(params_model$varsigma_D)), digits = 4)

# varsigma_C
round(HPDI(as.numeric(params_model$varsigma_C)), digits = 4)

# cut points
for (j in 2:(J-2)) {
  print(round(HPDI(as.numeric(params_model$gamma[,j])), digits = 4))
}


# histogram
# posterior density varsigma_D
par(mfrow = c(1,3))
hist(params_model$varsigma_D,
     col = "blueviolet",
     border = FALSE,
     main = "varsigma_D",
     xlab = "")
abline(v = 0.04, lwd = 2, col = "orange")  # TODO real data

# histogram
# posterior density varsigma_C
hist(params_model_1$varsigma_C,
     col = "mediumspringgreen",
     border = FALSE,
     main = "varsigma_C",
     xlab = "")
abline(v = 0.16, lwd = 2, col = "orange")  # TODO real data

# histogram
# posterior density cut points
for (j in 2:(J-2)) {
  hist(params_model_1$gamma[,j],
       col = "deeppink1",
       border = FALSE,
       main = paste("gamma_", j, sep=""),
       xlab = "")
  abline(v = j-1, lwd = 2, col = "orange")  # TODO real data
}
par(mfrow = c(1,1))



# fit - qualifier/race rank ####
# extract averaged predicted rank
# ( averaged over post-warmup iterations )
R_pred_avg <- matrix(data = NA, nrow = N, ncol = Q)

for (n in 1:N) {
  for (t in 1:Q) {
    
    R_pred_avg_temp <-
      get_posterior_mean(fit_model,
                         pars = paste("R_pred[",n,",",t,"]", sep = ""))[5]
    
    R_pred_avg[n,t] <- round(R_pred_avg_temp, digits = 0)
    
  }
}


# extract predicted rank
R_pred <- params_model$R_pred


# extract simulated rank
R_sim_temp <- params_model_sim$R_sim

R_sim <- R_sim_temp[40,,]


# time series plot
# averaged predicted rank ( pink ) vs simulated rank ( orange )
par(mfrow = c(5,2))
for (n in 1:N) {
  
  plot(R_sim[n,],  # TODO real data
       ylim = c(22, 1),
       type="l",
       col = "orange",
       main = paste("driver", n),  # TODO version 2, real data
       xlab = "qualifier/race",  # TODO real data
       ylab = "rank",
       xaxt = "n",
       yaxt = "n")
  axis(side = 1, at = c(1,19,38,59,79,100,121,138,159))
  axis(side = 2, at = c(22, 11, 1), las = 1)
  
  lines(R_pred_avg[n,], col = "deeppink1")
  
}
par(mfrow = c(1,1))


# time series plot
# averaged predicted rank 89% HDI
par(mfrow = c(5,2))
for (n in 1:N) {
  
  R_pred_U <- c()
  R_pred_L <- c()
  for (t in 1:Q) {
    
    R_pred_U_temp <- HPDI(as.numeric(R_pred[,n,t]))[2]
    R_pred_U <- c(R_pred_U, R_pred_U_temp)
    
    R_pred_L_temp <- HPDI(as.numeric(R_pred[,n,t]))[1]
    R_pred_L <- c(R_pred_L, R_pred_L_temp)
    
  }
  
  x <- 1:Q
  
  plot(x = x,
       y = R_pred_U,
       ylim = c(22, 1),
       type="l",
       col = "deeppink1",
       main = paste("driver", n),  # TODO version 2, real data
       xlab = "qualifier/race",  # TODO real data
       ylab = "rank",
       xaxt = "n",
       yaxt = "n")
  axis(side = 1, at = c(1,19,38,59,79,100,121,138,159))
  axis(side = 2, at = c(22, 11, 1), las = 1)
  
  lines(x = x, R_pred_L, col = "deeppink1")
  
  polygon(x = c(x, rev(x)),
          y = c(R_pred_U, rev(R_pred_L)),
          col = "deeppink1",
          lty = 0)
  
}
par(mfrow = c(1,1))



# fit - latent qualifier/race performance ####
# extract mu_P posterior mean
mu_P_pm <- matrix(data = NA, nrow = N, ncol = Q)

for (n in 1:N) {
  for (t in 1:Q) {
    
    mu_P_pm[n,t] <-
      get_posterior_mean(fit_model,
                         pars = paste("mu_P[",n,",",t,"]", sep = ""))[5]
    
  }
}


# extract estimated mu_P
mu_P_est <- params_model$mu_P


# extract simulated mu_P
mu_P_sim_temp <- params_model_sim$mu_P

mu_P_sim <- mu_P_sim_temp[40,,]


# time series plot
# mu_P posterior mean ( pink ) vs simulated mu_D ( orange )
par(mfrow = c(5,2))
for (n in 1:N) {
  
  plot(mu_P_sim[n,],
       ylim = c(-5, 25),
       type="l",
       col = "orange",
       main = paste("driver", n), # TODO version 2, real data
       xlab = "qualifier/race",  # TODO real data
       ylab = "performance",
       xaxt = "n",
       yaxt = "n")
  axis(side = 1, at = c(1,19,38,59,79,100,121,138,159))
  axis(side = 2, at = c(-5, 10, 25), las = 1)
  
  lines(mu_P_pm[n,], col = "deeppink1")
  
}
par(mfrow = c(1,1))


# mean squared error
# mu_P posterior mean vs simulated mu_P
MSE(mu_P_pm, mu_P_sim, N, Q, I_1)


# mean squared error 89% HDI
# estimated mu_P vs simulated mu_P
HDI_MSE(mu_P_est, mu_P_sim, N, Q, I_1, iter)


# time series plot
# estimated mu_P 89% HDI
par(mfrow = c(5,2))
for (n in 1:N) {
  
  mu_P_U <- c()
  mu_P_L <- c()
  for (t in 1:Q) {
    
    mu_P_U_temp <- HPDI(as.numeric(mu_P_est[,n,t]))[2]
    mu_P_U <- c(mu_P_U, mu_P_U_temp)
    
    mu_P_L_temp <- HPDI(as.numeric(mu_P_est[,n,t]))[1]
    mu_P_L <- c(mu_P_L, mu_P_L_temp)
    
  }
  
  x <- 1:Q
  
  plot(x = x,
       y = mu_P_U,
       ylim = c(-5, 25),
       type="l",
       col = "deeppink1",
       main = paste("driver", n),  # TODO version 2, real data
       xlab = "qualifier/race",  # TODO real data
       ylab = "performance",
       xaxt = "n",
       yaxt = "n")
  axis(side = 1, at = c(1,19,38,59,79,100,121,138,159))
  axis(side = 2, at = c(-5, 10, 25), las = 1)
  
  lines(x = x, mu_P_L, col = "deeppink1")
  
  polygon(x = c(x, rev(x)),
          y = c(mu_P_U, rev(mu_P_L)),
          col = "deeppink1",
          lty = 0)
  
}
par(mfrow = c(1,1))



# fit - latent driver ability ####
# extract mu_D posterior mean
mu_D_pm <- matrix(data = NA, nrow = N, ncol = Q)

for (n in 1:N) {
  for (t in 1:Q) {
    
    mu_D_pm[n,t] <-
      get_posterior_mean(fit_model,
                         pars = paste("mu_D[",n,",",t,"]", sep = ""))[5]
    
  }
}


# extract estimated mu_D
mu_D_est <- params_model$mu_D


# extract simulated mu_D
mu_D_sim_temp <- params_model_sim$mu_D

mu_D_sim <- mu_D_sim_temp[40,,]


# time series plot
# mu_D posterior mean ( violet ) vs simulated mu_D ( orange )
par(mfrow = c(5,2))
for (n in 1:N) {
  
  plot(mu_D_sim[n,],
       ylim = c(-5, 15),
       type="l",
       col = "orange",
       main = paste("driver", n),  # TODO version 2, real data
       xlab = "qualifier/race",  # TODO real data
       ylab = "ability",
       xaxt = "n",
       yaxt = "n")
  axis(side = 1, at = c(1,19,38,59,79,100,121,138,159))
  axis(side = 2, at = c(-5, 5, 15), las = 1)  # TODO adjust
  
  lines(mu_D_pm[n,], col = "blueviolet")
  
}
par(mfrow = c(1,1))


# mean squared error
# mu_D posterior mean vs simulated mu_D
MSE(mu_D_pm, mu_D_sim, N, Q, I_1)


# mean squared error 89% HDI
# estimated mu_D vs simulated mu_D
HDI_MSE(mu_D_est, mu_D_sim, N, Q, I_1, iter)


# time series plot
# estimated mu_D 89% HDI
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
  
  x <- 1:Q
  
  plot(x = x,
       y = mu_D_U,
       ylim = c(-5, 15),
       type="l",
       col = "blueviolet",
       main = paste("driver", n),  # TODO version 2, real data
       xlab = "qualifier/race",  # TODO real data
       ylab = "ability",
       xaxt = "n",
       yaxt = "n")
  axis(side = 1, at = c(1,19,38,59,79,100,121,138,159))
  axis(side = 2, at = c(-5, 5, 15), las = 1)  # TODO adjust
  
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
    
    mu_C_pm[k,t] <-
      get_posterior_mean(fit_model,
                         pars = paste("mu_C[",k,",",t,"]", sep = ""))[5]
    
  }
}


# extract estimated mu_C
mu_C_est <- params_model$mu_C


# extract simulated mu_C
mu_C_sim_temp <- params_model_sim$mu_C

mu_C_sim <- mu_C_sim_temp[40,,]


# time series plot
# mu_C posterior mean ( green ) vs simulated mu_C ( orange )
par(mfrow = c(5,2))
for (k in 1:K) {
  
  plot(mu_C_sim[k,],
       ylim = c(-5, 15),
       type="l",
       col = "orange",
       main = paste("constructor", k),  # TODO real data
       xlab = "qualifier/race",  # TODO real data
       ylab = "ability",
       xaxt = "n",
       yaxt = "n")
  axis(side = 1, at = c(1,19,38,59,79,100,121,138,159))
  axis(side = 2, at = c(-5, 5, 15), las = 1)  # TODO adjust
  
  lines(mu_C_pm[k,], col = "mediumspringgreen")
  
}
par(mfrow = c(1,1))


# mean squared error
# mu_C posterior mean vs simulated mu_C
MSE(mu_C_pm, mu_C_sim, K, Q, I_3)


# mean squared error 89% HDI
# estimated mu_C vs simulated mu_C
HDI_MSE(mu_C_est, mu_C_sim, K, Q, I_3, iter)


# time series plot
# estimated mu_C 89% HDI
par(mfrow = c(5,2))
for (k in 1:K) {
  
  mu_C_U <- c()
  mu_C_L <- c()
  for (t in 1:Q) {
    
    mu_C_U_temp <- HPDI(as.numeric(mu_C_est[,k,t]))[2]
    mu_C_U <- c(mu_C_U, mu_C_U_temp)
    
    mu_C_L_temp <- HPDI(as.numeric(mu_C_est[,k,t]))[1]
    mu_C_L <- c(mu_C_L, mu_C_L_temp)
    
  }
  
  x <- 1:Q
  
  plot(x = x,
       y = mu_C_U,
       ylim = c(-5, 15),
       type="l",
       col = "mediumspringgreen",
       main = paste("constructor", k),  # TODO real data
       xlab = "qualifier/race",  # TODO real data
       ylab = "ability",
       xaxt = "n",
       yaxt = "n")
  axis(side = 1, at = c(1,19,38,59,79,100,121,138,159))
  axis(side = 2, at = c(-5, 5, 15), las = 1)  # TODO adjust
  
  lines(x = x, mu_C_L, col = "mediumspringgreen")
  
  polygon(x = c(x, rev(x)),
          y = c(mu_C_U, rev(mu_C_L)),
          col = "mediumspringgreen",
          lty = 0)
  
}
par(mfrow = c(1,1))



# insights - latent driver vs constructor ability ####
# placeholder







