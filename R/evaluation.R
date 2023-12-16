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
library(openxlsx)
library(readxl)
library(dplyr)



# user-defined functions ####
# absolute error for scalar
AE <- function(est, sim) {
  
  AE_temp <- abs(as.numeric(est) - as.numeric(sim))

  AE <- round(SE_temp, digits = 4)
  
  return(AE)
    
}


# 89% HDI for absolute error for scalar
HDI_AE <- function(est, sim, iter) {
  
  ae <- rep(0, times = iter)
  
  for(i in 1:iter) {
    ae[i] <- AE(est[i], sim)
  }
  
  HDI_AE <- round(HPDI(ae), digits = 4)
  
  return(HDI_AE)
  
}


# mean absolute error for vector
MAE_1 <- function(est, sim, length) {
  
  AE <- 0
  
  for (l in 1:length) {
    AE <- AE + AE(est[l], sim[l])
  }
  
  MAE <- round(AE / length, digits = 4)
  
  return(MAE)
  
}


# 89% HDI for mean absolute error for vector
HDI_MAE_1 <- function(est, sim, length, iter) {
  
  mae <- rep(0, times = iter)
  
  for(i in 1:iter) {
    mae[i] <- MAE_1(est[i,], sim, length)
  }
  
  HDI_MAE <- round(HPDI(mae), digits = 4)
  
  return(HDI_MAE)
  
}


# mean absolute error for matrix
MAE_2 <- function(est, sim, X, Y, I) {
  
  AE <- 0
  
  for (x in 1:X) {
    for (y in 1:Y) {
      
      if (I[x,y] == 1) {
        AE <- AE + abs(as.numeric(est[x,y]) - as.numeric(sim[x,y]))
      }
      
    }
  }
  
  N <- sum(I)
  
  MAE <- round(AE / N, digits = 4)
  
  return(MAE)
  
}


# 89% HDI for mean absolute error for matrix
HDI_MAE_2 <- function(est, sim, X, Y, I, iter) {
  
  mae <- rep(0, times = iter)
  
  for(i in 1:iter) {
    mae[i] <- MAE_2(est[i,,], sim, X, Y, I)
  }
  
  HDI_MAE <- round(HPDI(mae), digits = 4)

  return(HDI_MAE)
  
}


# accuracy for matrix
ACC <- function(pred, obs, X, Y, I) {
  
  correct <- 0
  
  for (x in 1:X) {
    for (y in 1:Y) {
      
      if ((pred[x,y] == obs[x,y]) & I[x,y] == 1) {
        correct <- correct + 1
      }
      
    }
  }
  
  N <- sum(I)
  
  ACC <- round(correct / N, digits = 4)
  
  return(ACC)
  
}


# 89% HDI for accuracy for matrix
HDI_ACC <- function(pred, obs, X, Y, I, iter) {
  
  acc <- rep(0, times = iter)
  
  for(i in 1:iter) {
    acc[i] <- ACC(pred[i,,], obs, X, Y, I)
  }
  
  HDI_ACC <- round(HPDI(acc), digits = 4)
  
  return(HDI_ACC)
  
}



# evaluation prep ####
# load fit_model_sim
fit_model_sim <- readRDS("data/fit_m1_v1_sim_increased_fluctuations.rds")  # TODO data file

# extract simulations
params_model_sim <- rstan::extract(fit_model_sim)


# load fit_model
fit_model <- readRDS("results/fit_m1_v1_qualifier.rds")  # TODO data file

# extract samples
params_model <- rstan::extract(fit_model)


# number of total post-warmup iterations
iter_per_chain <- 2000

iter <- iter_per_chain / 2 * 4



# convergence ####
# effective sample size
# n_eff / total post-warmup iterations should be greater than 0.01
# source: Stan YouTube
neff_temp <- summary(fit_model)$summary[,'n_eff']

neff_temp <- neff_temp / iter

neff <- neff_temp[neff_temp <= 0.01]

neff_params <- names(neff)

neff_params <- neff_params[!grepl("temp", neff_params)]

neff_params <- ifelse(is.na(neff_params),
                         "missing",
                         neff_params)

neff_params <- neff_params[!grepl("missing", neff_params)]

neff[which(names(neff) %in% neff_params)]


# BULK ESS should be greater than 400 ( greater than 100 per chain )
# source: Stan YouTube
ESS_temp <- monitor(extract(fit_model, permuted = FALSE))

ESS_temp <- as.data.frame(ESS_temp)[,c(21,22)]

ESS <- subset(ESS_temp, Bulk_ESS <= 400 | Tail_ESS <= 400)

ESS_params <- row.names(ESS)

ESS_params <- ESS_params[!grepl("temp", ESS_params)]

ESS_params <- ifelse(is.na(ESS_params),
                      "missing",
                     ESS_params)

ESS_params <- ESS_params[!grepl("missing", ESS_params)]

ESS[ESS_params,]


# Rhat should be less than 1.1
# source: Stan YouTube
Rhat_temp <- summary(fit_model)$summary[,'Rhat']

Rhat <- Rhat_temp[Rhat_temp >= 1.1]

Rhat_params <- names(Rhat)

Rhat_params <- Rhat_params[!grepl("temp", Rhat_params)]

Rhat_params <- ifelse(is.na(Rhat_params),
                         "missing",
                      Rhat_params)

Rhat_params <- Rhat_params[!grepl("missing", Rhat_params)]

Rhat[which(names(Rhat) %in% Rhat_params)]


# trace plots
# varsigma_D
par(mfrow = c(3,1))
ts.plot(params_model$varsigma_D,
        col = "blueviolet",
        xlab = "Post-warmup iteration",
        ylab = "varsigma_D")

# varsigma_C
ts.plot(params_model$varsigma_C,
        col = "mediumspringgreen",
        xlab = "Post-warmup iteration",
        ylab = "varsigma_C")

# cut points
for (j in 2:(J-2)) {
  ts.plot(params_model$gamma[,j],
          col = "deeppink4",
          xlab = "Post-warmup iteration",
          ylab = paste("gamma_", j, sep=""))
}
par(mfrow = c(1,1))



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
abline(v = varsigma_D_sim, lwd = 2, col = "orange")  # TODO actual data

# histogram
# posterior density varsigma_C
hist(params_model$varsigma_C,
     col = "mediumspringgreen",
     border = FALSE,
     main = "varsigma_C",
     xlab = "")
abline(v = varsigma_C_sim, lwd = 2, col = "orange")  # TODO actual data

# histogram
# posterior density cut points
for (j in 2:(J-2)) {
  hist(params_model$gamma[,j],
       col = "deeppink1",
       border = FALSE,
       main = paste("gamma_", j, sep=""),
       xlab = "")
  abline(v = j-1, lwd = 2, col = "orange")  # TODO actual data
}
par(mfrow = c(1,1))

# absolute error 89% HDI
# estimated varsigma_D vs simulated varsigma_D
HDI_AE(params_model$varsigma_D,
       varsigma_D_sim,
       iter)

# absolute error
# varsigma_C posterior mean vs simulated varsigma_C
AE(get_posterior_mean(fit_model,
                      pars = "varsigma_C")[5],
   varsigma_C_sim)

get_pos

# absolute error 89% HDI
# estimated varsigma_C vs simulated varsigma_C
HDI_AE(params_model$varsigma_C,
       varsigma_C_sim,
       iter)

# extract cut points posterior means
gamma_pm <- rep(0, times = J-3)

for (j in 2:(J-2)) {
  
  gamma_pm[j-1] <- get_posterior_mean(fit_model,
                                      pars = paste("gamma_", j, sep=""))[5]
  
}

# extract estimated estimated cut points
gamma_est_temp <- params_model$gamma

gamma_est <- gamma_est_temp[,2:(J-2)]  # TODO double check

# simulated cut points
gamma_sim <- gamma_sim[2:(J-2)]

# mean absolute error
# cut points posterior mean vs simulated cut points
MAE(gamma_pm, gamma_sim, J-3)

# mean absolute error 89% HDI
# estimated cut points vs simulated cut points
HDI_MAE_1(gamma_est, gamma_sim, J-3, iter)



# fit - qualifier/race rank ####
# extract predicted ranks
R_pred <- params_model$R_pred

# extract median predicted ranks
# ( median over post-warmup iterations )
R_pred_mdn <- matrix(data = NA, nrow = N, ncol = Q)

for (n in 1:N) {
  for (t in 1:Q) {
    
    sample <- rep(0, times = iter)
    
    for (i in 1:iter) {
      
      vector_ranks[i] <- R_pred[i,n,t]
      
    }
    
    median_temp <- median(sample)
    
    median <- round(median_temp, digits = 0)
    
    P_pred_mdn[n,t] <- median
    
  }
}

write.xlsx(R_pred_mdn,
           "results/m1_v1_qualifier/R_pred_avg.xlsx",
           overwrite = TRUE)

R_pred_avg <-
  read_excel("results/m1_v1_qualifier/R_pred_avg.xlsx",  # TODO data file
             sheet = "Sheet 1")


# extract simulated or actual ranks
# simulated
# R_obs_temp <- params_model_sim$R_sim
# R_obs <- R_obs_temp[40,,]

# actual
R_obs <- R_act


# time series plot
# averaged predicted ranks ( pink ) vs observed ranks ( orange )
par(mfrow = c(5,2))
for (n in 1:N) {
  
  plot(R_obs[n,],
       ylim = c(22, 1),
       type="l",
       col = "orange",
       main = paste("driver", n),  # TODO version 2, actual data
       xlab = "qualifier/race",  # TODO actual data
       ylab = "rank",
       xaxt = "n",
       yaxt = "n")
  axis(side = 1, at = c(1,19,38,59,79,100,121,138,159))
  axis(side = 2, at = c(22, 11, 1), las = 1)
  
  lines(R_pred_mdn[n,], col = "deeppink1")
  
}
par(mfrow = c(1,1))


# accuracy
# averaged predicted ranks vs observed ranks
ACC(R_pred_mdn, R_obs, N, Q, I_1)


# accuracy 89% HDI 
# predicted ranks vs observed ranks
HDI_ACC(R_pred, R_obs, N, Q, I_1, iter)


# time series plot
# predicted ranks 89% HDI
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
       main = paste("driver", n),  # TODO version 2, actual data
       xlab = "qualifier/race",  # TODO actual data
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
# extract mu_P posterior means
mu_P_pm <- matrix(data = NA, nrow = N, ncol = Q)

for (n in 1:N) {
  for (t in 1:Q) {
    
    mu_P_pm[n,t] <-
      get_posterior_mean(fit_model,
                         pars = paste("mu_P[",n,",",t,"]", sep = ""))[5]
    
  }
}

write.xlsx(mu_P_pm,
           "results/m1_v1_missing_data/mu_P_pm.xlsx",
           overwrite = TRUE)

R_pred_avg <-
  read_excel("results/m1_v1_missing_data/mu_P_pm_avg.xlsx",  # TODO data file
             sheet = "Sheet 1")


# extract estimated mu_P
mu_P_est <- params_model$mu_P


# extract simulated mu_P
mu_P_sim_temp <- params_model_sim$mu_P

mu_P_sim <- mu_P_sim_temp[40,,]


# time series plot
# mu_P posterior mean ( pink ) vs simulated mu_D ( orange )
par(mfrow = c(5,2))
for (n in 1:N) {
  
  plot(mu_P_sim[n,],  # TODO actual data
       ylim = c(-5, 25),
       type="l",
       col = "orange",
       main = paste("driver", n), # TODO version 2, actual data
       xlab = "qualifier/race",  # TODO actual data
       ylab = "performance",
       xaxt = "n",
       yaxt = "n")
  axis(side = 1, at = c(1,19,38,59,79,100,121,138,159))
  axis(side = 2, at = c(-5, 10, 25), las = 1)
  
  lines(mu_P_pm[n,], col = "deeppink1")
  
}
par(mfrow = c(1,1))


# mean absolute error
# mu_P posterior mean vs simulated mu_P
MSE_2(mu_P_pm, mu_P_sim, N, Q, I_1)


# mean absolute error 89% HDI
# estimated mu_P vs simulated mu_P
HDI_MSE_2(mu_P_est, mu_P_sim, N, Q, I_1, iter)


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
       main = paste("driver", n),  # TODO version 2, actual data
       xlab = "qualifier/race",  # TODO actual data
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
# extract mu_D posterior means
mu_D_pm <- matrix(data = NA, nrow = N, ncol = Q)

for (n in 1:N) {
  for (t in 1:Q) {
    
    mu_D_pm[n,t] <-
      get_posterior_mean(fit_model,
                         pars = paste("mu_D[",n,",",t,"]", sep = ""))[5]
    
  }
}

write.xlsx(mu_D_pm,
           "results/m1_v1_missing_data/mu_D_pm.xlsx",
           overwrite = TRUE)

R_pred_avg <-
  read_excel("results/m1_v1_missing_data/mu_D_pm_avg.xlsx",  # TODO data file
             sheet = "Sheet 1")


# extract estimated mu_D
mu_D_est <- params_model$mu_D


# extract simulated mu_D
mu_D_sim_temp <- params_model_sim$mu_D

mu_D_sim <- mu_D_sim_temp[40,,]


# time series plot
# mu_D posterior mean ( violet ) vs simulated mu_D ( orange )
par(mfrow = c(5,2))
for (n in 1:N) {
  
  plot(mu_D_sim[n,],  # TODO actual data
       ylim = c(-7, 7),
       type="l",
       col = "orange",
       main = paste("driver", n),  # TODO version 2, actual data
       xlab = "qualifier/race",  # TODO actual data
       ylab = "ability",
       xaxt = "n",
       yaxt = "n")
  axis(side = 1, at = c(1,19,38,59,79,100,121,138,159))
  axis(side = 2, at = c(-7, 0, 7), las = 1)  # TODO adjust
  
  lines(mu_D_pm[n,], col = "blueviolet")
  
}
par(mfrow = c(1,1))


# mean absolute error
# mu_D posterior mean vs simulated mu_D
MSE_2(mu_D_pm, mu_D_sim, N, Q, I_1)


# mean absolute error 89% HDI
# estimated mu_D vs simulated mu_D
HDI_MSE_2(mu_D_est, mu_D_sim, N, Q, I_1, iter)


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
       ylim = c(-7, 7),
       type="l",
       col = "blueviolet",
       main = paste("driver", n),  # TODO version 2, actual data
       xlab = "qualifier/race",  # TODO actual data
       ylab = "ability",
       xaxt = "n",
       yaxt = "n")
  axis(side = 1, at = c(1,19,38,59,79,100,121,138,159))
  axis(side = 2, at = c(-7, 0, 7), las = 1)  # TODO adjust
  
  lines(x = x, mu_D_L, col = "blueviolet")
  
  polygon(x = c(x, rev(x)),
          y = c(mu_D_U, rev(mu_D_L)),
          col = "blueviolet",
          lty = 0)
  
}
par(mfrow = c(1,1))



# fit - latent constructor ability ####
# extract mu_C posterior means
mu_C_pm <- matrix(data = NA, nrow = K, ncol = Q)

for (k in 1:K) {
  for (t in 1:Q) {
    
    mu_C_pm[k,t] <-
      get_posterior_mean(fit_model,
                         pars = paste("mu_C[",k,",",t,"]", sep = ""))[5]
    
  }
}

write.xlsx(mu_C_pm,
           "results/m1_v1_missing_data/mu_C_pm.xlsx",
           overwrite = TRUE)

R_pred_avg <-
  read_excel("results/m1_v1_missing_data/mu_C_pm_avg.xlsx",  # TODO data file
             sheet = "Sheet 1")


# extract estimated mu_C
mu_C_est <- params_model$mu_C


# extract simulated mu_C
mu_C_sim_temp <- params_model_sim$mu_C

mu_C_sim <- mu_C_sim_temp[40,,]


# time series plot
# mu_C posterior mean ( green ) vs simulated mu_C ( orange )
par(mfrow = c(5,2))
for (k in 1:K) {
  
  plot(mu_C_sim[k,],  # TODO actual data
       ylim = c(-5, 25),
       type="l",
       col = "orange",
       main = paste("constructor", k),  # TODO actual data
       xlab = "qualifier/race",  # TODO actual data
       ylab = "ability",
       xaxt = "n",
       yaxt = "n")
  axis(side = 1, at = c(1,19,38,59,79,100,121,138,159))
  axis(side = 2, at = c(-5, 5, 15), las = 1)  # TODO adjust
  
  lines(mu_C_pm[k,], col = "mediumspringgreen")
  
}
par(mfrow = c(1,1))


# mean absolute error
# mu_C posterior mean vs simulated mu_C
MSE_2(mu_C_pm, mu_C_sim, K, Q, I_3)


# mean absolute error 89% HDI
# estimated mu_C vs simulated mu_C
HDI_MSE_2(mu_C_est, mu_C_sim, K, Q, I_3, iter)


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
       ylim = c(-5, 25),
       type="l",
       col = "mediumspringgreen",
       main = paste("constructor", k),  # TODO actual data
       xlab = "qualifier/race",  # TODO actual data
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


