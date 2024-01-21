# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


# closing the sections provides an overview of the script


# this file builds on the simulated_data.R and actual_data.R file


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
library(bridgesampling)
library(geometry)



# user-defined functions ####
# accuracy for matrix
ACC <- function(pred, obs, X, Y, I) {
  
  correct <- 0
  
  for (y in 1:Y) {
    for (x in 1:X) {
      
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
  
  acc <- rep(NA, times = iter)
  
  for(i in 1:iter) {
    acc[i] <- ACC(pred[i,,], obs, X, Y, I)
  }
  
  HDI_ACC <- round(HPDI(acc), digits = 4)
  
  return(HDI_ACC)
  
}


# precision for rank j
PRE <- function(pred, obs, X, Y, I, j) {
  
  TP <- 0
  TP_FP <- 0
  
  for(y in 1:Y) {
    for(x in 1:X) {
      if(I[x,y] == 1) {
        
        if(pred[x,y] == j & obs[x,y] == j) {
          TP <- TP +1
        }
        
        if(pred[x,y] == j) {
          TP_FP <- TP_FP + 1
        }
        
      }
    }
  }
  
  PRE <- TP / TP_FP
  
  return(PRE)
  
}


# recall for rank j
REC <- function(pred, obs, X, Y, I, j) {
  
  TP <- 0
  TP_FN <- 0
  
  for(y in 1:Y) {
    for(x in 1:X) {
      if(I[x,y] == 1) {
        
        if(pred[x,y] == j & obs[x,y] == j) {
          TP <- TP +1
        }
        
        if(obs[x,y] == j) {
          TP_FN <- TP_FN + 1
        }
        
      }
    }
  }
  
  REC <- TP / TP_FN
  
  return(REC)
  
}


# F1 score for matrix
F1 <- function(pred, obs, X, Y, I, J) {
  
  F1 <- rep(NA, times = J)
  
  for(j in 1:J) {
    
    pre <- PRE(pred, obs, X, Y, I, j)
    rec <- REC(pred, obs, X, Y, I, j)
    
    F1[j] <- 2 * (pre * rec) / (pre + rec)
    
  }
  
  return(F1)
  
}


# 89% HDI for F1 score for matrix
HDI_F1 <- function(pred, obs, X, Y, I, J, iter) {
  
  f1 <- matrix(data = NA, nrow = iter, ncol = J)
  
  for(i in 1:iter) {
    
    f1[i,] <- F1(pred[i,,], obs, X, Y, I, J)
    
  }
  
  HDI_F1 <- matrix(data = NA, nrow = 2, ncol = J)
  
  for(j in 1:J) {
    
    HDI_F1[1,j] <- HPDI(f1[,j])[1]
    
    HDI_upper <- HPDI(f1[,j])[2]
    HDI_F1[2,j] <- HDI_upper - HDI_F1[1,j]
    
  }
  
  return(HDI_F1)
  
}


# pairwise comparison accuracy for matrix
PCACC <- function(pred, obs, X, Y, I) {
  
  correct <- 0
  
  N <- 0
  
  for (y in 1:Y) {
    
    counter <- X-1
    
    for (x in 1:(X-1)) {
      
      for (c in 1:counter) {
        
        if (I[x,y] == 1 & I[x+c,y] == 1) {
          
          if (((pred[x,y] > pred[x+c,y]) & (obs[x,y] > obs[x+c,y])) |
              ((pred[x,y] < pred[x+c,y]) & (obs[x,y] < obs[x+c,y])) |
              ((pred[x,y] == pred[x+c,y]) & (obs[x,y] == obs[x+c,y])))
          {
            correct <- correct + 1
          }
          
        }
        
      }
      
      counter <- counter - 1
      
    }
    
    N <- N + (sum(I[,y])-1) * sum(I[,y]) / 2
    
  }
  
  PCACC <- round(correct / N, digits = 4)
  
  return(PCACC)
  
}


# 89% HDI for pairwise comparison accuracy for matrix
HDI_PCACC <- function(pred, obs, X, Y, I, iter) {
  
  pcacc <- rep(NA, times = iter)
  
  for(i in 1:iter) {
    pcacc[i] <- PCACC(pred[i,,], obs, X, Y, I)
  }
  
  HDI_PCACC <- round(HPDI(pcacc), digits = 4)
  
  return(HDI_PCACC)
  
}


# Spearman's rank correlation coefficient for matrix
RHO <- function(pred, obs, X, Y, I) {
  
  RHO <- rep(NA, times = Y)
  
  for(y in 1:Y) {
    
    pred_vector <- c()
    obs_vector <- c()
    
    element <- 1
    
    for(x in 1:X) {
      if(I[x,y] == 1) {
        
        pred_vector[element] <- pred[x,y]
        obs_vector[element] <- obs[x,y]
        
        element <- element + 1
        
      }
    }
    
    RHO[y] <- cor(x = pred_vector,
                  y = obs_vector,
                  method = "spearman")
    
  }
  
  return(RHO)
  
}


# 89% HDI for Spearman's rank correlation coefficient for matrix
HDI_RHO <- function(pred, obs, X, Y, I, iter) {
  
  rho <- matrix(data = NA, nrow = iter, ncol = Y)
  
  for(i in 1:iter) {
    
    rho[i,] <- RHO(pred[i,,], obs, X, Y, I)
    
  }
  
  HDI_RHO <- matrix(data = NA, nrow = 2, ncol = Y)
  
  for(y in 1:Y) {
    
    HDI_RHO[1,y] <- HPDI(rho[,y])[2]
    HDI_RHO[2,y] <- HPDI(rho[,y])[1]
    
  }
  
  return(HDI_RHO)
  
}


# absolute error for scalar
AE <- function(est, sim) {
  
  AE_temp <- abs(est - sim)

  AE <- round(AE_temp, digits = 4)
  
  return(AE)
    
}


# 89% HDI for absolute error for scalar
HDI_AE <- function(est, sim, iter) {
  
  ae <- rep(NA, times = iter)
  
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
  
  mae <- rep(NA, times = iter)
  
  for(i in 1:iter) {
    mae[i] <- MAE_1(est[i,], sim, length)
  }
  
  HDI_MAE <- round(HPDI(mae), digits = 4)
  
  return(HDI_MAE)
  
}


# mean absolute error for matrix
MAE_2 <- function(est, sim, X, Y, I) {
  
  AE <- 0
  
  for (y in 1:Y) {
    for (x in 1:X) {
      
      if (I[x,y] == 1) {
        AE <- AE + abs(est[x,y] - sim[x,y])
      }
      
    }
  }
  
  N <- sum(I)
  
  MAE <- round(AE / N, digits = 4)
  
  return(MAE)
  
}


# 89% HDI for mean absolute error for matrix
HDI_MAE_2 <- function(est, sim, X, Y, I, iter) {
  
  mae <- rep(NA, times = iter)
  
  for(i in 1:iter) {
    mae[i] <- MAE_2(est[i,,], sim, X, Y, I)
  }
  
  HDI_MAE <- round(HPDI(mae), digits = 4)

  return(HDI_MAE)
  
}


# evaluation prep - simulated data ####
# load fit_model_sim
fit_model_sim <-
  readRDS("data/fit_m1_v1_sim_clean_data.rds")  # TODO data file

# extract simulations
params_model_sim <- rstan::extract(fit_model_sim)



# evaluation prep - results ####
# load fit_model
fit_model <-
  readRDS("results/fit_m1_v1_qualifier.rds")  # TODO data file

# extract samples
params_model <- rstan::extract(fit_model)


# number of total sampling iterations
iter_per_chain <- 2000

iter <- iter_per_chain / 2 * 4



# convergence ####
# effective sample size
# n_eff / total sampling iterations should be greater than 0.01
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
        xlab = "Sampling iteration",
        ylab = "varsigma_D")

grid(nx = NA, ny = NULL)
par(new = TRUE)

ts.plot(params_model$varsigma_D,
        col = "blueviolet",
        xlab = "Sampling iteration",
        ylab = "varsigma_D")

# varsigma_C
ts.plot(params_model$varsigma_C,
        col = "mediumspringgreen",
        xlab = "Sampling iteration",
        ylab = "varsigma_C")

grid(nx = NA, ny = NULL)
par(new = TRUE)

ts.plot(params_model$varsigma_C,
        col = "mediumspringgreen",
        xlab = "Sampling iteration",
        ylab = "varsigma_C")

# cut points
for (j in 2:(J-2)) {
  ts.plot(params_model$gamma[,j],
          col = "deeppink4",
          xlab = "Sampling iteration",
          ylab = paste("gamma_", j, sep=""))
  
  grid(nx = NA, ny = NULL)
  par(new = TRUE)
  
  ts.plot(params_model$gamma[,j],
          col = "deeppink4",
          xlab = "Sampling iteration",
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

grid()
par(new = TRUE)

hist(params_model$varsigma_D,
     col = "blueviolet",
     border = FALSE,
     main = "varsigma_D",
     xlab = "")
abline(v = varsigma_D_sim, lwd = 2, col = "orange")  # TODO actual data
box()

# histogram
# posterior density varsigma_C
hist(params_model$varsigma_C,
     col = "mediumspringgreen",
     border = FALSE,
     main = "varsigma_C",
     xlab = "")

grid()
par(new = TRUE)

hist(params_model$varsigma_C,
     col = "mediumspringgreen",
     border = FALSE,
     main = "varsigma_C",
     xlab = "")
abline(v = varsigma_C_sim, lwd = 2, col = "orange")  # TODO actual data
box()

# histogram
# posterior density cut points
for (j in 2:(J-2)) {
  hist(params_model$gamma[,j],
       col = "deeppink4",
       border = FALSE,
       main = paste("gamma_", j, sep=""),
       xlab = "")
  
  grid()
  par(new = TRUE)
  
  hist(params_model$gamma[,j],
       col = "deeppink4",
       border = FALSE,
       main = paste("gamma_", j, sep=""),
       xlab = "")
  abline(v = j-1, lwd = 2, col = "orange")  # TODO actual data
  box()
}
par(mfrow = c(1,1))

# visualize posterior medians for cut points together in one plot
plot(NULL,
     xlim = c(0,20),
     ylim = c(0,1),
     ylab = "",
     xlab = "gamma",
     main = "posterior medians for cut points",
     yaxt = "n")

abline(v = 1, col = "deeppink4")
abline(v = 2, col = "deeppink4")
abline(v = 3, col = "deeppink4")
abline(v = 4, col = "deeppink4")
abline(v = 5, col = "deeppink4")
abline(v = 6, col = "deeppink4")
abline(v = 7, col = "deeppink4")
abline(v = 8, col = "deeppink4")
abline(v = 9, col = "deeppink4")
abline(v = 10, col = "deeppink4")
abline(v = 11, col = "deeppink4")
abline(v = 12, col = "deeppink4")
abline(v = 13, col = "deeppink4")
abline(v = 14, col = "deeppink4")
abline(v = 15, col = "deeppink4")
abline(v = 16, col = "deeppink4")
abline(v = 17, col = "deeppink4")
abline(v = 18, col = "deeppink4")
abline(v = 19, col = "deeppink4")

# absolute error
# varsigma_D posterior median vs simulated varsigma_D
varsigma_D_mdn <- median(params_model$varsigma_D)

AE(varsigma_D_mdn, varsigma_D_sim)

# absolute error 89% HDI
# estimated varsigma_D vs simulated varsigma_D
HDI_AE(params_model$varsigma_D,
       varsigma_D_sim,
       iter)

# absolute error
# varsigma_C posterior median vs simulated varsigma_C
varsigma_C_mdn <- median(params_model$varsigma_C)

AE(varsigma_C_mdn, varsigma_C_sim)

# absolute error 89% HDI
# estimated varsigma_C vs simulated varsigma_C
HDI_AE(params_model$varsigma_C,
       varsigma_C_sim,
       iter)

# extract estimated cut points
gamma_est_temp <- params_model$gamma

gamma_est <- gamma_est_temp[,2:(J-2)]

# extract cut points posterior median
gamma_mdn <- rep(0, times = J-3)

for (j in 1:(J-3)) {
  gamma_mdn[j] <- median(gamma_est[,j])
}

# simulated cut points
gamma_sim <- gamma_sim[2:(J-2)]

# mean absolute error
# cut points posterior median vs simulated cut points
MAE_1(gamma_mdn, gamma_sim, J-3)

# mean absolute error 89% HDI
# estimated cut points vs simulated cut points
HDI_MAE_1(gamma_est, gamma_sim, J-3, iter)



# fit - qualifier/race rank ####
# extract predicted ranks
R_pred <- params_model$R_pred


# extract median predicted ranks
# ( median over sampling iterations )
R_pred_mdn <- matrix(data = NA, nrow = N, ncol = Q)

for (n in 1:N) {
  for (t in 1:Q) {
    
    sample <- R_pred[,n,t]
    
    median_temp <- median(sample)
    
    median <- round(median_temp, digits = 0)
    
    R_pred_mdn[n,t] <- median
    
  }
}


# extract simulated or actual ranks
# simulated
R_obs_temp <- params_model_sim$R_sim
R_obs <- R_obs_temp[40,,]

# actual
R_obs <- R_act


# R_obs_with_NA
R_obs_with_NA <- R_obs

for (n in 1:N) {
  for (t in 1:Q) {
    
    if (I_1[n,t] == 0) {
      R_obs_with_NA[n,t] <- NA
    }
    
  }
}


# driver names
drv_names <- as.vector(as.matrix(R_act_temp[,2]))


# ctr cockpits
ctr_cockpits <- c("Mercedes 1", "Mercedes 2",
                  "Red Bull 1", "Red Bull 2",
                  "McLaren 1", "McLaren 2",
                  "Ferrari 1", "Ferrari 2",
                  "AlphaTauri 1", "AlphaTauri 2",
                  "Aston Martin 1", "Aston Martin 2",
                  "Williams 1", "Williams 2",
                  "Alfa Romeo 1", "Alfa Romeo 2",
                  "Alpine 1", "Alpine 2",
                  "Manor Marussia 1", "Manor Marussia 2",
                  "Caterham 1", "Caterham 2",
                  "Haas F1 Team 1", "Haas F1 Team 2")

# time series plot
# median predicted ranks ( pink ) vs observed ranks ( orange )
par(mfrow = c(5,2))
for (n in 1:N) {
  
  plot(R_obs_with_NA[n,],
       ylim = c(22, 1),
       type="l",
       col = "orange",
       main = paste("driver", n),  # TODO actual data
       # main = drv_names[n],  # TODO version 2
       xlab = "qualifier/race",  # TODO actual data
       ylab = "rank",
       xaxt = "n")
  axis(side = 1, at = c(1,19,38,59,79,100,121,138,159))
  
  grid(nx = NA, ny = NULL)
  par(new = TRUE)
  
  plot(R_obs_with_NA[n,],
       ylim = c(22, 1),
       type="l",
       col = "orange",
       main = paste("driver", n),  # TODO actual data
       # main = drv_names[n],  # TODO version 2
       xlab = "qualifier/race",  # TODO actual data
       ylab = "rank",
       xaxt = "n")
  axis(side = 1, at = c(1,19,38,59,79,100,121,138,159))
  
  lines(R_pred_mdn[n,], col = "deeppink1")
  
  for (t in 1:Q) {
    if (I_1[n,t] == 0) {
      abline(v = t, lwd = 0.125, col = "azure4")
    }
  }
  
  box()
  
}
par(mfrow = c(1,1))


# accuracy
# median predicted ranks vs observed ranks
ACC(R_pred_mdn, R_obs, N, Q, I_1)


# accuracy 89% HDI 
# predicted ranks vs observed ranks
HDI_ACC(R_pred, R_obs, N, Q, I_1, iter)


# bar plot
# F1 score
# median predicted ranks vs observed ranks
F1_result <- F1(R_pred_mdn, R_obs, N, Q, I_1, J)

barplot(F1_result ~ c(1:J),
        ylim = c(0, 1),
        col = "deeppink1",
        border = FALSE,
        main = "",  # TODO tbd
        xlab = "rank",
        ylab = "F1 score")

grid(nx = NA, ny = NULL)
par(new = TRUE)

barplot(F1_result ~ c(1:J),
        ylim = c(0, 1),
        col = "deeppink1",
        border = FALSE,
        main = "model 1 version 1 | qualifier data",  # TODO tbd
        xlab = "rank",
        ylab = "F1 score")
box()


# bar plot
# F1 score 89% HDI 
# predicted ranks vs observed ranks
HDI_F1_result <- HDI_F1(R_pred, R_obs, N, Q, I_1, J, iter)

colnames(HDI_F1_result) <- as.character(1:J)

pink_transp <- rgb(255, 20, 147,
                   max = 255,
                   alpha = 25,
                   names = "deeppink1")

barplot(HDI_F1_result,
        ylim = c(0, 1),
        col = c(pink_transp, "deeppink1"),
        border = "white",
        main = "",  # TODO tbd
        xlab = "rank",
        ylab = "F1 score")

grid(nx = NA, ny = NULL)
par(new = TRUE)

barplot(HDI_F1_result,
        ylim = c(0, 1),
        col = c(pink_transp, "deeppink1"),
        border = "white",
        main = "model 1 version 1 | qualifier data",  # TODO tbd
        xlab = "rank",
        ylab = "F1 score")
box()


# pairwise comparison accuracy
# median predicted ranks vs observed ranks
PCACC(R_pred_mdn, R_obs, N, Q, I_1)


# pairwise comparison accuracy 89% HDI 
# predicted ranks vs observed ranks
HDI_PCACC(R_pred, R_obs, N, Q, I_1, iter)


# time series plot
# Spearman's rank correlation coefficient
# median predicted ranks vs observed ranks
RHO_result <- RHO(R_pred_mdn, R_obs, N, Q, I_1)

plot(RHO_result,
     ylim = c(-1, 1),
     type="l",
     col = "deeppink1",
     main = "",  # TODO tbd
     xlab = "",
     ylab = "rho",
     xaxt = "n")
axis(side = 1, at = c(1,19,38,59,79,100,121,138,159))

grid(nx = NA, ny = NULL)
par(new = TRUE)

plot(RHO_result,
     ylim = c(-1, 1),
     type="l",
     col = "deeppink1",
     main = "model 1 version 1 | qualifier data",  # TODO tbd
     xlab = "qualifier",  # TODO actual data
     ylab = "rho",
     xaxt = "n")
axis(side = 1, at = c(1,19,38,59,79,100,121,138,159))
box()

# mean
round(mean(RHO_result), digits = 4)


# time series plot
# Spearman's rank correlation coefficient 89% HDI
# predicted ranks vs observed ranks
HDI_RHO_result <- HDI_RHO(R_pred, R_obs, N, Q, I_1, iter)

x <- 1:Q

plot(x = x,
     y = HDI_RHO_result[2,],
     ylim = c(-1, 1),
     type="l",
     col = "deeppink1",
     main = "",  # TODO tbd
     xlab = "",
     ylab = "rho",
     xaxt = "n")
axis(side = 1, at = c(1,19,38,59,79,100,121,138,159))

grid(nx = NA, ny = NULL)
par(new = TRUE)

plot(x = x,
     y = HDI_RHO_result[2,],
     ylim = c(-1, 1),
     type="l",
     col = "deeppink1",
     main = "model 1 version 1 | qualifier data",  # TODO tbd
     xlab = "qualifier",  # TODO actual data
     ylab = "rho",
     xaxt = "n")
axis(side = 1, at = c(1,19,38,59,79,100,121,138,159))

lines(x = x, HDI_RHO_result[1,], col = "deeppink1")

polygon(x = c(x, rev(x)),
        y = c(HDI_RHO_result[2,], rev(HDI_RHO_result[1,])),
        col = "deeppink1",
        lty = 0)
box()


# time series plot
# predicted ranks 89% HDI
par(mfrow = c(5,2))
for (n in 1:N) {
  
  R_pred_U <- c()
  R_pred_L <- c()
  for (t in 1:Q) {
    
    R_pred_U_temp <- HPDI(R_pred[,n,t])[2]
    R_pred_U <- c(R_pred_U, R_pred_U_temp)
    
    R_pred_L_temp <- HPDI(R_pred[,n,t])[1]
    R_pred_L <- c(R_pred_L, R_pred_L_temp)
    
  }
  
  x <- 1:Q
  
  plot(x = x,
       y = R_pred_U,
       ylim = c(22, 1),
       type="l",
       col = "white",
       main = paste("driver", n),  # TODO actual data
       # main = drv_names[n],  # TODO version 2
       xlab = "qualifier/race",  # TODO actual data
       ylab = "rank",
       xaxt = "n")
  axis(side = 1, at = c(1,19,38,59,79,100,121,138,159))
  
  grid(nx = NA, ny = NULL)
  par(new = TRUE)
  
  plot(x = x,
       y = R_pred_U,
       ylim = c(22, 1),
       type="l",
       col = "white",
       main = paste("driver", n),  # TODO actual data
       # main = drv_names[n],  # TODO version 2
       xlab = "qualifier/race",  # TODO actual data
       ylab = "rank",
       xaxt = "n")
  axis(side = 1, at = c(1,19,38,59,79,100,121,138,159))
  
  lines(x = x, R_pred_L, col = "white")
  
  polygon(x = c(x, rev(x)),
          y = c(R_pred_U, rev(R_pred_L)),
          col = pink_transp,
          lty = 0)
  
  lines(R_obs_with_NA[n,], col = "orange")
  lines(R_pred_mdn[n,], col = "deeppink1")
  
  for (t in 1:Q) {
    if (I_1[n,t] == 0) {
      abline(v = t, lwd = 0.125, col = "azure4")
    }
  }
  
  box()
  
}
par(mfrow = c(1,1))



# fit - latent qualifier/race performance ####
# extract estimated mu_P
mu_P_est <- params_model$mu_P


# extract mu_P posterior median
mu_P_mdn <- matrix(data = NA, nrow = N, ncol = Q)

for (n in 1:N) {
  for (t in 1:Q) {
    
    sample <- mu_P_est[,n,t]
    
    median_temp <- median(sample)
    
    median <- round(median_temp, digits = 4)
    
    mu_P_mdn[n,t] <- median
    
  }
}


# extract simulated mu_P
mu_P_sim_temp <- params_model_sim$mu_P

mu_P_sim <- mu_P_sim_temp[40,,]


# time series plot
# mu_P posterior median ( pink ) vs simulated mu_D ( orange )
par(mfrow = c(5,2))
for (n in 1:N) {
  
  plot(mu_P_sim[n,],  # TODO actual data
       # mu_P_mdn[n,],  # TODO actual data
       ylim = c(-5, 25),
       type="l",
       col = "orange",  # TODO actual data
       # col = "deeppink1",  # TODO actual data
       main = paste("driver", n),  # TODO actual data
       # main = drv_names[n], # TODO version 2
       xlab = "qualifier/race",  # TODO actual data
       ylab = "performance",
       xaxt = "n")
  axis(side = 1, at = c(1,19,38,59,79,100,121,138,159))
  
  grid(nx = NA, ny = NULL)
  par(new = TRUE)
  
  plot(mu_P_sim[n,],  # TODO actual data
       # mu_P_mdn[n,],  # TODO actual data
       ylim = c(-5, 25),
       type="l",
       col = "orange",  # TODO actual data
       # col = "deeppink1",  # TODO actual data
       main = paste("driver", n),  # TODO actual data
       # main = drv_names[n], # TODO version 2
       xlab = "qualifier/race",  # TODO actual data
       ylab = "performance",
       xaxt = "n")
  axis(side = 1, at = c(1,19,38,59,79,100,121,138,159))
  
  lines(mu_P_mdn[n,], col = "deeppink1")  # TODO actual data
  
  for (t in 1:Q) {
    if (I_1[n,t] == 0) {
      abline(v = t, lwd = 0.125, col = "azure4")
    }
  }
  
  box()
  
}
par(mfrow = c(1,1))


# mean absolute error
# mu_P posterior median vs simulated mu_P
MAE_2(mu_P_mdn, mu_P_sim, N, Q, I_1)


# mean absolute error 89% HDI
# estimated mu_P vs simulated mu_P
HDI_MAE_2(mu_P_est, mu_P_sim, N, Q, I_1, iter)


# time series plot
# estimated mu_P 89% HDI
par(mfrow = c(5,2))
for (n in 1:N) {
  
  mu_P_U <- c()
  mu_P_L <- c()
  for (t in 1:Q) {
    
    mu_P_U_temp <- HPDI(mu_P_est[,n,t])[2]
    mu_P_U <- c(mu_P_U, mu_P_U_temp)
    
    mu_P_L_temp <- HPDI(mu_P_est[,n,t])[1]
    mu_P_L <- c(mu_P_L, mu_P_L_temp)
    
  }
  
  x <- 1:Q
  
  plot(x = x,
       y = mu_P_U,
       ylim = c(-5, 25),
       type="l",
       col = "white",
       main = paste("driver", n),  # TODO actual data
       # main = drv_names[n],  # TODO version 2
       xlab = "qualifier/race",  # TODO actual data
       ylab = "performance",
       xaxt = "n")
  axis(side = 1, at = c(1,19,38,59,79,100,121,138,159))
  
  grid(nx = NA, ny = NULL)
  par(new = TRUE)
  
  plot(x = x,
       y = mu_P_U,
       ylim = c(-5, 25),
       type="l",
       col = "white",
       main = paste("driver", n),  # TODO actual data
       # main = drv_names[n],  # TODO version 2
       xlab = "qualifier/race",  # TODO actual data
       ylab = "performance",
       xaxt = "n")
  axis(side = 1, at = c(1,19,38,59,79,100,121,138,159))
  
  lines(x = x, mu_P_L, col = "white")
  
  polygon(x = c(x, rev(x)),
          y = c(mu_P_U, rev(mu_P_L)),
          col = pink_transp,
          lty = 0)
  
  lines(mu_P_sim[n,], col = "orange")  # TODO actual data
  lines(mu_P_mdn[n,], col = "deeppink1")
  
  for (t in 1:Q) {
    if (I_1[n,t] == 0) {
      abline(v = t, lwd = 0.125, col = "azure4")
    }
  }
  
  box()
  
}
par(mfrow = c(1,1))



# fit - latent driver ability ####
# extract estimated mu_D
mu_D_est <- params_model$mu_D


# extract mu_D posterior median
mu_D_mdn <- matrix(data = NA, nrow = N, ncol = Q)

for (n in 1:N) {
  for (t in 1:Q) {
    
    sample <- mu_D_est[,n,t]
    
    median_temp <- median(sample)
    
    median <- round(median_temp, digits = 4)
    
    mu_D_mdn[n,t] <- median
    
  }
}


# extract simulated mu_D
mu_D_sim_temp <- params_model_sim$mu_D

mu_D_sim <- mu_D_sim_temp[40,,]


# time series plot
# mu_D posterior median ( violet ) vs simulated mu_D ( orange )
par(mfrow = c(5,2))
for (n in 1:N) {
  
  plot(mu_D_sim[n,],  # TODO actual data
       # mu_D_mdn[n,],  # TODO actual data
       ylim = c(-2, 10),  # TODO adjust
       type="l",
       col = "orange",  # TODO actual data
       # col = "blueviolet",  # TODO actual data
       main = paste("driver", n),  # TODO actual data
       # main = drv_names[n],  # TODO version 2
       xlab = "qualifier/race",  # TODO actual data
       ylab = "ability",
       xaxt = "n")
  axis(side = 1, at = c(1,19,38,59,79,100,121,138,159))
  
  grid(nx = NA, ny = NULL)
  par(new = TRUE)
  
  plot(mu_D_sim[n,],  # TODO actual data
       # mu_D_mdn[n,],  # TODO actual data
       ylim = c(-2, 10),  # TODO adjust
       type="l",
       col = "orange",  # TODO actual data
       # col = "blueviolet",  # TODO actual data
       main = paste("driver", n),  # TODO actual data
       # main = drv_names[n],  # TODO version 2
       xlab = "qualifier/race",  # TODO actual data
       ylab = "ability",
       xaxt = "n")
  axis(side = 1, at = c(1,19,38,59,79,100,121,138,159))
  
  lines(mu_D_mdn[n,], col = "blueviolet")  # TODO actual data
  
  for (t in 1:Q) {
    if (I_1[n,t] == 0) {
      abline(v = t, lwd = 0.125, col = "azure4")
    }
  }
  
  box()
  
}
par(mfrow = c(1,1))


# mean absolute error
# mu_D posterior median vs simulated mu_D
MAE_2(mu_D_mdn, mu_D_sim, N, Q, I_1)


# mean absolute error 89% HDI
# estimated mu_D vs simulated mu_D
HDI_MAE_2(mu_D_est, mu_D_sim, N, Q, I_1, iter)


# time series plot
# estimated mu_D 89% HDI
violet_transp <- rgb(138, 43, 226,
                     max = 226,
                     alpha = 25,
                     names = "blueviolet")

par(mfrow = c(5,2))
for (n in 1:N) {
  
  mu_D_U <- c()
  mu_D_L <- c()
  for (t in 1:Q) {
    
    mu_D_U_temp <- HPDI(mu_D_est[,n,t])[2]
    mu_D_U <- c(mu_D_U, mu_D_U_temp)
    
    mu_D_L_temp <- HPDI(mu_D_est[,n,t])[1]
    mu_D_L <- c(mu_D_L, mu_D_L_temp)
    
  }
  
  x <- 1:Q
  
  plot(x = x,
       y = mu_D_U,
       ylim = c(-5, 15),  # TODO adjust
       type="l",
       col = "white",
       main = paste("driver", n),  # TODO actual data
       # main = drv_names[n],  # TODO version 2
       xlab = "qualifier/race",  # TODO actual data
       ylab = "ability",
       xaxt = "n")
  axis(side = 1, at = c(1,19,38,59,79,100,121,138,159))
  
  grid(nx = NA, ny = NULL)
  par(new = TRUE)
  
  plot(x = x,
       y = mu_D_U,
       ylim = c(-5, 15),  # TODO adjust
       type="l",
       col = "white",
       main = paste("driver", n),  # TODO actual data
       # main = drv_names[n],  # TODO version 2
       xlab = "qualifier/race",  # TODO actual data
       ylab = "ability",
       xaxt = "n")
  axis(side = 1, at = c(1,19,38,59,79,100,121,138,159))
  
  lines(x = x, mu_D_L, col = "white")
  
  polygon(x = c(x, rev(x)),
          y = c(mu_D_U, rev(mu_D_L)),
          col = violet_transp,
          lty = 0)
  
  lines(mu_D_sim[n,], col = "orange")  # TODO actual data
  lines(mu_D_mdn[n,], col = "blueviolet")
  
  for (t in 1:Q) {
    if (I_1[n,t] == 0) {
      abline(v = t, lwd = 0.125, col = "azure4")
    }
  }
  
  box()
  
}
par(mfrow = c(1,1))



# fit - latent constructor ability ####
# extract estimated mu_C
mu_C_est <- params_model$mu_C

# extract mu_C posterior median
mu_C_mdn <- matrix(data = NA, nrow = K, ncol = Q)

for (k in 1:K) {
  for (t in 1:Q) {
    
    sample <- mu_C_est[,k,t]
    
    median_temp <- median(sample)
    
    median <- round(median_temp, digits = 4)
    
    mu_C_mdn[k,t] <- median
    
  }
}


# extract simulated mu_C
mu_C_sim_temp <- params_model_sim$mu_C

mu_C_sim <- mu_C_sim_temp[40,,]


# constructor names
ctr_names <- c("Mercedes",
               "Red Bull",
               "McLaren",
               "Ferrari",
               "AlphaTauri",
               "Aston Martin",
               "Williams",
               "Alfa Romeo",
               "Alpine",
               "Manor Marussia",
               "Caterham",
               "Haas F1 Team")
  

# time series plot
# mu_C posterior median ( green ) vs simulated mu_C ( orange )
par(mfrow = c(5,2))
for (k in 1:K) {
  
  plot(mu_C_sim[k,],  # TODO actual data
       # mu_C_mdn[k,],  # TODO actual data
       ylim = c(-5, 25),  # TODO adjust
       type="l",
       col = "orange",  # TODO actual data
       # col = "mediumspringgreen",  # TODO actual data
       main = paste("constructor", k),  # TODO actual data
       # main = ctr_names[k],
       xlab = "qualifier/race",  # TODO actual data
       ylab = "ability",
       xaxt = "n",)
  axis(side = 1, at = c(1,19,38,59,79,100,121,138,159))
  
  grid(nx = NA, ny = NULL)
  par(new = TRUE)
  
  plot(mu_C_sim[k,],  # TODO actual data
       # mu_C_mdn[k,],  # TODO actual data
       ylim = c(-5, 25),  # TODO adjust
       type="l",
       col = "orange",  # TODO actual data
       # col = "mediumspringgreen",  # TODO actual data
       main = paste("constructor", k),  # TODO actual data
       # main = ctr_names[k],
       xlab = "qualifier/race",  # TODO actual data
       ylab = "ability",
       xaxt = "n",)
  axis(side = 1, at = c(1,19,38,59,79,100,121,138,159))
  
  lines(mu_C_mdn[k,], col = "mediumspringgreen")  # TODO actual data
  
  for (t in 1:Q) {
    if (I_3[k,t] == 0) {
      abline(v = t, lwd = 0.125, col = "azure4")
    }
  }
  
  box()
  
}
par(mfrow = c(1,1))


# mean absolute error
# mu_C posterior median vs simulated mu_C
MAE_2(mu_C_mdn, mu_C_sim, K, Q, I_3)


# mean absolute error 89% HDI
# estimated mu_C vs simulated mu_C
HDI_MAE_2(mu_C_est, mu_C_sim, K, Q, I_3, iter)


# time series plot
# estimated mu_C 89% HDI
green_transp <- rgb(0, 250, 154,
                    max = 250,
                    alpha = 25,
                    names = "mediumspringgreen")


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
       ylim = c(-5, 25),  # TODO adjust
       type="l",
       col = "white",
       main = paste("constructor", k),  # TODO actual data
       # main = ctr_names[k],
       xlab = "qualifier/race",  # TODO actual data
       ylab = "ability",
       xaxt = "n")
  axis(side = 1, at = c(1,19,38,59,79,100,121,138,159))
  
  grid(nx = NA, ny = NULL)
  par(new = TRUE)
  
  plot(x = x,
       y = mu_C_U,
       ylim = c(-5, 25),  # TODO adjust
       type="l",
       col = "white",
       main = paste("constructor", k),  # TODO actual data
       # main = ctr_names[k],
       xlab = "qualifier/race",  # TODO actual data
       ylab = "ability",
       xaxt = "n")
  axis(side = 1, at = c(1,19,38,59,79,100,121,138,159))
  
  lines(x = x, mu_C_L, col = "white")
  
  polygon(x = c(x, rev(x)),
          y = c(mu_C_U, rev(mu_C_L)),
          col = green_transp,
          lty = 0)
  
  lines(mu_C_sim[k,], col = "orange")  # TODO actual data
  lines(mu_C_mdn[k,], col = "mediumspringgreen")
  
  for (t in 1:Q) {
    if (I_3[k,t] == 0) {
      abline(v = t, lwd = 0.125, col = "azure4")
    }
  }
  
  box()
  
}
par(mfrow = c(1,1))



# model comparison - Bayes factor ####
# load fit_comp_model
fit_comp_model <-
  readRDS("results/fit_m1_v2_qualifier.rds")  # TODO data file

# compute log marginal likelihood
# model
bridge_model <- bridge_sampler(fit_model, silent = TRUE)

# comparison model
bridge_comp_model <- bridge_sampler(fit_comp_model, silent = TRUE)

# Bayes factor
BF <- bf(bridge_model, bridge_comp_model)
print(BF)


