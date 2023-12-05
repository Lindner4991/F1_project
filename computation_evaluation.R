# # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# CLOSING THE SECTIONS PROVIDES AN OVERVIEW OF THE SCRIPT
# # # # # # # # # # # # # # # # # # # # # # # # # # # # #



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
# MSE function
MSE <- function(post_mean, sim, M, Q) {
  
  MSE_temp <- 0
  
  for (t in 1:Q) {
    for (m in 1:M) {
      
      MSE_temp <- MSE_temp + (post_mean[m,t] - sim[m,t])^2
      
    }
  }
  
  total_number <- M * Q
  
  MSE <- round(MSE_temp / total_number,
               digits = 4)
  
  return(MSE)
  
}



# model 1 simulation - clean data ####
# latent qualifier performance ( mu )
mu_P <- c()
for(n in 1:N) { mu_P[n] <- mu_D_0[n] + I_1[n,] %*% mu_C_0 }
sort(mu_P, decreasing = TRUE)



# model 1 estimation - NUTS ####
model_1 <- stan_model("model_1_clean_data_V7.stan")

fit_model_1 <- sampling(model_1,
                        data = list(N = N,
                                    K = K,
                                    T = Q,
                                    I = I,
                                    J = J,
                                    R = R_sim,
                                    gamma_1 = 0,
                                    gamma_J_minus_1 = 20))


# save fit_model_1
saveRDS(fit_model_1, "fit_model_1_clean_data_V7.rds")

# load fit_model_1_sim
fit_model_1 <- readRDS("fit_model_1_clean_data_V7.rds")



# model 1 estimation - convergence ####
# warmup and sampling times ( seconds )
print(get_elapsed_time(fit_model_1))


# initial values
# get
inits <- get_inits(fit_model_1)
inits_chain1 <- inits[[1]]
inits_chain2 <- inits[[2]]
inits_chain3 <- inits[[3]]
inits_chain4 <- inits[[4]]

# chain1
inits_chain1$sigma_D
inits_chain1$sigma_C
inits_chain1$gamma

# chain2
inits_chain2$sigma_D
inits_chain2$sigma_C
inits_chain2$gamma

# chain3
inits_chain3$sigma_D
inits_chain3$sigma_C
inits_chain3$gamma

# chain4
inits_chain4$sigma_D
inits_chain4$sigma_C
inits_chain4$gamma


# divergent transitions
# get
dt <- get_sampler_params(fit_model_1)
dt_chain1 <- dt[[1]]
dt_chain2 <- dt[[2]]
dt_chain3 <- dt[[3]]
dt_chain4 <- dt[[4]]

sum(dt_chain1[1001:2000,5])
sum(dt_chain2[1001:2000,5])
sum(dt_chain3[1001:2000,5])
sum(dt_chain4[1001:2000,5])


# treedepth
mean(dt_chain1[1001:2000,3])
mean(dt_chain2[1001:2000,3])
mean(dt_chain3[1001:2000,3])
mean(dt_chain4[1001:2000,3])


# BMFI
round(get_bfmi(fit_model_1), digits = 4)


# n_eff / total iterations greater than 0.01
iter <- 4000

neff_temp <- summary(fit_model_1)$summary[,'n_eff']

neff_temp <- neff_temp / iter

neff <- neff_temp[neff_temp <= 0.01]


# BULK_ESS, Tail_ESS
ESS_temp <- monitor(extract(fit_model_1, permuted = FALSE))


# Rhat less than 1.1
Rhat_temp <- summary(fit_model_1)$summary[,'Rhat']

Rhat <- Rhat_temp[Rhat_temp >= 1.1]


# extract samples
params_model_1 <- rstan::extract(fit_model_1)


# trace plots
# sigma_D
par(mfrow = c(3,1))
ts.plot(params_model_1$sigma_D,
        col = "blueviolet",
        xlab = "Post-warmup iteration",
        ylab = "sigma_D")

# sigma_C
ts.plot(params_model_1$sigma_C,
        col = "mediumspringgreen",
        xlab = "Post-warmup iteration",
        ylab = "sigma_C")

# gamma
for (j in 2:(J-2)) {
  ts.plot(params_model_1$gamma[,j],
          col = "deeppink4",
          xlab = "Post-warmup iteration",
          ylab = paste("gamma[", j, "]", sep=""))
}
par(mfrow = c(1,1))


# autocorrelation plots
# sigma_D
color_scheme_set("purple")
mcmc_acf(fit_model_1,
         pars = c("sigma_D"),
         lags = 10)

# sigma_C
color_scheme_set("green")
mcmc_acf(fit_model_1,
         pars = c("sigma_C"),
         lags = 10)

# gamma[2]
color_scheme_set("red")
mcmc_acf(fit_model_1,
         pars = c("gamma[2]"),
         lags = 10)

# gamma[3]
color_scheme_set("red")
mcmc_acf(fit_model_1,
         pars = c("gamma[3]"),
         lags = 10)

# gamma[4]
color_scheme_set("red")
mcmc_acf(fit_model_1,
         pars = c("gamma[4]"),
         lags = 10)

# gamma[5]
color_scheme_set("red")
mcmc_acf(fit_model_1,
         pars = c("gamma[5]"),
         lags = 10)

# gamma[6]
color_scheme_set("red")
mcmc_acf(fit_model_1,
         pars = c("gamma[6]"),
         lags = 10)

# gamma[7]
color_scheme_set("red")
mcmc_acf(fit_model_1,
         pars = c("gamma[7]"),
         lags = 10)

# gamma[8]
color_scheme_set("red")
mcmc_acf(fit_model_1,
         pars = c("gamma[8]"),
         lags = 10)

# gamma[9]
color_scheme_set("red")
mcmc_acf(fit_model_1,
         pars = c("gamma[9]"),
         lags = 10)

# gamma[10]
color_scheme_set("red")
mcmc_acf(fit_model_1,
         pars = c("gamma[10]"),
         lags = 10)

# gamma[11]
color_scheme_set("red")
mcmc_acf(fit_model_1,
         pars = c("gamma[11]"),
         lags = 10)

# gamma[12]
color_scheme_set("red")
mcmc_acf(fit_model_1,
         pars = c("gamma[12]"),
         lags = 10)

# gamma[13]
color_scheme_set("red")
mcmc_acf(fit_model_1,
         pars = c("gamma[13]"),
         lags = 10)

# gamma[14]
color_scheme_set("red")
mcmc_acf(fit_model_1,
         pars = c("gamma[14]"),
         lags = 10)

# gamma[15]
color_scheme_set("red")
mcmc_acf(fit_model_1,
         pars = c("gamma[15]"),
         lags = 10)

# gamma[16]
color_scheme_set("red")
mcmc_acf(fit_model_1,
         pars = c("gamma[16]"),
         lags = 10)

# gamma[17]
color_scheme_set("red")
mcmc_acf(fit_model_1,
         pars = c("gamma[17]"),
         lags = 10)

# gamma[18]
color_scheme_set("red")
mcmc_acf(fit_model_1,
         pars = c("gamma[18]"),
         lags = 10)

# gamma[19]
color_scheme_set("red")
mcmc_acf(fit_model_1,
         pars = c("gamma[19]"),
         lags = 10)

# gamma[20]
color_scheme_set("red")
mcmc_acf(fit_model_1,
         pars = c("gamma[20]"),
         lags = 10)



# model 1 estimation - fit ####
# posterior mean, posterior median
# for sigma_D, sigma_C, corr_D_C, gamma
summary(fit_model_1,
        probs = c(0.5),
        pars = c("sigma_D",
                 "sigma_C",
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


# 89% HPDI for sigma_D
round(HPDI(as.numeric(params_model_1$sigma_D)), digits = 4)

# 89% HPDI for sigma_C
round(HPDI(as.numeric(params_model_1$sigma_C)), digits = 4)

# 89% HPDI for gamma
for (j in 2:(J-2)) {
  print(round(HPDI(as.numeric(params_model_1$gamma[,j])), digits = 4))
}


# posterior density
par(mfrow = c(1,3))
# sigma_D
hist(params_model_1$sigma_D,
     col = "blueviolet",
     border = FALSE,
     main = "sigma_D",
     xlab = "")
abline(v = 0.04, lwd = 2, col = "orange")

# sigma_C
hist(params_model_1$sigma_C,
     col = "mediumspringgreen",
     border = FALSE,
     main = "sigma_C",
     xlab = "")
abline(v = 0.16, lwd = 2, col = "orange")

# gamma
for (j in 2:(J-2)) {
  hist(params_model_1$gamma[,j],
       col = "deeppink1",
       border = FALSE,
       main = paste("gamma[", j, "]", sep=""),
       xlab = "")
  abline(v = j-1, lwd = 2, col = "orange")
}
par(mfrow = c(1,1))


# posterior predictive check
# predicted vs simulated rank
# extract predicted rank
R_pred <- matrix(data = NA, nrow = N, ncol = Q)

for (n in 1:N) {
  
  for (t in 1:Q) {
    
    R_pred_temp <-
      get_posterior_mean(fit_model_1,
                         pars = paste("R_pred[",n,",",t,"]", sep = ""))[5]
    
    R_pred[n,t] <- round(R_pred_temp, digits = 0)
    
  }

}


par(mfrow = c(5,2))
for (n in 1:N) {
  
  plot(R_sim[n,],
       ylim = c(22, 1),
       type="l",
       col = "orange",
       main = paste("driver", n),
       xlab = "qualifier",
       ylab = "rank",
       xaxt = "n",
       yaxt = "n")
  axis(side = 1, at = c(1,21,41,61,81,101,121,141,160))
  axis(side = 2, at = c(22, 11, 1), las = 1)
  
  lines(R_pred[n,], col = "deeppink1")
  
}
par(mfrow = c(1,1))


# accuracy of predicted vs simulated ranks
correct <- 0

for (t in 1:Q) {
  for (n in 1:N) {
    
    if (R_pred[n,t] == R_sim[n,t]) {
      correct = correct + 1
    }
    
  }
}

total_ranks <- N * Q

accuracy = round(correct / total_ranks,
                 digits = 4)


# accuracy of predicted vs simulated pairwise rank comparisons 
correct <- 0

for (t in 1:Q) {
  
  counter <- N-1
  
  for (n in 1:(N-1)) {
    
    for (c in 1:counter) {
      
      if (((R_pred[n,t] > R_pred[n+c,t]) & (R_sim[n,t] > R_sim[n+c,t])) |
          ((R_pred[n,t] < R_pred[n+c,t]) & (R_sim[n,t] < R_sim[n+c,t])) |
          ((R_pred[n,t] == R_pred[n+c,t]) & (R_sim[n,t] == R_sim[n+c,t])))
      {
        correct <- correct + 1
      }
      
    }
    
    counter <- counter - 1
    
  }
  
}

total_comparisons <- (N-1) * N / 2 * Q

accuracy <- round(correct / total_comparisons,
                  digits = 4)


# 89% HPDI for predicted rank
par(mfrow = c(5,2))
for (n in 1:N) {
  
  R_pred_U <- c()
  R_pred_L <- c()
  for (t in 1:Q) {
    
    R_pred_U_temp <- HPDI(as.numeric(params_model_1$R_pred[,n,t]))[2]
    R_pred_U <- c(R_pred_U, R_pred_U_temp)
    
    R_pred_L_temp <- HPDI(as.numeric(params_model_1$R_pred[,n,t]))[1]
    R_pred_L <- c(R_pred_L, R_pred_L_temp)
   
  }
  
  x <- 1:160
  
  plot(x = x,
       y = R_pred_U,
       ylim = c(22, 1),
       type="l",
       col = "deeppink1",
       main = paste("driver", n),
       xlab = "qualifier",
       ylab = "89% HPDI r",
       xaxt = "n",
       yaxt = "n")
  axis(side = 1, at = c(1,21,41,61,81,101,121,141,160))
  axis(side = 2, at = c(22, 11, 1), las = 1)
  
  lines(x = x, R_pred_L, col = "deeppink1")
  
  polygon(x = c(x, rev(x)),
          y = c(R_pred_U, rev(R_pred_L)),
          col = "deeppink1",
          lty = 0)
  
}
par(mfrow = c(1,1))


# posterior mean for mu_P vs simulated mu_P
# extract simulated mu_P
mu_P_sim_temp <- params_model_1_sim$mu_P

mu_P_sim <- matrix(data = NA, nrow = N, ncol = Q)

for (t in 1:Q) {
  iter_40 <- mu_P_sim_temp[40,,t]
  mu_P_sim[,t] <- iter_40
}


# extract posterior mean for mu_P
mu_P <- matrix(data = NA, nrow = N, ncol = Q)

for (n in 1:N) {
  
  for (t in 1:Q) {
    
    mu_P_temp <-
      get_posterior_mean(fit_model_1,
                         pars = paste("mu_P[",n,",",t,"]", sep = ""))[5]
    
    mu_P[n,t] <- round(mu_P_temp, digits = 0)
    
  }
  
}


par(mfrow = c(5,2))
for (n in 1:N) {
  
  plot(mu_P_sim[n,],
       ylim = c(-5, 25),
       type="l",
       col = "orange",
       main = paste("driver", n),
       xlab = "qualifier",
       ylab = "mu_P",
       xaxt = "n",
       yaxt = "n")
  axis(side = 1, at = c(1,21,41,61,81,101,121,141,160))
  axis(side = 2, at = c(-5, 5, 15), las = 1)
  
  lines(mu_P[n,], col = "deeppink1")
  
}
par(mfrow = c(1,1))


# posterior mean for mu_P vs simulated mu_P
# mean squared error
MSE_mu_P <- MSE(mu_P, mu_P_sim, N, Q)


# 89% HPDI for mu_P
par(mfrow = c(5,2))
for (n in 1:N) {
  
  mu_P_U <- c()
  mu_P_L <- c()
  for (t in 1:Q) {
    
    mu_P_U_temp <- HPDI(as.numeric(params_model_1$mu_P[,n,t]))[2]
    mu_P_U <- c(mu_P_U, mu_P_U_temp)
    
    mu_P_L_temp <- HPDI(as.numeric(params_model_1$mu_P[,n,t]))[1]
    mu_P_L <- c(mu_P_L, mu_P_L_temp)
    
  }
  
  x <- 1:160
  
  plot(x = x,
       y = mu_P_U,
       ylim = c(-5, 25),
       type="l",
       col = "deeppink1",
       main = paste("driver", n),
       xlab = "qualifier",
       ylab = "89% HPDI mu_P",
       xaxt = "n",
       yaxt = "n")
  axis(side = 1, at = c(1,21,41,61,81,101,121,141,160))
  axis(side = 2, at = c(20, 10, 1), las = 1)
  
  lines(x = x, mu_P_L, col = "deeppink1")
  
  polygon(x = c(x, rev(x)),
          y = c(mu_P_U, rev(mu_P_L)),
          col = "deeppink1",
          lty = 0)
  
}
par(mfrow = c(1,1))


# posterior mean for mu_D vs simulated mu_D
# extract simulated mu_D
mu_D_sim_temp <- params_m1_v1_sim$mu_D

mu_D_sim <- matrix(data = NA, nrow = N, ncol = Q)

for (t in 1:Q) {
  iter_40 <- mu_D_sim_temp[40,,t]
  mu_D_sim[,t] <- iter_40
}


# extract posterior mean for mu_D
mu_D <- matrix(data = NA, nrow = N, ncol = Q)

for (n in 1:N) {
  
  for (t in 1:Q) {
    
    mu_D_temp <-
      get_posterior_mean(fit_model_1,
                         pars = paste("mu_D[",n,",",t,"]", sep = ""))[5]
    
    mu_D[n,t] <- round(mu_D_temp, digits = 0)
    
  }
  
}

par(mfrow = c(5,2))
for (n in 1:N) {

  plot(mu_D_sim[17,],
       ylim = c(-5, 15),
       type="l",
       col = "orange",
       main = paste("driver", 19),
       xlab = "qualifier",
       ylab = "mu_D",
       xaxt = "n",
       yaxt = "n")
  axis(side = 1, at = c(1,21,41,61,81,101,121,141,159))
  axis(side = 2, at = c(-5, 5, 15), las = 1)
  
  lines(mu_D[n,], col = "blueviolet")
  
}
par(mfrow = c(1,1))


# posterior mean for mu_D vs simulated mu_D
# mean squared error
MSE_mu_D <- MSE(mu_D, mu_D_sim, N, Q)


# 89% HPDI for mu_D
par(mfrow = c(5,2))
for (n in 1:N) {
  
  mu_D_U <- c()
  mu_D_L <- c()
  for (t in 1:Q) {
    
    mu_D_U_temp <- HPDI(as.numeric(params_model_1$mu_D[,n,t]))[2]
    mu_D_U <- c(mu_D_U, mu_D_U_temp)
    
    mu_D_L_temp <- HPDI(as.numeric(params_model_1$mu_D[,n,t]))[1]
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
  axis(side = 1, at = c(1,21,41,61,81,101,121,141,160))
  axis(side = 2, at = c(-5, 5, 15), las = 1)
  
  lines(x = x, mu_D_L, col = "blueviolet")
  
  polygon(x = c(x, rev(x)),
          y = c(mu_D_U, rev(mu_D_L)),
          col = "blueviolet",
          lty = 0)
  
}
par(mfrow = c(1,1))


# posterior mean for mu_C vs simulated mu_C
# extract simulated mu_C
mu_C_sim_temp <- params_m1_v1_sim$mu_C

mu_C_sim <- matrix(data = NA, nrow = K, ncol = Q)

for (t in 1:Q) {
  iter_40 <- mu_C_sim_temp[40,,t]
  mu_C_sim[,t] <- iter_40
}


# extract posterior mean for mu_C
mu_C <- matrix(data = NA, nrow = K, ncol = Q)

for (k in 1:K) {
  
  for (t in 1:Q) {
    
    mu_C_temp <-
      get_posterior_mean(fit_model_1,
                         pars = paste("mu_C[",k,",",t,"]", sep = ""))[5]
    
    mu_C[k,t] <- round(mu_C_temp, digits = 0)
    
  }
  
}


par(mfrow = c(5,2))
for (k in 1:K) {
  
  plot(mu_C_sim[12,],
       ylim = c(-5, 15),
       type="l",
       col = "orange",
       main = paste("constructor", 10),
       xlab = "qualifier",
       ylab = "mu_C",
       xaxt = "n",
       yaxt = "n")
  axis(side = 1, at = c(1,21,41,61,81,101,121,141,159))
  axis(side = 2, at = c(-5, 5, 15), las = 1)
  
  lines(mu_C[k,], col = "mediumspringgreen")
  
}
par(mfrow = c(1,1))


# posterior mean for mu_C vs simulated mu_C
# mean squared error
MSE_mu_C <- MSE(mu_C, mu_C_sim, K, Q)


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


# mu_D vs mu_C ( posterior mean )
par(mfrow = c(2,2))
for (n in 1:N) {
  
  mu_C_I <- c()
  
  for (t in 1:Q) {
    
    k_I <- which(I[[t]][n,] == 1)
    mu_C_I <- c(mu_C_I, mu_C[k_I,t])
    
  }
  
  plot(mu_P[n,],
       ylim = c(-5, 25),
       type="l",
       col = "deeppink1",
       main = paste("driver",n),
       xlab = "qualifier",
       ylab = "mu",
       xaxt = "n",
       yaxt = "n")
  axis(side = 1, at = c(1,21,41,61,81,101,121,141,160))
  axis(side = 2, at = c(-5, 10, 25), las = 1)
  
  lines(mu_D[n,], col = "blueviolet")
  
  lines(mu_C_I, col = "springgreen2")
  
}
par(mfrow = c(1,1))


