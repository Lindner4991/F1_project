# Hamilton in RB vs Verstappen in MERC, 2021 season ####
mu_D_est <- mu_D_est[,,138:159]
mu_C_est <- mu_C_est[,,138:159]

mu_D_est_HAM <- mu_D_est[,1,]
mu_D_est_VER <- mu_D_est[,4,]

mu_C_est_MERC <- mu_C_est[,1,]
mu_C_est_RB <- mu_C_est[,2,]

mu_P_est_HAM <- mu_D_est_HAM + mu_C_est_RB
mu_P_est_VER <- mu_D_est_VER + mu_C_est_MERC

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

mu_P_U_HAM <- c()
mu_P_L_HAM <- c()
for (t in 1:22) {
  
  mu_P_U_HAM_temp <- HPDI(mu_P_est_HAM[,t])[2]
  mu_P_U_HAM <- c(mu_P_U_HAM, mu_P_U_HAM_temp)
  
  mu_P_L_HAM_temp <- HPDI(mu_P_est_HAM[,t])[1]
  mu_P_L_HAM <- c(mu_P_L_HAM, mu_P_L_HAM_temp)
  
}

mu_P_U_VER <- c()
mu_P_L_VER <- c()
for (t in 1:22) {
  
  mu_P_U_VER_temp <- HPDI(mu_P_est_VER[,t])[2]
  mu_P_U_VER <- c(mu_P_U_VER, mu_P_U_VER_temp)
  
  mu_P_L_VER_temp <- HPDI(mu_P_est_VER[,t])[1]
  mu_P_L_VER <- c(mu_P_L_VER, mu_P_L_VER_temp)
  
}

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


mu_P_mdn_HAM <- rep(NA, 22)

for (t in 1:22) {
  
  median <- mean(mu_P_est_HAM[,t])
  
  mu_P_mdn_HAM[t] <- median
    
}

mu_P_mdn_VER <- rep(NA, 22)

for (t in 1:22) {
  
  median <- mean(mu_P_est_VER[,t])
  
  mu_P_mdn_VER[t] <- median
  
}

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


x <- 1:22

VER_transp <- rgb(255, 0, 255,
                  max = 255,
                  alpha = 25,
                  names = "magenta1")

HAM_transp <- rgb(0, 255, 255,
                  max = 255,
                  alpha = 25,
                  names = "cyan1")

par(mfrow = c(2,1))

plot(x = x,
     y = mu_P_U_HAM,
     ylim = c(0, 30),
     type="l",
     col = "white",
     main = "",
     xlab = "",  
     ylab = "",
     xaxt = "n",
     yaxt = "n",
     cex.main = 1.5,
     cex.lab = 1.5,
     cex.axis = 1.5)

grid(nx = NA, ny = NULL)
par(new = TRUE)

plot(x = x,
     y = mu_P_U_HAM,
     ylim = c(0, 30),  
     type="l",
     col = "white",
     main = "a) expected qualifier performances",
     xlab = "qualifier",  
     ylab = "performance",
     xaxt = "n",
     yaxt = "n",
     cex.main = 1.5,
     cex.lab = 1.5,
     cex.axis = 1.5)
axis(side = 1, c(1,11,22))
axis(side = 2, at = c(0, 15, 30))

lines(x = x, mu_P_L_HAM, col = "white")

polygon(x = c(x, rev(x)),
        y = c(mu_P_U_HAM, rev(mu_P_L_HAM)),
        col = HAM_transp,
        lty = 0)

polygon(x = c(x, rev(x)),
        y = c(mu_P_U_VER, rev(mu_P_L_VER)),
        col = VER_transp,
        lty = 0)

lines(mu_P_mdn_HAM, col = "cyan1")
lines(mu_P_mdn_VER, col = "magenta1")

text(x = 3,
     y = 2.5,
     labels = "Lewis Hamilton",
     col = "cyan1")

text(x = 6,
     y = 2.5,
     labels = "Max Verstappen",
     col = "magenta1")

box()

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

prob_HAM_outperf_VER <- rep(NA, 22)

for (t in 1:22) {
  
  count_HAM_outperf_VER <- 0
  
  for (i in 1:iter) {
    
    if (mu_P_est_HAM[i,t] > mu_P_est_VER[i,t]) { 
      count_HAM_outperf_VER <- count_HAM_outperf_VER + 1
    }
    
  }
  
  prob_HAM_outperf_VER[t] <- count_HAM_outperf_VER / iter
  
}

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

names(prob_HAM_outperf_VER) <- as.character(1:22)

barplot(prob_HAM_outperf_VER,
        ylim = c(0, 1),
        col = "cyan1",
        border = "white",
        main = "",
        xlab = "",
        ylab = "",
        yaxt = "n",
        cex.main = 1.5,
        cex.lab = 1.5)

grid(nx = NA, ny = NULL)
par(new = TRUE)

barplot(prob_HAM_outperf_VER,
        ylim = c(0, 1),
        col = "cyan1",
        border = "white",
        main = "b) probability for Hamilton outperforming Verstappen",  # TODO tbd
        xlab = "qualifier",
        ylab = "probability",
        yaxt = "n",
        cex.main = 1.5,
        cex.lab = 1.5)
axis(side = 2, at = c(0, 0.5, 1))

abline(h = 0.5, lwd = 0.125, lty = 2)

box()

# Perez in RB vs Perez in Aston Martin, 2020 season ####
R_pred <- R_pred[,,121:137]
mu_D_est <- mu_D_est[,,121:137]
mu_C_est <- mu_C_est[,,121:137]
mu_P_est <- mu_P_est[,,121:137]

R_pred_PER_Aston <- R_pred[,12,]
mu_D_est_PER <- mu_D_est[,12,]
mu_C_est_RB <- mu_C_est[,2,]

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

m2_scenario <- stan_model("STAN/m2_scenario_analysis.stan")

fit_m2_scenario <- sampling(m2_scenario,
                            data = list(iter = iter,
                                        T = 17,
                                        mu_C = mu_C_est_RB,
                                        mu_D = mu_D_est_PER,
                                        J = J,
                                        varsigma_C = params_model$varsigma_C,
                                        varsigma_D = params_model$varsigma_D,
                                        gamma = params_model$gamma),
                            algorithm = "Fixed_param",
                            iter = 10,
                            warmup = 0)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

params_m2_scenario <- rstan::extract(fit_m2_scenario)

R_pred_temp <- params_m2_scenario$R_pred

R_pred_PER_RB <- R_pred_temp[40,,]

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

R_pred_U_PER_Aston <- c()
R_pred_L_PER_Aston <- c()
for (t in 1:17) {
  
  R_pred_U_PER_Aston_temp <- HPDI(R_pred_PER_Aston[,t], 0.89)[2]
  R_pred_U_PER_Aston <- c(R_pred_U_PER_Aston, R_pred_U_PER_Aston_temp)
  
  R_pred_L_PER_Aston_temp <- HPDI(R_pred_PER_Aston[,t], 0.89)[1]
  R_pred_L_PER_Aston <- c(R_pred_L_PER_Aston, R_pred_L_PER_Aston_temp)
  
}

R_pred_U_PER_RB <- c()
R_pred_L_PER_RB <- c()
for (t in 1:17) {
  
  R_pred_U_PER_RB_temp <- HPDI(R_pred_PER_RB[,t], 0.89)[2]
  R_pred_U_PER_RB <- c(R_pred_U_PER_RB, R_pred_U_PER_RB_temp)
  
  R_pred_L_PER_RB_temp <- HPDI(R_pred_PER_RB[,t], 0.89)[1]
  R_pred_L_PER_RB <- c(R_pred_L_PER_RB, R_pred_L_PER_RB_temp)
  
}

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

R_pred_mdn_PER_RB <- rep(NA, 17)

for (t in 1:17) {
  
  median <- median(R_pred_PER_RB[,t])
  
  R_pred_mdn_PER_RB[t] <- median
  
}

R_pred_mdn_PER_Aston <- rep(NA, 17)

for (t in 1:17) {
  
  median <- median(R_pred_PER_Aston[,t])
  
  R_pred_mdn_PER_Aston[t] <- median
  
}

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

R_pred_mean_PER_RB <- rep(NA, 17)

for (t in 1:17) {
  
  mean <- mean(R_pred_PER_RB[,t])
  
  R_pred_mean_PER_RB[t] <- mean
  
}

R_pred_mean_PER_Aston <- rep(NA, 17)

for (t in 1:17) {
  
  mean <- mean(R_pred_PER_Aston[,t])
  
  R_pred_mean_PER_Aston[t] <- mean
  
}

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

x <- 1:17

PER_RB_transp <- rgb(255, 0, 255,
                     max = 255,
                     alpha = 25,
                     names = "magenta1")

PER_Aston_transp <- rgb(0, 255, 255,
                        max = 255,
                        alpha = 25,
                        names = "cyan1")

par(mfrow = c(3,1))

plot(x = x,
     y = R_pred_U_PER_RB,
     ylim = c(22, 1),
     type="l",
     col = "white",
     main = "",
     xlab = "",  
     ylab = "",
     xaxt = "n",
     yaxt = "n",
     cex.main = 1.5,
     cex.lab = 1.5,
     cex.axis = 1.5)

grid(nx = NA, ny = NULL)
par(new = TRUE)

plot(x = x,
     y = R_pred_U_PER_RB,
     ylim = c(22, 1),  
     type="l",
     col = "white",
     main = "a) 89% HDI for ranking",
     xlab = "race",  
     ylab = "rank",
     xaxt = "n",
     yaxt = "n",
     cex.main = 1.5,
     cex.lab = 1.5,
     cex.axis = 1.5)
axis(side = 1, c(1,10,17))
axis(side = 2, at = c(22, 10, 5))

lines(x = x, R_pred_L_PER_RB, col = "white")

polygon(x = c(x, rev(x)),
        y = c(R_pred_U_PER_RB, rev(R_pred_L_PER_RB)),
        col = PER_RB_transp,
        lty = 0)

polygon(x = c(x, rev(x)),
        y = c(R_pred_U_PER_Aston, rev(R_pred_L_PER_Aston)),
        col = PER_Aston_transp,
        lty = 0)

lines(R_pred_mdn_PER_RB, col = "magenta1")
lines(R_pred_mdn_PER_Aston, col = "cyan1")

lines(R_pred_mean_PER_RB, col = "magenta1", lty = 2)
lines(R_pred_mean_PER_Aston, col = "cyan1", lty = 2)

text(x = 3,
     y = 21,
     labels = "Perez in Aston Martin",
     col = "cyan1",
     cex = 1.5)

text(x = 6,
     y = 21,
     labels = "Perez in Red Bull",
     col = "magenta1",
     cex = 1.5)

box()

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

mu_P_PER_RB <- mu_D_est_PER + mu_C_est_RB

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

prob_PER_RB_pole <- rep(NA, 17)

for (t in 1:17) {
  
  count_pole <- 0
  
  for (i in 1:iter) {
    
    if (mu_P_PER_RB[i,t] > max(mu_P_est[i,,t])) { 
      count_pole <- count_pole + 1
    }
    
  }
  
  prob_PER_RB_pole[t] <- count_pole / iter
  
}

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

prob_PER_RB_podium <- rep(NA, 17)

for (t in 1:17) {
  
  count_podium <- 0
  
  for (i in 1:iter) {
    
    if (mu_P_PER_RB[i,t] > sort(mu_P_est[i,,t],
                                decreasing = TRUE)[4]) { 
      count_podium <- count_podium + 1
    }
    
  }
  
  prob_PER_RB_podium[t] <- count_podium / iter
  
}

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

names(prob_PER_RB_podium) <- as.character(1:17)

barplot(prob_PER_RB_podium,
        ylim = c(0, 1),
        col = "magenta1",
        border = "white",
        main = "",
        xlab = "",
        ylab = "",
        yaxt = "n",
        cex.main = 1.5,
        cex.lab = 1.5)

grid(nx = NA, ny = NULL)
par(new = TRUE)

barplot(prob_PER_RB_podium,
        ylim = c(0, 1),
        col = "magenta1",
        border = "white",
        main = "c) podium probability for PER in RB based on race peformance",
        xlab = "qualifier",
        ylab = "probability",
        yaxt = "n",
        cex.main = 1.5,
        cex.lab = 1.5)
axis(side = 2, at = c(0, 0.5, 1))

abline(h = 0.5, lwd = 0.125, lty = 2)

box()








