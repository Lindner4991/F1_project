# # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# CLOSING THE SECTIONS PROVIDES AN OVERVIEW OF THE SCRIPT
# # # # # # # # # # # # # # # # # # # # # # # # # # # # #



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
library(rstan)
rstan_options(auto_write = TRUE)



# model 1 version 1 - clean data ####
# number of constructors in time series
K <- 11

# number of drivers in time series
N <- 22

# number of qualifiers/races
Q <- 159

# constructor qualifier/race NA indicators
I_3 <- matrix(data = 1, nrow = K, ncol = Q)

# constructor indicators
I_2_1 <- matrix(data = NA, nrow = N, ncol = K)
I_2_1[1,] <- c(1,0,0,0,0,0,0,0,0,0,0)
I_2_1[2,] <- c(1,0,0,0,0,0,0,0,0,0,0)
I_2_1[3,] <- c(0,1,0,0,0,0,0,0,0,0,0)
I_2_1[4,] <- c(0,1,0,0,0,0,0,0,0,0,0)
I_2_1[5,] <- c(0,0,1,0,0,0,0,0,0,0,0)
I_2_1[6,] <- c(0,0,1,0,0,0,0,0,0,0,0)
I_2_1[7,] <- c(0,0,0,1,0,0,0,0,0,0,0)
I_2_1[8,] <- c(0,0,0,1,0,0,0,0,0,0,0)
I_2_1[9,] <- c(0,0,0,0,1,0,0,0,0,0,0)
I_2_1[10,] <- c(0,0,0,0,1,0,0,0,0,0,0)
I_2_1[11,] <- c(0,0,0,0,0,1,0,0,0,0,0)
I_2_1[12,] <- c(0,0,0,0,0,1,0,0,0,0,0)
I_2_1[13,] <- c(0,0,0,0,0,0,1,0,0,0,0)
I_2_1[14,] <- c(0,0,0,0,0,0,1,0,0,0,0)
I_2_1[15,] <- c(0,0,0,0,0,0,0,1,0,0,0)
I_2_1[16,] <- c(0,0,0,0,0,0,0,1,0,0,0)
I_2_1[17,] <- c(0,0,0,0,0,0,0,0,1,0,0)
I_2_1[18,] <- c(0,0,0,0,0,0,0,0,1,0,0)
I_2_1[19,] <- c(0,0,0,0,0,0,0,0,0,1,0)
I_2_1[20,] <- c(0,0,0,0,0,0,0,0,0,1,0)
I_2_1[21,] <- c(0,0,0,0,0,0,0,0,0,0,1)
I_2_1[22,] <- c(0,0,0,0,0,0,0,0,0,0,1)

I_2_2 <- matrix(data = NA, nrow = N, ncol = K)
I_2_2[1,] <- c(1,0,0,0,0,0,0,0,0,0,0)
I_2_2[2,] <- c(1,0,0,0,0,0,0,0,0,0,0)
I_2_2[3,] <- c(0,0,0,0,0,0,0,0,0,1,0)  # switch
I_2_2[4,] <- c(0,1,0,0,0,0,0,0,0,0,0)
I_2_2[5,] <- c(0,0,1,0,0,0,0,0,0,0,0)
I_2_2[6,] <- c(0,0,1,0,0,0,0,0,0,0,0)
I_2_2[7,] <- c(0,0,0,1,0,0,0,0,0,0,0)
I_2_2[8,] <- c(0,0,0,1,0,0,0,0,0,0,0)
I_2_2[9,] <- c(0,0,0,0,1,0,0,0,0,0,0)
I_2_2[10,] <- c(0,0,0,0,1,0,0,0,0,0,0)
I_2_2[11,] <- c(0,0,0,0,0,1,0,0,0,0,0)
I_2_2[12,] <- c(0,0,0,0,0,1,0,0,0,0,0)
I_2_2[13,] <- c(0,0,0,0,0,0,1,0,0,0,0)
I_2_2[14,] <- c(0,0,0,0,0,0,1,0,0,0,0)
I_2_2[15,] <- c(0,0,0,0,0,0,0,1,0,0,0)
I_2_2[16,] <- c(0,0,0,0,0,0,0,1,0,0,0)
I_2_2[17,] <- c(0,0,0,0,0,0,0,0,1,0,0)
I_2_2[18,] <- c(0,0,0,0,0,0,0,0,1,0,0)
I_2_2[19,] <- c(0,1,0,0,0,0,0,0,0,0,0)  # switch
I_2_2[20,] <- c(0,0,0,0,0,0,0,0,0,1,0)
I_2_2[21,] <- c(0,0,0,0,0,0,0,0,0,0,1)
I_2_2[22,] <- c(0,0,0,0,0,0,0,0,0,0,1)

I_2 <- list(I_2_1,I_2_1,I_2_1,I_2_1,I_2_1,I_2_1,I_2_1,I_2_1,I_2_1,I_2_1,
            I_2_1,I_2_1,I_2_1,I_2_1,I_2_1,I_2_1,I_2_1,I_2_1,I_2_1,I_2_1,
            I_2_1,I_2_1,I_2_1,I_2_1,I_2_1,I_2_1,I_2_1,I_2_1,I_2_1,I_2_1,
            I_2_1,I_2_1,I_2_1,I_2_1,I_2_1,I_2_1,I_2_1,I_2_1,I_2_1,I_2_1,
            I_2_1,I_2_1,I_2_1,I_2_1,I_2_1,I_2_1,I_2_1,I_2_1,I_2_1,I_2_1,
            I_2_1,I_2_1,I_2_1,I_2_1,I_2_1,I_2_1,I_2_1,I_2_1,I_2_1,I_2_1,
            I_2_1,I_2_1,I_2_1,I_2_1,I_2_1,I_2_1,I_2_1,I_2_1,I_2_1,I_2_1,
            I_2_2,I_2_2,I_2_2,I_2_2,I_2_2,I_2_2,I_2_2,I_2_2,I_2_2,I_2_2,
            I_2_2,I_2_2,I_2_2,I_2_2,I_2_2,I_2_2,I_2_2,I_2_2,I_2_2,I_2_2,
            I_2_2,I_2_2,I_2_2,I_2_2,I_2_2,I_2_2,I_2_2,I_2_2,I_2_2,I_2_2,
            I_2_2,I_2_2,I_2_2,I_2_2,I_2_2,I_2_2,I_2_2,I_2_2,I_2_2,I_2_2,
            I_2_2,I_2_2,I_2_2,I_2_2,I_2_2,I_2_2,I_2_2,I_2_2,I_2_2,I_2_2,
            I_2_2,I_2_2,I_2_2,I_2_2,I_2_2,I_2_2,I_2_2,I_2_2,I_2_2,I_2_2,
            I_2_2,I_2_2,I_2_2,I_2_2,I_2_2,I_2_2,I_2_2,I_2_2,I_2_2,I_2_2,
            I_2_2,I_2_2,I_2_2,I_2_2,I_2_2,I_2_2,I_2_2,I_2_2,I_2_2,I_2_2,
            I_2_2,I_2_2,I_2_2,I_2_2,I_2_2,I_2_2,I_2_2,I_2_2,I_2_2)

# driver qualifier/race NA indicators
I_1 <- matrix(data = 1, nrow = N, ncol = Q)

# number of ranks per qualifier/race
J <- 22

# initial conditions for latent constructor ability state equations
c_0 <- c(10,9,8,7,6,5,4,3,2,1,0)

# initial conditions for latent driver ability state equations
d_0 <- c(10.25,9.75,9.25,8.75,8.25,
         7.75,7.25,6.75,6.25,5.75,
         5.25,4.75,4.25,3.75,3.25,
         2.75,2.25,1.75,1.25,0.75,
         0.25,-0.25)

# fixed cut points
gamma_lower <- 0
gamma_upper <- J - 1

# load fit_m1_v1_sim
fit_model_1_sim <- readRDS("data/fit_m1_v1_sim_clean_data.rds")

# extract simulations
params_m1_v1_sim <- rstan::extract(fit_m1_v1_sim)

# extract simulated qualifier/race ranks
R_sim_temp <- params_m1_v1_sim$R_sim

R_sim <- matrix(data = NA, nrow = N, ncol = Q)

for (t in 1:Q) {
  iter_40 <- R_sim_temp[40,,t]
  R_sim[,t] <- iter_40
}



# model 1 version 1 - estimation ####
# computation with NUTS in STAN
m1_v1 <- stan_model("STAN/m1_v1.stan")

fit_m1_v1 <- sampling(m1_v1,
                      data = list(K = K,
                                  N = N,
                                  T = Q,
                                  I_3 = I_3,
                                  I_2 = I_2,
                                  I_1 = I_1,
                                  J = J,
                                  c_0 = c_0,
                                  d_0 = d_0,
                                  gamma_lower,
                                  gamma_upper,
                                  R = R_sim),
                      iter = 2000)

# save fit_m1_v1
saveRDS(fit_model_1, "results/fit_m1_v1_clean_data.rds")


