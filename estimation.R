# # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# CLOSING THE SECTIONS PROVIDES AN OVERVIEW OF THE SCRIPT #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # #



# # # # # # # # # # # # # # # # # # # # # # # # #
# THIS FILE BUILDS ON THE simulated_data.R FILE #
# # # # # # # # # # # # # # # # # # # # # # # # #



# general prep ####
# set working directory
setwd("C:/Users/Diiim/Documents/job_uni/Master/Thesis/F1_project")
# setwd("~/F1_project")

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
library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)



# model 1 version 1 - simulated data - clean data ####
# fixed cut points
gamma_lower <- 0
gamma_upper <- J - 1

# load fit_m1_v1_sim
fit_m1_v1_sim <- readRDS("data/fit_m1_v1_sim_clean_data.rds")
# fit_m1_v1_sim <- readRDS("fit_m1_v1_sim_clean_data.rds")

# extract simulations
params_m1_v1_sim <- rstan::extract(fit_m1_v1_sim)

# extract simulated qualifier/race ranks
R_sim_temp <- params_m1_v1_sim$R_sim

R_sim <- R_sim_temp[40,,]



# model 1 version 1 - simulated data - missing data ####
# number of constructors in time series
# fixed cut points
gamma_lower <- 0
gamma_upper <- J - 1

# load fit_m1_v1_sim
fit_m1_v1_sim <- readRDS("data/fit_m1_v1_sim_missing_data.rds")
# fit_m1_v1_sim <- readRDS("fit_m1_v1_sim_missing_data.rds")

# extract simulations
params_m1_v1_sim <- rstan::extract(fit_m1_v1_sim)

# extract simulated qualifier/race ranks
R_sim_temp <- params_m1_v1_sim$R_sim

# extract simulated qualifier/race ranks
R_sim_temp <- params_m1_v1_sim$R_sim

R_sim <- R_sim_temp[40,,]



# model 1 version 1 - greater ability fluctuations ####
# number of constructors in time series



# model 1 version 1 - 20% driver 80% constructor ####
# number of constructors in time series



# model 1 version 1 - F1 hybrid era qualifiers ####
# constructor qualifier NA indicators
I_3 <- matrix(data = 1, nrow = K, ncol = Q)

# NAs for constructor with ID 10 ( manor )
I_3[10,c(17:19,60:160)] <- 0 # TODO first race

# NAs for constructor with ID 11 ( caterham )
I_3[11,17:160] <- 0 # TODO first race

# NAs for constructor with ID 12 ( haas )
I_3[12,1:39] <- 0 # TODO first race



# model 1 version 1 - estimation ####
# computation with NUTS in STAN
m1_v1 <- stan_model("STAN/m1_v1.stan")
# m1_v1 <- stan_model("m1_v1.stan")

# number of post-warmup iterations per chain
iter_per_chain <- 2000

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
                                  gamma_lower = gamma_lower,
                                  gamma_upper = gamma_upper,
                                  R = R_sim),
                      iter = iter_per_chain)

# save fit_m1_v1
saveRDS(fit_m1_v1, "results/fit_m1_v1_clean_data.rds")
# saveRDS(fit_m1_v1, "fit_m1_v1_missing_data.rds")


