# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


# closing the sections provides an overview of the script


# this file builds on the simulated_data.R and actual_data.R file


# required data files:
# fit_m___sim___.rds
# where ___ is a placeholder


# required model files:
# m___.stan
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
# todor::todor_file("R/estimation.R")

library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)



# model 1 version 1 - simulated data - clean data ####
# fixed cut points
gamma_lower <- 0
gamma_upper <- J - 2

# load fit_m1_v1_sim
fit_m1_v1_sim <- readRDS("data/fit_m1_v1_sim_clean_data.rds")
# fit_m1_v1_sim <- readRDS("fit_m1_v1_sim_clean_data.rds")

# extract simulations
params_m1_v1_sim <- rstan::extract(fit_m1_v1_sim)

# extract simulated qualifier/race ranks
R_sim_temp <- params_m1_v1_sim$R_sim

R_sim <- R_sim_temp[40,,]



# model 1 version 1 - simulated data - missing data ####
# fixed cut points
gamma_lower <- 0
gamma_upper <- J - 2

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



# model 1 version 1 - increased fluctuations ####
# fixed cut points
gamma_lower <- 0
gamma_upper <- J - 2

# load fit_m1_v1_sim
fit_m1_v1_sim <- readRDS("data/fit_m1_v1_sim_increased_fluctuations.rds")
# fit_m1_v1_sim <- readRDS("fit_m1_v1_sim_greater_fluctuations.rds")

# extract simulations
params_m1_v1_sim <- rstan::extract(fit_m1_v1_sim)

# extract simulated qualifier/race ranks
R_sim_temp <- params_m1_v1_sim$R_sim

# extract simulated qualifier/race ranks
R_sim_temp <- params_m1_v1_sim$R_sim

R_sim <- R_sim_temp[40,,]



# model 1 version 1 - dominant ctr ability ####
# fixed cut points
gamma_lower <- 0
gamma_upper <- J - 2

# load fit_m1_v1_sim
fit_m1_v1_sim <- readRDS("data/fit_m1_v1_sim_dominant_ctr_ability.rds")
# fit_m1_v1_sim <- readRDS("fit_m1_v1_sim_missing_data.rds")

# extract simulations
params_m1_v1_sim <- rstan::extract(fit_m1_v1_sim)

# extract simulated qualifier/race ranks
R_sim_temp <- params_m1_v1_sim$R_sim

# extract simulated qualifier/race ranks
R_sim_temp <- params_m1_v1_sim$R_sim

R_sim <- R_sim_temp[40,,]



# model 1 version 1 - F1 hybrid era qualifier data ####
# fixed cut points
gamma_lower <- 0
gamma_upper <- J - 2



# model 1 version 1 - F1 hybrid era race data ####
# initial conditions for latent constructor ability state equation ( 75% )
# fixed cut points
gamma_lower <- 0
gamma_upper <- J - 2





# model 1 version 1 - estimation - local ####
# computation with NUTS in STAN
m1_v1 <- stan_model("STAN/m1_v1.stan")

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
                                  mu_C_0 = mu_C_0,
                                  mu_D_0 = mu_D_0,
                                  gamma_lower = gamma_lower,
                                  gamma_upper = gamma_upper,
                                  R = R_sim),  # TODO actual data
                      iter = iter_per_chain)

# save fit_m1_v1
saveRDS(fit_m1_v1,
        "results/fit_m1_v1_dominant_ctr_ability.rds")  # TODO adjust name



# model 1 version 1 - estimation - DSRI ####
# computation with NUTS in STAN
m1_v1 <- stan_model("m1_v1.stan")

# number of post-warmup iterations per chain
iter_per_chain <- 2000

job::job({
  
  fit_m1_v1 <- sampling(m1_v1,
                        data = list(K = K,
                                    N = N,
                                    T = Q,
                                    I_3 = I_3,
                                    I_2 = I_2,
                                    I_1 = I_1,
                                    J = J,
                                    mu_C_0 = mu_C_0,
                                    mu_D_0 = mu_D_0,
                                    gamma_lower = gamma_lower,
                                    gamma_upper = gamma_upper,
                                    R = R_sim),  # TODO actual data
                        iter = iter_per_chain)
  
  # save fit_m1_v1
  saveRDS(fit_m1_v1,
          "fit_m1_v1_dominant_ctr_ability.rds")  # TODO adjust name
  
})



# model 1 version 2 - simulated data - clean data ####
# fixed cut points
gamma_lower <- 0
gamma_upper <- J - 2

# load fit_m1_v2_sim
fit_m1_v2_sim <- readRDS("data/fit_m1_v2_sim_clean_data_test.rds")
# fit_m1_v2_sim <- readRDS("fit_m1_v2_sim_clean_data.rds")

# extract simulations
params_m1_v2_sim <- rstan::extract(fit_m1_v2_sim)

# extract simulated qualifier/race ranks
R_sim_temp <- params_m1_v2_sim$R_sim

R_sim <- R_sim_temp[40,,]



# model 1 version 2 - simulated data - missing data ####
# fixed cut points
gamma_lower <- 0
gamma_upper <- J - 2

# load fit_m1_v2_sim
fit_m1_v2_sim <- readRDS("data/fit_m1_v2_sim_missing_data.rds")
# fit_m1_v2_sim <- readRDS("fit_m1_v2_sim_missing_data.rds")

# extract simulations
params_m1_v2_sim <- rstan::extract(fit_m1_v2_sim)

# extract simulated qualifier/race ranks
R_sim_temp <- params_m1_v2_sim$R_sim

# extract simulated qualifier/race ranks
R_sim_temp <- params_m1_v2_sim$R_sim

R_sim <- R_sim_temp[40,,]



# model 1 version 2 - increased fluctuations ####
# fixed cut points
gamma_lower <- 0
gamma_upper <- J - 2

# load fit_m1_v2_sim
fit_m1_v2_sim <- readRDS("data/fit_m1_v2_sim_increased_fluctuations.rds")
# fit_m1_v2_sim <- readRDS("fit_m1_v2_sim_greater_fluctuations.rds")

# extract simulations
params_m1_v2_sim <- rstan::extract(fit_m1_v2_sim)

# extract simulated qualifier/race ranks
R_sim_temp <- params_m1_v2_sim$R_sim

# extract simulated qualifier/race ranks
R_sim_temp <- params_m1_v2_sim$R_sim

R_sim <- R_sim_temp[40,,]



# model 1 version 2 - dominant ctr ability ####
# fixed cut points
gamma_lower <- 0
gamma_upper <- J - 2

# load fit_m1_v2_sim
fit_m1_v2_sim <- readRDS("data/fit_m1_v2_sim_dominant_ctr_ability.rds")
# fit_m1_v2_sim <- readRDS("fit_m1_v2_sim_missing_data.rds")

# extract simulations
params_m1_v2_sim <- rstan::extract(fit_m1_v2_sim)

# extract simulated qualifier/race ranks
R_sim_temp <- params_m1_v2_sim$R_sim

# extract simulated qualifier/race ranks
R_sim_temp <- params_m1_v2_sim$R_sim

R_sim <- R_sim_temp[40,,]



# model 1 version 2 - F1 hybrid era qualifier data ####
# fixed cut points
gamma_lower <- 0
gamma_upper <- J - 2



# model 1 version 2 - F1 hybrid era race data ####
# initial conditions for latent constructor ability state equation ( 75% )
# fixed cut points
gamma_lower <- 0
gamma_upper <- J - 2





# model 1 version 2 - estimation - local ####
# computation with NUTS in STAN
m1_v2 <- stan_model("STAN/m1_v2_test.stan")

# number of post-warmup iterations per chain
iter_per_chain <- 2000

fit_m1_v2 <- sampling(m1_v2,
                      data = list(K = K,
                                  N = N,
                                  T = Q,
                                  I_3 = I_3,
                                  I_2 = I_2,
                                  I_1 = I_1,
                                  I_4 = I_4,
                                  J = J,
                                  mu_C_0 = mu_C_0,
                                  mu_D_0 = mu_D_0,
                                  kappa_D = kappa_D,
                                  gamma_lower = gamma_lower,
                                  gamma_upper = gamma_upper,
                                  R = R_sim),  # TODO actual data
                      iter = iter_per_chain)

# save fit_m1_v2
saveRDS(fit_m1_v2,
        "results/fit_m1_v2_clean_data_test.rds")  # TODO adjust name



# model 1 version 2 - estimation - DSRI ####
# computation with NUTS in STAN
m1_v1 <- stan_model("m1_v2.stan")

# number of post-warmup iterations per chain
iter_per_chain <- 2000

job::job({
  
  fit_m1_v2 <- sampling(m1_v2,
                        data = list(K = K,
                                    N = N,
                                    T = Q,
                                    I_3 = I_3,
                                    I_2 = I_2,
                                    I_1 = I_1,
                                    I_4 = I_4,
                                    I_5 = I_5,
                                    J = J,
                                    mu_C_0 = mu_C_0,
                                    mu_D_0 = mu_D_0,
                                    gamma_lower = gamma_lower,
                                    gamma_upper = gamma_upper,
                                    R = R_sim),  # TODO actual data
                        iter = iter_per_chain)
  
  # save fit_m1_v2
  saveRDS(fit_m1_v2,
          "fit_m1_v2_clean_data.rds")  # TODO adjust name
  
})


