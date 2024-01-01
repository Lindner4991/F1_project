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

# extract simulations
params_m1_v1_sim <- rstan::extract(fit_m1_v1_sim)

# extract simulated qualifier/race ranks
R_sim_temp <- params_m1_v1_sim$R_sim

R_sim <- R_sim_temp[40,,]



# model 1 version 1 - increased fluctuations ####
# fixed cut points
gamma_lower <- 0
gamma_upper <- J - 2

# load fit_m1_v1_sim
fit_m1_v1_sim <- readRDS("data/fit_m1_v1_sim_increased_fluctuations.rds")

# extract simulations
params_m1_v1_sim <- rstan::extract(fit_m1_v1_sim)

# extract simulated qualifier/race ranks
R_sim_temp <- params_m1_v1_sim$R_sim

R_sim <- R_sim_temp[40,,]



# model 1 version 1 - dominant ctr ability ####
# fixed cut points
gamma_lower <- 0
gamma_upper <- J - 2

# load fit_m1_v1_sim
fit_m1_v1_sim <- readRDS("data/fit_m1_v1_sim_dominant_ctr_ability.rds")

# extract simulations
params_m1_v1_sim <- rstan::extract(fit_m1_v1_sim)

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





# model 1 version 1 - estimation - without job ####
# computation with NUTS in STAN
m1_v1 <- stan_model("STAN/m1_v1.stan")

# number sampling iterations per chain
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



# model 1 version 1 - estimation - with job ####
# computation with NUTS in STAN
m1_v1 <- stan_model("m1_v1.stan")

# number of sampling iterations per chain
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
fit_m1_v2_sim <- readRDS("data/fit_m1_v2_sim_clean_data.rds")

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

# extract simulations
params_m1_v2_sim <- rstan::extract(fit_m1_v2_sim)

# extract simulated qualifier/race ranks
R_sim_temp <- params_m1_v2_sim$R_sim

R_sim <- R_sim_temp[40,,]



# model 1 version 2 - increased fluctuations ####
# fixed cut points
gamma_lower <- 0
gamma_upper <- J - 2

# load fit_m1_v2_sim
fit_m1_v2_sim <- readRDS("data/fit_m1_v2_sim_increased_fluctuations.rds")

# extract simulations
params_m1_v2_sim <- rstan::extract(fit_m1_v2_sim)

# extract simulated qualifier/race ranks
R_sim_temp <- params_m1_v2_sim$R_sim

R_sim <- R_sim_temp[40,,]



# model 1 version 2 - dominant ctr ability ####
# fixed cut points
gamma_lower <- 0
gamma_upper <- J - 2

# load fit_m1_v2_sim
fit_m1_v2_sim <- readRDS("data/fit_m1_v2_sim_dominant_ctr_ability.rds")

# extract simulations
params_m1_v2_sim <- rstan::extract(fit_m1_v2_sim)

# extract simulated qualifier/race ranks
R_sim_temp <- params_m1_v2_sim$R_sim

R_sim <- R_sim_temp[40,,]



# model 1 version 2 - F1 hybrid era qualifier data ####
# SD increase for error for latent driver ability state equation
# in case of driver change
# high value
kappa_D <- 0.5

# low value
# kappa_D <- 0.1

# fixed cut points
gamma_lower <- 0
gamma_upper <- J - 2



# model 1 version 2 - F1 hybrid era race data ####
# initial conditions for latent constructor ability state equation ( 75% )
# SD increase for error for latent driver ability state equation
# in case of driver change
# high value
kappa_D <- 0.5

# low value
# kappa_D <- 0.1

# fixed cut points
gamma_lower <- 0
gamma_upper <- J - 2





# model 1 version 2 - estimation - without job ####
# computation with NUTS in STAN
m1_v2 <- stan_model("STAN/m1_v2.stan")

# number of sampling iterations per chain
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
        "results/fit_m1_v2_clean_data.rds")  # TODO adjust name



# model 1 version 2 - estimation - with job ####
# computation with NUTS in STAN
m1_v2 <- stan_model("m1_v2.stan")

# number of sampling iterations per chain
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
          "fit_m1_v2_clean_data.rds")  # TODO adjust name
  
})



# model 2 - simulated data - clean data ####
# fixed cut points
gamma_lower <- 0
gamma_upper <- J - 2

# load fit_m2_sim
fit_m2_sim <- readRDS("data/fit_m2_sim_clean_data.rds")

# extract simulations
params_m2_sim <- rstan::extract(fit_m2_sim)

# extract simulated qualifier/race ranks
R_sim_temp <- params_m2_sim$R_sim

R_sim <- R_sim_temp[40,,]



# model 2 - simulated data - missing data ####
# fixed cut points
gamma_lower <- 0
gamma_upper <- J - 2

# load fit_m2_sim
fit_m2_sim <- readRDS("data/fit_m2_sim_missing_data.rds")

# extract simulations
params_m2_sim <- rstan::extract(fit_m2_sim)

# extract simulated qualifier/race ranks
R_sim_temp <- params_m2_sim$R_sim

R_sim <- R_sim_temp[40,,]



# model 2 - increased fluctuations ####
# fixed cut points
gamma_lower <- 0
gamma_upper <- J - 2

# load fit_m2_sim
fit_m2_sim <- readRDS("data/fit_m2_sim_increased_fluctuations.rds")

# extract simulations
params_m2_sim <- rstan::extract(fit_m2_sim)

# extract simulated qualifier/race ranks
R_sim_temp <- params_m2_sim$R_sim

R_sim <- R_sim_temp[40,,]



# model 2 - dominant ctr ability ####
# fixed cut points
gamma_lower <- 0
gamma_upper <- J - 2

# load fit_m2_sim
fit_m1_v2_sim <- readRDS("data/fit_m2_sim_dominant_ctr_ability.rds")

# extract simulations
params_m2_sim <- rstan::extract(fit_m2_sim)

# extract simulated qualifier/race ranks
R_sim_temp <- params_m2_sim$R_sim

R_sim <- R_sim_temp[40,,]



# model 2 - F1 hybrid era qualifier data ####
# SD increase for error for latent constructor ability state equation
# in case of first race of a season
# high value
# kappa_C <- 0.5

# low value
kappa_C <- 0.1

# SD increase for error for latent driver ability state equation
# in case of driver change
# high value
kappa_D <- 0.5

# fixed cut points
gamma_lower <- 0
gamma_upper <- J - 2



# model 2 - F1 hybrid era race data ####
# SD increase for error for latent constructor ability state equation
# in case of first race of a season
# high value
# kappa_C <- 0.5

# low value
kappa_C <- 0.1

# SD increase for error for latent driver ability state equation
# in case of driver change
# high value
kappa_D <- 0.5

# fixed cut points
gamma_lower <- 0
gamma_upper <- J - 2





# model 2 - estimation - without job ####
# computation with NUTS in STAN
m2 <- stan_model("STAN/m2.stan")

# number of sampling iterations per chain
iter_per_chain <- 2000

fit_m2 <- sampling(m2,
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
                               kappa_C = kappa_C,
                               kappa_D = kappa_D,
                               gamma_lower = gamma_lower,
                               gamma_upper = gamma_upper,
                               R = R_sim),  # TODO actual data
                   iter = iter_per_chain)

# save fit_m2
saveRDS(fit_m2,
        "results/fit_m2_clean_data.rds")  # TODO adjust name



# model 2 - estimation - with job ####
# computation with NUTS in STAN
m2 <- stan_model("m2.stan")

# number of sampling iterations per chain
iter_per_chain <- 2000

job::job({
  
  fit_m2 <- sampling(m2,
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
                                 kappa_C = kappa_C,
                                 kappa_D = kappa_D,
                                 gamma_lower = gamma_lower,
                                 gamma_upper = gamma_upper,
                                 R = R_sim),  # TODO actual data
                     iter = iter_per_chain)
  
  # save fit_m2
  saveRDS(fit_m2,
          "fit_m2_clean_data.rds")  # TODO adjust name
  
})


