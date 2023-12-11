# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


# closing the sections provides an overview of the script


# this file builds on the simulated_data.R file


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

library(readxl)



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



# model 1 version 1 - greater fluctuations ####
# fixed cut points
gamma_lower <- 0
gamma_upper <- J - 2

# load fit_m1_v1_sim
fit_m1_v1_sim <- readRDS("data/fit_m1_v1_sim_greater_fluctuations.rds")
# fit_m1_v1_sim <- readRDS("fit_m1_v1_sim_greater_fluctuations.rds")

# extract simulations
params_m1_v1_sim <- rstan::extract(fit_m1_v1_sim)

# extract simulated qualifier/race ranks
R_sim_temp <- params_m1_v1_sim$R_sim

# extract simulated qualifier/race ranks
R_sim_temp <- params_m1_v1_sim$R_sim

R_sim <- R_sim_temp[40,,]



# model 1 version 1 - 25% drv 75% ctr ####
# fixed cut points
gamma_lower <- 0
gamma_upper <- J - 2

# load fit_m1_v1_sim
fit_m1_v1_sim <- readRDS("data/fit_m1_v1_sim_25drv_75ctr.rds")
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
ctr_ability_rank <- c(15,14,13,12,11,10,9,8,7,6,5,5)

mu_C_0 <- rep(NA, times = K)

mu_C_0[1] <- ctr_ability_rank[2]  # 2013 ctr standings
mu_C_0[2] <- ctr_ability_rank[1]  # 2013 ctr standings
mu_C_0[3] <- ctr_ability_rank[5]  # 2013 ctr standings
mu_C_0[4] <- ctr_ability_rank[3]  # 2013 ctr standings
mu_C_0[5] <- ctr_ability_rank[8]  # 2013 ctr standings
mu_C_0[6] <- ctr_ability_rank[6]  # 2013 ctr standings
mu_C_0[7] <- ctr_ability_rank[9]  # 2013 ctr standings
mu_C_0[8] <- ctr_ability_rank[7]  # 2013 ctr standings
mu_C_0[9] <- ctr_ability_rank[4]  # 2013 ctr standings
mu_C_0[10] <- ctr_ability_rank[10]  # 2013 ctr standings
mu_C_0[11] <- ctr_ability_rank[11]  # 2013 ctr standings
mu_C_0[12] <- ctr_ability_rank[11]  # min ctr_ability_rank ( haas )

# initial conditions for latent driver ability state equation ( 25% )
drv_ability_rank <- c(5.25,4.75,4.25,3.75,3.25,
                      2.75,2.25,1.75,1.25,0.75,
                      0.25,-0.25,-0.75,-1.25,-1.75,
                      -2.25,-2.75,-3.25,-3.75,-4.25,
                      -4.75 -5.25)

mu_D_0 <- rep(NA, times = N)

mu_D_0[1] <- drv_ability_rank[4]  # 2013 drv standings
mu_D_0[2] <- drv_ability_rank[14]  # 2013 drv standings
mu_D_0[3] <- drv_ability_rank[6]  # 2013 drv standings
mu_D_0[4] <- drv_ability_rank[]
mu_D_0[5] <- drv_ability_rank[2]  # 2013 drv standings
mu_D_0[6] <- drv_ability_rank[15]  # 2013 drv standings
mu_D_0[7] <- drv_ability_rank[10]  # 2013 drv standings
mu_D_0[8] <- drv_ability_rank[]
mu_D_0[9] <- drv_ability_rank[8]  # 2013 drv standings
mu_D_0[10] <- drv_ability_rank[17]  # 2013 drv standings
mu_D_0[11] <- drv_ability_rank[9]  # 2013 drv standings
mu_D_0[12] <- drv_ability_rank[5]  # 2013 drv standings
mu_D_0[13] <- drv_ability_rank[1]  # 2013 drv standings
mu_D_0[14] <- drv_ability_rank[13]  # 2013 drv standings
mu_D_0[15] <- drv_ability_rank[]
mu_D_0[16] <- drv_ability_rank[11]  # 2013 drv standings
mu_D_0[17] <- drv_ability_rank[22]  # 2013 drv standings
mu_D_0[18] <- drv_ability_rank[19]  # 2013 drv standings
mu_D_0[19] <- drv_ability_rank[16]  # 2013 drv standings
mu_D_0[20] <- drv_ability_rank[]
mu_D_0[21] <- drv_ability_rank[7]  # 2013 drv standings
mu_D_0[22] <- drv_ability_rank[18]  # 2013 drv standings



# model 1 version 1 - estimation - local ####
# computation with NUTS in STAN
m1_v1 <- stan_model("STAN/m1_v1.stan")  # TODO model file

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
                                  R = R_sim),
                      iter = iter_per_chain)

# save fit_m1_v1
saveRDS(fit_m1_v1, "results/fit_m1_v1_25drv_75ctr.rds")



# model 1 version 1 - estimation - DSRI ####
# computation with NUTS in STAN
m1_v1 <- stan_model("m1_v1.stan")  # TODO model file

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
                                    R = R_sim),
                        iter = iter_per_chain)
  
  # save fit_m1_v1
  saveRDS(fit_m1_v1, "fit_m1_v1_25drv_75ctr.rds")
  
})


