# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


# closing the sections provides an overview of the script


# this file builds on the simulated_data.R file


# required data files ( occurrences in script marked with TODO data file ):
# fit_m___sim___.rds
# R_act_qualifier.xlsx
# R_act_race.xlsx
# where ___ is a placeholder


# required model files ( occurrences in script marked with TODO model file ):
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
library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

library(readxl)



# model 1 version 1 - simulated data - clean data ####
# fixed cut points
gamma_lower <- 0
gamma_upper <- J - 2

# load fit_m1_v1_sim
fit_m1_v1_sim <- readRDS("data/fit_m1_v1_sim_clean_data.rds")  # TODO data file
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
fit_m1_v1_sim <- readRDS("data/fit_m1_v1_sim_missing_data.rds")  # TODO data file
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
fit_m1_v1_sim <- readRDS("data/fit_m1_v1_sim_greater_fluctuations.rds")  # TODO data file
# fit_m1_v1_sim <- readRDS("fit_m1_v1_sim_missing_data.rds")

# extract simulations
params_m1_v1_sim <- rstan::extract(fit_m1_v1_sim)

# extract simulated qualifier/race ranks
R_sim_temp <- params_m1_v1_sim$R_sim

# extract simulated qualifier/race ranks
R_sim_temp <- params_m1_v1_sim$R_sim

R_sim <- R_sim_temp[40,,]



# model 1 version 1 - 20% drv 80% ctr ####
# placeholder



# model 1 version 1 - F1 hybrid era qualifier data ####
# actual qualifier ranks
# load R_act_temp
R_act_temp <- read_excel("data/R_act_qualifier.xlsx",  # TODO data file
                         sheet = "Sheet1")
# R_act_temp <- read_excel("R_act_qualifier.xlsx",  # TODO data file
#                          sheet = "Sheet1")

# data processing - delete columns 1, 2, 3, and 4
R_act_temp <- R_act_temp[,-c(1:4)]

# data processing - chr --> numeric
R_act <- sapply(R_act_temp, FUN = as.numeric)


# number of constructors in time series
K <- 12

# number of drivers in time series
N <- 51

# number of qualifiers/races
Q <- 159 # TODO first race

# constructor qualifier NA indicators
I_3 <- matrix(data = 1, nrow = K, ncol = Q)

# NAs for constructor with ID 10 ( manor )
I_3[10,c(16:18,59:159)] <- 0 # TODO first race

# NAs for constructor with ID 11 ( caterham )
I_3[11,16:159] <- 0 # TODO first race

# NAs for constructor with ID 12 ( haas )
I_3[12,1:37] <- 0 # TODO first race

# constructor indicators
# placeholder

# driver qualifier/race NA indicators
I_1 <- matrix(data = 1, nrow = N, ncol = Q)

for (n in 1:N) {
  for (t in 1:Q) {
    
    if (is.na(R_act[n,t])) {
      I_1[n,t] <- 0
    }
    
  }
}

# actual qualifier ranks continued
# data processing - NAs --> 22
for (n in 1:N) {
  for (t in 1:Q) {
    
    if (is.na(R_act[n,t])) {
      R_act[n,t] <- 22
    }
    
  }
}

# number of ranks per qualifier/race
J <- 22

# initial conditions for latent constructor ability state equation
# placeholder

# initial conditions for latent driver ability state equation
# placeholder

# fixed cut points
gamma_lower <- 0
gamma_upper <- J - 2



# model 1 version 1 - F1 hybrid era race data ####
# placeholder



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
saveRDS(fit_m1_v1, "results/fit_m1_v1_greater_fluctuations.rds")



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
  saveRDS(fit_m1_v1, "fit_m1_v1_greater_fluctuations.rds")
  
})


