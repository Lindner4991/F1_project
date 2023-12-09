# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


# closing the sections provides an overview of the script


# this file builds on the simulated_data.R file


# required data files ( occurrences in script marked with TODO data file ):
# fit_m___sim___.rds
# R_act___.xlsx
# drv_ctr___.xlsx
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
# fit_m1_v1_sim <- readRDS("fit_m1_v1_sim_greater_fluctuations.rds")

# extract simulations
params_m1_v1_sim <- rstan::extract(fit_m1_v1_sim)

# extract simulated qualifier/race ranks
R_sim_temp <- params_m1_v1_sim$R_sim

# extract simulated qualifier/race ranks
R_sim_temp <- params_m1_v1_sim$R_sim

R_sim <- R_sim_temp[40,,]



# model 1 version 1 - 33.3% drv 66.7% ctr ####
# fixed cut points
gamma_lower <- 0
gamma_upper <- J - 2

# load fit_m1_v1_sim
fit_m1_v1_sim <- readRDS("data/fit_m1_v1_sim_33drv_66ctr.rds")  # TODO data file
# fit_m1_v1_sim <- readRDS("fit_m1_v1_sim_missing_data.rds")

# extract simulations
params_m1_v1_sim <- rstan::extract(fit_m1_v1_sim)

# extract simulated qualifier/race ranks
R_sim_temp <- params_m1_v1_sim$R_sim

# extract simulated qualifier/race ranks
R_sim_temp <- params_m1_v1_sim$R_sim

R_sim <- R_sim_temp[40,,]



# model 1 version 1 - F1 hybrid era qualifier data ####
# actual qualifier ranks
# load R_act_temp
R_act_temp <- read_excel("data/R_act_qualifier.xlsx",  # TODO data file
                         sheet = "Sheet 1")
# R_act_temp <- read_excel("R_act_qualifier.xlsx",  # TODO data file
#                          sheet = "Sheet1")

# data processing - delete columns 1, 2, 3, and 4
R_act <- R_act_temp[,-c(1:4)]

# data processing - chr --> numeric
R_act <- sapply(R_act_temp, FUN = as.numeric)


# number of constructors in time series
K <- 12

# number of drivers in time series
N <- 51

# number of qualifiers/races
Q <- 159

# constructor qualifier NA indicators
I_3 <- matrix(data = 1, nrow = K, ncol = Q)

# NAs for constructor with ID 10 ( manor )
I_3[10,c(16:18,59:159)] <- 0

# NAs for constructor with ID 11 ( caterham )
I_3[11,16:159] <- 0

# NAs for constructor with ID 12 ( haas )
I_3[12,1:37] <- 0

# constructor indicators
# load drv_ctr
drv_ctr <- read_excel("data/drv_ctr_qualifier.xlsx",  # TODO data file
                      sheet = "Sheet 1")
# drv_ctr <- read_excel("drv_ctr_qualifier.xlsx",  # TODO data file
#                       sheet = "Sheet 1")

# test - same order of drv in drv_ctr and R_obs_qualifier
test <- 0

for (n in 1:N) {
  
  if (R_act_temp[n,1] == drv_ctr[n,1]) {
    test <- test + 1
  }
    
}

test == N

# data processing - delete column 1 and 2
drv_ctr <- drv_ctr[,-c(1:2)]

# data processing - constructor name changes
for (n in 1:N) {
  for (t in 1:Q) {
    
    if (!is.na(drv_ctr[n,t])) {
      
      if (drv_ctr[n,t] == "toro_rosso") {
        drv_ctr[n,t] <- "alphatauri"
      }
      
      if (drv_ctr[n,t] == "force_india" | drv_ctr[n,t] == "racing_point") {
        drv_ctr[n,t] <- "aston_martin"
      }
      
      if (drv_ctr[n,t] == "sauber") {
        drv_ctr[n,t] <- "alfa"
      }
      
      if (drv_ctr[n,t] == "marussia") {
        drv_ctr[n,t] <- "manor"
      }
      
      if (drv_ctr[n,t] == "lotus_f1" | drv_ctr[n,t] == "renault") {
        drv_ctr[n,t] <- "alpine"
      }
      
    }
    
  }
}

# data processing - replace NA for first qualifier with first non-NA value
for (n in 1:N) {
  
  if (is.na(drv_ctr[n,1])) {
    
    t <- 2
    success <- FALSE
    
    while (success == FALSE) {
      
      if (is.na(drv_ctr[n,t])) {
        
        t <- t + 1
        
      }
      
      else { success <- TRUE }
      
    }
    
    drv_ctr[n,1] <- drv_ctr[n,t]
    
  }
  
}

# data processing - replace NA for qualifier t with t-1 value
for (n in 1:N) {
  for (t in 2:Q) {
    
    if (is.na(drv_ctr[n,t])) {
      
      drv_ctr[n,t] <- drv_ctr[n,t-1]
      
    }
    
  }
}

# data processing - constructor indicators for each qualifier
I_2 <- list()

for (t in 1:Q) {
  
  drv_ctr_extract <- drv_ctr[,t]
  
  I_2_temp <- matrix(data = NA, nrow = N, ncol = K)
  
  for (n in 1:N) {
    
    if (drv_ctr_extract[n] == "mercedes") {
      I_2_temp[n,] <- c(1,0,0,0,0,0,0,0,0,0,0,0)
    }
    
    if (drv_ctr_extract[n] == "red_bull") {
      I_2_temp[n,] <- c(0,1,0,0,0,0,0,0,0,0,0,0)
    }
    
    if (drv_ctr_extract[n] == "mclaren") {
      I_2_temp[n,] <- c(0,0,1,0,0,0,0,0,0,0,0,0)
    }
    
    if (drv_ctr_extract[n] == "ferrari") {
      I_2_temp[n,] <- c(0,0,0,1,0,0,0,0,0,0,0,0)
    }
    
    if (drv_ctr_extract[n] == "alphatauri") {
      I_2_temp[n,] <- c(0,0,0,0,1,0,0,0,0,0,0,0)
    }
    
    if (drv_ctr_extract[n] == "aston_martin") {
      I_2_temp[n,] <- c(0,0,0,0,0,1,0,0,0,0,0,0)
    }
    
    if (drv_ctr_extract[n] == "williams") {
      I_2_temp[n,] <- c(0,0,0,0,0,0,1,0,0,0,0,0)
    }
    
    if (drv_ctr_extract[n] == "alfa") {
      I_2_temp[n,] <- c(0,0,0,0,0,0,0,1,0,0,0,0)
    }
    
    if (drv_ctr_extract[n] == "alpine") {
      I_2_temp[n,] <- c(0,0,0,0,0,0,0,0,1,0,0,0)
    }
    
    if (drv_ctr_extract[n] == "manor") {
      I_2_temp[n,] <- c(0,0,0,0,0,0,0,0,0,1,0,0)
    }
    
    if (drv_ctr_extract[n] == "caterham") {
      I_2_temp[n,] <- c(0,0,0,0,0,0,0,0,0,0,1,0)
    }
    
    if (drv_ctr_extract[n] == "haas") {
      I_2_temp[n,] <- c(0,0,0,0,0,0,0,0,0,0,0,1)
    }
    
  }
  
  I_2[[t]] <- I_2_temp

}

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
saveRDS(fit_m1_v1, "results/fit_m1_v1_33drv_66ctr.rds")



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
  saveRDS(fit_m1_v1, "fit_m1_v1_33drv_66ctr.rds")
  
})


