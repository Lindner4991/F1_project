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
# mean squared error
MSE <- function(data_est, data_sim, X, Y, I) {
  
  MSE_temp <- 0
  
  for (y in 1:Y) {
    for (x in 1:X) {
      
      if (I[x,y] == 1) {
        MSE_temp <- MSE_temp + (data_est[x,y] - data_sim[x,y])^2
      }
      
    }
  }
  
  N <- sum(I)
  
  MSE <- round(MSE_temp / N, digits = 4)
  
  return(MSE)
  
}


# 89% HPDI for mean squared error
HPDI_MSE <- function(data_est, data_sim, X, Y, I, iter) {
  
  MSE_temp <- rep(0, times = iter)
  
  for(i in 1:iter) {
    MSE_temp[iter] <- MSE(data_est[i,,], data_sim, X, Y, I)
  }
  
  HPDI_MSE <- HPDI(MSE_temp)
  
  return(HPDI_MSE)
  
}






# as numeric

















