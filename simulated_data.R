# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


# closing the sections provides an overview of the script


# required model files ( occurrences in script marked with TODO model file ):
# m___sim.stan
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



# model 1 version 1 - clean data ####
# number of constructors in time series
K <- 11

# number of drivers in time series
N <- 22

# number of qualifiers/races
Q <- 159 # TODO first race

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
I_2_2[2,] <- c(0,0,0,0,0,0,0,0,1,0,0)  # switch
I_2_2[3,] <- c(0,1,0,0,0,0,0,0,0,0,0)
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
I_2_2[17,] <- c(1,0,0,0,0,0,0,0,0,0,0)  # switch
I_2_2[18,] <- c(0,0,0,0,0,0,0,0,1,0,0)
I_2_2[19,] <- c(0,0,0,0,0,0,0,0,0,1,0)
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
            I_2_2,I_2_2,I_2_2,I_2_2,I_2_2,I_2_2,I_2_2,I_2_2,I_2_2)  # TODO first race

# driver qualifier/race NA indicators
I_1 <- matrix(data = 1, nrow = N, ncol = Q)


# number of ranks per qualifier/race
J <- 22

# initial conditions for latent constructor ability state equation
mu_C_0 <- c(10,9,8,7,6,5,4,3,2,1,0)

# initial conditions for latent driver ability state equation
mu_D_0 <- c(10.25,9.75,9.25,8.75,8.25,
            7.75,7.25,6.75,6.25,5.75,
            5.25,4.75,4.25,3.75,3.25,
            2.75,2.25,1.75,1.25,0.75,
            0.25,-0.25)

# SD for error for latent constructor ability state equation
varsigma_C <- 0.16

# SD for error for latent driver ability state equation
varsigma_D <- 0.04

# cut points
gamma <- seq(from = 0, to = 20, by = 1)



# model 1 version 1 - missing data ####
# number of constructors in time series
K <- 12

# number of drivers in time series
N <- 24

# number of qualifiers/races
Q <- 159 # TODO first race

# constructor qualifier/race NA indicators
I_3 <- matrix(data = 1, nrow = K, ncol = Q)

# NAs for constructor with ID 10 ( manor )
I_3[10,c(16:18,59:159)] <- 0 # TODO first race

# NAs for constructor with ID 11 ( caterham )
I_3[11,16:159] <- 0 # TODO first race

# NAs for constructor with ID 12 ( haas )
I_3[12,1:37] <- 0 # TODO first race

# constructor indicators
I_2_1 <- matrix(data = NA, nrow = N, ncol = K)
I_2_1[1,] <- c(1,0,0,0,0,0,0,0,0,0,0,0)
I_2_1[2,] <- c(1,0,0,0,0,0,0,0,0,0,0,0)
I_2_1[3,] <- c(0,1,0,0,0,0,0,0,0,0,0,0)
I_2_1[4,] <- c(0,1,0,0,0,0,0,0,0,0,0,0)
I_2_1[5,] <- c(0,0,1,0,0,0,0,0,0,0,0,0)
I_2_1[6,] <- c(0,0,1,0,0,0,0,0,0,0,0,0)
I_2_1[7,] <- c(0,0,0,1,0,0,0,0,0,0,0,0)
I_2_1[8,] <- c(0,0,0,1,0,0,0,0,0,0,0,0)
I_2_1[9,] <- c(0,0,0,0,1,0,0,0,0,0,0,0)
I_2_1[10,] <- c(0,0,0,0,1,0,0,0,0,0,0,0)
I_2_1[11,] <- c(0,0,0,0,0,1,0,0,0,0,0,0)
I_2_1[12,] <- c(0,0,0,0,0,1,0,0,0,0,0,0)
I_2_1[13,] <- c(0,0,0,0,0,0,1,0,0,0,0,0)
I_2_1[14,] <- c(0,0,0,0,0,0,1,0,0,0,0,0)
I_2_1[15,] <- c(0,0,0,0,0,0,0,1,0,0,0,0)
I_2_1[16,] <- c(0,0,0,0,0,0,0,1,0,0,0,0)
I_2_1[17,] <- c(0,0,0,0,0,0,0,0,1,0,0,0)
I_2_1[18,] <- c(0,0,0,0,0,0,0,0,1,0,0,0)
I_2_1[19,] <- c(0,0,0,0,0,0,0,0,0,1,0,0)
I_2_1[20,] <- c(0,0,0,0,0,0,0,0,0,1,0,0)
I_2_1[21,] <- c(0,0,0,0,0,0,0,0,0,0,1,0)
I_2_1[22,] <- c(0,0,0,0,0,0,0,0,0,0,1,0)
I_2_1[23,] <- c(0,0,0,0,0,0,0,0,0,0,0,1)
I_2_1[24,] <- c(0,0,0,0,0,0,0,0,0,0,0,1)

I_2_2 <- matrix(data = NA, nrow = N, ncol = K)
I_2_2[1,] <- c(1,0,0,0,0,0,0,0,0,0,0,0)
I_2_2[2,] <- c(0,0,0,0,0,0,0,0,1,0,0,0)  # switch
I_2_2[3,] <- c(0,1,0,0,0,0,0,0,0,0,0,0)
I_2_2[4,] <- c(0,1,0,0,0,0,0,0,0,0,0,0)
I_2_2[5,] <- c(0,0,1,0,0,0,0,0,0,0,0,0)
I_2_2[6,] <- c(0,0,1,0,0,0,0,0,0,0,0,0)
I_2_2[7,] <- c(0,0,0,1,0,0,0,0,0,0,0,0)
I_2_2[8,] <- c(0,0,0,1,0,0,0,0,0,0,0,0)
I_2_2[9,] <- c(0,0,0,0,1,0,0,0,0,0,0,0)
I_2_2[10,] <- c(0,0,0,0,1,0,0,0,0,0,0,0)
I_2_2[11,] <- c(0,0,0,0,0,1,0,0,0,0,0,0)
I_2_2[12,] <- c(0,0,0,0,0,1,0,0,0,0,0,0)
I_2_2[13,] <- c(0,0,0,0,0,0,1,0,0,0,0,0)
I_2_2[14,] <- c(0,0,0,0,0,0,1,0,0,0,0,0)
I_2_2[15,] <- c(0,0,0,0,0,0,0,1,0,0,0,0)
I_2_2[16,] <- c(0,0,0,0,0,0,0,1,0,0,0,0)
I_2_2[17,] <- c(1,0,0,0,0,0,0,0,0,0,0,0)  # switch
I_2_2[18,] <- c(0,0,0,0,0,0,0,0,1,0,0,0)
I_2_2[19,] <- c(0,0,0,0,0,0,0,0,0,1,0,0)
I_2_2[20,] <- c(0,0,0,0,0,0,0,0,0,1,0,0)
I_2_2[21,] <- c(0,0,0,0,0,0,0,0,0,0,1,0)
I_2_2[22,] <- c(0,0,0,0,0,0,0,0,0,0,1,0)
I_2_2[23,] <- c(0,0,0,0,0,0,0,0,0,0,0,1)
I_2_2[24,] <- c(0,0,0,0,0,0,0,0,0,0,0,1)

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
            I_2_2,I_2_2,I_2_2,I_2_2,I_2_2,I_2_2,I_2_2,I_2_2,I_2_2)  # TODO first race

# driver qualifier/race NA indicators
I_1 <- matrix(data = 1, nrow = N, ncol = Q)

# NAs for driver with ID 19 ( manor driver )
I_1[19,c(16:18,59:159)] <- 0 # TODO first race

# NAs for driver with ID 20 ( manor driver )
I_1[20,c(16:18,59:159)] <- 0 # TODO first race

# NAs for driver with ID 21 ( caterham driver )
I_1[21,16:159] <- 0 # TODO first race

# NAs for driver with ID 22 ( caterham driver )
I_1[22,16:159] <- 0 # TODO first race

# NAs for constructor with ID 23 ( haas driver )
I_1[23,1:37] <- 0 # TODO first race

# NAs for constructor with ID 24 ( haas driver )
I_1[24,1:37] <- 0 # TODO first race

# number of ranks per qualifier/race
J <- 22

# initial conditions for latent constructor ability state equation
mu_C_0 <- c(10,9,8,7,6,5,4,3,2,1,0,0)  # haas: 0

# initial conditions for latent driver ability state equation
mu_D_0 <- c(10.25,9.75,9.25,8.75,8.25,
            7.75,7.25,6.75,6.25,5.75,
            5.25,4.75,4.25,3.75,3.25,
            2.75,2.25,1.75,1.25,0.75,
            0.25,-0.25,-0.25,-0.25)  # haas drivers: -0.25

# SD for error for latent constructor ability state equations
varsigma_C <- 0.16

# SD for error for latent driver ability state equations
varsigma_D <- 0.04

# cut points
gamma <- seq(from = 0, to = 20, by = 1)



# model 1 version 1 - greater fluctuations ####
# number of constructors in time series
K <- 12

# number of drivers in time series
N <- 24

# number of qualifiers/races
Q <- 159 # TODO first race

# constructor qualifier/race NA indicators
I_3 <- matrix(data = 1, nrow = K, ncol = Q)

# NAs for constructor with ID 10 ( manor )
I_3[10,c(16:18,59:159)] <- 0 # TODO first race

# NAs for constructor with ID 11 ( caterham )
I_3[11,16:159] <- 0 # TODO first race

# NAs for constructor with ID 12 ( haas )
I_3[12,1:37] <- 0 # TODO first race

# constructor indicators
I_2_1 <- matrix(data = NA, nrow = N, ncol = K)
I_2_1[1,] <- c(1,0,0,0,0,0,0,0,0,0,0,0)
I_2_1[2,] <- c(1,0,0,0,0,0,0,0,0,0,0,0)
I_2_1[3,] <- c(0,1,0,0,0,0,0,0,0,0,0,0)
I_2_1[4,] <- c(0,1,0,0,0,0,0,0,0,0,0,0)
I_2_1[5,] <- c(0,0,1,0,0,0,0,0,0,0,0,0)
I_2_1[6,] <- c(0,0,1,0,0,0,0,0,0,0,0,0)
I_2_1[7,] <- c(0,0,0,1,0,0,0,0,0,0,0,0)
I_2_1[8,] <- c(0,0,0,1,0,0,0,0,0,0,0,0)
I_2_1[9,] <- c(0,0,0,0,1,0,0,0,0,0,0,0)
I_2_1[10,] <- c(0,0,0,0,1,0,0,0,0,0,0,0)
I_2_1[11,] <- c(0,0,0,0,0,1,0,0,0,0,0,0)
I_2_1[12,] <- c(0,0,0,0,0,1,0,0,0,0,0,0)
I_2_1[13,] <- c(0,0,0,0,0,0,1,0,0,0,0,0)
I_2_1[14,] <- c(0,0,0,0,0,0,1,0,0,0,0,0)
I_2_1[15,] <- c(0,0,0,0,0,0,0,1,0,0,0,0)
I_2_1[16,] <- c(0,0,0,0,0,0,0,1,0,0,0,0)
I_2_1[17,] <- c(0,0,0,0,0,0,0,0,1,0,0,0)
I_2_1[18,] <- c(0,0,0,0,0,0,0,0,1,0,0,0)
I_2_1[19,] <- c(0,0,0,0,0,0,0,0,0,1,0,0)
I_2_1[20,] <- c(0,0,0,0,0,0,0,0,0,1,0,0)
I_2_1[21,] <- c(0,0,0,0,0,0,0,0,0,0,1,0)
I_2_1[22,] <- c(0,0,0,0,0,0,0,0,0,0,1,0)
I_2_1[23,] <- c(0,0,0,0,0,0,0,0,0,0,0,1)
I_2_1[24,] <- c(0,0,0,0,0,0,0,0,0,0,0,1)

I_2_2 <- matrix(data = NA, nrow = N, ncol = K)
I_2_2[1,] <- c(1,0,0,0,0,0,0,0,0,0,0,0)
I_2_2[2,] <- c(0,0,0,0,0,0,0,0,1,0,0,0)  # switch
I_2_2[3,] <- c(0,1,0,0,0,0,0,0,0,0,0,0)
I_2_2[4,] <- c(0,1,0,0,0,0,0,0,0,0,0,0)
I_2_2[5,] <- c(0,0,1,0,0,0,0,0,0,0,0,0)
I_2_2[6,] <- c(0,0,1,0,0,0,0,0,0,0,0,0)
I_2_2[7,] <- c(0,0,0,1,0,0,0,0,0,0,0,0)
I_2_2[8,] <- c(0,0,0,1,0,0,0,0,0,0,0,0)
I_2_2[9,] <- c(0,0,0,0,1,0,0,0,0,0,0,0)
I_2_2[10,] <- c(0,0,0,0,1,0,0,0,0,0,0,0)
I_2_2[11,] <- c(0,0,0,0,0,1,0,0,0,0,0,0)
I_2_2[12,] <- c(0,0,0,0,0,1,0,0,0,0,0,0)
I_2_2[13,] <- c(0,0,0,0,0,0,1,0,0,0,0,0)
I_2_2[14,] <- c(0,0,0,0,0,0,1,0,0,0,0,0)
I_2_2[15,] <- c(0,0,0,0,0,0,0,1,0,0,0,0)
I_2_2[16,] <- c(0,0,0,0,0,0,0,1,0,0,0,0)
I_2_2[17,] <- c(1,0,0,0,0,0,0,0,0,0,0,0)  # switch
I_2_2[18,] <- c(0,0,0,0,0,0,0,0,1,0,0,0)
I_2_2[19,] <- c(0,0,0,0,0,0,0,0,0,1,0,0)
I_2_2[20,] <- c(0,0,0,0,0,0,0,0,0,1,0,0)
I_2_2[21,] <- c(0,0,0,0,0,0,0,0,0,0,1,0)
I_2_2[22,] <- c(0,0,0,0,0,0,0,0,0,0,1,0)
I_2_2[23,] <- c(0,0,0,0,0,0,0,0,0,0,0,1)
I_2_2[24,] <- c(0,0,0,0,0,0,0,0,0,0,0,1)

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
            I_2_2,I_2_2,I_2_2,I_2_2,I_2_2,I_2_2,I_2_2,I_2_2,I_2_2)  # TODO first race

# driver qualifier/race NA indicators
I_1 <- matrix(data = 1, nrow = N, ncol = Q)

# NAs for driver with ID 19 ( manor driver )
I_1[19,c(16:18,59:159)] <- 0 # TODO first race

# NAs for driver with ID 20 ( manor driver )
I_1[20,c(16:18,59:159)] <- 0 # TODO first race

# NAs for driver with ID 21 ( caterham driver )
I_1[21,16:159] <- 0 # TODO first race

# NAs for driver with ID 22 ( caterham driver )
I_1[22,16:159] <- 0 # TODO first race

# NAs for constructor with ID 23 ( haas driver )
I_1[23,1:37] <- 0 # TODO first race

# NAs for constructor with ID 24 ( haas driver )
I_1[24,1:37] <- 0 # TODO first race

# number of ranks per qualifier/race
J <- 22

# initial conditions for latent constructor ability state equation
mu_C_0 <- c(10,9,8,7,6,5,4,3,2,1,0,1)  # haas: 1

# initial conditions for latent driver ability state equation
mu_D_0 <- c(10.25,9.75,9.25,8.75,8.25,
            7.75,7.25,6.75,6.25,5.75,
            5.25,4.75,4.25,3.75,3.25,
            2.75,2.25,1.75,1.25,0.75,
            0.25,-0.25,0.75,0.75)  # haas drivers: 0.75

# SD for error for latent constructor ability state equations
varsigma_C <- 0.4

# SD for error for latent driver ability state equations
varsigma_D <- 0.08

# cut points
gamma <- seq(from = 0, to = 20, by = 1)



# model 1 version 1 - 20% drv 80% ctr ####
# placeholder



# model 1 version 1 - simulation ####
# computation with NUTS in STAN
m1_v1_sim <- stan_model("STAN/m1_v1_sim.stan")  # TODO model file

fit_m1_v1_sim <- sampling(m1_v1_sim,
                          data = list(K = K,
                                      N = N,
                                      T = Q,
                                      I_3 = I_3,
                                      I_2 = I_2,
                                      I_1 = I_1,
                                      J = J,
                                      mu_C_0 = mu_C_0,
                                      mu_D_0 = mu_D_0,
                                      varsigma_C = varsigma_C,
                                      varsigma_D = varsigma_D,
                                      gamma = gamma),
                          algorithm = "Fixed_param",
                          iter = 10,
                          warmup = 0)

# save fit_m1_v1_sim
saveRDS(fit_m1_v1_sim, "data/fit_m1_v1_sim_greater_fluctuations.rds")

# load fit_m1_v1_sim
fit_m1_v1_sim <- readRDS("data/fit_m1_v1_sim_greater_fluctuations.rds")


# extract simulations
params_m1_v1_sim <- rstan::extract(fit_m1_v1_sim)

# extract simulated qualifier/race ranks
R_sim_temp <- params_m1_v1_sim$R_sim

R_sim <- R_sim_temp[40,,]


# time series plot
# simulated qualifier/race rank
par(mfrow = c(5,2))
for (n in 1:N) {
  
  plot(R_sim[n,],
       ylim = c(22, 1),
       type="l",
       col = "orange",
       main = paste("driver", n),
       xlab = "qualifier/race",
       ylab = "rank",
       xaxt = "n",
       yaxt = "n")
  axis(side = 1, at = c(1,19,38,59,79,100,121,138,159))  # TODO first race
  axis(side = 2, at = c(22, 11, 1), las = 1)
  
}
par(mfrow = c(1,1))


