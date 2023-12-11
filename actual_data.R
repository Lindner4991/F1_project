# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


# closing the sections provides an overview of the script

# required data files:
# R_act___.xlsx
# drv_ctr___.xlsx
# drv_ctr_cockpit___.xlsx
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

library(readxl)
library(openxlsx)



# user-defined functions ####
# median constructor rank
median_ctr_rank <- function(cockpit1, cockpit2, period, rank_matrix) {
  
  ranks <- c()
  
  for (n in c(cockpit1, cockpit2)) {
    for (t in period) {
      
      ranks <- c(ranks,rank_matrix[n,t])
      
    }
  }
  
  median_rank <- round(median(ranks), digits = 0)
  
  return(median_rank)
  
}
  

  
# model 1 version 1 - F1 hybrid era qualifier data ####
# actual qualifier ranks
# load R_act_temp
R_act_temp <- read_excel("data/R_act_qualifier.xlsx",
                         sheet = "Sheet 1")
# R_act_temp <- read_excel("R_act_qualifier.xlsx",
#                         sheet = "Sheet1")

# data processing - delete columns 1, 2, 3, and 4
R_act <- R_act_temp[,-c(1:4)]

# data processing - chr --> numeric
R_act <- sapply(R_act, FUN = as.numeric)


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
I_3[11,c(16:17,19:159)] <- 0

# NAs for constructor with ID 12 ( haas )
I_3[12,1:37] <- 0

K * Q - sum(I_3)

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

drv_ctr <- as.matrix(drv_ctr)

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

N * Q - sum(I_1)

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
mu_D_0[4] <- drv_ability_rank[4]  # t=0 qualifier 2014 season
mu_D_0[5] <- drv_ability_rank[2]  # 2013 drv standings
mu_D_0[6] <- drv_ability_rank[15]  # 2013 drv standings
mu_D_0[7] <- drv_ability_rank[10]  # 2013 drv standings
mu_D_0[8] <- drv_ability_rank[8]  # first qualifier 2014 season
mu_D_0[9] <- drv_ability_rank[8]  # 2013 drv standings
mu_D_0[10] <- drv_ability_rank[17]  # 2013 drv standings
mu_D_0[11] <- drv_ability_rank[9]  # 2013 drv standings
mu_D_0[12] <- drv_ability_rank[5]  # 2013 drv standings
mu_D_0[13] <- drv_ability_rank[1]  # 2013 drv standings
mu_D_0[14] <- drv_ability_rank[13]  # 2013 drv standings
mu_D_0[15] <- drv_ability_rank[15]  # t=0 qualifier 2014 season
mu_D_0[16] <- drv_ability_rank[11]  # 2013 drv standings
mu_D_0[17] <- drv_ability_rank[22]  # 2013 drv standings
mu_D_0[18] <- drv_ability_rank[19]  # 2013 drv standings
mu_D_0[19] <- drv_ability_rank[16]  # 2013 drv standings
mu_D_0[20] <- drv_ability_rank[20]  # t=0 qualifier 2014 season
mu_D_0[21] <- drv_ability_rank[7]  # 2013 drv standings
mu_D_0[22] <- drv_ability_rank[18]  # 2013 drv standings


# # # one time data processing - initial conditions NA drivers t=1 qualifier # #
# load R_act_cockpit
R_act_cockpit <- read_excel("data/R_act_qualifier_cockpit.xlsx",
                            sheet = "Sheet 1")

# data.frame --> matrix
R_act_cockpit <- as.matrix(R_act_cockpit)

median_ctr_rank(23, 24, c(39:138), R_act_cockpit)
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


# initial conditions NA drivers t=1 qualifier
mu_D_0[23] <- drv_ability_rank[20]
mu_D_0[24] <- drv_ability_rank[20]
mu_D_0[25] <- drv_ability_rank[11]
mu_D_0[26] <- drv_ability_rank[15]
mu_D_0[27] <- drv_ability_rank[11]
mu_D_0[28] <- drv_ability_rank[19]
mu_D_0[29] <- drv_ability_rank[19]
mu_D_0[30] <- drv_ability_rank[14]
mu_D_0[31] <- drv_ability_rank[19]
mu_D_0[32] <- drv_ability_rank[19]
mu_D_0[33] <- drv_ability_rank[12]
mu_D_0[34] <- drv_ability_rank[19]
mu_D_0[35] <- drv_ability_rank[16]
mu_D_0[36] <- drv_ability_rank[7]
mu_D_0[37] <- drv_ability_rank[8]
mu_D_0[38] <- drv_ability_rank[12]
mu_D_0[39] <- drv_ability_rank[12]
mu_D_0[40] <- drv_ability_rank[17]
mu_D_0[41] <- drv_ability_rank[8]
mu_D_0[42] <- drv_ability_rank[13]
mu_D_0[43] <- drv_ability_rank[13]
mu_D_0[44] <- drv_ability_rank[10]
mu_D_0[45] <- drv_ability_rank[10]
mu_D_0[46] <- drv_ability_rank[12]
mu_D_0[47] <- drv_ability_rank[14]
mu_D_0[48] <- drv_ability_rank[14]
mu_D_0[49] <- drv_ability_rank[12]
mu_D_0[50] <- drv_ability_rank[14]
mu_D_0[51] <- drv_ability_rank[14]


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



# model 1 version 2 - F1 hybrid era qualifier data ####
# number of constructors in time series
K <- 12

# number of drivers in time series
N <- 24

# number of qualifiers/races
Q <- 159

# # # # # # # # one time data processing - actual qualifier ranks # # # # # # #
# load R_act_temp
R_act_temp <- read_excel("data/R_act_qualifier.xlsx",
                         sheet = "Sheet 1")

# delete columns 1, 2, and 3
R_act_temp <- R_act_temp[,-c(1:3)]

# chr --> numeric
R_act_temp <- sapply(R_act_temp, FUN = as.numeric)

# load drv_ctr_cockpit ( manually derived from drv_ctr )
drv_ctr_cockpit_temp <- read_excel("data/drv_ctr_cockpit_qualifier.xlsx",
                         sheet = "Sheet 1")

# delete column 1
drv_ctr_cockpit <- drv_ctr_cockpit_temp[,-1]

# constructor cockpits
ctr_cockpits <- c("mercedes1", "mercedes2",
                  "red_bull1", "red_bull2",
                  "mclaren1", "mclaren2",
                  "ferrari1", "ferrari2",
                  "alphatauri1", "alphatauri2",
                  "aston_martin1", "aston_martin2",
                  "williams1", "williams2",
                  "alfa1", "alfa2",
                  "alpine1", "alpine2",
                  "manor1", "manor2",
                  "caterham1", "caterham2",
                  "haas1", "haas2")

# create R_act
R_act <- matrix(data = NA, nrow = N, ncol = Q+1)

for (n in 1:18) {
  for (t in 1:(Q+1)) {
    
    position <- which(drv_ctr_cockpit[,t] == ctr_cockpits[n])
    
    R_act[n,t] <- R_act_temp[position,t]
    
  }
}

for (n in 19:20) {  # manor
  for (t in c(1:16,20:59)) {
    
    position <- which(drv_ctr_cockpit[,t] == ctr_cockpits[n])
    
    R_act[n,t] <- R_act_temp[position,t]
    
  }
}

for (n in 21:22) {  # caterham
  for (t in c(1:16,19)) {
    
    position <- which(drv_ctr_cockpit[,t] == ctr_cockpits[n])
    
    R_act[n,t] <- R_act_temp[position,t]
    
  }
}

for (n in 23:24) {  # haas
  for (t in c(39:160)) {
    
    position <- which(drv_ctr_cockpit[,t] == ctr_cockpits[n])
    
    R_act[n,t] <- R_act_temp[position,t]
    
  }
}

write.xlsx(as.data.frame(R_act),
           "data/R_act_qualifier_cockpit.xlsx",
           overwrite = TRUE)
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


# placeholder
  
  
