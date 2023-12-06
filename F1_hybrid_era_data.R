# # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# CLOSING THE SECTIONS PROVIDES AN OVERVIEW OF THE SCRIPT #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # #



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
library(todor)
library(httr)
library(jsonlite)
library(tidyverse)
library(writexl)



# user-defined functions ####
# data processing - ranks
get_ranks <- function(data_temp, t) {
  
  data_temp <- fromJSON(rawToChar(data_temp$content))
  data_temp <- data_temp$MRData
  data_temp <- data_temp$RaceTable
  data_temp <- data_temp$Races
  data_temp <- data_temp$QualifyingResults
  data_temp <- data_temp[[1]]
  
  data <- data.frame(data_temp$Driver$driverId,
                     data_temp$position)
  
  colnames(data) <- c("driver",
                      paste("rank t", t, sep = ""))
  
  return(data)
  
}


merge_ranks <- function(R_obs_temp, data) {
  
  R_obs <- full_join(x = R_obs_temp,
                     y = data,
                     by = "driver")
  
  return(R_obs)
  
}


# data processing - unique constructors
get_unique_ctr <- function(data_temp) {
  
  data_temp <- fromJSON(rawToChar(data_temp$content))
  data_temp <- data_temp$MRData
  data_temp <- data_temp$RaceTable
  data_temp <- data_temp$Races
  data_temp <- data_temp$QualifyingResults
  data_temp <- data_temp[[1]]
  
  data <- data.frame(data_temp$Constructor$constructorId)
  
  colnames(data) <- c("ctr")
  
  data <- unique(data)
  
  return(data)
  
}


merge_unique_ctr <- function(unique_ctr_temp, data) {
  
  
  unique_ctr <- full_join(x = unique_ctr_temp,
                          y = data,
                          by = "ctr")
  
  return(unique_ctr)
  
}


get_drv_ctr <- function(data_temp, t) {
  
  data_temp <- fromJSON(rawToChar(data_temp$content))
  data_temp <- data_temp$MRData
  data_temp <- data_temp$RaceTable
  data_temp <- data_temp$Races
  data_temp <- data_temp$QualifyingResults
  data_temp <- data_temp[[1]]
  
  data <- data.frame(data_temp$Driver$driverId,
                     data_temp$Constructor$constructorId)
  
  colnames(data) <- c("driver",
                      paste("ctr t", t, sep = ""))
  
  return(data)
  
}


merge_drv_ctr <- function(drv_ctr_temp, data) {
  
  drv_ctr <- full_join(x = drv_ctr_temp,
                       y = data,
                       by = "driver")
  
  return(drv_ctr)
  
}



# qualifier data 2014-2021 - metadata ####
# season years
SY <- c(2014,2015,2016,2017,2018,2019,2020,2021)

# number of qualifiers per season
QS <- c(19,19,21,20,21,21,17,22)

# number of seasons
S <- length(SY)

# number of qualifiers
Q <- sum(QS)



# qualifier data 2014-2021 - ranks ####
R_obs <- data.frame(matrix(ncol = 1, nrow = 0))
colnames(R_obs) <- c("driver")
R_obs$driver <- as.character(R_obs$driver)

log_file <- data.frame(matrix(ncol = 4, nrow = 0))
colnames(log_file) <- c("t", "s", "qs", "success")

request <- 1
retries <- 10

t <- 1


# get and merge ranks
for (s in 1:S) {
  
  for (qs in 1:QS[s]) {
    
    url <- paste("http://ergast.com/api/f1/", SY[s], "/", qs, "/qualifying.json",
                 sep = "")
    
    success <- FALSE
    waiting_time <- 60
    retry_count <- 0
    
    while (success == FALSE & retry_count <= retries) {
     
      if ( request %% 2 == 0) { Sys.sleep(60) }
    
      res <- try(GET(url), silent = TRUE)
      
      if (class(res) == "response") {
        
        if (res$status_code == 200) {
          
          data <- get_ranks(res, t)
          R_obs <- merge_ranks(R_obs, data)
          write_xlsx(R_obs, "R_obs.xlsx")
          
          log_file[request,1] <- t
          log_file[request,2] <- s
          log_file[request,3] <- qs
          log_file[request,4] <- "success"
          write_xlsx(log_file, "log_file.xlsx")
          
          request <- request + 1
          t <- t + 1
          success <- TRUE
          
        }
      
      } else {
        
        log_file[request,1] <- t
        log_file[request,2] <- s
        log_file[request,3] <- qs
        log_file[request,4] <- "failure"
        write_xlsx(log_file, "log_file.xlsx")
        
        request <- request + 1
        
        Sys.sleep(waiting_time)
        waiting_time <- waiting_time + 60
        retry_count <- retry_count + 1
        
      }
    
    }
    
    if (retry_count > retries) {
      
      stop("retry_count reached limit")
      
    }
  
  }
  
}



# qualifier data 2014-2021 - unique constructors ####
unique_ctr <- data.frame(matrix(ncol = 1, nrow = 0))
colnames(unique_ctr) <- c("ctr")
unique_ctr$ctr <- as.character(unique_ctr$ctr)

log_file <- data.frame(matrix(ncol = 4, nrow = 0))
colnames(log_file) <- c("t", "s", "qs", "success")

request <- 1
retries <- 10

t <- 1


# get and merge unique constructors
for (s in 1:S) {
  
  for (qs in 1:QS[s]) {
    
    url <- paste("http://ergast.com/api/f1/", SY[s], "/", qs, "/qualifying.json",
                 sep = "")
    
    success <- FALSE
    waiting_time <- 60
    retry_count <- 0
    
    while (success == FALSE & retry_count <= retries) {
      
      if ( request %% 2 == 0) { Sys.sleep(60) }
      
      res <- try(GET(url), silent = TRUE)
      
      if (class(res) == "response") {
        
        if (res$status_code == 200) {
          
          data <- get_unique_ctr(res)
          unique_ctr <- merge_unique_ctr(unique_ctr, data)
          write_xlsx(unique_ctr, "unique_ctr.xlsx")
          
          log_file[request,1] <- t
          log_file[request,2] <- s
          log_file[request,3] <- qs
          log_file[request,4] <- "success"
          write_xlsx(log_file, "log_file.xlsx")
          
          request <- request + 1
          t <- t + 1
          success <- TRUE
          
        }
        
      } else {
        
        log_file[request,1] <- t
        log_file[request,2] <- s
        log_file[request,3] <- qs
        log_file[request,4] <- "failure"
        write_xlsx(log_file, "log_file.xlsx")
        
        request <- request + 1
        
        Sys.sleep(waiting_time)
        waiting_time <- waiting_time + 60
        retry_count <- retry_count + 1
        
      }
      
    }
    
    if (retry_count > retries) {
      
      stop("retry_count reached limit")
      
    }
    
  }
  
}



# qualifier data 2014-2021 - drv-ctr ####
drv_ctr <- data.frame(matrix(ncol = 1, nrow = 0))
colnames(drv_ctr) <- c("driver")
drv_ctr$driver <- as.character(drv_ctr$driver)

log_file <- data.frame(matrix(ncol = 4, nrow = 0))
colnames(log_file) <- c("t", "s", "qs", "success")

request <- 1
retries <- 10

t <- 1


# get and merge drv-ctr
for (s in 1:S) {
  
  for (qs in 1:QS[s]) {
    
    url <- paste("http://ergast.com/api/f1/", SY[s], "/", qs, "/qualifying.json",
                 sep = "")
    
    success <- FALSE
    waiting_time <- 60
    retry_count <- 0
    
    while (success == FALSE & retry_count <= retries) {
      
      if ( request %% 2 == 0) { Sys.sleep(60) }
      
      res <- try(GET(url), silent = TRUE)
      
      if (class(res) == "response") {
        
        if (res$status_code == 200) {
          
          data <- get_drv_ctr(res, t)
          drv_ctr <- merge_drv_ctr(drv_ctr, data)
          write_xlsx(drv_ctr, "drv_ctr.xlsx")
          
          log_file[request,1] <- t
          log_file[request,2] <- s
          log_file[request,3] <- qs
          log_file[request,4] <- "success"
          write_xlsx(log_file, "log_file.xlsx")
          
          request <- request + 1
          t <- t + 1
          success <- TRUE
          
        }
        
      } else {
        
        log_file[request,1] <- t
        log_file[request,2] <- s
        log_file[request,3] <- qs
        log_file[request,4] <- "failure"
        write_xlsx(log_file, "log_file.xlsx")
        
        request <- request + 1
        
        Sys.sleep(waiting_time)
        waiting_time <- waiting_time + 60
        retry_count <- retry_count + 1
        
      }
      
    }
    
    if (retry_count > retries) {
      
      stop("retry_count reached limit")
      
    }
    
  }
  
}


