# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


# closing the sections provides an overview of the script


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
# todor::todor_file("R/F1_hybrid_era_qualifier_data.R")

library(todor)
library(httr)
library(jsonlite)
library(tidyverse)
library(openxlsx)



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
                     data_temp$Driver$familyName,
                     data_temp$Driver$code,
                     data_temp$position)
  
  colnames(data) <- c("driver_id",
                      "driver_name",
                      "driver_code",
                      paste("rank t", t, sep = ""))
  
  return(data)
  
}


merge_ranks <- function(R_act_temp, data) {
  
  R_act <- full_join(x = R_act_temp,
                     y = data,
                     by = c("driver_id",
                            "driver_name",
                            "driver_code"))
  
  return(R_act)
  
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


# data processing - drv-ctr
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
R_act <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(R_act) <- c("driver_id",
                     "driver_name",
                     "driver_code")
R_act$driver_id <- as.character(R_act$driver_id)
R_act$driver_name <- as.character(R_act$driver_name)
R_act$driver_code <- as.character(R_act$driver_code)

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
          R_act <- merge_ranks(R_act, data)
          write.xlsx(R_act,
                     "data/R_act_qualifier.xlsx",
                     overwrite = TRUE)
          
          log_file[request,1] <- t
          log_file[request,2] <- s
          log_file[request,3] <- qs
          log_file[request,4] <- "success"
          write.xlsx(log_file,
                     "log_file.xlsx",
                     overwrite = TRUE)
          
          request <- request + 1
          t <- t + 1
          success <- TRUE
          
        }
      
      } else {
        
        log_file[request,1] <- t
        log_file[request,2] <- s
        log_file[request,3] <- qs
        log_file[request,4] <- "failure"
        write.xlsx(log_file,
                   "log_file.xlsx",
                   overwrite = TRUE)
        
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
          write.xlsx(unique_ctr,
                     "unique_ctr.xlsx",
                     overwrite = TRUE)
          
          log_file[request,1] <- t
          log_file[request,2] <- s
          log_file[request,3] <- qs
          log_file[request,4] <- "success"
          write.xlsx(log_file,
                     "log_file.xlsx",
                     overwrite = TRUE)
          
          request <- request + 1
          t <- t + 1
          success <- TRUE
          
        }
        
      } else {
        
        log_file[request,1] <- t
        log_file[request,2] <- s
        log_file[request,3] <- qs
        log_file[request,4] <- "failure"
        write.xlsx(log_file,
                   "log_file.xlsx",
                   overwrite = TRUE)
        
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
          write.xlsx(drv_ctr,
                     "data/drv_ctr.xlsx",
                     overwrite = TRUE)
          
          log_file[request,1] <- t
          log_file[request,2] <- s
          log_file[request,3] <- qs
          log_file[request,4] <- "success"
          write.xlsx(log_file,
                     "log_file.xlsx",
                     overwrite = TRUE)
          
          request <- request + 1
          t <- t + 1
          success <- TRUE
          
        }
        
      } else {
        
        log_file[request,1] <- t
        log_file[request,2] <- s
        log_file[request,3] <- qs
        log_file[request,4] <- "failure"
        write.xlsx(log_file,
                   "log_file.xlsx",
                   overwrite = TRUE)
        
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


