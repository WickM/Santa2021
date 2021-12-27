
library(TSP)
library(tidyverse)

library(doParallel)
registerDoParallel()

concorde_path(here::here("tsp_solver/"))
unzip(zipfile = here::here("02_Data/atsp_full.zip"), 
      overwrite = TRUE, exdir = here::here("02_Data/"))
atsp_all <- readRDS(here::here("02_Data/atsp_full.rds"))
# tsp_all <- readRDS(here::here("02_Data/tsp_full.rds"))

ind <- grep("^12", row.names(atsp_all), invert = TRUE)
atsp <- ATSP(atsp_all[ind, ind])
atsp[atsp == 7] <- Inf

tour_name <- here::here("tsp_solver", "linkern.20211225")

clo <- paste0(c("-K 1",
                "-s 1337",
                # paste0("-o ", tour_name), # save final tour -- not available via R
                # paste0("-y ", tour_name), # starting cycle
                paste0("-S ", tour_name, ".01.tour"), # save tour after every 10k kicks
                "-t 3600",#"-t 172800",
                "-R 999999999"), collapse = " ")

tour <- solve_TSP(atsp, method = "linkern", as_TSP = TRUE,
                  control = list(clo = clo), precision = 0)
save("tour", file = paste0(tour_name, ".01.Rdata"))

tour_length_bool <- TRUE
ii <- 1

while(ii <= 24 & tour_length_bool){#2:24){# ii <- 1

  ii <- ii+1  
  tour0 <- tour
  
  clo <- paste0(c("-K 1",
                  #"-s 1337",
                  # paste0("-o ", tour_name), # save final tour -- not available via R
                  paste0("-y ", tour_name, ".", ifelse((ii-1) < 10, "0", ""), ii-1, ".tour"), # starting cycle
                  paste0("-S ", tour_name, ".", ifelse(ii < 10, "0", ""), ii, ".tour"), # save tour after every 10k kicks
                  "-t 3600",#"-t 172800",
                  "-R 999999999"), collapse = " ")
  
  tour <- solve_TSP(atsp, method = "linkern", as_TSP = TRUE,
                   control = list(clo = clo), precision = 0)
  save("tour", file = paste0(tour_name, ".", ifelse(ii < 10, "0", ""), ii, ".Rdata"))
  tour_length_bool <- tour_length(tour) < tour_length(tour0)
}
