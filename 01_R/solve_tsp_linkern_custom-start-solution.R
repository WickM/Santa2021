#################################################x
#' Project: Santa 2021 Teil2
#' Script purpose: Skript mit welchem eine TSP lösung mit linkern gesucht wird
#' Sat Dec 04 08:18:51 2021
#' Author: Manuel Wick-Eckl 
#################################################x

library(TSP)
library(tidyverse)
library(doParallel)

source("01_R/custom_functions.R")
registerDoParallel()

concorde_path(here::here("tsp_solver/"))

###-Load Data----
unzip(zipfile = here::here("02_Data/santa_teile_matix_custom-start-solution.zip"), 
      overwrite = TRUE, exdir = here::here("02_Data/"))
# permutationen <- readRDS(here::here("02_Data/permutationen.rds"))
santa_matrix <- readRDS(here::here("02_Data/santa_teile_matix_custom-start-solution.rds"))

###
# TSP::linkern_help()
santa_tour <- lapply(1:3, function(ll){
  sm <- santa_matrix[[ll]]
  sm[sm == 7] <- Inf
  sm <- sm[order(row.names(sm)),][, sort(row.names(sm))]
  atsp <- ATSP(sm)
  tr <- solve_TSP(atsp, 
                  method = "linkern",
                  as_TSP = TRUE,
                  precision = 0,
                  control = list("clo" = paste0("-K 1 ",
                                                "-s 1337 ", 
                                                "-t 18000 ", 
                                                "-S linkern_custom-start-solution-", ll, ".tour ", 
                                                "-R 1000000000")), 
                  verbose = TRUE)
  tr
  })

santa_tour[[1]]
lapply(santa_tour, TSP::tour_length)
lapply(santa_tour, length)
lapply(1:3, function(ll){
  sm <- santa_matrix[[ll]]
  sm[sm == 7] <- Inf
  sm <- sm[order(row.names(sm)),][, sort(row.names(sm))]
  st <- santa_tour[[ll]]
  sum(sm[cbind(st[-length(st)], st[-1])])
})

write_rds(santa_tour, here::here("02_Data/tsp_tour_teile_2512-custom-start-solution.rds"))

