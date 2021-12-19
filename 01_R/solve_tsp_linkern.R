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
#unzip(zipfile = here::here("02_Data/santa_matrix_teil2.zip"), overwrite = TRUE, exdir = here::here("02_Data/"))
permutationen <- readRDS(here::here("02_Data/permutationen.rds"))
santa_matrix <- readRDS(here::here("02_Data/santa_teile_matix.rds"))

###
TSP::linkern_help()
santa_tour <- map(santa_matrix, ~ generate_santa_tour(dat = .x, control = list(
  "clo" = "-t 18000 -S linkern.tour -R 1000000000"), verbose = FALSE
  )
  )

santa_tour[[1]]
write_rds(santa_tour, here::here("02_Data/tsp_tour_teile_1912.rds"))
^
