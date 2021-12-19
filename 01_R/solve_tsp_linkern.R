#################################################x
#' Project: Santa 2021 Teil2
#' Script purpose: Skript mit welchem eine TSP lösung mit linkern gesucht wird
#' Sat Dec 04 08:18:51 2021
#' Author: Manuel Wick-Eckl 
#################################################x

library(TSP)
library(tidyverse)

library(doParallel)
registerDoParallel()

concorde_path(here::here("tsp_solver/"))

###-Load Data----
unzip(zipfile = here::here("02_Data/santa_matrix_teil2.zip"), overwrite = TRUE, exdir = here::here("02_Data/"))
permutationen <- readRDS(here::here("02_Data/permutationen.rds"))
santa_matrix <- readRDS(here::here("02_Data/02_Data/santa_matrix_teil2.rds"))

###

santa_tour <- map(santa_matrix, ~ generate_santa_tour(dat = .x))

test <- santa_tour[[1]]
test

write_rds(santa_tour, here::here("02_Data/tsp_tour_teil2_0712.rds"))

