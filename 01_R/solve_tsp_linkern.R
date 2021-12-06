#################################################x
#' Project: Santa 2021
#' Script purpose: Skript mit welchem eine TSP lösung mit linkern gesucht wird
#' Sat Dec 04 08:18:51 2021
#' Author: Manuel Wick-Eckl 
#################################################x

library(TSP)
library(tidyverse)

library(doParallel)
registerDoParallel()

###-Load Data----
unzip(zipfile = here::here("02_Data/santa_matrix.zip"), overwrite = TRUE, exdir = here::here("02_Data/"))
permutationen <- readRDS(here::here("02_Data/permutationen.rds"))

# da hab ich beim zippen nicht aufgepasst, daher der scheiß pfad
santa_matrix <- readRDS(here::here("02_Data/Users/ick/Documents/projekte_privat/Santa2021/02_Data/santa_matrix.rds"))

###start TSP----

head(santa_matrix$santa_small)

santa_tsp_data <- santa_matrix$santa_small %>% 
  select(-dataset) %>% 
  mutate(across(.cols = everything(), ~replace_na(.x, Inf))) %>% 
  as.data.frame()

row.names(santa_tsp_data) <- santa_tsp_data$permutation

santa_tsp_data <-santa_tsp_data %>% 
  select(-permutation) %>% 
  as.matrix()

atsp <- ATSP(santa_tsp_data)

## use some methods
n_of_cities(atsp)
labels(atsp)

data <- matrix(runif(10^2), ncol = 10, dimnames = list(1:10, 1:10))

atsp <- ATSP(data)

concorde_path(here::here("tsp_solver/linkern.exe/"))

tour <- solve_TSP(atsp, method = "linkern", as_TSP = TRUE)

tour

