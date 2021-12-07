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

generate_santa_tour <- function(dat) {
  
  santa_tsp_data <- dat %>% 
    select(-dataset) %>% 
    mutate(across(.cols = everything(), ~replace_na(.x, Inf))) %>% 
    mutate(across(.cols = everything(), ~ifelse(.x == 7, Inf, .x))) %>% 
    as.data.frame()
  
  row.names(santa_tsp_data) <- santa_tsp_data$permutation
  
  santa_tsp_data <-santa_tsp_data %>% 
    select(-permutation) %>% 
    as.matrix()
  
  atsp <- ATSP(santa_tsp_data)
  
  tour <- solve_TSP(atsp, method = "linkern", as_TSP = TRUE)
  
  return(tour)
}
###

santa_tour <- map(santa_matrix, ~ generate_santa_tour(dat = .x))

test <- santa_tour[[1]]
test

write_rds(santa_tour, here::here("02_Data/tsp_tour_teil2_0712.rds"))

