#################################################x
#' Project:Santa 2021
#' Script purpose:Skript mit welchem die Tour in drei Teile geteilt für eine erneutes training vorgereitet werden
#' Mon Dec 06 23:33:32 2021
#' Author: Manuel Wick-Eckl 
#################################################x
options(stringsAsFactors = FALSE)

library(tidyverse)
library(glue)

#####Daten----

permutationen <- readRDS(here::here(("02_Data/permutationen.rds")))
santa_matrix <- readRDS(here::here(("02_Data/santa_matrix.rds")))
tour <- readRDS(here::here(("02_Data/tsp_tour_0612.rds")))

tour
names(tour)
#### in drei teile teilen ----

tour_length <- length(tour)

ind_1 <- seq(1:1640)
max(ind_1)
ind_2 <- seq(1641: 3281)+1640
max(ind_2)
ind_3 <- seq(3282: 4920)+3281
max(ind_3)
santa1 <- names(tour)[ind_1]
santa2 <- names(tour)[ind_2]
santa3 <- names(tour)[ind_3]

#Checks
sum(length(santa1) + length(santa2) + length(santa3))  == 4920 
#Muss True sein
length(setdiff(c(santa1, santa2, santa3), names(tour))) == 0
#muss True sein
length(setdiff(names(tour), c(santa1, santa2, santa3))) == 0
#muss True sein
any(duplicated(c(santa1, santa2, santa3)))
#Muss FALSE sein

santa1_2 <- permutationen$santa_1_2_perm

###
santa1 <- c(santa1, santa1_2)
santa2 <- c(santa2, santa1_2)
santa3 <- c(santa3, santa1_2)

###in ATSP matrix verwandeln----

source(here::here("01_R/custom_functions.R"))

data <- list ("data" = list(santa1, santa2, santa3))

santa_data <- map(.x = data$data, ~ generate_dist_matrix_teil2(permutation_vector = .x) )

write_rds(x = santa_data, file = "02_Data/santa_matrix_teil2.rds")
zip(zipfile = "02_Data/santa_matrix_teil2.zip", files = "02_Data/santa_matrix_teil2.rds")
