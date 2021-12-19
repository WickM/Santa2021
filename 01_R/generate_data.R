#################################################x
#' Project: Santa Data generation
#' Script purpose: mit dem Skript wird die beste verfügbare sollution in 3 Teile aufgebrochen und die jeweils fehlenden Permutationen werden angehängt
#' Sat Dec 18 22:55:49 2021
#' Author: Manuel Wick-Eckl 
#################################################x
options(stringsAsFactors = FALSE)

library(tidyverse)
library(rlist)

source("01_R/custom_functions.R")


#####Daten einlesen
best_avaiable_solution <- readLines("02_Data/best_avaiable_solution.txt")
permutation <- readRDS("02_Data/permutationen.rds")

#best verfügbare solution in permutationen aufbrechen
anz_perm <- length(permutation$santa_1_2_perm) + length(permutation$santa_rest_perm)

solution <- break_solution(solution = best_avaiable_solution, anz_perm = anz_perm)

any(duplicated(solution))
all( c(permutation$santa_1_2_perm, permutation$santa_rest_perm) %in% solution)

santa_teile <- teilen(anz_teile = 3, permutationen_1_2 = permutation$santa_1_2_perm, solution = solution)

all(permutation$santa_1_2_perm %in% santa_teile[[1]])
all(permutation$santa_1_2_perm %in% santa_teile[[2]])
all(permutation$santa_1_2_perm %in% santa_teile[[3]])

all(permutation$santa_rest_perm %in% c(santa_teile[[1]], santa_teile[[2]], santa_teile[[3]] ))

santa_teile_matrix <- map(santa_teile, ~ generate_dist_matrix_teil2(permutation_vector = .x) )

write_rds(x = santa_teile_matrix, "02_Data/santa_teile_matix.rds")
