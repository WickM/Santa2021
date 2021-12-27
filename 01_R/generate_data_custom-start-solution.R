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
best_custom_solution <- readLines(here::here("tsp_solver", "linkern.20211225.02.tour"))
permutation <- readRDS("02_Data/permutationen.rds")

unzip(zipfile = here::here("02_Data/atsp_full.zip"), 
      overwrite = TRUE, exdir = here::here("02_Data/"))
atsp_all <- readRDS(here::here("02_Data/atsp_full.rds"))

#
ind <- grep("^12", row.names(atsp_all), invert = TRUE)
atsp <- ATSP(atsp_all[ind, ind])
atsp[atsp == 7] <- Inf

solution <- strsplit(paste0(best_custom_solution[-1], collapse = ""), " ") %>% unlist %>% as.numeric
solution <- solution + 1
solution <- solution[solution <= nrow(atsp)]
sum(atsp[cbind(solution, c(solution[-1], solution[1]))])
solution_ind <- solution
solution <- rownames(atsp)[solution]

#
any(duplicated(solution))
all(c(permutation$santa_1_2_perm, permutation$santa_rest_perm) %in% solution)
all(permutation$santa_rest_perm %in% solution)

# teile so, dass anzahl permutationen gleich ist?
# oder teile so, dass summe der distanzen gleich ist? <-!
dist_solution <- atsp[cbind(solution_ind, c(solution_ind[-1], solution_ind[1]))]
dist_split <- apply(abs(outer(cumsum(dist_solution) / sum(dist_solution), c(1/3, 2/3), "-")), 2, which.min)
santa_teile <- list(solution[1:dist_split[1]],
                    solution[(dist_split[1]+1):dist_split[2]],
                    solution[(dist_split[2]+1):length(solution)])
santa_teile <- lapply(santa_teile, function(st) c(permutation$santa_1_2_perm, st))

all(permutation$santa_1_2_perm %in% santa_teile[[1]])
all(permutation$santa_1_2_perm %in% santa_teile[[2]])
all(permutation$santa_1_2_perm %in% santa_teile[[3]])
all(permutation$santa_rest_perm %in% c(santa_teile[[1]], santa_teile[[2]], santa_teile[[3]] ))

santa_teile_matrix <- lapply(santa_teile, function(st){
  ind <- match(st, row.names(atsp_all))
  atsp_all[ind, ind]
} )

write_rds(x = santa_teile_matrix, "02_Data/santa_teile_matix_custom-start-solution.rds")
zip(zipfile = here::here("02_Data/santa_teile_matix_custom-start-solution.zip"), 
    files = here::here("02_Data/santa_teile_matix_custom-start-solution.rds"))
