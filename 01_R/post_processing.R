#################################################x
#' Project:Santa 2021
#' Script purpose:Skript mit welchem die gefundenen Lösungen optimiert werden
#' Mon Dec 06 23:33:32 2021
#' Author: Manuel Wick-Eckl 
#################################################x
options(stringsAsFactors = FALSE)

library(tidyverse)
library(glue)
library(utf8)
library(readr)
library(rlist)

source("01_R/custom_functions.R")
#####Prep solution----
#
santa_solution <- readRDS("02_Data/tsp_tour_teile_1912.rds")
permutationen <- readRDS("02_Data/permutationen.rds")

#solution <- santa_submission(santa_solution, permutationen)
solution <- santa_solution
# Step 1 Check alle Permutationen enthalten alle santa permutationen enthalten
solution_names <- c(names(solution[[1]]), names(solution[[2]]), names(solution[[3]]))


solution <- list("solution_permutationen" = list(
  "solution1" = names(solution[[1]]),
  "solution2" = names(solution[[2]]),
  "solution3" = names(solution[[3]])))

check(solution_list = solution$solution_permutationen, permutation_list = permutationen$permutationen)

#Step2 Strings zusammenlegen
solution <- rlist::list.append(
  solution, "solution_cut" = map(solution$solution_permutationen, ~ {cut_permutation(.x)
  })
)

check(solution_list = solution$solution_cut, permutation_list = permutationen$permutationen)

#String distance 
solution <- rlist::list.append(
  solution, "solution_distance" = map(solution$solution_permutationen, ~ {
   temp <- .x
   imap_dbl(temp,  ~ combin_distance(.x, temp[.y+1]))
  })
)

####Post_Processing
#Wildcards 

#'Idee: 6er Cluster mit Wildcard versehen und an die stelle schieben an welcher sie den max beitrag leistet 
#'Idealerweise nur Cluster wählen welche nicht 12er Permutationen beinhalten 

cluster_identifizieren <- function(soution_permutation, solution_distance, santa_1_2_perm) {
  ind <- which(solution_distance == 6)
  
  cluster <- imap(ind[seq_along(ind)-1], ~ {
                    cluster <- solution_permutation[seq(.x, ind[.y+1])]
                    cluster_distance <- solution_distance[seq(.x, ind[.y+1])]
                    cluster_length <- length(cluster_distance)
                    santa1_2_perm <- any(santa_1_2_perm %in% cluster)
                    ind_from = .x
                    ind_to = ind[.y+1]
                    
                    return(list("cluster" = cluster,
                                "cluster_distance" = cluster_distance,
                                "cluster_length" = cluster_length,
                                "santa1_2_perm" = santa1_2_perm, 
                                "ind_from" = ind_from, 
                                "ind_to" = ind_to)
                           )
                           })
  
  return(cluster)
  
}

santa_cluster <- map2(.x = solution$solution_permutationen, 
                      .y = solution$solution_distance, 
                      ~ cluster_identifizieren(soution_permutation = .x, 
                                               solution_distance = .y, 
                                               santa_1_2_perm = permutationen$permutationen$santa_1_2_perm)
                      )
