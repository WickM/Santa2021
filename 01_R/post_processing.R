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

## 1 Cluster Ränder identifizieren

require(furrr)
plan(multisession, workers = future::availableCores())

santa_cluster <- map2(.x = solution$solution_permutationen, 
                      .y = solution$solution_distance, 
                      ~ cluster_identifizieren(solution_permutation = .x, 
                                               solution_distance = .y, 
                                               santa_1_2_perm = permutationen$permutationen$santa_1_2_perm)
)

tictoc::tic()

cluster_matrix <- map2(.x = santa_cluster, .y = solution$solution_permutationen, ~{
  
  cluster <- .x
  solution_permutation <- .y
  
  ## 2 Wildcards plazieren permutation Vector aufbauen matxix erstellen
  vec <- map_chr(cluster, ~ pluck(.x, "cluster") [1])
  #Cluster Anfänge 
  combinat <- combinat::combn(x = vec, m = 2, simplify = FALSE)
  
  sol_matrix <- furrr::future_map(combinat, ~{
    combin <- .x 
    
    vec <- map_chr(combin , ~{str_replace(.x, pattern = str_split(.x, pattern = "", simplify = TRUE)[1], 
                                          replacement = "X")
    })
    
    perm_begin <- map_chr(cluster, ~ pluck(.x, "cluster") [1])
    perm_begin <- perm_begin[which(! perm_begin %in% combin)]
    perm_begin <- c(vec, perm_begin, solution_permutation[1])
    
    ind <- map_dbl(cluster, ~ pluck(.x, "ind_from") -1)
    perm_ende <- solution_permutation[ind]
    perm_ende <- c(perm_ende, solution_permutation[length(solution_permutation)])
    
    permutation_vector <- c(perm_begin, perm_ende)
    santa_teile_matrix <- generate_dist_matrix_teil2(permutation_vector = permutation_vector)
    
    ind_begin <- which(colnames(santa_teile_matrix) %in% perm_begin )
    ind_ende <- which(colnames(santa_teile_matrix) %in% perm_ende )
    
    small_cluster <- perm_begin[which(perm_begin %in% perm_ende)]
    
    for (ii in seq(1, nrow(santa_teile_matrix))) {
      perm <- santa_teile_matrix$permutation[ii]
      
      if (! perm %in% small_cluster) {
        if (perm %in% perm_begin) {santa_teile_matrix[ii, ind_begin] <-NA}
        if (perm %in% perm_ende) {santa_teile_matrix[ii, ind_ende] <-NA}
      }
      if (perm %in% small_cluster) {santa_teile_matrix[ii, perm] <-NA}
    }
    
    return(santa_teile_matrix)
    
  })
  
  cli::cli_alert_success("Cluster Matrix string erstellt")
  
  perm_begin <- map_chr(cluster, ~ pluck(.x, "cluster") [1])
  
  ind <- map_dbl(cluster, ~ pluck(.x, "ind_from") -1)
  perm_ende <- solution_permutation[ind]
  
  ind <- which(solution_permutation %in%  unique(c(perm_begin, perm_ende)))
  
  cluster_tour <- solution_permutation[ind]
  
  return(list("matrix" =sol_matrix, 
              "cluster_tour" = cluster_tour)
         )
})

tictoc::toc()

saveRDS(object = cluster_matrix, file = "02_Data/cluster_matrix.rds")


save(cluster_matrix, solution, santa_cluster, file = "02_Data/wildcard_postprocessing.Rdata")
