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
santa_solution <- readRDS("02_Data/post-optim-Teil2-best-39.rds")

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

#Cluster identifizieren
santa_cluster <- map2(.x = solution$solution_permutationen, 
                      .y = solution$solution_distance, 
                      ~ cluster_identifizieren(solution_permutation = .x, 
                                               solution_distance = .y, 
                                               santa_1_2_perm = permutationen$permutationen$santa_1_2_perm,
                                               cluster_distance = 6)
)


###Cluster optimieren
#jeden String auf einem eigenen Core
require(furrr)
plan(multisession, workers = 8)


wildcards_optim <- function(solution, times) {
  wildcards_solution_old <- solution
  
  solution_distance <- imap_dbl(wildcards_solution_old, ~{combin_distance(
    var1 = .x, 
    var2 = wildcards_solution_old[.y +1])})
  
  cluster <- cluster_identifizieren(solution_permutation = wildcards_solution_old, 
                                    solution_distance = solution_distance,
                                    santa_1_2_perm = permutationen$permutationen$santa_1_2_perm,
                                    cluster_distance =max(solution_distance) -1)
  cluster_old <- cluster
  
  baseline <- imap_dbl(wildcards_solution_old, ~{combin_distance(
    var1 = .x, 
    var2 = wildcards_solution_old[.y +1])})
  baseline_sum <- sum(baseline)
  
  vec <- map_chr(cluster, ~ pluck(.x, "cluster_begin") )
  #Cluster Anfänge 
  combinat <- combinat::combn(x = vec, m = 2, simplify = FALSE)
  
  sol_matrix <- function(combin){
    
    vec <- map_chr(combin , ~{str_replace(.x, pattern = str_split(.x, pattern = "", simplify = TRUE)[1], 
                                          replacement = "X")
    })
    
    perm_begin <- map_chr(cluster, ~ pluck(.x, "cluster_begin") )
    perm_begin <- perm_begin[which(! perm_begin %in% combin)]
    perm_begin <- c(vec, perm_begin)
    
    perm_ende <- map_chr(cluster, ~ pluck(.x, "cluster_end") )
    
    permutation_vector <- c(perm_begin, perm_ende)
    santa_teile_matrix <- generate_dist_matrix_teil2(permutation_vector = permutation_vector)
    
    ind_begin <- which(colnames(santa_teile_matrix) %in% perm_begin )
    ind_ende <- which(colnames(santa_teile_matrix) %in% perm_ende )
    
    perm_begin <- map_chr(cluster, ~ pluck(.x, "cluster_begin") )
    small_cluster <- perm_begin[which(perm_begin %in% perm_ende)]
    
    for (ii in seq(1, nrow(santa_teile_matrix))) {
      perm <- santa_teile_matrix$permutation[ii]
      
      if (! perm %in% small_cluster) {
        if (perm %in% perm_begin) {santa_teile_matrix[ii, ind_begin] <-NA}
        if (perm %in% perm_ende) {santa_teile_matrix[ii, ind_ende] <-NA}
        if (perm %in% vec) {santa_teile_matrix[ii, ind_begin] <-NA}
        
      }
      if (perm %in% small_cluster) {
        ind_small_cluser <- which(colnames(santa_teile_matrix) %in% perm )
        santa_teile_matrix[ii, ind_small_cluser] <-NA}
    }
    
    return(santa_teile_matrix)
    
  }
  ind_solution <- 1

  repeat{
    ind_combinat <- sample(x = seq(1, length(combinat)), 1)
    wildcards_matrix <- sol_matrix(combin = combinat[[ind_combinat]])
    wildcards_akt <- combinat[[ind_combinat]]
    
    cluster <- cluster_old
    
    cluster_wildcard <- map_dbl(cluster, ~ {pluck(.x, "cluster_begin") %in% wildcards_akt })
    cluster_wildcard <-which(cluster_wildcard == 1)
    
    for (ii in cluster_wildcard) {
      cluster_x <- cluster[[ii]]$cluster
      cluster_x[1] <- str_replace(cluster_x[1], 
                                  pattern = str_split(cluster_x[1], pattern = "", simplify = TRUE)[1], 
                                  replacement = "X") 
      
      cluster[[ii]]$cluster <- cluster_x
      cluster[[ii]]$cluster_begin <-  cluster_x[1]
      
    }
    
    ###
    tour_data <- generate_santa_tour(dat = wildcards_matrix)
    tour <- solve_TSP(x = tour_data, method = "cheapest_insertion" , as_TSP = TRUE,  two_opt = TRUE)
    
    sum_baseline <- sum(imap_dbl(names(tour), ~combin_distance(var1 = .x, var2 = names(tour)[.y +1])))
    repat_two_opt <- times
    inc <- 1
    
    #Repeat Schleife um optimale Tour zu finden
    repeat{
      inc <- inc + 1
      tour <- solve_TSP(x = tour_data, method = "two_opt" ,control = list(tour), as_TSP = TRUE)
      sum_two_opt <- sum(imap_dbl(names(tour), ~combin_distance(var1 = .x, var2 = names(tour)[.y +1])))
      
      if (sum_two_opt < sum_baseline) {
        sum_baseline <- sum_two_opt
        inc <- 0
      }
      
      if (inc == repat_two_opt) {break}
    }
    
    
    #1 verwendbare Cluster aus Tour identifizieren
    # Ende Anfang verbindung + distance kleiner als distance in alter lösung 
    
    perm_begin <-  map_chr(cluster, ~ pluck(.x, "cluster_begin") )
    perm_ende <-   map_chr(cluster, ~ pluck(.x, "cluster_end") )
    
    ind_verwendbare_cluster <-  imap_dbl(names(tour), ~ {
      verwendbar <- FALSE
      perm <- .x
      perm_ind <- .y
      
      if ( (perm %in% perm_ende) & names(tour)[perm_ind +1] %in% perm_begin & perm_ind +1 != length(tour)) {
        dist_new <- combin_distance(var1 = perm, 
                                   var2 =  names(tour)[perm_ind +1])
        
        cluster_end_old <- keep(cluster, ~ {pluck(.x, "cluster_end") == perm})
        cluster_begin_old <- keep(cluster, ~ {pluck(.x, "ind_from") == pluck(cluster_end_old, 1,"ind_to") +1})
    
        if (length(cluster_begin_old) != 0) {
          dist_old <- combin_distance(var1 = pluck(cluster_end_old,1,"cluster_end"), 
                                      var2 = pluck(cluster_begin_old,1,"cluster_begin") )
          
          if (dist_old > dist_new) {verwendbar <- TRUE}
        } else {verwendbar <- TRUE}
      }
      return(verwendbar)
      })
    ind_verwendbare_cluster <- which(ind_verwendbare_cluster == 1)
    ind_verwendbare_cluster <- c(ind_verwendbare_cluster, ind_verwendbare_cluster +1) %>% 
      sort.int()
    
    names_verwendbare_cluster <- names(tour) [ind_verwendbare_cluster]
    
    #2 Verwendbare Cluster zusammenhängen
    
    tour_cluster <- map(names_verwendbare_cluster, ~{
      name <- .x
      keep(cluster, ~{pluck( .x, "cluster_begin") == name | pluck( .x, "cluster_end") == name})
           }) %>% flatten()
    
    tour_cluster <- map(seq(1, length(tour_cluster)), ~ pluck(tour_cluster, .x, "cluster")) %>% 
      flatten_chr()
    
    tour_old <- map(seq(1, length(cluster)), ~ pluck(cluster, .x, "cluster")) %>% 
      flatten_chr()
    
    length(wildcards_solution_old) == length(tour_old)
    
    #3 fehlende permutatioen ergänzen
    ind <- which(!tour_old  %in% tour_cluster)
    tour_new <- c(tour_cluster, tour_old[ind])
    
    all(tour_new %in% tour_old)
    
    #4 überzählige Wildcards entfernen besten behalten
    
    ind_wildcards <- str_which(tour_new, pattern = "X")
    if (length(ind_wildcards) > 2) {
      
      dist <- map_dbl(ind_wildcards, ~ combin_distance(var1 = tour_new[.x -1],var2 = tour_new[.x]) ) %>% order()
      
      wildcards_keep <- ind_wildcards[dist] [1:2]
      wildcards_discard <- ind_wildcards[which(! ind_wildcards %in% wildcards_keep)]
      
      tour_new[wildcards_discard] <- map(wildcards_discard, ~ fix_wildcard(
        var1 = tour_new[.x-1], var2 = tour_new[.x], ind_wildcard = 2)) %>% 
        map_chr(., ~pluck(.x, "perm"))
      
    }
    
    dist_old <- sum(imap_dbl(tour_old, ~combin_distance(var1 = .x, var2 = tour_old[.y +1])))
    dist_new <- sum(imap_dbl(tour_new, ~combin_distance(var1 = .x, var2 = tour_new[.y +1])))
    
    
    if (dist_new <= dist_old) {
      tour_old <- tour_new
      ind_solution <- 1}
    
    if (dist_old < dist_new) {
      ind_solution <- ind_solution +1}
  
    if (ind_solution == times) {break}
  }
  return(tour_old)
}

solution_new <- furrr::future_map(solution$solution_permutationen, ~ wildcards_optim(solution = .x, times = 100))


map(solution_new, ~ {
  tour <- .x
  sum(imap_dbl(tour, ~combin_distance(var1 = .x, var2 = tour[.y +1])))
  })

map(solution$solution_permutationen, ~ {
  tour <- .x
  sum(imap_dbl(tour, ~combin_distance(var1 = .x, var2 = tour[.y +1])))
})

