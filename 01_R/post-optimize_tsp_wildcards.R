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
plan(multisession, workers = 3)





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
    
    
    ### Clusterweise Tour aufbauen
    perm_begin <-  map_chr(cluster, ~ pluck(.x, "cluster_begin") )
    perm_ende <-   map_chr(cluster, ~ pluck(.x, "cluster_end") )
    perm_begin_cluster <- c(perm_begin, names(tour)[str_which(string = names(tour), pattern = "X")])
    
    ind_verwendbare_cluster <-  imap(names(tour), ~ {
      ifelse((.x %in% perm_ende) & (names(tour)[.y +1] %in% perm_begin_cluster), 
             which(.x == names(tour)), NA)}) %>% 
      purrr::discard(~is.na(.x)) %>% 
      flatten_dbl()
    
    cluster_start <- keep(cluster, ~ {pluck(.x, "cluster_end") == names(tour)[ind_verwendbare_cluster[1]] })
    tour_new <- pluck(cluster_start,1, "cluster")
      laufvar <- 1
    #Repeat Schleife zum Tour Aufbau
    repeat{
      
      akt_tour_end <- tour_new[length(tour_new)]
      
      cluster_akt <- keep(cluster, ~ {pluck(.x, "cluster_end") == akt_tour_end })
      
      ii <- which(names(tour) == akt_tour_end)
      cluster_begin_new <- keep(cluster, ~ {pluck(.x, "cluster_begin") == names(tour)[ii+1] })
      cluster_begin_old <- keep(cluster, ~ {pluck(.x, "ind_from") == pluck(cluster_akt,1,"ind_to") +1})
      
      if (length(cluster_begin_old) == 0) {cluster_begin_old <- cluster_begin_new} 
      if (any( pluck(cluster_begin_old,1,"cluster") %in% tour_new)) {cluster_begin_old <- cluster_begin_new} 
      if (any( pluck(cluster_begin_new, 1,"cluster") %in% tour_new)) {break}
      
      ind_wildcards_tour <- str_which(tour_new, pattern = "X")
      wildcards_cluster <- any(str_detect(string = c(pluck(cluster_begin_new,1,"cluster_begin"), pluck(cluster_begin_old,1,"cluster_begin")),
                                              pattern = "X"))
      
      #'Wenn noch keine oder weniger wie 2 Wildcards in der Tour vorkommen
      if (length(ind_wildcards_tour) < 2 | wildcards_cluster == FALSE) {
        dist_new <- combin_distance(var1 = pluck(cluster_akt,1,"cluster_end"), var2 = pluck(cluster_begin_new,1,"cluster_begin") )
        dist_old <- combin_distance(var1 = pluck(cluster_akt,1,"cluster_end"), var2 = pluck(cluster_begin_old,1,"cluster_begin") )
        
        # wenn die neue Tour kürzer ist als die alte
        if (dist_new <= dist_old) {tour_new <- c(tour_new, pluck(cluster_begin_new,1,"cluster"))}
        #Wenn die alte kürzer ist als die neue nehme ich die alte sofern die alte lösung in der neuen Tour keine bessere verwendung hat
        if (dist_old < dist_new) {
                    cluster_begin_new2 <- keep(cluster, ~ {pluck(.x, "cluster_begin") == names(tour)[ii+1] })
          dist_new2 <- combin_distance(var1 = pluck(cluster_begin_old,1,"cluster_end"), var2 = pluck(cluster_begin_new2,1,"cluster_begin") )
          
          #die alte Lösung ist die bessere
          if (dist_old < dist_new2) {tour_new <- c(tour_new, pluck(cluster_begin_old,1,"cluster"))}
          #die alte Lösung ist in der neuen Tour besser verwendet
          if (dist_new2 < dist_old) {tour_new <- c(tour_new, pluck(cluster_begin_new,1,"cluster"))}
        }
      }
      
      #'Wenn bereits 2 Wildcards in der Tour vorkommen und im cluster eine zusätzliche wildcard kommen würde
      if (length(ind_wildcards_tour) == 2 & wildcards_cluster == TRUE) {
          
          dist_new <- combin_distance(var1 = pluck(cluster_akt,1,"cluster_end"), var2 = pluck(cluster_begin_new,1,"cluster_begin") )
          dist_old <- combin_distance(var1 = pluck(cluster_akt,1,"cluster_end"), var2 = pluck(cluster_begin_old,1,"cluster_begin") )
          
          dist_wildcard <- map_dbl(ind_wildcards_tour, ~{combin_distance(var1 = tour[.x], var2 =  tour[.x+1])})
          
          # wenn die neue Tour kürzer ist als die alte und die neue wildcard mehr bringt als die alte
          if (dist_new <= dist_old) {
            
            if (str_detect(string = pluck(cluster_begin_new,1,"cluster_begin"), "X") == FALSE) {
              tour_new <- c(tour_new, pluck(cluster_begin_new,1,"cluster"))}
            
            #Wenn die neue Wildcard besser als eine der alten ist
            if (str_detect(string = pluck(cluster_begin_new,1,"cluster_begin"), "X") == TRUE) {
              
              ind <- which(dist_wildcard > dist_new) [1]
              ind <- ind_wildcards_tour[ind]
              
              #Neue Wildcard ist schlechter als die bisherigen
              if (is.na(ind) == TRUE) {
                cluster <- pluck(cluster_begin_new,1,"cluster")
                perm <- fix_wildcard(var1 = cluster[1], var2 = cluster[2], ind_wildcard = 1)
                cluster[1] <- perm$perm 
                tour_new <- c(tour_new, cluster)
              }
            
            #neue Wildcard ist besser
            if (is.na(ind) == FALSE)  {
              
              wildcard_old <-  fix_wildcard(var1 = tour_new[ind], var2 = tour_new[ind +1], ind_wildcard = 1)
              tour_new[ind] <- wildcard_old
              
              tour_new <- c(tour_new, pluck(cluster_begin_new,1,"cluster"))
            }
              
              }
            
          }
          
          #Wenn die alte kürzer ist als die neue nehme ich die alte sofern die alte lösung in der neuen Tour keine bessere verwendung hat
          if (dist_old < dist_new) {
            
            ii <- which(names(tour) == pluck(cluster_begin_old, 1, "cluster_end"))
            cluster_begin_new2 <- keep(cluster, ~ {pluck(.x, "cluster_begin") == names(tour)[ii+1] })
            dist_new2 <- combin_distance(var1 = pluck(cluster_begin_old,1,"cluster_end"), var2 = pluck(cluster_begin_new2,1,"cluster_begin") )
            
            ##Keine Wildcard 
            
            if (str_detect(string = pluck(cluster_begin_old,1,"cluster_begin"), "X") == FALSE & dist_old < dist_new2) {
              tour_new <- c(tour_new, pluck(cluster_begin_old, 1,"cluster"))}
            
            if (str_detect(string = pluck(cluster_begin_old,1,"cluster_begin"), "X") == FALSE & dist_new2 < dist_old) {
              tour_new <- c(tour_new, pluck(cluster_begin_new, 1,"cluster"))}
            
            ##mit Wildcard 
            
            if (str_detect(string = pluck(cluster_begin_old,1,"cluster_begin"), "X") == TRUE & dist_old < dist_new2) {
              
              ind <- which(dist_wildcard > dist_new) [1]
              ind <- ind_wildcards_tour[ind]
              
              #Neue Wildcard ist schlechter als die bisherigen
              if (is.na(ind) == TRUE) {
                cluster <- pluck(cluster_begin_old,1,"cluster")
                perm <- fix_wildcard(var1 = cluster[1], var2 = cluster[2], ind_wildcard = 1)
                cluster[1] <- perm$perm 
                tour_new <- c(tour_new, cluster)
              }
              
              #neue Wildcard ist besser
              if (is.na(ind) == FALSE)  {
                
                wildcard_old <-  fix_wildcard(var1 = tour_new[ind], var2 = tour_new[ind +1], ind_wildcard = 1)
                tour_new[ind] <- wildcard_old
                
                tour_new <- c(tour_new, pluck(cluster_begin_old,1,"cluster"))
              }
            }
            if (str_detect(string = pluck(cluster_begin_new,1,"cluster_begin"), "X") == TRUE & dist_new2 < dist_old) {
              ind <- which(dist_wildcard > dist_new) [1]
              ind <- ind_wildcards_tour[ind]
              
              #Neue Wildcard ist schlechter als die bisherigen
              if (is.na(ind) == TRUE) {
                cluster <- pluck(cluster_begin_new,1,"cluster")
                perm <- fix_wildcard(var1 = cluster[1], var2 = cluster[2], ind_wildcard = 1)
                cluster[1] <- perm$perm 
                tour_new <- c(tour_new, cluster)
              }
              
              #neue Wildcard ist besser
              if (is.na(ind) == FALSE)  {
                
                wildcard_old <-  fix_wildcard(var1 = tour_new[ind], var2 = tour_new[ind +1], ind_wildcard = 1)
                tour_new[ind] <- wildcard_old
                
                tour_new <- c(tour_new, pluck(cluster_begin_new,1,"cluster"))
              }
            }
      }
      
      if (all(wildcards_solution_old %in% tour_new)) {
        
        dist_old <- sum(imap_dbl(wildcards_solution_old, ~combin_distance(var1 = .x, var2 = wildcards_solution_old[.y +1])))
        dist_new <- sum(imap_dbl(tour_new, ~combin_distance(var1 = .x, var2 = tour_new[.y +1])))
        
        if (dist_new <= dist_old) {
          wildcards_solution_old <- tour_new
          ind_solution <- 1
          break}
        
        if (dist_old < dist_new) {
          ind_solution <- ind_solution +1
          break}
      }
    }
      
      ###
      #Check
      
      print(laufvar)
      print(any(duplicated(tour_new)))
      print(akt_tour_end)
      
      
      laufvar <- laufvar + 1
      anz_wildcards <- str_which(tour_new, "X")
      if (length(anz_wildcards) != 0) {
        
        tour_x <- tour_new
        tour_x[anz_wildcards] <- map_chr(anz_wildcards, ~{fix_wildcard(var1 = tour_new [.x] ,var2 = tour_new [.x +1],ind_wildcard = 1) %>% 
            pluck(., "perm")})
        
      } else {tour_x <- tour_new}
      
      if (all(solution %in% tour_x)) {break}
  }
  
    if (ind_solution == times) {break}
  }
  return(wildcards_solution_old)
}

#fix_wildcard(var1 = "X654327", var2 = "7543276", ind_wildcard = 1)
