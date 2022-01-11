  #################################################x
  #' Project:
  #' Script purpose: Test verschiedener Varianten 
  #' Wed Jan 05 22:39:53 2022
  #' Author: Manuel Wick-Eckl 
  #################################################x
  options(stringsAsFactors = FALSE)
  
  library(TSP)
  library(tidyverse)
  
  source("01_R/custom_functions.R")
  #####Prep solution----
  #
  here::set_here()
  load(here::here("02_Data/wildcard_postprocessing.Rdata"))
  
  
  # Test der verschiedenen TSP Methoden-----
  
  matrix <- cluster_matrix$solution1$matrix[[1]]
  perm_begin <- map_chr(santa_cluster$solution1, ~ pluck(.x, "cluster") [1])
  ind <- map_dbl(santa_cluster$solution1, ~ pluck(.x, "ind_from") -1)
  perm_ende <- solution$solution_permutation[[1]][ind]
  
  ind <- which(solution$solution_permutationen$solution1 %in% c(perm_begin, perm_ende))
  cluster_tour <- solution$solution_permutationen$solution1[ind]
    
  cluster_tour %in% perm_ende
  
  cluster_distance <-   imap_dbl(cluster_tour, ~ {
    ifelse((.x %in% perm_ende) & (cluster_tour[.y +1] %in% perm_begin), 
           combin_distance(var1 = .x, var2 = cluster_tour[.y +1]), 0)
  })
  
  sum(cluster_distance)
  
  
  ###
  tour_data <- generate_santa_tour(dat = matrix)
  tour <- solve_TSP(x = tour_data, method = "cheapest_insertion" , as_TSP = TRUE,  two_opt = TRUE)
  perm_begin_cluster <- c(perm_begin, names(tour)[str_which(string = names(tour), pattern = "X")])
  
  names(tour) %in% perm_ende
  imap_dbl(names(tour), ~combin_distance(var1 = .x, var2 = names(tour)[.y +1]))
  names(tour)
  
  distance <-   imap_dbl(names(tour), ~ {
    ifelse((.x %in% perm_ende) & (names(tour)[.y +1] %in% perm_begin_cluster), 
           combin_distance(var1 = .x, var2 = names(tour)[.y +1]), 0)
  })
  
  
  
  repeat{
    
    tour <- solve_TSP(x = tour_data, method = "cheapest_insertion" , as_TSP = TRUE,  two_opt = TRUE)
    perm_begin_cluster <- c(perm_begin, names(tour)[str_which(string = names(tour), pattern = "X")])
    
    ind_verwendbare_cluster <-  imap(names(tour), ~ {
      ifelse((.x %in% perm_ende) & (names(tour)[.y +1] %in% perm_begin_cluster), 
             which(.x == names(tour)), NA)}) %>% 
      purrr::discard(~is.na(.x)) %>% 
      flatten_dbl()
    
    tour_new <- NULL
    
    ind <- 
      cluster <- pluck(santa_cluster$solution1, ii_cluster)
      cluster_perm <- cluster$cluster
    
  }
  
  
  #####
  