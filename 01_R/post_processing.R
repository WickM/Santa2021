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

cluster_identifizieren <- function(solution_permutation, solution_distance, santa_1_2_perm) {
  ind <- which(solution_distance == 6)
  
  cluster <- imap(ind[seq_along(ind)-1], ~ {
    ind_x <- .x+1
    ind_y <- ind[.y+1]
                    cluster <- solution_permutation[seq(ind_x, ind_y)]
                    cluster_distance <- solution_distance[seq(ind_x, ind_y)]
                    cluster_length <- length(cluster_distance)
                    santa1_2_perm <- any(santa_1_2_perm %in% cluster)
                    ind_from = ind_x
                    ind_to = ind_y
                    
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
                      ~ cluster_identifizieren(solution_permutation = .x, 
                                               solution_distance = .y, 
                                               santa_1_2_perm = permutationen$permutationen$santa_1_2_perm)
                      )

#Wildcards versuch 1
#Wildcard in jedem String einfügen und den optimalen platz suchen ohne das man die cluster bewegt
# mittels einfachen Hill climbing 

#Fügt in jedem String bei einer Permutation mit Distance > 6 2 Wildcards ein
wildcards_permutation <- function(wildcard_solution) {
  
  solution_permutation <- wildcard_solution$solution_permutation
  solution_distance <- wildcard_solution$solution_distance 
  perm_old_ind <- wildcard_solution$perm_old_ind
  perm_old <- wildcard_solution$perm_old
  
  #+1 Da ich immer die 2. Permutation mit Wildcards versehen will 
  distance <-  which(solution_distance >= 6) +1 
  ind_perm <- sample(distance, 2, replace = FALSE)
  
  #remove old WIldcars
  solution_permutation[perm_old_ind] <- perm_old
  
  perm_old <- solution_permutation[ind_perm]
  perm_old_ind <- ind_perm
  
  # place Wildecards minimise distance 
  for (ii in ind_perm) {
    
    wildcard_distance <- tibble ("var_n" = seq (1,6), 
                                 "var1" =  solution_permutation[ii -1], 
                                 "var2" = solution_permutation[ii])
    
    wildcard_distance$var2 <- map_chr(seq (1,6), ~ {str_replace(solution_permutation[ii], 
                                                                pattern = str_sub(solution_permutation[ii], start = .x, end = .x), 
                                                                replacement = "X")
      })
    
    wildcard_distance$distance <- map2_dbl(.x = wildcard_distance$var1, .y = wildcard_distance$var2, ~ combin_distance(var1 = .x, var2 = .y) )
    
    solution_permutation[ii] <- wildcard_distance$var2 [which.min(wildcard_distance$distance)]
    solution_distance[ii-1] <- wildcard_distance$distance [which.min(wildcard_distance$distance)]
    
  }
 
  return(list ("solution_permutation" = solution_permutation, 
               "solution_distance" = solution_distance, 
               "perm_old" = perm_old, 
               "perm_old_ind" = perm_old_ind,
               "run" = 1))
} 

start_solution <- function(solution) {
  ind <- seq(1,3)
  
  temp_solution <- map(ind, ~ {
    
    solution_permutation <- pluck(solution, "solution_permutationen", .x)
    solution_distance <- pluck(solution, "solution_distance", .x)
    perm_old <- solution_permutation[1:2]
    perm_old_ind <- c(1,2)
    
    return(list ("solution_permutation" = solution_permutation, 
                 "solution_distance" = solution_distance, 
                 "perm_old" = perm_old, 
                 "perm_old_ind" = perm_old_ind,
                 "run" = 1))
  })
  
  return(temp_solution)
  
}

wildcard_check <- function(wildcards_solution_old, wildcards_solution_jung) {
  
  if (sum(wildcards_solution_jung$solution_distance) < sum(wildcards_solution_old$solution_distance) ) {
    return(return(list ("solution_permutation" = wildcards_solution_jung$solution_permutation, 
                        "solution_distance" = wildcards_solution_jung$solution_distance, 
                        "perm_old" = wildcards_solution_jung$perm_old, 
                        "perm_old_ind" = wildcards_solution_jung$perm_old_ind,
                        "run" = 1)))
  } else {return(list ("solution_permutation" = wildcards_solution_old$solution_permutation, 
                       "solution_distance" = wildcards_solution_old$solution_distance, 
                       "perm_old" = wildcards_solution_old$perm_old, 
                       "perm_old_ind" = wildcards_solution_old$perm_old_ind,
                       "run" = wildcards_solution_old$run +1))
  }
  
}

wildcards_solution_old <- start_solution(solution)

run <- 0
# repeat {
# 
#   wildcards_solution_jung <- map(wildcards_solution_old, ~ wildcards_permutation(.x))
# 
#   wildcards_solution_old <- map2(
#     .x = wildcards_solution_old,
#     .y = wildcards_solution_jung, ~ wildcard_check(wildcards_solution_old = .x ,
#                                                    wildcards_solution_jung = .y )
#     )
# 
#   print (map(wildcards_solution_jung, ~sum(.x$solution_distance)))
# 
#   run <- run +1
# 
#   if (run == 10000) {break}
# }

## Bringt nichts da schon in einem lakalem minimum sobald abstand 6 ist hilft die Wildcard nichts mehr  

# Idee 2 Wildcard an jeder position ausprobieren


wildcard_check2 <- function(solution_permutationen, solution_distance) {
  
  optimierung <- map_dbl (.x = seq(1, length(solution_permutationen) -1), ~ {
    ii <- .x
    
    wildcard_distance <- tibble ("var_n" = seq (1,6), 
                                 "var1" =  solution_permutationen[ii], 
                                 "var2" = solution_permutationen[ii+1])
    
    wildcard_distance$var2 <- map_chr(seq (1,6), ~ {str_replace(solution_permutationen[ii+1], 
                                                                pattern = str_sub(solution_permutationen[ii+1], start = .x, end = .x), 
                                                                replacement = "X")
    })
    
    wildcard_distance$distance <- map2_dbl(.x = wildcard_distance$var1, .y = wildcard_distance$var2, ~ combin_distance(var1 = .x, var2 = .y) )
    
    optimierung <-  wildcard_distance$distance [which.min(wildcard_distance$distance)] - solution_distance[ii]
    
    return (optimierung)
  })
  
}


optimierung <- map2(solution$solution_permutationen, solution$solution_distance, ~ wildcard_check2(solution_permutationen = .x, solution_distance = .y))


map(optimierung, ~all(.x == 0))

# Bringt auch nichts 