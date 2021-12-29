library(TSP)
library(tidyverse)

library(doParallel)
registerDoParallel()

concorde_path(here::here("tsp_solver/"))

add.lead <- function (x, width = max(nchar(x))) {
  sprintf(paste("%0", width, "i", sep = ""), 
          x)
}
diff_time <- function(t2, t1){
  dt <- as.numeric(round(difftime(t2, t1, units = "sec"), 1))
  paste0(add.lead(dt %/% 3600, 2), ":", add.lead((dt %% 3600) %/% 60, 2), ":",
         add.lead(floor(dt %% 60), 2), ".", 
         gsub("0.", "", sprintf("%2.1f", round(dt%%60 - floor(dt %% 60), 1)), fixed = TRUE))
}

###-Load Data----
unzip(here::here("02_Data/atsp_full.zip"))
santa_tour <- readRDS(here::here("02_Data/tsp_tour_teile_2512-custom-start-solution.rds"))
santa_matrix_full <- readRDS(here::here(list.files(here::here(), "atsp_full.rds", recursive = TRUE)))

###
tour_lengths <- function(x, cyclic = FALSE){# x <- santa_tour[[1]]
  ind <- match(names(x), row.names(santa_matrix_full))
  if(cyclic){
    setNames(santa_matrix_full[cbind(ind, c(ind[-1], ind[1]))], 
             row.names(santa_matrix_full)[c(ind[-1], ind[1])])
    
  } else {
    setNames(santa_matrix_full[cbind(ind[-length(ind)], ind[-1])],
             row.names(santa_matrix_full)[ind[-1]])
  }
}

gen_tour_string <- function(tour){
  ind <- match(names(tour), row.names(santa_matrix_full))
  santa_matrix_full[ind]
}

lapply(santa_tour, TSP::tour_length)
lapply(santa_tour, tour_lengths, cyclic = TRUE) %>% lapply(., sum)

###
perturbate <- function(tours){# tours <- santa_tour
  
  tl <- lapply(tours, tour_lengths, cyclic = TRUE)
  take <- lapply(tl, sum) %>% unlist
  ind_from <- which.max(take)
  ind_to <- which.min(take)
  
  # head(tours[[ind_from]], 12)
  # head(tl[[ind_from]], 12)
  # diff(which(tl[[ind_from]] == 6))
  
  # if seventh dist is a 6 then permutation from seven to eight is of dist 6.
  # hence if i-th and j-th dist is a 6 then cut out permutations i+1 to j.
  
  # cut and move a cluster such that maximal cluster length is halve of the 
  # difference of the longest and the shortest subtour
  max_cl <- abs((sum(tl[[ind_to]]) - sum(tl[[ind_from]])) / 2)
  cluster_lengths <- paste0(tl[[ind_from]], collapse = "") %>% strsplit(., "6") %>% 
    unlist %>% sapply(., function(x) sum(as.numeric(unlist(strsplit(x, ""))))) %>% 
    unname
  use_size <- sample(cluster_lengths[cluster_lengths>0&cluster_lengths<=max_cl], 1)
  use_cluster <- sample(which(cluster_lengths == use_size), 1)
  ind_cluster6 <- which(tl[[ind_from]] == 6)[use_cluster]
  
  if(use_cluster == 1) ind_perms <- seq(1, ind_cluster6)
  if(use_cluster == length(cluster_lengths)) ind_perms <- seq(ind_cluster6+1, length(tours[[ind_from]]))
  if(use_cluster > 1 & use_cluster < length(cluster_lengths)) {
    ind_cluster6_low <- which(tl[[ind_from]] == 6)[use_cluster - 1]
    ind_perms <- seq(ind_cluster6_low + 1, ind_cluster6)
  }
  
  perms <- tours[[ind_from]][ind_perms]
  substrs <- substr(names(perms), 1, 2)
  if(all(substrs == "12")){
    "break"
  } else {
    perms <- perms[substrs != "12"]
    return(list("perms" = perms, "from" = ind_from, "to" = ind_to))
  }
}
perturbate.v2 <- function(tours){# tours <- santa_tour
  
  tl <- lapply(tours, tour_lengths, cyclic = TRUE)
  take <- lapply(tl, sum) %>% unlist
  ind_from <- which.max(take)
  ind_to <- which.min(take)
  
  # head(tours[[ind_from]], 12)
  # head(tl[[ind_from]], 12)
  # diff(which(tl[[ind_from]] == 6))
  
  # if seventh dist is a 6 then permutation from seven to eight is of dist 6.
  # hence if i-th and j-th dist is a 6 then cut out permutations i+1 to j.
  
  # cut and move a cluster such that maximal cluster length is halve of the 
  # difference of the longest and the shortest subtour
  max_cl <- abs((sum(tl[[ind_to]]) - sum(tl[[ind_from]])) / 2)
  cluster_lengths <- paste0(tl[[ind_from]], collapse = "") %>% strsplit(., "6") %>% 
    unlist %>% sapply(., function(x) sum(as.numeric(unlist(strsplit(x, ""))))) %>% 
    unname
  use_size <- sample(cluster_lengths[cluster_lengths>0&cluster_lengths<=max_cl], 1)
  use_cluster <- sample(which(cluster_lengths == use_size), 1)
  ind_cluster6 <- which(tl[[ind_from]] == 6)[use_cluster]
  
  if(use_cluster == 1) ind_perms <- seq(1, ind_cluster6)
  if(use_cluster == length(cluster_lengths)) ind_perms <- seq(ind_cluster6+1, length(tours[[ind_from]]))
  if(use_cluster > 1 & use_cluster < length(cluster_lengths)) {
    ind_cluster6_low <- which(tl[[ind_from]] == 6)[use_cluster - 1]
    ind_perms <- seq(ind_cluster6_low + 1, ind_cluster6)
  }
  
  perms <- tours[[ind_from]][ind_perms]
  substrs <- substr(names(perms), 1, 2)
  if(all(substrs == "12")){
    "break"
  } else {
    perms <- perms[substrs != "12"]
    return(list("perms" = names(perms), "from" = ind_from, "to" = ind_to))
  }
}

inc <- santa_tour
mx_inc <- lapply(inc, tour_lengths, cyclic = TRUE) %>% lapply(., sum) %>% unlist %>% max 
mxs <- mx_inc
breaks <- 0
clo <- paste0(c("-K 1",
                "-s 1337",
                #paste0("-S ", here::here("tsp_solver", "linkern.tour")),
                "-t 3600*2.5",
                "-R 1000000000"), collapse = " ")

start <- Sys.time()
cat("\rii = ", 0, "\tmax = ", mx_inc, "\tno mx = ", length(mxs), "\truntime = ", diff_time(Sys.time(), start))
for(ii in 1:1000){# ii <- 1
  
  pert <- perturbate(inc)
  if(identical(pert, "break")){
    breaks <- breaks+1
    cat("\rii = ", ii, "\tbreaks = ", breaks, "\truntime = ", diff_time(Sys.time(), start))
  } else {
    cat("\n")
    child <- inc
    ind1 <- match(sort(setdiff(attr(child[[pert$from]], "names"), pert$perms)),
                  row.names(santa_matrix_full))
    sm1 <- santa_matrix_full[ind1, ind1]
    sm1[sm1 == 7] <- Inf
    sm1 <- sm1[order(row.names(sm1)),][, sort(row.names(sm1))]
    atsp <- ATSP(sm1)
    child[[pert$from]] <-  solve_TSP(atsp, 
                                     method = "linkern", 
                                     as_TSP = TRUE, 
                                     control = list("clo" = clo), 
                                     verbose = FALSE
    )
    
    ind2 <- match(sort(union(attr(child[[pert$to]], "names"), pert$perms)),
                  row.names(santa_matrix_full))
    sm2 <- santa_matrix_full[ind2, ind2]
    sm2[sm2 == 7] <- Inf
    sm2 <- sm2[order(row.names(sm2)),][, sort(row.names(sm2))]
    atsp <- ATSP(sm2)
    child[[pert$to]] <-  solve_TSP(atsp, 
                                   method = "linkern", 
                                   as_TSP = TRUE, 
                                   control = list("clo" = clo), 
                                   verbose = FALSE)
   
    mx_child <- lapply(child, tour_lengths, cyclic = TRUE) %>% lapply(., sum) %>% unlist %>% max 
    mxs <- c(mxs, mx_child)
    if(mx_child < mx_inc) {
      inc <- child
      mx_inc <- mx_child
    }
    cat("\rii = ", ii, "\tmax = ", mx_child, "\tno mx = ", length(mxs), "\truntime = ", diff_time(Sys.time(), start))
  }
  
}
