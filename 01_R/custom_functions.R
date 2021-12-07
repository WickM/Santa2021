#################################################x
#' Project: santa 2021
#' Script purpose: Custom Functions 
#' Fri Dec 03 22:35:49 2021
#' Author: Manuel Wick-Eckl 
#################################################x
options(stringsAsFactors = FALSE)


#####


#' Funktion for calculation permutation distance
#' @param var1 Fist character string
#' 
#' @param var2 Second character string
#' 
#' @return character Distance as list  
#' distance_1_2 = Distance form String1 to String2
#' distance_2_1 = Distance form String2 to String1
combin_distance <- function(var1, var2) {
  ind <- nchar(var1)
  repeat {
    ind <- ind -1
    match <- ifelse (stringr::str_sub(var1, -ind) == stringr::str_sub(var2, 1, ind), TRUE, FALSE)
    if (match == TRUE | ind == 0) {break}
  }
  comb_dist <- 7- ind
  
  return(comb_dist)
}

generate_tibble <- function(combin_list) {
  require(tibble)
  require(purrr)
  dat <- tibble("permutation" = map_chr(combin_list, ~ pluck(.x, 1)), 
                "combination" = map_chr(combin_list, ~ pluck(.x, 2)) )
  
  return(dat)
}

generate_dist_matrix <- function(dat) {
  dat <- tidyr::pivot_wider(dat, 
                            id_cols = c("dataset", "permutation"), 
                            names_from = combination, 
                            values_from = distance)
  return(dat)
}

generate_dist_matrix_teil2 <- function(permutation_vector) {
  require(combinat)
  require(furrr)
  
  combinat <- combinat::combn(x = permutation_vector, m = 2, simplify = FALSE)
  dat <- generate_tibble(combinat)
  
  plan(multisession, workers = future::availableCores())
  dat$distance_perm_combin <- furrr::future_map_dbl(
    combinat, ~ combin_distance(.x[1], .x[2])
  )
  
  dat$distance_combin_perm <- furrr::future_map_dbl(
    combinat, ~ combin_distance(.x[2], .x[1])
  )
  
  dat_1 <- dat %>% 
    select(permutation, combination, distance_perm_combin)
  
  dat_2 <- dat 
  dat_2$permutation <- dat$combination
  dat_2$combination <- dat$permutation
  dat_2$distance_perm_combin <- dat$distance_combin_perm
  dat_2 <- dat_2 %>% 
    select(- distance_combin_perm)
  
  dat <- rbind(dat_1, dat_2)
  dat <- distinct(dat, permutation, combination, .keep_all = TRUE) %>% 
    rename(., distance = "distance_perm_combin")
  
  dat$dataset <- "dataset_part"
  
  dat <- generate_dist_matrix(dat = dat)
  
  return(dat)
}

