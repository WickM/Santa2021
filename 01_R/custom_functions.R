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

#'
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
