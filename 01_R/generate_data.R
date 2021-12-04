#################################################x
#' Project: Santa 2021
#' Script purpose: Skript mit welchem die Daten für die spätere optimierung generiert werden
#' Sat Dec 04 08:18:51 2021
#' Author: Manuel Wick-Eckl 
#################################################x
options(stringsAsFactors = FALSE)

library(tidyverse)
library(combinat)
library(furrr)

source(here::here("01_R/custom_functions.R"))

generate_tibble <- function(combin_list) {
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

###generate Data----

perm <- combinat::permn(x = c("1","2","3","4","5","6","7"))
perm_str <- map(perm, ~ stringr::str_c(.x, collapse = ""))

#### 🎅🤶 Dataset  ----

perm_1_2 <- keep(perm_str, ~str_starts(string = .x, pattern = "12"))
perm_1_2 <- unlist(perm_1_2)
perm_1_2_combinat <- combinat::combn(x = perm_1_2, m = 2, simplify = FALSE)

dat_santa_1_2 <- generate_tibble(perm_1_2_combinat)
dat_santa_1_2$dataset <- "Santa_1_2"

#### ! 🎅🤶 Dataset  ----

perm_rest <- keep(perm_str, ~str_starts(string = .x, pattern = "12", negate = TRUE))
perm_rest <- unlist(perm_rest)
perm_rest_combinat <- combinat::combn(x = perm_rest, m = 2, simplify = FALSE)

dat_santa_rest <- generate_tibble(perm_rest_combinat)
dat_santa_rest$dataset <- "Santa_rest"

plan(multisession, workers = future::availableCores())

dat_santa_rest$distance_perm_combin <- furrr::future_map_dbl(
  perm_rest_combinat, ~ combin_distance(.x[1], .x[2])
  )

dat_santa_rest$distance_combin_perm <- furrr::future_map_dbl(
  perm_rest_combinat, ~ combin_distance(.x[2], .x[1])
)


dat_santa_rest_1 <- dat_santa_rest %>% 
  select(dataset, permutation, combination, distance_perm_combin)

dat_santa_rest_2 <- dat_santa_rest %>% 
  select(dataset)

dat_santa_rest_2$permutation <- dat_santa_rest$combination
dat_santa_rest_2$combination <- dat_santa_rest$permutation
dat_santa_rest_2$distance_perm_combin <- dat_santa_rest$distance_combin_perm

dat_santa_rest <- rbind(dat_santa_rest_1, dat_santa_rest_2)
dat_santa_rest <- distinct(dat_santa_rest, permutation, combination, .keep_all = TRUE) %>% 
  rename(., distance = "distance_perm_combin",
         permuatation = "")

dat_santa_rest_small <- generate_dist_matrix(dat_santa_rest[which(dat_santa_rest$distance != 7), ])
dat_santa_rest_large <- generate_dist_matrix(dat_santa_rest)

### Daten abspeichern ----

permutationen <- list("santa_1_2_perm" = perm_1_2, 
                       "santa_rest_perm" = perm_rest)

santa_matrix <- list("santa_small" = dat_santa_rest_small, 
                     "santa_large" = dat_santa_rest_large)

write_rds(permutationen, here::here("02_Data/permutationen.rds"))
write_rds(santa_matrix, here::here("02_Data/santa_matrix.rds"))

zip(zipfile = here::here("02_Data/santa_matrix.zip"), files = here::here("02_Data/santa_matrix.rds"))
