
library(TSP)
library(tidyverse)
library(furrr)
library(doParallel)
registerDoParallel()

concorde_path(here::here("tsp_solver/"))
source(here::here("01_R/custom_functions.R"))

###-Load Data----
unzip(zipfile = here::here("02_Data/santa_matrix_teil2.zip"), overwrite = TRUE, exdir = here::here("02_Data/"))
permutationen <- readRDS(here::here("02_Data/permutationen.rds"))
santa_matrix <- readRDS(here::here("02_Data/02_Data/santa_matrix_teil2.rds"))

unzip(zipfile = here::here("02_Data/santa_matrix.zip"), overwrite = TRUE, exdir = here::here("02_Data/"))
santa_matrix2 <- readRDS(here::here("02_Data/Users/ick/Documents/projekte_privat/Santa2021/02_Data/santa_matrix.rds"))

santa_matrix <- c(santa_matrix, santa_matrix2)
###-generate santa_matrix_full ----
perm_all <- unname(sort(unlist(permutationen)))
santa_matrix_full <- matrix(NA, nrow = length(perm_all), ncol = length(perm_all),
                            dimnames = list(perm_all, perm_all))

# suche die heraus, die wir schon haben
for(ll in length(santa_matrix)){# ll <- 1
  sm <- santa_matrix[[ll]]
  sm <- as.data.frame(sm)
  row.names(sm) <- sm$permutation
  sm <- sm[,sm$permutation]
  sm <- as.matrix(sm)
  santa_matrix_full[cbind(rep(match(row.names(sm), row.names(santa_matrix_full)), ncol(sm)),
                          rep(match(colnames(sm), colnames(santa_matrix_full)), each = nrow(sm)))] <- c(sm)
}

table(c(santa_matrix_full), useNA = "a")
diag(santa_matrix_full) <- 0
santa_matrix_full[which(santa_matrix_full == 7)] <- 7
table(c(santa_matrix_full), useNA = "a")

# weitere hinzufügen
ind <- which(is.na(santa_matrix_full), arr.ind = TRUE)
for(ii in 1:nrow(ind)){
  santa_matrix_full[ind[ii,,drop = FALSE]] <- 
    combin_distance(row.names(santa_matrix_full)[ind[ii,"row"]], 
                    colnames(santa_matrix_full)[ind[ii,"col"]])
}

diag(santa_matrix_full) <- 0
santa_matrix_full[which(santa_matrix_full == 7)] <- 7
table(c(santa_matrix_full), useNA = "a")

# als symmetrische matrix speichern
atsp <- ATSP(santa_matrix_full)
tsp <- reformulate_ATSP_as_TSP(atsp)

#----
write_rds(atsp, here::here("02_Data/atsp_full.rds"))
write_rds(atsp, here::here("02_Data/tsp_full.rds"))

zip(zipfile = here::here("02_Data/atsp_full.zip"), files = here::here("02_Data/atsp_full.rds"))
zip(zipfile = here::here("02_Data/tsp_full.zip"), files = here::here("02_Data/tsp_full.rds"))
