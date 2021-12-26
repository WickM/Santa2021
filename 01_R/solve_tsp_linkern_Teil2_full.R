
library(TSP)
library(tidyverse)

library(doParallel)
registerDoParallel()

concorde_path(here::here("tsp_solver/"))
atsp_all <- readRDS(here::here("02_Data/atsp_full.rds"))
# tsp_all <- readRDS(here::here("02_Data/tsp_full.rds"))

ind <- grep("^12", row.names(atsp_all), invert = TRUE)
atsp <- ATSP(atsp_all[ind, ind])

clo <- paste0(c("-K 1",
                "-s 1337",
                paste0("-S ", here::here("tsp_solver", "linkern.tour")),
                "-t 172800",
                "-R 999999999"), collapse = " ")

tour <- solve_TSP(atsp, method = "linkern", as_TSP = TRUE,
                   control = list(clo = clo))
