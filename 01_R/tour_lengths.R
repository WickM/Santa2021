
santa_solution <- readRDS("02_Data/tsp_tour_teil2_0712.rds")
santa_matrix <- readRDS(here::here("02_Data/02_Data/santa_matrix_teil2.rds"))

lapply(1:3, function(ii){
  ind <- santa_solution[[ii]]
  sum(as.numeric(as.matrix(santa_matrix[[ii]][, -(1:2)])[
    cbind(ind[-length(ind)],
          ind[-1])]))
})

lapply(1:3, function(ii){
  ind <- santa_solution[[ii]]
  sum(as.numeric(as.matrix(santa_matrix[[ii]][, -(1:2)])[
    cbind(ind,
          c(ind[-1], ind[1]))
  ]))
})

lapply(santa_solution, function(ss) attr(ss, "tour_length"))
