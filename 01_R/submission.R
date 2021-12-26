#################################################x
#' Project:Santa 2021
#' Script purpose:Skript mit welchem die Tour in drei Teile geteilt für eine erneutes training vorgereitet werden
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

santa_solution <- readRDS("02_Data/tsp_tour_teile_1912.rds")
permutationen <- readRDS("02_Data/permutationen.rds")

# permutationen <- list(
#   "permutationen" = list (
#     "santa_1_2_perm" = permutationen$santa_1_2_perm, 
#     "santa_rest_perm" = permutationen$santa_rest_perm
#     )
# )
# 
# permutationen <- rlist::list.append(permutationen, 
#                                     "permutation_renamed" = map(permutationen$permutationen, ~ {
#                                       temp <- str_replace_all(.x,   "1", "🎅")
#                                       temp <- str_replace_all(temp, "2", "🤶")
#                                       temp <- str_replace_all(temp, "3", "🦌")
#                                       temp <- str_replace_all(temp, "4", "🧝")
#                                       temp <- str_replace_all(temp, "5", "🎄")
#                                       temp <- str_replace_all(temp, "6", "🎁")
#                                       temp <- str_replace_all(temp, "7", "🎀")
#                                       
#                                       return(temp)
#                                     })
#                                     )
# 
# write_rds(permutationen, file = "02_Data/permutationen.rds")



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

####Post_Processing----

####Submission ----
#Step umbenennen
# 🎅, 🤶, 🦌, 🧝, 🎄, 🎁, 🎀

solution <- rlist::list.append(
  solution, "solution_renamed" = map(solution$solution_cut, ~ {
    temp <- str_replace_all(.x,   "1", "🎅")
    temp <- str_replace_all(temp, "2", "🤶")
    temp <- str_replace_all(temp, "3", "🦌")
    temp <- str_replace_all(temp, "4", "🧝")
    temp <- str_replace_all(temp, "5", "🎄")
    temp <- str_replace_all(temp, "6", "🎁")
    temp <- str_replace_all(temp, "7", "🎀")
    
    return(temp)
  })
)
check(solution_list = solution$solution_renamed, permutation_list = permutationen$permutation_renamed)

if (check(solution_list = solution$solution_renamed, permutation_list = permutationen$permutation_renamed) == FALSE) {
  date <- Sys.Date()
  ####
  submission <- read_csv("02_Data/sample_submission.csv")
  
  submission$schedule[1] <- paste(utf8::utf8_print(solution$solution_renamed$solution1,quote = FALSE),collapse= "")
  submission$schedule[2] <- paste(utf8::utf8_print(solution$solution_renamed$solution2,quote = FALSE),collapse= "")
  submission$schedule[3] <- paste(utf8::utf8_print(solution$solution_renamed$solution3,quote = FALSE),collapse= "")
  
  write_csv(submission, glue("03_submission/santa_submission_{date}.csv"), quote = c("none"))
  
}
