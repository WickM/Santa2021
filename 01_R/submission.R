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
#####Daten----

santa_solution <- readRDS("02_Data/tsp_tour_teil2_0712.rds")
permutationen <- readRDS("02_Data/permutationen.rds")

#solution <- santa_submission(santa_solution, permutationen)
solution <- santa_solution
# Step 1 Check alle Permutationen enthalten alle santa permutationen enthalten
solution_names <- c(names(solution[[1]]), names(solution[[2]]), names(solution[[3]]))

#Step2 Strings zusammenlegen
solution <- list ("solution1" = names(solution[[1]]),
               "solution2" = names(solution[[2]]),
               "solution3" = names(solution[[3]]))

  string <- paste(solution$solution1, solution$solution2, solution$solution3, collapse = "")
  all(map_lgl(permutationen$santa_1_2_perm_rep, ~ str_detect(string = string, pattern = .x)))
  all(map_lgl(permutationen$santa_rest_perm_rep, ~ str_detect(string = string, pattern = .x)))
  
  solution <- map(dat, ~ cut_permutation(.x))
  
  string <- paste(solution$solution1, solution$solution2, solution$solution3, collapse = "")
  all(map_lgl(permutationen$santa_1_2_perm_rep, ~ str_detect(string = string, pattern = .x)))
  all(map_lgl(permutationen$santa_rest_perm_rep, ~ str_detect(string = string, pattern = .x)))
  
  
  #Step3 umbenennen
  # 🎅, 🤶, 🦌, 🧝, 🎄, 🎁, 🎀
  
  solution <- map(solution, ~ {
    temp <- str_replace_all(.x,   "1", "🎅")
    temp <- str_replace_all(temp, "2", "🤶")
    temp <- str_replace_all(temp, "3", "🦌")
    temp <- str_replace_all(temp, "4", "🧝")
    temp <- str_replace_all(temp, "5", "🎄")
    temp <- str_replace_all(temp, "6", "🎁")
    temp <- str_replace_all(temp, "7", "🎀")
    
    return(temp)
  })

  permutationen <- rlist::list.append(permutationen, 
                                      "santa_1_2_perm_rep" = map(permutationen$santa_1_2_perm, ~ {
                                        temp <- str_replace_all(.x,   "1", "🎅")
                                        temp <- str_replace_all(temp, "2", "🤶")
                                        temp <- str_replace_all(temp, "3", "🦌")
                                        temp <- str_replace_all(temp, "4", "🧝")
                                        temp <- str_replace_all(temp, "5", "🎄")
                                        temp <- str_replace_all(temp, "6", "🎁")
                                        temp <- str_replace_all(temp, "7", "🎀")
                                        
                                        return(temp)
                                      }),
                                      "santa_rest_perm_rep" = map(permutationen$santa_rest_perm, ~ {
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
  
  string <- paste(solution$solution1, solution$solution2, solution$solution3, collapse = "")
  all(map_lgl(permutationen$santa_1_2_perm_rep, ~ str_detect(string = string, pattern = .x)))
  all(map_lgl(permutationen$santa_rest_perm_rep, ~ str_detect(string = string, pattern = .x)))
  
  missing <- map_lgl(permutationen$santa_rest_perm_rep, ~ str_detect(string = string, pattern = .x))
  which(missing == FALSE)
  
  permutationen$santa_rest_perm_rep[484]
  permutationen$santa_rest_perm[484]
  
  solution$solution1 <- paste (solution$solution1, permutationen$santa_rest_perm_rep[484], collapse = "") 
  
  string <- paste(solution$solution1, solution$solution2, solution$solution3, collapse = "")
  all(map_lgl(permutationen$santa_1_2_perm_rep, ~ str_detect(string = string, pattern = .x)))
  all(map_lgl(permutationen$santa_rest_perm_rep, ~ str_detect(string = string, pattern = .x)))
  
  submission <- read_csv("02_Data/sample_submission.csv")

  submission$schedule[1] <- paste(utf8::utf8_print(solution$solution1,quote = FALSE),collapse= "")
  submission$schedule[2] <- paste(utf8::utf8_print(solution$solution2,quote = FALSE),collapse= "")
  submission$schedule[3] <- paste(utf8::utf8_print(solution$solution3,quote = FALSE),collapse= "")
  
  write_csv(submission, "03_submission/santa_submission_1112.csv", quote = c("none"))
