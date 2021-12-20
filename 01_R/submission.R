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

santa_solution <- readRDS("02_Data/tsp_tour_teile_1912.rds")
permutationen <- readRDS("02_Data/permutationen.rds")

#solution <- santa_submission(santa_solution, permutationen)
solution <- santa_solution
# Step 1 Check alle Permutationen enthalten alle santa permutationen enthalten
solution_names <- c(names(solution[[1]]), names(solution[[2]]), names(solution[[3]]))

#Step2 Strings zusammenlegen
solution <- list ("solution1" = names(solution[[1]]),
               "solution2" = names(solution[[2]]),
               "solution3" = names(solution[[3]]))


  all(permutationen$santa_1_2_perm %in% solution$solution1)
  all(permutationen$santa_1_2_perm %in% solution$solution2)
  all(permutationen$santa_1_2_perm %in% solution$solution3)
  
  all(permutationen$santa_rest_perm %in% c(solution$solution3, solution$solution2, solution$solution1))
  
  solution <- map(solution, ~ cut_permutation(.x))
  
  string <- paste(solution$solution1, solution$solution2, solution$solution3, collapse = "")
  all(map_lgl(permutationen$santa_1_2_perm, ~ str_detect(string = solution$solution1, pattern = .x)))
  all(map_lgl(permutationen$santa_1_2_perm, ~ str_detect(string = solution$solution2, pattern = .x)))
  all(map_lgl(permutationen$santa_1_2_perm, ~ str_detect(string = solution$solution3, pattern = .x)))
  
  all(map_lgl(permutationen$santa_rest_perm, ~ str_detect(string = string, pattern = .x)))

  
  nchar(solution$solution1)
  
all(map_lgl(permutationen$santa_1_2_perm_rep, ~ str_detect(string = solution$solution1, pattern = .x)))
all(map_lgl(permutationen$santa_1_2_perm_rep, ~ str_detect(string = solution$solution2, pattern = .x)))
all(map_lgl(permutationen$santa_1_2_perm_rep, ~ str_detect(string = solution$solution3, pattern = .x)))



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
  
  all(map_lgl(permutationen$santa_rest_perm_rep, ~ str_detect(string = string, pattern = .x)))
  
  all(map_lgl(permutationen$santa_1_2_perm_rep, ~ str_detect(string = solution$solution1, pattern = .x)))
  all(map_lgl(permutationen$santa_1_2_perm_rep, ~ str_detect(string = solution$solution2, pattern = .x)))
  all(map_lgl(permutationen$santa_1_2_perm_rep, ~ str_detect(string = solution$solution3, pattern = .x)))
  
  submission <- read_csv("02_Data/sample_submission.csv")

  submission$schedule[1] <- paste(utf8::utf8_print(solution$solution1,quote = FALSE),collapse= "")
  submission$schedule[2] <- paste(utf8::utf8_print(solution$solution2,quote = FALSE),collapse= "")
  submission$schedule[3] <- paste(utf8::utf8_print(solution$solution3,quote = FALSE),collapse= "")
  
  write_csv(submission, "03_submission/santa_submission_2012.csv", quote = c("none"))
