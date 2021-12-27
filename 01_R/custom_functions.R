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
  
  if (is.na(var2) != TRUE) {
    ind <- nchar(var1)
    repeat {
      ind <- ind -1
      match <- ifelse (stringr::str_sub(var1, -ind) == stringr::str_sub(var2, 1, ind), TRUE, FALSE)
      if (match == TRUE | ind == 0) {break}
    }
    comb_dist <- 7- ind
    
    return(comb_dist)
  } else {return(0)}
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

cut_permutation <- function(solution) {
  
 string <- map_chr(seq(1, length(solution) -1), ~ {
    
    var1 <- solution[.x]
    var2 <- solution[.x + 1]
    
    dist <- combin_distance(var1 = var1, var2 = var2)
    
    cut_string <- stringr::str_sub(var1, 1 ,dist)
    return(cut_string)
    })
 
 string <-paste0(paste0(string, collapse = ""), solution[length(solution)], collapse = "")
 return(string)
}

santa_submission <- function(solution, permutationen) {
  
  
  # Step 1 Check alle Permutationen enthalten alle santa permutationen enthalten
  solution_names <- c(names(solution[[1]]), names(solution[[2]]), names(solution[[3]]))
  
  if(
    all(c(permutationen$santa_1_2_perm, permutationen$santa_rest_perm) %in% solution_names) &
    all(solution_names %in% c(permutationen$santa_1_2_perm, permutationen$santa_rest_perm)) &
    any(duplicated(solution_names [which(! solution_names %in% permutationen$santa_1_2_perm)])) == FALSE) {
    
    #Step2 Strings zusammenlegen
    dat <- list ("solution1" = names(solution[[1]]),
                 "solution2" = names(solution[[2]]),
                 "solution3" = names(solution[[3]]))
    
    solution <- map(dat, ~ cut_permutation(.x))
    
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
    
    return(solution)
    
  } else {cat("Submission File has an Error")}
  
}

santa_submission_submission_file <- function(solution, file) {
  #schedule 
  submission <- read_csv("02_Data/sample_submission.csv")
  submission$schedule[1] <- paste(utf8::utf8_print(solution$solution1,quote = FALSE),collapse= "")
  submission$schedule[2] <- paste(utf8::utf8_print(solution$solution2,quote = FALSE),collapse= "")
  submission$schedule[3] <- paste(utf8::utf8_print(solution$solution3,quote = FALSE),collapse= "")
  
  write_csv(submission,file = file)
  
}

#' Funktion mit welcher der Datensatz umgewandelt un dem solver übergeben wird
#' 
generate_santa_tour <- function(dat, method = "linkern", control = NULL, verbose =TRUE) {
  
  santa_tsp_data <- dat %>% 
    select(-dataset) %>% 
    mutate(across(.cols = everything(), ~replace_na(.x, Inf))) %>% 
    mutate(across(.cols = everything(), ~ifelse(.x == 7, Inf, .x))) %>% 
    as.data.frame()
  
  row.names(santa_tsp_data) <- santa_tsp_data$permutation
  
  santa_tsp_data <-santa_tsp_data %>% 
    select(-permutation) %>% 
    as.matrix()
  
  santa_tsp_data <- santa_tsp_data[order(row.names(santa_tsp_data)), ][, sort(row.names(santa_tsp_data))]
  atsp <- ATSP(santa_tsp_data)
  
  tour <- solve_TSP(atsp, method = method, as_TSP = TRUE, control =control, verbose	 =  verbose)
  
  return(tour)
}
###

#' Funktion mu einen solutui sting in seine permutationen aufzubrechen
break_solution <- function(solution, anz_perm) {
  
  search_position <- 1
  vec <- c()
  for (perm in seq(1, anz_perm)) {
    
    repeat {
      match <- ifelse ( all(
        c("1", "2", "3","4","5","6","7") %in% str_split( str_sub(solution, search_position, search_position+6), pattern = "", simplify = TRUE) 
      ) ,str_sub(solution, search_position, search_position+6) , FALSE)
      if (match == FALSE) {search_position <- search_position +1} else {break}
    }
    search_position <- search_position +1
    vec <- append(vec, match)
  }
  
    
  
  return(vec)
}

#eine Liste mit x permutationen in x teile teilen wobei sämtliche fehlenden 1_2 permutationen in jedem Teil vorkommen müssen
teilen <- function(anz_teile, permutationen_1_2, solution) {
  begin <- seq(1, length(solution), by=  length(solution) / anz_teile)
  end <- begin + (length(solution) / anz_teile -1 )
  
  teile <- map2(begin, end, ~{
    teil <- map_chr(seq(.x, .y), ~ pluck(solution, .x))
    
    ind <- which(! permutationen_1_2 %in% teil)
    teil <- c(teil , permutationen_1_2[ind])
    
  })
}

#' check Function
#' Function welche schaut ob die submission Kriterien erfüllt sind 
#' Alle 12 permutationen in allen strings und in summe alle permutatinen vorhanden
check <- function(solution_list, permutation_list) {
  require(cli)
  
  submission_error <- FALSE
  
  what_is_missing <- function(solution_temp, check_perm){
    ind <- which( map_lgl(check_perm, ~ str_detect(string = paste(solution_temp, collapse = ""), pattern = .x)))
    anz <- length(ind)
    missing <- check_perm[ind]
    
    cli_alert_danger("Es fehlen {anz} Permutationen im submission String. {missing}")
    
  }
  string <- paste(map_chr(solution_list, ~ paste(.x, collapse = "")), collapse = "")
  ii <- 1
  
    #Check alle 1-2 Permutationen in jedem String enthalten----
  for (sub1_2 in solution_list) {
    
    if(  all (map_lgl(.x = permutation_list$santa_1_2_perm, 
                       ~ str_detect(string = paste(sub1_2, collapse = ""), pattern = .x)))  == TRUE) {
      cli::cli_alert_success("Keine fehlenden 1-2 Permutationen im Submission String {ii}")
    } else {
      submission_error <- TRUE
      cli_h1("Sting nr {ii}")
      walk(solution_list, ~ what_is_missing(solution_temp = .x, check_perm = permutation_list$santa_1_2_perm))
    }
    
    ii <- ii + 1
  }
    
    
    #Check alle ! 1-2 Permutationen in den Strings in enthalten----
    
    if (all(map_lgl(permutation_list$santa_rest_perm, ~ str_detect(string = string, pattern = .x))) == TRUE) {
      cli::cli_alert_success("Alle Permutationen in der Submission enthalten")
      
    } else {
      submission_error <- TRUE
      walk(solution_list, ~ what_is_missing(solution_temp = .x, check_perm = permutation_list$santa_rest_perm))
    }

  cli::cli_alert_success("Länge der Submission {nchar(solution_list)}")
  
  return(submission_error)
}
