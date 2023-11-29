library("tidyverse")
library("testthat")

input <- readLines("input/day02.txt")

num_mover <- function(numpad, moves, start = 5){
  mover <- function(pos, move){
    case_when(move == "U" ~ c(pos[1], pos[2] - 1),
              move == "D" ~ c(pos[1], pos[2] + 1),
              move == "L" ~ c(pos[1] - 1, pos[2]),
              move == "R" ~ c(pos[1] + 1, pos[2]))
    
  }
  
  move_list <- map(moves, ~strsplit(.x, "") %>% .[[1]])
  button_tracker <- c()
  
  for(m1 in move_list){
    pos <- c(numpad$x[numpad$id == start],
             numpad$y[numpad$id == start])
    
    for(i in 1:length(m1)){
      next_coord <- mover(pos, m1[i])
      next_id <- numpad$id[numpad$x == next_coord[1] & numpad$y == next_coord[2]]
      if(!identical(next_id, character(0))){
        if(next_id != "X") pos <- next_coord
      } 
    }
    
    start <- numpad$id[numpad$x == pos[1] & numpad$y == pos[2]]
    button_tracker <- c(button_tracker, start)
  }
  
  return(paste0(button_tracker, collapse = ""))
}



part1_numpad <- tibble(id = as.character(c(1:9)), x = rep(1:3, 3), y = rep(1:3, each = 3))
expect_equal(num_mover(part1_numpad, c("ULL", "RRDDD", "LURDL", "UUUUD")), "1985")
num_mover(part1_numpad, input)

part2_numpad <- tibble(id = c("X", "X", "1", "X", "X", 
                              "X", "2", "3", "4", "X", 
                              "5", "6", "7", "8", "9", 
                              "X", "A", "B", "C", "X", 
                              "X", "X", "D", "X", "X"), 
                       x = rep(1:5, 5), 
                       y = rep(1:5, each = 5))

expect_equal(num_mover(part2_numpad, c("ULL", "RRDDD", "LURDL", "UUUUD")), "5DB3")
num_mover(part2_numpad,input)
