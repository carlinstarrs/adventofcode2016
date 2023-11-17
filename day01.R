library("tidyverse")
library("testthat")

input <- "inputs/day01.txt" %>% readLines() %>% strsplit(", ") %>% .[[1]]

move_grid <- function(dir, turn, coord, dist){
  dir_turn <- case_when((dir == "N" & turn == "R") | (dir == "S" & turn == "L") ~ "E", 
                        (dir == "N" & turn == "L") | (dir == "S" & turn == "R") ~ "W", 
                        (dir == "E" & turn == "R") | (dir == "W" & turn == "L") ~ "S", 
                        (dir == "E" & turn == "L") | (dir == "W" & turn == "R") ~ "N")
  
  next_coord <- case_when(dir_turn == "E" ~ c(coord[1], coord[2] + dist), 
                          dir_turn == "W" ~ c(coord[1], coord[2] - dist), 
                          dir_turn == "S" ~ c(coord[1] - dist, coord[2]), 
                          dir_turn == "N" ~ c(coord[1] + dist, coord[2]))
  
  return(list(dir_turn, next_coord))
}

route_finder <- function(route){
  dir <- "N"
  coord <- c(0,0)
  i <- 1

  coord_tracker <- tibble(x = coord[1], y = coord[2], id = 0)
  while(i <= length(route)){
    turn <- str_extract(route[i], "^(L|R)")
    dist <- as.numeric(str_extract(route[i], "\\d+$"))
    
    pos <- move_grid(dir, turn, coord, dist)
    dir <- pos[[1]]
    coord_tracker <- coord_tracker %>% bind_rows(tibble(x = coord[1]:pos[[2]][1], y = coord[2]:pos[[2]][2], id = i) %>% slice(-1))
    coord <- pos[[2]]
    i <- i + 1
  }
  
 
  final_dupe <- coord_tracker %>% 
    group_by(x, y) %>% 
    mutate(total = 1:n()) %>% 
    ungroup() %>% 
    filter(total == 2) %>% 
    slice(1)
  
  final_dist <- sum(abs(pos[[2]]))
  final_dupe_dist <- sum(c(abs(final_dupe$x), abs(final_dupe$y)))
  
  return(list("final_dist" = final_dist, "first_duplicate" = final_dupe_dist))
}



testthat::expect_equal(route_finder(c("R2", "L3"))$final_dist, 5)
testthat::expect_equal(route_finder(c("R2", "R2", "R2"))$final_dist, 2)
testthat::expect_equal(route_finder(c("R5", "L5", "R5", "R3"))$final_dist, 12)

testthat::expect_equal(route_finder(c("R8", "R4", "R4", "R8"))$first_duplicate, 4)


route_finder(input)


