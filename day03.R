library("tidyverse")
library("testthat")

input <- readLines("input/day03.txt") %>% 
  tibble(V1 = .) %>% 
  mutate(V1 = trimws(V1)) %>% 
  separate(col = "V1", sep = " +", into = c("V1", "V2", "V3")) %>% 
  mutate(across(everything(), as.numeric)) 

triangle_checker <- function(V1, V2, V3, ...){
  c(V1 + V2 > V3, 
    V2 + V3 > V1, 
    V1 + V3 > V2) %>% 
    sum()
}

input %>% 
  mutate(CHECK = pmap(., triangle_checker)) %>% 
  filter(CHECK == 3) %>% 
  nrow()

input %>% 
  mutate(n = rep(1:(nrow(.)/3), each = 3)) %>% 
  pivot_longer(-n) %>% 
  group_by(n, name) %>% 
  mutate(name2 = paste0("V", 1:3)) %>% 
  ungroup() %>%
  pivot_wider(values_from = value, 
              names_from = name2) %>% 
  mutate(CHECK = pmap(., triangle_checker)) %>% 
  filter(CHECK == 3) %>% 
  nrow()
