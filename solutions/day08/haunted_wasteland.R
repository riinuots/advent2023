library(tidyverse)
library(tictoc) # see how long code ran
library(pracma) # Lcm() least common multiple function

instr_input = read_lines("solutions/day08/input", n_max = 1) %>% 
  str_split_1("")

map = read_delim("solutions/day08/input", skip = 2, delim = " = ", col_names = c("from", "to")) %>% 
  mutate(to = str_remove_all(to, "\\(|\\)"),
         dir = "L, R") %>% 
  separate_longer_delim(c(to, dir), delim = ", ")

# Part I
loc = "DNA"
n = 0
tic()
for (instr in rep(instr_input, 1e6)){
  n = n + 1
  loc = map %>% 
    filter(from == loc, dir == instr) %>% 
    pull(to)
  if (loc == "ZZZ"){
    print(n)
    toc()
    break
  }
}

# Part II
locations = map %>% 
  filter(str_ends(from, "A")) %>% 
  pull(from)

get_n = function(loc){
  n = 0
  for (instr in rep(instr_input, 1e6)){
    n = n + 1
    loc = map %>% 
      filter(from == loc, dir == instr) %>% 
      pull(to)
    if (str_ends(loc, "Z")){
      print(n)
      return(n)
      break
    }
  }
}
steps = map(locations, ~get_n(.x))
options("scipen"=100, "digits"=4)
reduce(unlist(steps), Lcm)
