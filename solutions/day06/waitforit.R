library(tidyverse)

# Part I
times = read_lines("solutions/day06/input", n_max = 1) %>% 
  str_squish() %>% 
  str_remove("Time: ") %>% 
  str_split_1(" ") %>% 
  parse_number()

distances = read_lines("solutions/day06/input", skip = 1) %>% 
  str_squish() %>% 
  str_remove("Distance: ") %>% 
  str_split_1(" ") %>% 
  parse_number()


tibble(time = times, record = distances) %>% 
  rowid_to_column("race") %>% 
  rowwise() %>% 
  mutate(hold = list(seq(1, time))) %>% 
  unnest(hold) %>% 
  mutate(distance = (time-hold)*hold) %>% 
  filter(distance > record) %>% 
  group_by(race) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  summarise(prod(n))

# Part II 
times = read_lines("solutions/day06/input", n_max = 1) %>% 
  str_remove("Time: ") %>% 
  str_remove_all(" ") %>% 
  parse_number()

distances = read_lines("solutions/day06/input", skip = 1) %>% 
  str_remove("Distance: ") %>% 
  str_remove_all(" ") %>% 
  parse_number()

tibble(time = times, record = distances) %>% 
  rowid_to_column("race") %>% 
  rowwise() %>% 
  mutate(hold = list(seq(1, time))) %>% 
  unnest(hold) %>% 
  mutate(distance = (time-hold)*hold) %>% 
  filter(distance > record) %>% 
  group_by(race) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  summarise(prod(n))
