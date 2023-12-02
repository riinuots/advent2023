library(tidyverse)

input_orig = read_delim("solutions/day02/input", col_names = c("game", "balls"))

bag_orig = tibble(balls = "12 red cubes, 13 green cubes, and 14 blue cubes")

# Wrangling inputs
bag = bag_orig %>% 
  separate_rows(balls, sep = ", ") %>% 
  mutate(balls = str_remove_all(balls, "and | cubes")) %>% 
  separate(balls, into = c("bag_total", "colour"), convert = TRUE)

games = input_orig %>% 
  mutate(game = parse_number(game),
         balls = str_trim(balls)) %>% 
  separate_rows(balls, sep = "; ") %>% 
  rowid_to_column("set") %>% 
  separate_rows(balls, sep = ", ") %>% 
  separate(balls, into = c("n", "colour"), convert = TRUE)

# Part I
games %>% 
  left_join(bag) %>% 
  group_by(game) %>% 
  mutate(enough = n <= bag_total) %>% 
  filter(all(enough)) %>% 
  ungroup() %>% 
  summarise(sum(unique(game)))

# Part II 
games %>% 
  group_by(game, colour) %>% 
  slice_max(n, with_ties = FALSE) %>% 
  group_by(game) %>% 
  summarise(power = prod(n)) %>% 
  ungroup() %>% 
  summarise(sum(power))
