library(tidyverse)

input_orig = read_delim("solutions/day04/input", delim = "|", col_names = c("winning", "yours")) %>% 
  separate(winning, into = c("card", "winning"), sep = ":")

# Part I
cards = input_orig %>% 
  rowwise() %>% 
  mutate(winning = list(str_split_1(winning, " ") %>% parse_number() %>% na.omit()),
         yours = list(str_split_1(yours, " ") %>% parse_number() %>% na.omit()),
         n = length(intersect(winning, yours)),
         score = if_else(n == 0, 0, 2**(n-1))) %>% 
  ungroup()

cards %>% 
  summarise(sum(score))

# Part II - incomplete
yields = cards %>% 
  mutate(card_n = parse_number(card)) %>% 
  rowwise() %>% 
  mutate(get_cards = list(card_n:(card_n+n))) %>% 
  select(card_n, get_cards) %>% 
  add_row(card_n = 0, get_cards = list(0))
