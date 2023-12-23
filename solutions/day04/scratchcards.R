library(tidyverse)

input_orig = read_delim("solutions/day04/input-test", delim = "|", col_names = c("winning", "yours")) %>% 
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
cards = cards %>% 
  mutate(card_n = parse_number(card)) %>% 
  rowwise() %>% 
  mutate(get_cards = if_else(n == 0, list(NULL), list(card_n + 1:n)))


yields = pull(cards, get_cards)
cardlist = pull(cards, card_n)
i = 1
while (TRUE){
  cardlist = c(cardlist, yields[[cardlist[i]]])
  #print(cardlist)
  i = i + 1
  if (i == length(cardlist)){
    print(i)
    break
  }
}
