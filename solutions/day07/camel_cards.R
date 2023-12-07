library(tidyverse)

input_orig = read_delim("solutions/day07/input", col_names = c("hand", "bid"))

hands = input_orig %>% 
  mutate(hand_id = hand) %>% 
  separate_rows(hand, sep = "") %>% 
  rename(card = hand) %>% 
  filter(card != "")

# Part I
card_order = rev(letters[1:13])
names(card_order) = c(2:9, "T", "J", "Q", "K", "A")

hands %>% 
  count(hand_id, bid, card) %>% 
  group_by(hand_id, bid) %>% 
  arrange(-n) %>% 
  summarise(count = list(n)) %>% 
  rowwise() %>% 
  mutate(n_max = max(count),
         n_sec = count[2]) %>% 
  mutate(hand_rank = str_replace_all(hand_id, card_order)) %>% 
  ungroup() %>% 
  arrange(n_max, n_sec, desc(hand_rank)) %>% 
  rowid_to_column("rank") %>% 
  mutate(score = bid*rank) %>% 
  summarise(sum(score))
  
# Part II
names(card_order) = c("J", 2:9, "T", "Q", "K", "A")

hands %>% 
  mutate(n_jokers = str_count(hand_id, "J")) %>% 
  filter(card != "J" | n_jokers == 5) %>% 
  count(hand_id, bid, card, n_jokers) %>% 
  group_by(hand_id, bid, n_jokers) %>% 
  arrange(-n) %>% 
  summarise(count = list(n)) %>% 
  rowwise() %>% 
  mutate(n_max = if_else(n_jokers == 5, 5, max(count) + n_jokers),
         n_sec = count[2]) %>% 
  mutate(hand_rank = str_replace_all(hand_id, card_order)) %>% 
  ungroup() %>% 
  arrange(n_max, n_sec, desc(hand_rank)) %>% 
  rowid_to_column("rank") %>% 
  mutate(score = bid*rank) %>% 
  summarise(sum(score))

# too high   249021871
# too low    247366890
# just wrong 247857527
