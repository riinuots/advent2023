library(tidyverse)

input_orig = read_csv("solutions/day03/input", col_names = "part")

input = input_orig %>% 
  rowid_to_column("x") %>% 
  separate_rows(part, sep = "") %>% 
  filter(part != "") %>% # not sure why it 'gets' empty value from every line beginning
  mutate(.by = x, .after = x,
         y = seq_along(x))

# get which numbers form a group (i.e., single multidigit number)
groups = input %>% 
  mutate(is_group = str_detect(part, "\\d")) %>% 
  filter(is_group | lag(is_group)) %>% 
  mutate(group_id = cumsum(!is_group)) %>% 
  filter(is_group) %>% 
  mutate(id = paste(x, y, sep = "-"))


neigbhours = crossing(dy = -1:1, dx = -1:1)

engine = input %>% 
  crossing(neigbhours) %>% 
  mutate(nx = x+dx,
         ny = y+dy) %>% 
  left_join(rename(input, nb = part), by = c("nx" = "x", "ny" = "y")) %>% 
  drop_na() %>% 
  filter(nb != ".")

# Part I

keep = engine %>% 
  filter(!str_detect(nb, "\\d")) %>% 
  select(x, y) %>% 
  mutate(id = paste(x, y, sep = "-"))

keep_groups = groups %>% 
  filter(id %in% keep$id) %>% 
  pull(group_id) %>% 
  unique()

groups %>% 
  filter(group_id %in% keep_groups) %>% 
  summarise(.by = group_id,
            part = paste(part, collapse = "") %>% parse_number()) %>% 
  summarise(sum(part))

# Part II
gears = engine %>% 
  filter(part == "*") %>% 
  filter(str_detect(nb, "\\d")) %>% 
  mutate(id_nb = paste(nx, ny, sep = "-"),
         id_gear = paste(x, y, sep = "-")) %>% 
  distinct(id_gear, id_nb)

gears %>% 
  left_join(groups, by = c("id_nb" = "id")) %>% 
  group_by(id_gear) %>% 
  mutate(n_gears = n_distinct(group_id)) %>% 
  filter(n_gears > 1) %>% 
  distinct(group_id) %>% 
  left_join(groups) %>% 
  group_by(id_gear, group_id) %>% 
  summarise(part = paste(part, collapse = "") %>% parse_number()) %>% 
  group_by(id_gear) %>% 
  summarise(ratio = prod(part)) %>% 
  summarise(sum(ratio))
