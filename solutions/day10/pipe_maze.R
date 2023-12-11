library(tidyverse)
library(igraph)

input_orig = read_lines("solutions/day10/input-test2")
cols = input_orig[[1]] %>% str_count("")

input = input_orig %>% 
  map(~str_split_1(.x, "")) %>% 
  unlist() %>% 
  matrix(ncol = cols) %>% 
  t()

pipes = tribble(~value, ~dy, ~dx,
                "|",     -1,   0,
                "|",      1,   0,
                "-",      0,  -1,
                "-",      0,   1,
                "L",     -1,   0,
                "L",      0,   1,
                "J",     -1,   0,
                "J",      0,  -1,
                "7",      1,   0,
                "7",      0,  -1,
                "F",      1,   0,
                "F",      0,   1,
                "S",      0,  -1 # test2 S is 7
#                "S",    -1,  0) # my S can only be a J
)
# wrangle input into a long tibble
input_df = input %>% 
  as_tibble() %>% 
  rowid_to_column("y") %>% 
  pivot_longer(-y, names_to = "x") %>% 
  mutate(x = parse_number(x)) %>% 
  left_join(pipes, relationship = "many-to-many") %>% 
  drop_na() %>% 
  mutate(nb_x = x + dx,
         nb_y = y + dy) %>% 
  filter(nb_x > 0, nb_y > 0) %>% 
  mutate(from = paste(   x,    y, sep = "-"),
         to   = paste(nb_x, nb_y, sep = "-"))

# create graph
g = input_df %>% 
  select(from, to) %>% 
  graph_from_data_frame(directed = TRUE)

# get coordinates for S
s = input_df %>% 
  filter(value == "S") %>% 
  pull(from)

# Part I 
# I know the loop ends at 10-62 from looking at the input
# (just left of my S)
loop = all_shortest_paths(g, from = s, to = "10-62")
loop = all_shortest_paths(g, from = s, to = "5-2") # test2
length(loop$res[[1]])/2

# 3346/2 = 1673 too low - entered wrong endpoint
# 13512/2 = 6756 too low - entered wrong endpoint
# 13514/2 = 6757 right answer

# Part II - plot only, no solution
input_df %>% 
  ggplot(aes(x, -y, label = value)) +
  geom_text(aes(colour = from %in% names(loop$res[[1]]))) +
  theme_void()+
  theme(legend.position = "none") +
  scale_colour_viridis_d()

#ggsave("solutions/day10/day10_loop.jpeg", height = 10, width = 10)

pipes_to_cross = input_df %>% 
  filter(y < 11) %>% 
  filter(value %in% c("|", "-")) %>% 
  filter(from %in% names(loop$res[[1]]))

tiles = tidyr::crossing(x = 1:20, y = 1:10) %>% 
  mutate(id = paste(x, y, sep = "-")) %>% 
  filter(!id %in% names(loop$res[[1]])) %>% 
  filter(x != 1, x != cols, y != 1, y != cols) # no need to check edges

input_df %>% 
  ggplot(aes(x, -y, label = value)) +
  geom_text(aes(colour = from %in% names(loop$res[[1]]))) +
  theme_void()+
  theme(legend.position = "none") +
  scale_colour_viridis_d() +
  geom_point(data = tiles, aes(x, -y, label = x), colour = "black")
