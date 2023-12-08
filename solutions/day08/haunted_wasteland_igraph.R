library(tidyverse)
library(igraph)

instr_input = read_lines("solutions/day08/input", n_max = 1) %>% 
  str_split_1("")

map = read_delim("solutions/day08/input", skip = 2, delim = " = ", col_names = c("from", "to")) %>% 
  mutate(to = str_remove_all(to, "\\(|\\)"),
         dir = "L, R") %>% 
  separate_longer_delim(c(to, dir), delim = ", ")


map %>% 
  slice_sample(prop = 0.1) %>% 
  graph_from_data_frame(directed = TRUE) %>% 
  plot()
