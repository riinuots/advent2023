library(tidyverse)
library(igraph)

cols = read_lines("solutions/day21/input", n_max = 1) %>% str_count("")
input_orig = read.fwf("solutions/day21/input", width = rep(1, cols), comment.char = "")

m = input_orig |> as.matrix()

start = which(m == "S", arr.ind = TRUE) |> paste(collapse = "-")

plots = which(m == "." | m == "S", arr.ind = TRUE) |> 
  as_tibble() |> 
  rename(x = row, y = col) |> 
  mutate(id = paste(x, y, sep = "-"))

neigbhours = tidyr::crossing(dy = -1:1, dx = -1:1) |> 
  filter(! (abs(dy) + abs(dx)) == 2)  # no diagnal steps!

map = plots %>% 
  tidyr::crossing(neigbhours) %>% 
  mutate(id_nb = paste(x+dx, y+dy, sep = "-")) |> 
  filter(id_nb %in% plots$id) |> 
  filter(id != id_nb)

g = map |> 
  select(id, id_nb) |> 
  graph_from_data_frame()

# Part I
d = distances(g)[start, ]
tibble(to = names(d),
       d = d) |> 
  filter(d %% 2 == 0) |> # can step back and forth
  filter(d <= 64) |> 
  nrow()
