library(tidyverse)
library(igraph)
theme_set(theme_bw())

cols = read_lines("solutions/day23/input", n_max = 1) %>% str_count("")
input_orig = read.fwf("solutions/day23/input", width = rep(1, cols), comment.char = "")

m = input_orig |> as.matrix()

start = which(m == ".", arr.ind = TRUE) |> head(1) |> paste(collapse = "-")
end = which(m == ".", arr.ind = TRUE) |> tail(1) |> paste(collapse = "-")

paths = which(m == "." | m == "v" | m == ">", arr.ind = TRUE) |> 
  as_tibble() |> 
  rename(x = row, y = col) |> 
  mutate(id = paste(x, y, sep = "-"))

neigbhours = tribble(~dy, ~dx, -1, 0, 0, -1, 1, 0, 0, 1) |> 
  mutate(dir = c("left", "up", "right", "down"))

trails = paths %>% 
  tidyr::crossing(neigbhours) %>% 
  mutate(id_nb = paste(x+dx, y+dy, sep = "-")) |> 
  filter(id != id_nb)

g = trails |> 
  select(id, id_nb) |> 
  graph_from_data_frame()

# doesn't work, too many options
ways = all_simple_paths(g, from = start, to = end)
path_id = which(lengths(ways) == max(lengths(ways)))
path = ways[[path_id]] |> unlist() |> names()

tibble(id = path) |> 
  mutate(id_nb = lead(id)) |> 
  separate(id,    into = c("x", "y"), sep = "-", convert = TRUE) |> 
  separate(id_nb, into = c("x_nb", "y_nb"), sep = "-", convert = TRUE) |> 
  mutate(slopex = (lead(x) - x) > 1,
         slopey = (lead(y) - y) > 1) |> 
  summarise(n = n(),
            x = sum(slopex, na.rm = TRUE),
            y = sum(slopey, na.rm = TRUE)) |> 
  mutate(hike = n + y + x -1 )
