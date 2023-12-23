library(tidyverse)
library(igraph)
theme_set(theme_bw())

cols = read_lines("solutions/day23/input", n_max = 1) %>% str_count("")
input_orig = read.fwf("solutions/day23/input", width = rep(1, cols), comment.char = "")

m = input_orig |> as.matrix()

start = which(m == ".", arr.ind = TRUE) |> head(1) |> paste(collapse = "-")
end = which(m == ".", arr.ind = TRUE) |> tail(1) |> paste(collapse = "-")

paths = which(m == ".", arr.ind = TRUE) |> 
  as_tibble() |> 
  rename(x = row, y = col) |> 
  mutate(id = paste(x, y, sep = "-"))

slope_right = which(m == ">", arr.ind = TRUE) |> 
  as_tibble() |> 
  rename(x = row, y = col) |> 
  mutate(id = paste(x, y, sep = "-"))

slope_down = which(m == "v", arr.ind = TRUE) |> 
  as_tibble() |> 
  rename(x = row, y = col) |> 
  mutate(id = paste(x, y, sep = "-"))

neigbhours = tribble(~dy, ~dx, -1, 0, 0, -1, 1, 0, 0, 1) |> 
  mutate(dir = c("left", "up", "right", "down"))

trails = paths %>% 
  tidyr::crossing(neigbhours) %>% 
  mutate(id_nb = paste(x+dx, y+dy, sep = "-")) |> 
  filter(id != id_nb) |> 
  mutate(nb_type = case_when(id_nb %in% paths$id       ~ "path",
                             id_nb %in% slope_down$id  ~ "v",
                             id_nb %in% slope_right$id ~ ">",
                             .default = "forest")) |> 
  filter(nb_type != "forest") |> 
  mutate(id_nb = if_else(nb_type != "path",
                          paste(x+2*dx, y+2*dy, sep = "-"),
                          id_nb
                          )) |> 
  filter(!(dir == "up"   & nb_type == "v")) |> # can't go uphill
  filter(!(dir == "left" & nb_type == ">")) |>
  arrange(nb_type == "path") |> # bring slopes to top so can remove 'duplicate' paths
  distinct(id, id_nb, .keep_all = TRUE)

g = trails |> 
  select(id, id_nb) |> 
  graph_from_data_frame()

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

# trails |> 
#   separate(id,    into = c("x", "y"), sep = "-", convert = TRUE) |> 
#   separate(id_nb, into = c("x_nb", "y_nb"), sep = "-", convert = TRUE) |> 
#   rowid_to_column("group") |> 
#   pivot_longer(starts_with("x"), values_to = "x") |> 
#   pivot_longer(starts_with("y"), names_to = "whatever", values_to = "y") |> 
#   ggplot(aes(y, -x)) +
#   geom_point() +
#   geom_line(aes(group = group)) +
#   geom_point(data = slope_down, shape = "V", colour = "blue", size = 7) +
#   geom_point(data = slope_right, shape = ">", colour = "blue", size = 7)

# tibble(id = path) |> 
#   mutate(id_nb = lead(id)) |> 
#   separate(id,    into = c("x", "y"), sep = "-", convert = TRUE) |> 
#   separate(id_nb, into = c("x_nb", "y_nb"), sep = "-", convert = TRUE) |> 
#   rowid_to_column("group") |> 
#   pivot_longer(starts_with("x"), values_to = "x") |> 
#   pivot_longer(starts_with("y"), names_to = "whatever", values_to = "y") |> 
#   ggplot(aes(y, -x)) +
#   geom_point() +
#   geom_line(aes(group = group)) +
#   geom_label(aes(label = paste(x, y, sep = "-"))) +
#   geom_point(data = slope_down, shape = "V", colour = "blue", size = 7) +
#   geom_point(data = slope_right, shape = ">", colour = "blue", size = 7)