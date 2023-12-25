library(tidyverse)
library(igraph)
library(gtools)

input_orig = read_delim("solutions/day25/input", delim = ": ", col_names = c("from", "to"))

input = input_orig |> 
  separate_longer_delim(to, delim = " ") |> 
  rowid_to_column()

three = combinations(nrow(input), 3) |> 
  as_tibble()

removed = three |> 
  mutate(all_nodes = list(input)) |> 
  rowid_to_column() |>
  group_by(rowid) |>
  mutate(removed = map(all_nodes, ~slice(.x, -c(V1, V2, V3))))

results = removed |> 
  mutate(g = map(removed, ~graph_from_data_frame(select(.x, from, to))),
         g_clusters = map(g, ~clusters(.x)))

g_final = results |> 
  mutate(n = map_dbl(g_clusters, ~pluck(.x, "no"))) |> 
  filter(n == 2) |> 
  pull(g_clusters)

g_final[[1]]$csize |> prod()
