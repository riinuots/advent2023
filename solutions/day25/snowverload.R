library(tidyverse)
library(igraph)

input_orig = read_delim("solutions/day25/input", delim = ": ", col_names = c("from", "to"))

input = input_orig |> 
  separate_longer_delim(to, delim = " ") |> 
  rowid_to_column()

con1 = c("tjz", "vph") # 155
con2 = c("lnr", "pgt") # 2962
con3 = c("zkt", "jhq") # 3146

g = input |> 
  select(from, to) |> 
  # filter(! (from %in% con1 & to %in% con1)) |> 
  # filter(! (from %in% con2 & to %in% con2)) |> 
  # filter(! (from %in% con3 & to %in% con3)) |> 
  #slice(seq(1, 3318, 2)) |> 
  #slice(1:500, 2600:3318) |> 
  slice(-c(155, 2962, 3146)) |> # final answer
  graph_from_data_frame(directed = FALSE)

# manual investigations using the filters/slices and these two plots
plot(g)
tkplot(g)

clusters(g)$csize |> prod()


# works for input data, but too memory hungry and slow for real:
# cluster_optimal(g)
# 
# three = combinations(nrow(input), 3) |> 
#   as_tibble()
# 
# removed = three |> 
#   mutate(all_nodes = list(input)) |> 
#   rowid_to_column() |>
#   group_by(rowid) |>
#   mutate(removed = map(all_nodes, ~slice(.x, -c(V1, V2, V3))))
# 
# results = removed |> 
#   mutate(g = map(removed, ~graph_from_data_frame(select(.x, from, to))),
#          g_clusters = map(g, ~clusters(.x)))
# 
# g_final = results |> 
#   mutate(n = map_dbl(g_clusters, ~pluck(.x, "no"))) |> 
#   filter(n == 2) |> 
#   pull(g_clusters)
# 
# g_final[[1]]$csize |> prod()
