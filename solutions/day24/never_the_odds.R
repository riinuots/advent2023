library(tidyverse)

input_orig = read_csv("solutions/day24/input", col_names = c("x", "y", "z_dx", "dy", "dz"))

vectors = input_orig |> 
  separate(z_dx, into = c("z", "dx"), sep = " @ ", convert = TRUE) |> 
  rowid_to_column("stone_id")

stones = crossing(vectors,
                  rename_with(vectors, ~paste0(.x, "1"))) |> 
  filter(stone_id != stone_id1) |> 
  rowwise() |> 
  mutate(pair_id = paste(min(stone_id, stone_id1), max(stone_id, stone_id1), sep = "-")) |> 
  distinct(pair_id, .keep_all = TRUE) |> 
  ungroup()

# Part I
parallel = stones |> 
  mutate(x = dx1/dx,
         y = dy1/dy) |> 
  filter(x == y) |> 
  rowwise() |> 
  mutate(pair_id = paste(min(stone_id, stone_id1), max(stone_id, stone_id1), sep = "-")) |> 
  pull(pair_id)

stones |> 
  filter(! pair_id %in% parallel) |> 
  rowwise() |> 
  mutate(left  = matrix(c(dx1, -dx, dy1, -dy),
                        ncol = 2, byrow = TRUE) |> list(),
         right = matrix(c(x - x1, y - y1)) |> list()) |> 
  mutate(t = solve(left, right)[2],
         u = solve(left, right)[1],
         meet_x = x + dx*t,
         meet_y = y + dy*t) |> 
  filter(t > 0 & u > 0) |> 
  filter(between(meet_x, 200000000000000, 400000000000000),
         between(meet_y, 200000000000000, 400000000000000)) |> 
  nrow()
