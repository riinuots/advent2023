library(tidyverse)

input_orig = readLines("solutions/day15/input")

# Part I
input_orig |> 
  str_split_1(",") |> 
  map(~utf8ToInt(.x)) |> 
  map(~reduce(.x, ~(((.x + .y)*17) %% 256), .init = 0)) |> 
  unlist() |> 
  sum()

# Part II
tibble(lense = str_split_1(input_orig, ",")) |> 
  rowid_to_column("instr_id") |> 
  mutate(label = str_extract(lense, "[a-z]+"),
         op    = str_extract(lense, "=|-"),
         focal = suppressWarnings(parse_number(lense))) |> 
  rowwise() |> 
  mutate(box_id = map(label, ~utf8ToInt(.x)) |> 
           map_dbl(~reduce(.x, ~(((.x + .y)*17) %% 256), .init = 0))) |> 
  ungroup() |> 
  arrange(box_id, instr_id) |> 
  group_by(box_id, label) |> 
  mutate(keep = if_else(op == "-", FALSE, NA)) |> 
  fill(keep, .direction = "up") |> 
  filter(keep | is.na(keep)) |> 
  mutate(focal = last(focal)) |> 
  distinct(box_id, focal) |> 
  group_by(box_id) |> 
  mutate(slot = seq_along(box_id)) |> 
  mutate(power = (box_id + 1)*(slot)*focal) |> 
  ungroup() |> 
  summarise(sum(power))
