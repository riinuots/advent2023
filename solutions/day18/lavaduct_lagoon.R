library(tidyverse)
library(sf)
theme_set(theme_bw())

input_orig = read_delim("solutions/day18/input-test", delim = " ", col_names = c("dir", "n", "colour"))

# Part I
waypoints = input_orig |> 
  mutate(x = accumulate2(n, case_match(dir,
                                        "R" ~  0,
                                        "U" ~ -1,
                                        "D" ~  1,
                                        "L" ~  0),
                          ~(..1 + ..2*..3), .init = 1) |> head(-1),
         y = accumulate2(n, case_match(dir,
                                        "R" ~  1,
                                        "U" ~  0,
                                        "D" ~  0,
                                        "L" ~ -1),
                          ~(..1 + ..2*..3), .init = 1) |> head(-1))


poly = st_polygon(list(cbind(x = c(waypoints$x, 1), y = c(waypoints$y, 1))))
plot(poly)
# wrong area:
st_area(poly) # not sure what this area value is about. Not right.

# right area (create a bunch of points and get their intersection with the loop:
st_as_sf(crossing(x = min(waypoints$x):max(waypoints$x), y = min(waypoints$y):max(waypoints$y)), coords = c("x", "y")) |> 
  st_intersects(poly, sparse = FALSE) |> 
  sum()

# Part II

waypoints = input_orig |> 
  mutate(n   = str_sub(colour, 3, 7) |> strtoi(base = 16),
         dir = str_sub(colour, 8, 8)) |> 
  mutate(x = accumulate2(n, case_match(dir,
                                       "0" ~  0,
                                       "3" ~ -1,
                                       "1" ~  1,
                                       "2" ~  0),
                         ~(..1 + ..2*..3), .init = 1) |> head(-1),
         y = accumulate2(n, case_match(dir,
                                       "0" ~  1,
                                       "3" ~  0,
                                       "1" ~  0,
                                       "2" ~ -1),
                         ~(..1 + ..2*..3), .init = 1) |> head(-1))

poly = st_polygon(list(cbind(x = c(waypoints$x, 1), y = c(waypoints$y, 1))))
st_area(poly) |> as.character()

# right area (create a bunch of points and get their intersection with the loop:
st_as_sf(crossing(x = min(waypoints$x):max(waypoints$x), y = min(waypoints$y):max(waypoints$y)), coords = c("x", "y")) |> 
  st_intersects(poly, sparse = FALSE) |> 
  sum()

# Plotting 
path = waypoints |>  
  mutate(x1 = lead(x, default = first(x)),
         y1 = lead(y, default = first(y))) |> 
  rowwise() |> 
  mutate(path_x = list(x:x1),
         path_y = list(y:y1)) |> 
  unnest(path_x) |> 
  unnest(path_y)

path |> 
  ggplot(aes(path_y, path_x)) +
  geom_point(shape = "#", size = 10) +
  geom_point(data = crossing(x = 1:10, y = 1:10), aes(x, y))
