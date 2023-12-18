library(tidyverse)
library(sf)
theme_set(theme_bw())

input_orig = read_delim("solutions/day18/input", delim = " ", col_names = c("dir", "n", "colour"))

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

# wrong answer:
st_area(poly) # this is the inside area only without the trench itself


# create a bunch of points and get their intersection with the trench:
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

# the intersection solution no longer works as the area is too big,
# so had to figure out how to add the trench to the inside area

interior = st_polygon(list(cbind(x = c(waypoints$x, 1), y = c(waypoints$y, 1))))
trench   = st_multilinestring(list(cbind(x = c(waypoints$x, 1), y = c(waypoints$y, 1))))


as.character(
  st_area(interior) + st_length(trench)/2 + 1 # trial and error
)
