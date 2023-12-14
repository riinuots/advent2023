library(tidyverse)
library(animation)
library(tictoc)
theme_set(theme_bw())

cols = read_lines("solutions/day14/input", n_max = 1) %>% str_count("")
input_orig = read.fwf("solutions/day14/input", width = rep(1, cols), comment.char = "")

m = as.matrix(input_orig)[nrow(input_orig):1, ] # reverse rows

# Part I
while (TRUE){
  n_changes = 0
  for (x in (nrow(m)-1):1){
    print(x)
    openings = m[x+1, ] == "."
    rocks    = m[x, ]   == "O"
    openings & rocks
    m[x+1, openings & rocks] = "O"
    m[x, openings & rocks] = "."
    n_changes = n_changes + sum(openings & rocks)
  }
  if (n_changes == 0){
    break
  }
}
which(m == "O", arr.ind = TRUE)[, 1] |> sum()

# Optional - plotting
myplot = function(m){
  bind_rows(
    which(m == "O", arr.ind = TRUE) |> as_tibble() |> mutate(type = "O"), 
    which(m == "#", arr.ind = TRUE) |> as_tibble() |> mutate(type = "#")) |> 
    ggplot(aes(col, row, shape = type)) +
    geom_point(size = 1) +
    scale_shape_identity()
}
myplot(m)