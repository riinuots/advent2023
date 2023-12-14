library(tidyverse)
library(animation)
library(tictoc)
theme_set(theme_bw())

cols = read_lines("solutions/day14/input", n_max = 1) %>% str_count("")
input_orig = read.fwf("solutions/day14/input", width = rep(1, cols), comment.char = "")

m = as.matrix(input_orig)[nrow(input_orig):1, ] # reverse rows
m_orig = m
# Part I ----
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
    which(m == "#", arr.ind = TRUE) |> as_tibble() |> mutate(type = "#"),
    which(m == "N", arr.ind = TRUE) |> as_tibble() |> mutate(type = "N"),
    which(m == "E", arr.ind = TRUE) |> as_tibble() |> mutate(type = "E"),
    which(m == "S", arr.ind = TRUE) |> as_tibble() |> mutate(type = "S"),
    which(m == "W", arr.ind = TRUE) |> as_tibble() |> mutate(type = "W")) |> 
    ggplot(aes(col, row, shape = type)) +
    geom_point(size = 5) +
    scale_shape_identity()
}
myplot(m)

# Part II ----
turn = function(m){
  apply(t(m), 2, rev)
}

m = m_orig
cycle = 0
m_list = list(m_orig)
do_cycles = function(m, m_list, n_cycles){
  while (TRUE){
    cycle = cycle + 1
    if(cycle %% 1 == 0){print(paste("cycle:", cycle))}
    #print(m_list)
    for (tilt in 1:4){
      while (TRUE){
        n_changes = 0
        for (x in (nrow(m)-1):1){
          #print(x)
          openings = m[x+1, ] == "."
          rocks    = m[x, ]   == "O"
          openings & rocks
          m[x+1, openings & rocks] = "O"
          m[x, openings & rocks] = "."
          n_changes = n_changes + sum(openings & rocks)
        }
        if (n_changes == 0){
          #print(myplot(m) + ggtitle(paste("tilt:", tilt, "; cycle:", cycle)))
          if (tilt == 4){
            #myplot(turn(m)) |> print()
            if(which(m == "O", arr.ind = TRUE)[, 1] |> sum() == 64){print("64!")}
          }
          m = turn(m)
          break
        }
      }
    }
    # if (map(m_list, ~all(.x == m)) |> unlist() |> any()){
    #   print(cycle)
    #   return(cycle)
    # }
    m_list = append(m_list, list(m))
    if (cycle == n_cycles){
      return(m_list)
      break
    }
  }
}

m = m_orig
cycle = 0
m_list_orig = list(m_orig)
m_list = do_cycles(m, m_list_orig, 200)

phases = tibble(id = map(m_list, ~unlist(.x) |> paste0(collapse = ""))) |> 
  unnest(id) |> 
  rowid_to_column("cycle") |>
  add_count(id) |> 
  mutate(id_n = fct_anon(id))

start_cycles = phases |> 
  mutate(cycle = cycle -1) |> 
  filter(n > 1) |> 
  slice(1) |> 
  pull(cycle)

start_id = phases |> 
  mutate(cycle = cycle -1) |> 
  filter(n > 1) |> 
  slice(1) |> 
  pull(id_n)

phase_length = phases |> 
  filter(id_n == start_id) |> 
  pull(cycle) |> 
  diff() |> 
  unique()

n = start_cycles + phase_length + ((1000000000 - start_cycles) %% phase_length)

which(m_list[[n + 1]] == "O", arr.ind = TRUE)[, 1] |> sum()
