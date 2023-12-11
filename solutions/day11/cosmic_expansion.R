library(tidyverse)

input_orig = read_lines("solutions/day11/input")
nrows = length(input_orig)

input_matrix = input_orig %>% 
  map(~str_split_1(.x, "")) %>% 
  unlist() %>% 
  matrix(nrow = nrows, byrow = TRUE)

galaxies = which(input_matrix == "#", arr.ind = TRUE) %>% 
  as_tibble() %>% 
  rowid_to_column("id")

empty_rows = which(rowSums(input_matrix == "#") == 0)
empty_cols = which(colSums(input_matrix == "#") == 0)

# Part I
wrangle_add = function(n_add = 0){
  crossing(galaxies,
           galaxies %>% rename(id2 = id, row2 = row, col2 = col)) %>% 
    filter(id != id2) %>%
    mutate(distance = abs(row2-row) + abs(col2-col)) %>% 
    rowwise() %>% 
    mutate(row_expansions = length(row:row2 %>% intersect(empty_rows)),
           col_expansions = length(col:col2 %>% intersect(empty_cols))) %>% 
    ungroup() %>% 
    mutate(total = distance + n_add*row_expansions + n_add*col_expansions) %>% 
    summarise(sum(total)/2)
}

wrangle_add(1)

# Part II
wrangle_add(999999)

