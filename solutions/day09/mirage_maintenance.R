library(tidyverse)
library(scales)
theme_set(theme_bw())

report = read_lines("solutions/day09/input") %>% 
  map(~str_split_1(.x, " ") %>% parse_number())

# Part I
get_last = function(history){
  pred = tail(history, 1)
  if (any(history != 0)){
    pred = pred + get_last(diff(history)) # hello recursion
  }
  return(pred)
}
map_dbl(report, ~get_last(.x)) %>%
  sum() %>% 
  as.character() # for non-scientific printing

# Part II
get_first = function(history){
  pred = head(history, 1)
  if (any(history != 0)){
    pred = c(pred, get_first(diff(history)))
  }
  pred = rev(pred)
  d = 0
  for (i in 1:length(pred)){
    d = pred[i] - d
  }
  return(d)
}
map_dbl(report, ~get_first(.x)) %>%
  sum()
