library(tidyverse)
library(scales)
library(words2number)
theme_set(theme_bw())

# Part I

input_orig = read_csv("solutions/day01/input", col_names = "values") %>% 
  rowid_to_column()

input_orig %>% 
  separate_rows(values, sep = "") %>% 
  mutate(values = parse_number(values)) %>% 
  summarise(.by = rowid,
            first = first(values, na_rm = TRUE),
            last = last(values, na_rm = TRUE)) %>% 
  mutate(value = paste0(first, last) %>% parse_number()) %>% 
  summarise(sum(value))


# Part II

rev_string = function(string){
  paste0(rev(str_split_1(string, "")), collapse = "")
}
input_orig %>% 
  mutate(values = str_replace_all(values, "9", "nine") %>% 
           str_replace_all("8", "eight") %>% 
           str_replace_all("7", "seven") %>% 
           str_replace_all("6", "six"  ) %>% 
           str_replace_all("5", "five" ) %>% 
           str_replace_all("4", "four" ) %>% 
           str_replace_all("3", "three") %>% 
           str_replace_all("2", "two"  ) %>% 
           str_replace_all("1", "one")) %>% 
  rowwise() %>% 
  mutate(first = str_extract(values,     "one|two|three|four|five|six|seven|eight|nine"),
         last  = str_extract(rev_string(values),
                             rev_string("one|two|three|four|five|six|seven|eight|nine")) %>% 
           rev_string()) %>% # turns the number back right way round
  pivot_longer(c(first, last)) %>% 
  mutate(value = to_number(value)) %>% 
  summarise(.by = rowid,
            calib = paste0(value, collapse = "") %>% 
              parse_number()) %>% 
  summarise(sum(calib))
  
