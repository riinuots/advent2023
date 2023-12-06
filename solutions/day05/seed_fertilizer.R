library(tidyverse)

input_orig = read_csv("solutions/day05/input", col_names = "all", skip = 1)
seeds = read_lines("solutions/day05/input", n_max = 1) %>% 
  str_remove("seeds: ") %>% 
  str_split_1(" ") %>% 
  parse_number()

almanac = input_orig %>%
  mutate(map = if_else(str_detect(all, "map"), str_remove(all, " map:"), NA)) %>% 
  fill(map) %>% 
  filter(!str_detect(all, "map")) %>% 
  separate(all, into = c("dest0", "source0", "n"), convert = TRUE) %>% 
  separate(map, into = c("from", "to"), sep = "-to-") %>% 
  mutate(dest1 = dest0 + n -1,
         source1 = source0 + n -1) %>% 
  select(from, to, source0, source1, dest0, dest1)

# Part I
find_min_loc = function(seeds){
  minvalue = 1e10
  for (myvalue in seeds){
    for (mytype in unique(almanac$to)){
      df = almanac %>%
        filter(to == mytype) %>% 
        filter(myvalue >= source0 & myvalue <= source1)
      if (nrow(df) == 0){
        myvalue = myvalue
      }else{
        myvalue = df$dest0 + myvalue - df$source0
      }
      #print(myvalue)
      if (mytype == "location" & myvalue < minvalue){
        minvalue = myvalue
      }
    }
  }
  minvalue
}

find_min_loc(seeds)