library(tidyverse)
library(unglue)

filename = "solutions/day19/input"
n_workflows = which(read_lines(filename) == "") -1

workflows_orig = read_lines(filename, n_max = n_workflows)
ratings_orig   = read_csv(filename, skip = n_workflows+1, col_names = c("x", "m", "a", "s")) |> 
  mutate_all(parse_number)

ratings = ratings_orig |> 
  rowid_to_column() |> 
  pivot_longer(-rowid, names_to = "part")


workflows = workflows_orig |> 
  str_replace("\\{", "[") |>
  str_replace("\\}", "]") |>
  unglue_data("{name}[{rules}]", convert = TRUE) |> 
  separate_longer_delim(rules, delim = ",") |> 
  separate(rules, into = c("condition", "outcome"), sep = ":", fill = "left") |> 
  mutate(comp = str_extract(condition, "<|>")) |> 
  separate(condition, into = c("part", "comp_value"), sep = "<|>", convert = TRUE)

# Part I
check_parts = function(myparts, myname = "in"){
  while (TRUE){
    myname = workflows |> 
      filter(name == myname) |> 
      left_join(myparts, by = join_by(part)) |> 
      rowwise() |> 
      mutate(rule = if_else(is.na(comp),
                              "TRUE",
                              paste0(value, comp, comp_value)),
      result = eval(parse(text = rule))) |> 
      ungroup() |> 
      filter(result) |> 
      slice(1) |> 
      pull(outcome)
    if (myname %in% c("A", "R")){
      return(myname)
    }
  }
}

tictoc::tic()
ratings |> 
  group_by(rowid) |> 
  nest() |> 
  filter(map_chr(data, ~check_parts(.x)) == "A") |> 
  ungroup() |> 
  unnest(data) |> 
  summarise(sum(value))
tictoc::toc()  
# 4.5 seconds, needs to be much more clever

# Part II