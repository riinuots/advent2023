library(tidyverse)
library(unglue)

filename = "solutions/day19/input"
n_workflows = which(read_lines(filename) == "") - 1

workflows_orig = read_lines(filename, n_max = n_workflows) |> 
  str_replace("\\{", "[") |>
  str_replace("\\}", "]") |>
  unglue_data("{name}[{rules}]", convert = TRUE)

ratings_orig   = read_csv(filename, skip = n_workflows+1, col_names = c("x", "m", "a", "s")) |> 
  mutate_all(parse_number)

ratings = ratings_orig |> 
  rowid_to_column() |> 
  pivot_longer(-rowid, names_to = "part")


workflows =  workflows_orig |> 
  separate_longer_delim(rules, delim = ",") |> 
  separate(rules, into = c("condition", "outcome"), sep = ":", fill = "left") |> 
  mutate(comp = str_extract(condition, "<|>")) |> 
  separate(condition, into = c("part", "comp_value"), sep = "<|>", convert = TRUE)

# Part I ----
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

# 2-4 seconds, needs to be much more clever

# Part II ----

# generate all paths that lead to A from in
paths = tibble(path = "A", to = "A")
myoutcomes = "A"
while (TRUE){
  paths = paths |>
    left_join(
      workflows |>
        filter(outcome %in% myoutcomes) |>
        distinct(to = outcome, from = name),
      by = join_by(to)
    ) |>
    mutate(path = if_else(is.na(from), path,
                          paste(path, from, sep = "-"))) |>
    select(path, to = from)
  
  myoutcomes = paths |>
    filter(to != "in") |>
    distinct(to) |>
    pull(to)
  if (all(length(myoutcomes) == 0)){
    paths = select(paths, -to)
    break
  }
}

# create pairs of steps within each path, e.g., in-px, px-A
steps = paths |> 
  rowid_to_column("path_id") |> 
  separate_longer_delim(path, delim = "-") |> 
  group_by(path_id) |> 
  mutate(order = seq_along(path_id)) |> 
  arrange(path_id, -order) |> 
  mutate(pair = paste(path, lead(path, default = "A"), sep = "-")) |> 
  filter(pair != "A-A") |> 
  select(-path) |> 
  mutate(order = seq_along(path_id)) |> 
  separate(pair, into = c("from", "to"), sep = "-")

# re-format input as needed for this solution
workflows2 = workflows_orig |> 
  separate(rules, into = c("rule1", "rule2", "rule3", "rule4"), sep = ",") |> 
  separate(rule1, into = c("rule1", "to1"), sep = ":") |> 
  separate(rule2, into = c("rule2", "to2"), sep = ":") |> 
  separate(rule3, into = c("rule3", "to3"), sep = ":") |> 
  separate(rule4, into = c("rule4", "to4"), sep = ":")

options = steps |> 
  left_join(workflows2,
            by = join_by(from == name),
            relationship = "many-to-many")

op_switches = c(">=", "<=")
names(op_switches) = c("<", ">(?!=)")

# figure out which rules must be applied, and which will be ignored
conditions = options |> 
  #filter(path_id %in% c(2, 3)) |> 
  mutate(result1 = if_else(to == to1, paste(rule1, "THIS"), str_replace_all(rule1, op_switches)),
         result2 = if_else(to == to2, paste(rule2, "THIS"), str_replace_all(rule2, op_switches)),
         result3 = if_else(to == to2, paste(rule3, "THIS"), str_replace_all(rule3, op_switches)),
         result4 = if_else(to == to2, paste(rule4, "THIS"), str_replace_all(rule4, op_switches))) |> 
  select(path_id, order, from, to, starts_with("result")) |> 
  pivot_longer(starts_with("result"), names_to = "rule_id", values_to = "condition") |> 
  drop_na() |> 
  group_by(path_id, order) |> 
  mutate(keep = case_when(all(! str_detect(condition, "THIS")) ~ 0,
                          str_detect(condition, "THIS") ~ 1,
                          .default = NA)) |>
  fill(keep, .direction = "down") |> 
  mutate(keep = if_else(is.na(keep), 0, keep)) |> 
  mutate(keep = cumsum(keep)) |> 
  filter(keep < 2) |> 
  mutate(condition = str_remove(condition, " THIS")) |> 
  ungroup() |> 
  select(-keep, -rule_id) |>
  filter(to != condition) |> 
  mutate(part = str_extract(condition, "^."), 
         condition = str_remove(condition, "^.")) |>
  pivot_wider(names_from = part, values_from = condition)


final = conditions |> 
  pivot_longer(all_of(c("a", "x", "s", "m"))) |> 
  mutate(value_min = case_when(is.na(value) ~ 1,
                               str_starts(value, "<") ~ 1,
                               str_starts(value, ">=") ~ parse_number(value),
                               str_starts(value, ">") ~ parse_number(value) + 1,
                               .default = NA),
         value_max = case_when(is.na(value) ~ 4000,
                               str_starts(value, ">") ~ 4000,
                               str_starts(value, "<=") ~ parse_number(value),
                               str_starts(value, "<") ~ parse_number(value) - 1,
                               .default = NA)) |> 
  group_by(path_id, name) |> 
  summarise(value_min = max(value_min),
            value_max = min(value_max)) |> 
  mutate(n = value_max - value_min + 1)

# wrong
total = final |> 
  group_by(path_id) |> 
  summarise(comb = prod(n)) |> 
  summarise(total = sum(comb)) |> 
  pull(total)

options("scipen"=100, "digits"=4)
total/167409079868000


