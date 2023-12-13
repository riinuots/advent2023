# input wrangling
input_file = "solutions/day13/input"
input_full = readLines(input_file)
file_breaks = which(input_full == "")
ranges = tibble::tibble(start  = c(1, file_breaks + 1),
                    end    = c(file_breaks, length(input_full) + 1),
                    width = nchar(input_full[start]))

input = ranges |> 
  dplyr::rowwise() |> 
  dplyr::mutate(m = list(read.fwf(input_file,
                      widths = rep(1, width),
                      comment.char = "",
                      skip = start - 1,
                      n = end-start)))

# functions to solve both parts
find_col = function(col_splits, m, ncols, smudge = 0){
  for (y in col_splits){
    len_left = y-1
    len_right = ncols - y + 1
    len =  min(len_left, len_right)
    
    m1 = m[, (y-1):(y-len)]
    m2 = m[, y:(y+len-1)]
    if (sum(m1 != m2) == smudge){
      return(y-1)
    }
  }
}

find_row = function(row_splits, m, nrows, smudge = 0){
  for (x in row_splits){
    len_left = x - 1
    len_right = nrows - x + 1
    len =  min(len_left, len_right)
    
    m1 = m[(x-1):(x-len), ]
    m2 = m[x:(x+len-1), ]
    if (sum(m1 != m2) == smudge){
      return(100*(x-1))
    }
  }
}

pattern_result = function(m, smudge = 0){
  ncols = ncol(m)
  nrows = nrow(m)
  col_splits = c(2:ncols)
  row_splits = c(2:nrows)
  
  split_col = find_col(col_splits, m, ncols, smudge)
  if(!is.null(split_col)){
    return(split_col)
  } else{
    return(find_row(row_splits, m, nrows, smudge))
  }
}

# Part I
purrr::map_dbl(input$m, ~pattern_result(.x)) |> sum()

# Part II
purrr::map_dbl(input$m, ~pattern_result(.x, smudge = 1)) |> sum()
