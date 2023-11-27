# R functions 

# function to show Variable Name, Col Index, and class for data frame. Adapted from stack overflow. 
custom_view = function(df){
  data.frame(
    col_name = colnames(df),
    col_index = 1:ncol(df),
    col_class = sapply(df, class),
    row.names = NULL
  )
}