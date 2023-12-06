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


# histograms, box plots, bar charts(cat data)
cust_barplots = function(df, colNames){
  for(i in colNames){
    # barplots % of observations 
    barplot(prop.table(table(df[i])), xlab = "categories", ylab = "% observations", main = as.character(i))
    # barplots with sum counts 
    print(ggplot(df) + geom_bar(aes_string(x = i)) + ggtitle(as.character(i)) +
            xlab("categories") +
            theme(plot.title = element_text(hjust = 0.5)))
  }
}

# plots for continuous variables
cust_hist = function(df, colNames){
  for(i in colNames){
    hist(prop.table(table(df[i])), xlab = "numerical value", main = as.character(i), breaks = 10) # change breaks for bins
  }
}

get_lower_tri <- function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}

get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)] <- NA
  return(cormat)
}