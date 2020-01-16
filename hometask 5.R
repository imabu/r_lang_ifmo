library(dplyr)

calculate_standart_value <- function(df_column_data, fun=mean) {
  calculated_value <- NULL
  class_of_data <- class(df_column_data)
  if (class_of_data == 'numeric') {
    calculated_value <- fun(df_column_data)
  } else if (class_of_data == 'character' || class_of_data == 'logical' || class_of_data == 'factor') {
    calculated_value <- table(df_column_data)
  } 
  calculated_value
}


explore_df <- function(dataframe, row_selector, column_selector, split_by=1, fun=mean) {
  dataframes_list <- dataframe[row_selector, column_selector] %>% 
            split(split_by)
  calculated_values <- dataframes_list %>% 
          lapply(function(x) lapply(x, function(y) calculate_standart_value(y, fun)))
  
  list(calculated_values, dataframes_list)
}

example_dataframe <- iris
result <- explore_df(example_dataframe, example_dataframe$Petal.Length > 1.4, split_by = example_dataframe$Species, fun=sum)
print(result[[1]])




