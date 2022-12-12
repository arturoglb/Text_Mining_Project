sentiment_function <- function(token_data, id_column, sentiment_column){
  sentiment_data <- inner_join(token_data, get_sentiments("nrc"),
                                by = c("word" = "word"))
  
  sentiment_matrix <- table(sentiment_data[[id_column]],
                                  sentiment_data[[sentiment_column]])
  return(sentiment_matrix)
}
