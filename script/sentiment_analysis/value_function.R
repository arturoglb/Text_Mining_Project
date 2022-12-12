value_function <- function(data_text, data_token, id_column, value_column){
  # Get "afinn" sentiment value
  data_value_afinn<- inner_join(data_token, get_sentiments("afinn"),
                              by = c("word" = "word")) %>%
    group_by(review_id = {{id_column}}) %>% 
    summarise(afinn_value = mean({{value_column}}))
  
  # Get "sentimentr" sentiment value
  data_value_sentimentr <- get_sentences(data_text) %>% 
    sentiment() %>% 
    group_by(review_id = element_id) %>% 
    summarise(sentimentr_value = mean(sentiment))
  
  # Join the data into one table
  data_value <- left_join(data_value_afinn, data_value_sentimentr,
                          by = c("review_id" = "review_id"))
  
  return(data_value)
}
