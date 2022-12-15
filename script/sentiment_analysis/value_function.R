value_function <- function(data_text=NULL, data_token=NULL, id_column=NULL, value_column=NULL){
  data_value_afinn <- NULL
  data_value_sentimentr <- NULL
  
  if (!is.null(data_token) | !is.null(data_text)){
    if (!is.null(data_token)){
      # Get "afinn" sentiment value
      data_value_afinn <- inner_join(data_token, get_sentiments("afinn"),
                                  by = c("word" = "word")) %>%
        group_by(review_id = {{id_column}}) %>% 
        summarise(afinn_value = mean({{value_column}})) %>% 
        melt(id.vars = "review_id")
    }
    
    if (!is.null(data_text)){
      # Get "sentimentr" sentiment value
      data_value_sentimentr <- get_sentences(data_text) %>% 
        sentiment() %>% 
        group_by(review_id = element_id) %>% 
        summarise(sentimentr_value = mean(sentiment)) %>% 
        melt(id.vars = "review_id")
    }
    
    if (!is.null(data_value_afinn) & !is.null(data_value_sentimentr)){
      # Join the data into one table
      data_value <- rbind(data_value_afinn, data_value_sentimentr)
      return(data_value)
      
    } else if (!is.null(data_value_afinn)){
      return(data_value_afinn)
      
    } else if (!is.null(data_value_sentimentr)){
      return(data_value_sentimentr)
      
    } else {
      return(NULL)
    }
  }
}
