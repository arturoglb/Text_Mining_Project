source(here::here("script/setup.R"))
source(here::here("script/functions/sentiment_function.R"))
source(here::here("script/functions/value_function.R"))

# # This analysis doesn't use the tidyverse/quanteda package for sentiments. It
# # returns positive, negative and neg_pos values. We prefer working with more 
# # granular classification/valuation from tidytext ("nrc", "afinn") and
# # sentimentr packages

# # Read data
all_reviews <- read.csv(here::here("data/smartphone_reviews_final.csv"))
apple <- read.csv(here::here("data/Apple_final.csv"))
samsung <- read.csv(here::here("data/Samsung_final.csv"))

# # Unnest tokens
all_reviews_token <- all_reviews %>% 
  mutate(review_id = seq(1:nrow(all_reviews))) %>% 
  relocate(review_id, .before = "Brand") %>% 
  unnest_tokens(output = "word",
                input = "Reviews",
                to_lower = TRUE,
                strip_punct = TRUE,
                strip_numeric = TRUE)

apple_token <- apple %>% 
  mutate(review_id = seq(1:nrow(apple))) %>% 
  relocate(review_id, .before = "Brand") %>% 
  unnest_tokens(output = "word",
                input = "Reviews",
                to_lower = TRUE,
                strip_punct = TRUE,
                strip_numeric = TRUE)

samsung_token <- samsung %>% 
  mutate(review_id = seq(1:nrow(samsung))) %>% 
  relocate(review_id, .before = "Brand") %>% 
  unnest_tokens(output = "word",
                input = "Reviews",
                to_lower = TRUE,
                strip_punct = TRUE,
                strip_numeric = TRUE)

# # Get sentiment analysis: Apple vs. Samsung
# Sentiment based using "nrc" dictionary
for (i in list(apple_token, samsung_token)){
  # Perform the sentiment analysis
  sentiment_analysis <- sentiment_function(i, "review_id", "sentiment")
  
  # Print the plot
  print(ggplot(tibble(sentiment = names(colSums(sentiment_analysis)),
                      sum_value = colSums(sentiment_analysis)),
               aes(x = reorder(sentiment, -sum_value), y = sum_value)) +
          geom_col() +
          ggtitle("Sentiment analysis results (nrc dictionary)", subtitle = i$Brand[1]) +
          ylab("Number of tokens") +
          xlab("Sentiment"))
}

# Value based using "afinn" dictionary and "sentimentr"
index <- 1
data_list <- list(apple, samsung)
for (i in list(apple_token, samsung_token)){
  # Perform the sentiment value analysis
  value_analysis <- value_function(data_list[[index]]$Reviews, i, review_id, value)
  
  # Print the plot
  print(ggplot(value_analysis, aes(y = value, fill = variable)) +
          geom_boxplot() +
          ylab("Average sentiment value") +
          ggtitle("Sentiment analysis results (afinn dictionary and sentimentr)", subtitle=i$Brand[1]) +
          scale_y_continuous(breaks = seq(0,5,0.2), limits = c(0,5)) +
          guides(fill=guide_legend("Sentiment method")))
  
  # Increment index
  index <- index + 1
}

# Graph per model
# Perform to the sentiment and value analysis per model
for (i in unique(all_reviews$Model)){
  # Store model name
  model_name <- i

  # Keep data per model only
  model_data <- all_reviews %>%
    filter(Model == model_name)
  model_token <- all_reviews_token %>%
    filter(Model == model_name)

  # Store brand name
  brand_name <- model_data$Brand[1]

  # Perform the sentiment analysis
  sentiment_analysis <- sentiment_function(model_token, "review_id", "sentiment")

  # Print the plot
  print(ggplot(tibble(sentiment = names(colSums(sentiment_analysis)),
                      sum_value = colSums(sentiment_analysis)),
               aes(x = reorder(sentiment, -sum_value), y = sum_value)) +
          geom_col() +
          ggtitle("Sentiment analysis results (nrc dictionary)", subtitle = paste0(brand_name, ": ", model_name)) +
          ylab("Number of tokens") +
          xlab("Sentiment"))

  # Perform the sentiment value analysis
  value_analysis <- value_function(model_data$Reviews, model_token, review_id, value)

  # Print the plot
  print(ggplot(value_analysis, aes(y = value, fill = variable)) +
          geom_boxplot() +
          ylab("Average sentiment value") +
          ggtitle("Sentiment analysis results (afinn dictionary and sentimentr)", subtitle = paste0(brand_name, ": ", model_name)) +
          scale_y_continuous(breaks = seq(0,5,0.2), limits = c(0,5)) +
          guides(fill=guide_legend("Sentiment method")))+
    theme(axis.ticks.x = element_blank(),
          axis.text.x = element_blank())
}
