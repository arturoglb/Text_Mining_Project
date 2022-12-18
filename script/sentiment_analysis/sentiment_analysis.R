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

# # Get sentiment per review
sentimentr_per_review <- get_sentences(all_reviews) %>% 
  sentiment() %>% 
  group_by(review_id = element_id, Brand, Model) %>%
  summarise(sentimentr_value = mean(sentiment)) %>% 
  ungroup()

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
# Valence shifter: https://www.r-bloggers.com/2020/04/sentiment-analysis-in-r-with-sentimentr-that-handles-negation-valence-shifters/
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

# # Facet graph of all models
# Perform sentiment analysis per model (nrc dictionary)
sentiment_per_model <- all_reviews_token %>%
  group_by(Model) %>%
  do(data.frame(val=sentiment_function(., "review_id", "sentiment"))) %>% 
  group_by(Model, val.Var2) %>% 
  summarise(sum_value = sum(val.Freq))

ggplot(sentiment_per_model, aes(x = reorder(val.Var2, -sum_value), y = sum_value)) +
  geom_col() +
  ggtitle("Sentiment analysis results (nrc dictionary)", subtitle = "Per model") +
  ylab("Number of tokens") +
  xlab("Sentiment") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5),
        strip.text = element_text(size=7)) +
  facet_wrap(vars(Model), scales = "free_y")

# Perform the sentiment value analysis per model (afinn dictionary and sentimentr)
afinn_per_model <- all_reviews_token %>% 
  group_by(Model) %>% 
  do(data.frame(val=value_function(data_token=., id_column = review_id, value_column = value)))

sentimentr_per_model <- all_reviews %>%
  group_by(Model) %>%
  do(data.frame(val=value_function(data_text=.))) 

# Rbind afinn_per_model and sentimentr_per_model
sentiment_value_per_model <- rbind(afinn_per_model, sentimentr_per_model)

# Add count of reviews to be displayed as subtitle in facet_wrap graphs
sentiment_value_per_model <- sentiment_value_per_model %>%
  group_by(Model) %>%
  mutate(review_count = n()) %>%
  ungroup() %>%
  unite(Model, c(Model, review_count), sep='\n')

# Print the plot (afinn dictionary)
ggplot(sentiment_value_per_model %>% filter(val.variable == "afinn_value"), aes(y = val.value)) +
  geom_boxplot() +
  ylab("Average sentiment value") +
  ggtitle("Sentiment analysis results (afinn dictionary)", subtitle = "Per model (+ number of reviews") +
  scale_y_continuous(breaks = seq(-5,5,1), limits = c(-5,5)) +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        strip.text = element_text(size=7)) +
  facet_wrap(vars(Model))

# Print the plot (sentimentr package)
ggplot(sentiment_value_per_model %>% filter(val.variable == "sentimentr_value"), aes(y = val.value)) +
  geom_boxplot() +
  ylab("Average sentiment value") +
  ggtitle("Sentiment analysis results (sentimentr package)", subtitle = "Per model (+ number of reviews") +
  scale_y_continuous(breaks = seq(-3,3,1), limits = c(-3,3)) +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        strip.text = element_text(size=7)) +
  facet_wrap(vars(Model))

# Print the plot (sentimentr package) (zoomed in)
ggplot(sentiment_value_per_model %>% filter(val.variable == "sentimentr_value"), aes(y = val.value)) +
  geom_boxplot() +
  ylab("Average sentiment value") +
  ggtitle("Sentiment analysis results zoomed in (sentimentr package)", subtitle = "Per model (+ number of reviews") +
  scale_y_continuous(breaks = seq(-1,1,0.3), limits = c(-1,1)) +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        strip.text = element_text(size=7)) +
  facet_wrap(vars(Model))

# Analysis per base/pro/pro max for Apple phones
sentimentr_apple_models <- all_reviews %>%
  filter(Brand == "Apple") %>% 
  mutate(Type = case_when(
    str_detect(Model, "Max") ~ "Pro Max",
    str_detect(Model, "Pro") ~ "Pro",
    TRUE ~ "Base"
  )) %>% 
  group_by(Type) %>%
  do(data.frame(val=value_function(data_text=.))) 

# Add count of reviews to be displayed as subtitle in facet_wrap graphs
sentimentr_apple_models <- sentimentr_apple_models %>%
  group_by(Type) %>%
  mutate(review_count = n()) %>%
  ungroup() %>%
  unite(Type, c(Type, review_count), sep='\n')

ggplot(sentimentr_apple_models, aes(y = val.value)) +
  geom_boxplot() +
  ylab("Average sentiment value") +
  ggtitle("Sentiment analysis results (sentimentr package)", subtitle = "Apple model types (+ number of reviews)") +
  scale_y_continuous(breaks = seq(-3,3,0.3), limits = c(-3,3)) +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        strip.text = element_text(size=7)) +
  facet_wrap(vars(Type))

# Save sentimentr value for each review in a dataset
write.csv(sentimentr_per_review, "script/sentiment_analysis/sentiment_data/sentimentr_per_review.csv")
